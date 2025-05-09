# Elk Sightability Analysis
# 0 SETUP ---------------------------------------------------------------------

# get script start time
start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# get uploaded file
file_path <- paste0(commandArgs(trailingOnly = TRUE))
# file_path <- "C:/Users/teb31/Downloads/sightability_template (2025).xlsx"
# file_path <- "input/sightability_WCR_elk_2020-2024_May29_2024.xlsx"

runModel <- function(file_path) {
  # set CRAN repo
  file.edit(".Rprofile")
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  
  # record that script is running
  sink("running.txt")
  cat("TRUE")
  sink()
  
  # record start time
  sink("progress.txt")
  cat("Start time:", start_time, "\n")
  cat("\n")
  sink()
  
  # get ready to catch errors
  sink("error.txt")
  tryCatch({
    
    # 1 LOAD AND CLEAN ------------------------------------------------------------
    
    ## 1.1 Load packages ####
    
    library(readxl)
    library(tidyverse)
    library(R2jags)
    library(runjags)
    
    wd <- getwd()
    
    ## 1.2 Build functions ####
    
    # Name_fixer converts misspelled or abbreviated EPU names to standard names
    name_fixer <- function(EPU_list, EPU_string) {
      output <- character(length(EPU_string))
      for (i in seq_along(EPU_string)) {
        for (h in seq_along(EPU_list$abbr)) {
          if (str_detect(EPU_string[i], regex(EPU_list$abbr[h], ignore_case = T))) {
            output[i] <- EPU_list$EPU[h]
            break
          }
        }
      }
      return(output)
    }
    
    # Standard_survey standardizes survey types
    standard_survey <- function(survey_string) {
      output <- case_when(
        grepl("incident", survey_string, ignore.case = TRUE) ~ "Incidental",
        grepl("telem", survey_string, ignore.case = TRUE) ~ "Telemetry",
        grepl("trans", survey_string, ignore.case = TRUE) ~ "Inventory",
        grepl("invent", survey_string, ignore.case = TRUE) ~ "Inventory",
        grepl("capt", survey_string, ignore.case = TRUE) ~ "Capture",
        TRUE ~ "Other"
      )
      return(output)
    }
    
    # compile_sheets binds all sheets in a file (filepath) that follow a certain naming patter (type)
    # E.g. to bind survey data sheets from all years, type should be "Data"
    compile_sheets <- function(filepath, type) {
      sheets <- excel_sheets(filepath)
      sheets <- subset(sheets, str_detect(sheets, paste0(type)) == T)
      output <- data.frame(matrix(ncol = 0, nrow = 0))
      for (i in 1:length(sheets))
      {
        output <-
          bind_rows(output, read_excel(filepath, sheet = paste0(sheets[i])))
      }
      return(output)
    }
    
    # oper.datify formats obs into oper.dat
    oper.datify <- function(df) {
      transmute(
        .data = df,
        x = voc,
        ym1 = y - 1,
        h = as.double(h),
        q = 1,
        z = 1,
        yr = year.ID,
        subunits = as.double(subunits),
        survey.type = NULL
      )
    }
    
    # augment adds augmented records to obs
    augment <- function(df) {
      aug <- df %>%
        # need to determine max annual m of each h
        group_by(yr, h, subunits) %>%
        summarize(m = n()) %>%
        ungroup() %>%
        group_by(h) %>%
        reframe(
          yr = yr,
          subunits = subunits,
          m = m,
          m.max = max(m)
        ) %>%
        ungroup() %>%
        mutate(b = 2*m.max) %>%
        # b needs to be greater than the max number of groups in the population -> add 10 to b if b<10
        mutate(b = if_else(b<10, b+10, b),
               aug = b - m)
      # create augmented dataframe
      oper.dat.aug <- aug[rep(1:nrow(aug), aug$aug), ] %>%
        mutate(
          x = NA,
          ym1 = NA,
          h = h,
          q = NA,
          z = 0,
          yr = yr,
          subunits = subunits,
          .keep = "none"
        ) %>%
        ungroup()
      # combine dataframes
      output <- rbind(df, oper.dat.aug) %>%
        arrange(yr, subunits, h, desc(z))
      return(output)
    }
    
    # plot.datify formats oper.dat for plot.dat
    plot.datify <- function(df) {
      df %>%
        select(yr, h, subunits) %>%
        distinct() %>%
        mutate(
          h.plots = h,
          yr.plots = yr,
          subunits.plots = subunits
        ) %>%
        select(h.plots, yr.plots, subunits.plots) %>%
        arrange(yr.plots, subunits.plots, h.plots)
    }
    
    # scalar.datify creates the scalar.dat file from oper.dat, plot.dat, and sight.dat
    scalar.datify <- function(operdat, plotdat, sightdat) {
      if (!is.null(plotdat$h.plots)) {
      output <- as.data.frame(matrix(NA, 1, (nrow(plotdat))))
      i <- 1
      for (i in 1:nrow(plotdat)) {
        output[, i] <-
          as.double(nrow(
            operdat %>% filter(
              yr == plotdat$yr.plots[i],
              h == plotdat$h.plots[i],
              subunits == plotdat$subunits.plots[i]
            )
          ))
        colnames(output)[i] <-
          paste0(
            "h",
            plotdat$h.plots[i],
            "y",
            plotdat$yr.plots[i],
            "sub",
            plotdat$subunits.plots[i]
          )
      }
      } else {
        output <- as.data.frame(matrix(NA, 1, (nrow(plotdat))))
        i <- 1
        for (i in 1:nrow(plotdat)) {
          output[, i] <-
            as.double(nrow(
              operdat %>% filter(
                yr == plotdat$yr.plots[i],
                subunits == plotdat$subunits.plots[i]
              )
            ))
          colnames(output)[i] <-
            paste0(
              "y",
              plotdat$yr.plots[i],
              "sub",
              plotdat$subunits.plots[i]
            )
        }
      }
      
      output <- output %>%
        mutate(
          R = as.double(nrow(sightdat)),
          Ngroups = as.double(nrow(operdat)),
          Nsubunits.yr = as.double(nrow(plotdat))
        )
      return(output)
    }
    
    # scalar.sumsify creates the scalar.sums file from plot.dat and scalar.dat
    scalar.sumsify <- function(plotdat, scalardat) {
      # Create scalar.sums to ease modelling
      # tells us how many rows belong to each year/stratum/subunit combo
      output <- matrix(NA, nrow(plotdat), 2)
      for (i in 1:nrow(plotdat)) {
        t <- i - 1
        output[i, 1] <- sum(scalardat[, 0:t], 1)
        output[i, 2] <- sum(scalardat[, 0:i])
      }
      return(output)
    }
    
    # rjags_to_table turns the rjags object into a readable table
    rjags_to_table <-
      function(jagsoutput,
               scalardat,
               totalscalardat,
               year_list,
               EPU.ID,
               stratum_list) {
        jags.summary <- as.data.frame(jagsoutput$BUGSoutput$summary)
        
        tau.jags <- matrix(NA, (nrow(jags.summary) - 3), 11)
        tau.jags <- as.data.frame(tau.jags)
        tau.jags[, 1] <-
          c(as.numeric(str_extract(colnames(scalardat)[1:length(jagsoutput$BUGSoutput$median$tau.hat)], "(?<=sub)[:digit:]{1,2}")),
            as.numeric(str_extract(colnames(totalscalardat)[1:length(jagsoutput$BUGSoutput$median$total.tau.hat)], "(?<=sub)[:digit:]{1,2}")))
        tau.jags[, 2] <-
          c(as.numeric(str_extract(colnames(scalardat)[1:length(jagsoutput$BUGSoutput$median$tau.hat)], "(?<=h)[:digit:]{1,2}")),
            rep_len(0, length(jagsoutput$BUGSoutput$median$total.tau.hat)))
        tau.jags[, 3] <-
          c(as.numeric(str_extract(colnames(scalardat)[1:length(jagsoutput$BUGSoutput$median$tau.hat)], "(?<=y)[:digit:]{1,2}")),
            as.numeric(str_extract(colnames(totalscalardat)[1:length(jagsoutput$BUGSoutput$median$total.tau.hat)], "(?<=y)[:digit:]{1,2}")))
        tau.jags[, 4] <- round(jags.summary$`50%`[4:nrow(jags.summary)])
        tau.jags[, 5] <- round(jags.summary$`2.5%`[4:nrow(jags.summary)])
        tau.jags[, 6] <- round(jags.summary$`97.5%`[4:nrow(jags.summary)])
        tau.jags[, 7] <- round(jags.summary$`25%`[4:nrow(jags.summary)])
        tau.jags[, 8] <- round(jags.summary$`75%`[4:nrow(jags.summary)])
        tau.jags[, 9] <- round(Rhat[4:nrow(jags.summary)], 3)
        tau.jags[, 10] <-
          round(jags.summary$sd[4:nrow(jags.summary)] / jags.summary$mean[4:nrow(jags.summary)], 3)
        tau.jags[, 11] <-
          as.numeric(n.eff[4:nrow(jags.summary)])
        
        colnames(tau.jags) <-
          c(
            "subunit.ID",
            "stratum.ID",
            "year.ID",
            "Model",
            "lcl_95",
            "ucl_95",
            "lcl_50",
            "ucl_50",
            "Rhat",
            "cv",
            "n.eff"
          )
        output <- left_join(tau.jags, year_list, by = "year.ID") %>%
          left_join(EPU.ID, by = c("subunit.ID" = "ID")) %>%
          left_join(stratum_list, by = c("stratum.ID" = "h")) %>%
          mutate(stratum = if_else(stratum.ID==0, "total", stratum)) %>%
          select(-year.ID,-stratum.ID,-subunit.ID) %>%
          select(year, EPU, stratum, Model:n.eff)
        
        return(output)
      }
    
    ## 1.3 Load data ####
    
    # Save EPU names from reliable source
    EPU_list <- read_excel(file_path, sheet = "EPU_list") %>%
      # fill in abbreviations as full names when missing
      mutate(abbr = if_else(is.na(abbr), EPU, abbr))
    EPU_names <- unique(EPU_list$EPU)
    
    # Extract observations from all years
    # If you didn't name your survey data sheets with "Data", replace below
    obs.all <- compile_sheets(file_path, "\\d{4} Data")
    # warning when there are duplicate rows (suggesting more than one data sheet per year)
    # if(sum(duplicated(obs.all)) > 20*length(unique(obs.all$year))){
    #   cat("Warning: ", sum(duplicated(obs.all)), " duplicate rows in dataset. Make sure you only have one `Data` sheet per year.\n")
    # }
    
    # fix EPU names
    obs.all$EPU <- name_fixer(EPU_list, obs.all$EPU)
    # warning when a bunch of EPUs are not in EPU_list
    # if(nrow(obs.all[obs.all$EPU=="",]) > 5*length(unique(obs.all$year))){
    #   stop(nrow(obs.all[obs.all$EPU=="",]), " observations have EPU names not listed in EPU_list. Is you EPU_list complete?\n")
    # }
    # fix survey types
    obs.all$survey.type <- standard_survey(obs.all$survey.type)
    
    # Bring in summary data from each year
    # This acts as a list of all the EPUs that were properly surveyed that year
    eff <- compile_sheets(file_path, "\\d{4} Summary") %>%
      filter(!is.na(min_count))
    
    # throw an error if there are an unequal amount of data and summary sheets
    if(length(setdiff(unique(obs.all$year), unique(eff$year))) > 0){
      stop("Data or summary sheet missing for ", setdiff(unique(obs.all$year), unique(eff$year))[1], ". Make sure you include both sheets for each year.")
    }

    # put eff through name fixer
    eff$EPU <- name_fixer(EPU_list, eff$EPU)
    # throw an error if it leaves more than one EPU name empty (will mess up reporting)
    if(nrow(eff[eff$EPU=="",]) > 1){
      stop("Not all EPUs listed in summary sheets are represented in EPU_list sheet. Please check your EPU_list.\n")
    }
    
    ## 1.4 Sightability dataset ####
    
    # we will have one sightability dataframe for all sexes/ages, which means we're assuming
    # equal sightability of bulls, cows, and calves -> may not be true.
    # cows/bulls could be stratified in the future but for now we don't have enough collared bulls
    
    sight <- obs.all %>%
      # we'll create a field to tell us whether each year counts toward sightability data
      group_by(year) %>%
      mutate(sight = if_else(all(
        c("Inventory", "Telemetry") %in% unique(survey.type)
      ), TRUE, FALSE)) %>%
      # we only want inventory and telemetry obs to keep things simple
      filter(survey.type == "Telemetry" | survey.type == "Inventory",
             # only keep years where sightability trials were done
             sight == TRUE,
             # we only want observations with collars
             collars > 0,
             # remove any rows that didn't pass the name check
             EPU != "")
    
    # if there is no sightability data, throw an error
    if (nrow(sight) == 0) {
      stop("No sightability data detected. Make sure you have inventory AND telemetry observations in at least one year.\n", call.=F, domain = NA)
    }
    
    # duplicate observations with >1 collars
    sight.dup <- head(sight, 0)
    
    for (i in seq_along(sight$collars)) {
      if (sight$collars[i] > 1) {
        for (k in 2:sight$collars[i]) {
          sight.dup <- rbind(sight.dup, sight[i,])
        }
      }
    }
    
    if (nrow(sight.dup) > 0) {
      sight <- bind_rows(sight, sight.dup)
    }
    
    ## 1.5 Observational dataset ####
    
    # make sure we're only keeping data from EPUs with summary data that year
    obs <-
      inner_join(obs.all, eff %>% select(year, EPU), by = c("EPU", "year")) %>%
      mutate(
        subunit = EPU,
        # make sure there are no NAs in observation counts
        mutate(across(
          c(cow, calf, spike, bull, UC, total), ~ if_else(is.na(.), 0, .)
        )),
        voc = voc,
        .keep = "unused"
      ) %>%
      # remove incidental observations
      filter(survey.type == "Inventory" |
               survey.type == "Capture" | survey.type == "Telemetry") %>%
      # then turn any "capture" to "inventory"
      mutate(survey.type = if_else(survey.type == "Capture", "Inventory", survey.type))
    
    # make sure all totals = sum of cows, calves, etc
    
    obs <- obs %>%
      mutate(
        UC = if_else(
          total > (cow + calf + spike + bull + UC),
          as.numeric(total - (cow + calf + spike + bull)),
          as.numeric(UC)
        ),
        total = as.numeric(cow + calf + spike + bull + UC)
      )
    
    ### 1.5.1 Observed dataset ####
    
    observed <- obs %>%
      rename("EPU" = "subunit") %>%
      group_by(year, EPU) %>%
      summarize(cows_observed = sum(cow)-sum(spike),
                bulls_observed = sum(bull),
                yearlings_observed = sum(spike)*2,
                calves_observed = sum(calf),
                unclassified_observed = sum(UC),
                total_observed = sum(total))
    
    ## 1.6 Effort ####
    
    # Amend EPU.list to only include surveyed EPUs, then assign ID numbers
    EPU.ID <- data.frame(EPU = unique(obs$subunit)) %>%
      mutate(ID = seq(1, length(EPU), 1))
    
    eff <- inner_join(eff, EPU.ID, by = "EPU")
    
    # Add ID to obs
    
    # convert sex/age to stratum
    obs <- obs %>%
      pivot_longer(cow:total, names_to = "stratum", values_to = "y") %>%
      filter(y > 0,
             stratum != "total")
    
    stratum.ID <- obs %>%
      select(stratum) %>%
      distinct()
    stratum.ID$h <- seq(1:nrow(stratum.ID))
    
    obs <- obs %>%
      left_join(stratum.ID, by = "stratum")
    
    obs <-
      inner_join(obs, eff %>% select(year, EPU, ID), by = c(c("subunit" = "EPU"), "year")) %>%
      mutate(subunits = ID,
             .keep = "unused")
    
    ## 1.7 Telem adjustment ####
    #including telemetry obs helps some EPUs but hurts others -> depends on effect on average group size
    
    group <- obs %>%
      group_by(subunit, stratum) %>%
      summarize(avg_group = mean(y))
    
    # if keeping telems brings avg groupsize closer to EPU's total average, then keep them
    telem.stats <- obs %>%
      ungroup() %>%
      group_by(year, subunit, stratum, survey.type) %>%
      summarize(n = n(),
                count = sum(y)) %>%
      ungroup() %>%
      pivot_wider(names_from = survey.type,
                  values_from = c(n, count)) %>%
      mutate(
        n_telem = replace_na(n_Telemetry, 0),
        n_nontelem = replace_na(n_Inventory, 0),
        count_telem = replace_na(count_Telemetry, 0),
        count_nontelem = replace_na(count_Inventory, 0),
        .keep = "unused"
      ) %>%
      filter(count_telem > 0) %>%
      mutate(
        avg_group_nontelem = (count_nontelem / n_nontelem),
        avg_group_telem = (count_telem + count_nontelem) / (n_telem + n_nontelem)
      ) %>%
      left_join(group, by = c("subunit", "stratum")) %>%
      mutate(keep_telem = if_else(
        abs(avg_group_telem - avg_group) >= abs(avg_group_nontelem - avg_group),
        F,
        T
      ))
    
    # add telem direction to obs
    obs <-
      left_join(
        obs,
        telem.stats %>% select(year, subunit, stratum, keep_telem),
        by = c("year", "subunit", "stratum")
      ) %>%
      filter(!(survey.type == "Telemetry" & keep_telem == F))
    
    # 2 PREPARE DATA -------------------------------------------------------------
    
    ## 2.1 Sight.dat ####
    
    # s = habitat indicator (Mature foest, young forest, non-forest)
    # x = visual obstruction measurements associated with the test trial data used to develop the sightability model
    # a = activity indicator (bedded, standing/moving)
    # z = detection indicator (1 if the group was observed, 0 otherwise)
    # t = group size
    
    sight.dat <- sight %>%
      mutate(observed = as.integer(if_else(survey.type == "Inventory", 1, 0)),
             grpsize = as.integer(total))  %>%
      # standardize habitat
      mutate(
        # HABITAT KEY WORDS
        # 1 - rock / meadow / riparian (field, meadow, riparian, wetland, river)
        # 2 - cutblock / powerline (block, powerline, NSR, FTG)
        # 3 - mature forest (mature, old)
        habitat = case_when(
          grepl("mature|old|conifer", habitat, ignore.case = TRUE) ~ "Mature Forest",
          grepl("block|powerline|nsr|ftg", habitat, ignore.case = TRUE) ~ "Young Forest",
          grepl(
            "field|meadow|riparian|wetland|river|gravel|landfill|road|wtp|other|slide",
            habitat,
            ignore.case = TRUE
          ) ~ "No Forest",
        ),
        # standardize activity
        activity = case_when(
          grepl("standing|moving|run", activity, ignore.case = TRUE) ~ "Standing/Moving",
          grepl("bed", activity, ignore.case = TRUE) ~ "Bedded"
        ),
        a = as.factor(activity),
        s = factor(habitat, levels = c("No Forest", "Young Forest", "Mature Forest")),
        t = as.double(grpsize),
        x.tilde = as.double(voc),
        z.tilde = as.double(observed)
      ) %>%
      select(a, s, t, x.tilde, z.tilde)
    
    ### 2.1.1 test correlations ####
    # UNCOMMENT BELOW IF YOU WANT TO TEST THE CORRELATION OF GROUP SIZE, HABITAT, ACTIVITY, VOC WITH SIGHTABILITY
    # cor.sum <- sight.dat %>%
    #   group_by(z.tilde) %>%
    #   summarize(median.x = median(x.tilde, na.rm=T), mean.x = mean(x.tilde, na.rm=T), sd.x = sd(x.tilde, na.rm=T), se.x = sd(x.tilde, na.rm=T)/sqrt(length(x.tilde[!is.na(x.tilde)])),
    #             median.t = median(t, na.rm=T), mean.t = mean(t, na.rm=T), sd.t = sd(t, na.rm=T), se.t = sd(t, na.rm = T)/sqrt(length(t[!is.na(t)])))
    #
    # # VOC plot
    # ggplot(sight.dat %>% filter(!is.na(x.tilde)) %>% rename ("Visual Obstruction" = x.tilde) %>% mutate(z.tilde = if_else(z.tilde==1, "Seen", "Missed")), aes(as.factor(z.tilde), `Visual Obstruction`)) +
    #   geom_boxplot(aes(fill = z.tilde)) +
    #   stat_summary(fun = "mean", geom = "point", shape = 8, size = 3)+
    #   ylim(c(0,1)) +
    #   scale_fill_brewer(palette = "Paired") +
    #   xlab("") +
    #   theme_classic() +
    #   theme(legend.position="none", 
    #         axis.title.y = element_text(size = 14, margin = margin(0,10,0,0)),
    #         axis.text.y = element_text(size = 12),
    #         axis.text.x = element_text(size = 14, color="black"))
    # 
    # # Group size plot
    # ggplot(sight.dat %>% filter(!is.na(t)) %>% rename ("Group size" = t) %>% mutate(z.tilde = if_else(z.tilde==1, "Seen", "Missed")), aes(as.factor(z.tilde), `Group size`)) +
    #   geom_boxplot(aes(fill = z.tilde)) +
    #   stat_summary(fun = "mean", geom = "point", shape = 8, size = 3)+
    #   ylim(c(0,50)) +
    #   xlab("") +
    #   scale_fill_brewer(palette = "Paired") +
    #   theme_classic() +
    #   theme(legend.position="none", 
    #         axis.title.y = element_text(size = 14, margin = margin(0,10,0,0)),
    #         axis.text.y = element_text(size = 12),
    #         axis.text.x = element_text(size = 14, color="black"))
    # 
    # # Habitat plot
    # 
    # sight.prop <- sight.dat %>%
    #   filter(!is.na(s)) %>%
    #   group_by(z.tilde) %>%
    #   summarize(n.z = n())
    # sight.prop.s <- sight.dat %>%
    #   filter(!is.na(s)) %>%
    #   group_by(z.tilde, s) %>%
    #   summarize(n.s = n()) %>%
    #   inner_join(sight.prop, by="z.tilde") %>%
    #   mutate(prop = n.s/n.z,
    #          z.tilde = if_else(z.tilde == 1, "Seen", "Missed")) %>%
    #   rename("Habitat" = s)
    #
    # ggplot(sight.prop.s, mapping = aes(as.factor(z.tilde), prop)) +
    #   geom_col(aes(fill = Habitat),
    #            position = "dodge",
    #            color = "grey30") +
    #   scale_y_continuous(labels = scales::percent) +
    #   labs(x = "", y = "Percentage of Observations") +
    #   scale_fill_brewer(palette = "Greens") +
    #   theme_classic() +
    #   theme(
    #     axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
    #     axis.text.y = element_text(size = 12),
    #     axis.text.x = element_text(size = 14, color = "black"),
    #     legend.text = element_text(size = 12, color = "grey30"),
    #     legend.title = element_text(size = 14)
    #   )
    # 
    # # Activity plot
    # 
    # sight.prop <- sight.dat %>%
    #   filter(!is.na(a)) %>%
    #   group_by(z.tilde) %>%
    #   summarize(n.z = n())
    # sight.prop.a <- sight.dat %>%
    #   filter(!is.na(a)) %>%
    #   group_by(z.tilde, a) %>%
    #   summarize(n.a = n()) %>%
    #   inner_join(sight.prop, by="z.tilde") %>%
    #   mutate(prop = n.a/n.z,
    #          z.tilde = if_else(z.tilde == 1, "Seen", "Missed")) %>%
    #   rename("Activity" = a)
    # 
    # ggplot(sight.prop.a, mapping = aes(as.factor(z.tilde), prop)) +
    #   geom_col(aes(fill=Activity), color = "grey30", position = "dodge") +
    #   scale_y_continuous(labels = scales::percent) +
    #   labs(x = "", y = "Percentage of Observations") +
    #   theme_classic() +
    #   scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    #   theme(axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
    #         axis.text.y = element_text(size = 12),
    #         axis.text.x = element_text(size = 14, color = "black"),
    #         legend.text = element_text(size = 12, color = "grey30"),
    #         legend.title = element_text(size = 14))
    # 
    # x.z <- t.test(sight.dat$x.tilde[sight.dat$z.tilde==0], sight.dat$x.tilde[sight.dat$z.tilde==1])
    # t.z <- t.test(sight.dat$t[sight.dat$z.tilde==0 & !is.na(sight.dat$t)], sight.dat$t[sight.dat$z.tilde==1 & !is.na(sight.dat$t)])
    # a.z <- chisq.test(table(sight.dat$a, sight.dat$z.tilde))
    # s.z <- chisq.test(table(sight.dat$s, sight.dat$z.tilde))
    # 
    # Correlation <- as.data.frame(matrix(NA, 4, 5))
    # Correlation[1,] <- c("VOC", x.z$method, x.z$statistic, x.z$parameter, x.z$p.value)
    # Correlation[2,] <- c("Group size", t.z$method, t.z$statistic, t.z$parameter, t.z$p.value)
    # Correlation[3,] <- c("Activity", a.z$method, a.z$statistic, a.z$parameter, a.z$p.value)
    # Correlation[4,] <- c("Habitat", s.z$method, s.z$statistic, s.z$parameter, s.z$p.value)
    # colnames(Correlation) <- c("Variable", "Method", "Statistic", "DF", "p")
    # 
    # write.csv(Correlation, "C:/Users/TBRUSH/R/SightabilityModels/output/Correlation.csv", row.names = FALSE)
    # write.csv(sight.dat, "C:/Users/TBRUSH/R/SightabilityModels/output/Sightability_2023.csv", row.names = FALSE)

    ### 2.1.2 finish sight.dat ####
    # voc is the only factor significantly correlated with sightability -> select only voc
    sight.dat <- sight.dat %>% select(x.tilde, z.tilde) %>%
      filter(!is.na(x.tilde)) %>%
      ungroup()
    
    ## 2.2 Oper.dat ####
    
    ### 2.2.1 non-augmented data ####
    
    # Get year ID
    year.ID <- as.data.frame(matrix(NA, length(unique(obs$year)), 2))
    colnames(year.ID) <- c("year", "year.ID")
    
    year.ID[, 1] <- unique(obs$year) %>% sort()
    year.ID[, 2] <- seq(1, length(unique(obs$year)))
    
    # join to oper.dat
    oper.dat <- left_join(obs, year.ID, by = "year")
    
    # organize non-augmented data
    oper.dat <- oper.dat %>%
      # model won't accept voc = 0 or 1, fix below
      mutate(voc = if_else(voc == 1, 0.99,
                           if_else(voc == 0, 0.01, voc)))
    
    oper.dat <- oper.dat %>%
      oper.datify()
    
    ### 2.2.2 augmented data ####
    
    oper.dat <- oper.dat %>%
      augment()
    
    ## 2.3 Plot.dat ####
    
    plot.dat <- oper.dat %>%
      plot.datify()
    
    total.plot.dat <- plot.dat %>%
      select(-h.plots) %>%
      distinct()
    
    ## 2.4 Scalar.dat and sums ####
    scalar.dat <- scalar.datify(oper.dat, plot.dat, sight.dat)
    total.scalar.dat <- suppressWarnings(scalar.datify(oper.dat, total.plot.dat, sight.dat))
    
    # Create scalar.sums to ease modelling
    # tells us how many rows belong to each year/stratum combo
    scalar.sums <- scalar.sumsify(plot.dat, scalar.dat)
    total.scalar.sums <- scalar.sumsify(total.plot.dat, total.scalar.dat)
    
    ## 2.5 Save inputs ####
    # JAGS inputs
    jags_input_names <-
      c("sight.dat",
        "oper.dat",
        "plot.dat",
        "total.plot.dat",
        "scalar.dat",
        "total.scalar.dat",
        "eff",
        "scalar.sums")
    jags_input <-
      ls(pattern = paste0("^", paste(jags_input_names, collapse = "|")))
    save(list = jags_input,
         file = paste0(
           "input/jags_input_",
           format(Sys.time(), "%Y%b%d_%H%M"),
           ".rdata"
         ))
    # other inputs
    other_inputs <-
      c(
        "compile_sheets",
        "name_fixer",
        "rjags_to_table",
        "file",
        "EPU.ID",
        "EPU_list",
        "year.ID",
        "stratum.ID",
        "eff",
        "observed"
      )
    save(list = other_inputs,
         file = paste0(
           "input/other_inputs_",
           format(Sys.time(), "%Y%b%d_%H%M"),
           ".rdata"
         ))
    
    files_to_keep <- c(jags_input, other_inputs, "other_inputs")
    
    rm(list = setdiff(ls(), files_to_keep))
    
    # 3 BAYESIAN ANALYSIS ---------------------------------------------------------
    
    ## 3.1 Set parameters ####
    
    # specify initial values
    inits <-  function()
      list(bo = runif(1), bvoc = runif(1))
    
    # Parameters monitored
    params <- c("bo", "bvoc", "tau.hat", "total.tau.hat")
    
    # MCMC settings
    ni <- 800
    nt <- 2
    nb <- ni / 2
    nc <- 3
    
    # record any error messages that occured since tryCatch()
  }, error = function(e) {
    # Print the error message
    cat("Error message:", e$message, "\n")
  })
  
  # finish sinking to errors.txt
  sink()
  
  ## 3.2 Run the model ####
  
  # All data
  bundle.dat <-
    list(
      x.tilde = sight.dat$x.tilde,
      z.tilde = sight.dat$z.tilde,
      #sight.dat
      x = oper.dat$x,
      ym1 = oper.dat$ym1,
      h = oper.dat$h,
      q = oper.dat$q,
      z = oper.dat$z,
      yr = oper.dat$yr,
      subunits = oper.dat$subunits,
      # oper.dat
      h.plots = plot.dat$h.plots,
      yr.plots = plot.dat$yr.plots,
      # plot.dat
      R = scalar.dat$R,
      Ngroups = scalar.dat$Ngroups,
      Nstrat.subunits.yr = scalar.dat$Nsubunits.yr,
      Nsubunits.yr = total.scalar.dat$Nsubunits.yr,
      scalars = scalar.sums,
      total.scalars = total.scalar.sums,
      #scalar.dat
      years = length(unique(plot.dat$yr.plots)),
      stratums = length(unique(plot.dat$h.plots))
    )
  
  # sink all progress to progress.txt
  sink("progress.txt")
  cat("Start time:", paste(start_time), "\n\n\n")
  cat("Progress: 0% done \n", paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "\n", sep = "")
  sink() 
  
  # run the first 2%
  jags_output <-
    jags(bundle.dat,
         inits,
         params,
         "www/beta_binom_model_elk2024.txt",
         nc,
         ni,
         nb,
         nt)
  
  # n.eff and Rhat are only reported for the last update - we need to keep track for all updates and combine
  n.eff <- jags_output$BUGSoutput$summary[,"n.eff"]
  Rhat <- jags_output$BUGSoutput$summary[,"Rhat"]
  
  # continue sinking
  sink("progress.txt")
  
  time_elapsed <-
    as.numeric(difftime(Sys.time(), as.POSIXct(start_time), units = "secs"))
  end_time <-
    format(as.POSIXct(start_time) + (time_elapsed / 2 * 100),
           "%Y-%m-%d %H:%M")
  
  cat("Start time:",
      paste(start_time),
      "\nEstimated end time:",
      end_time,
      "\n\n")
  cat("Progress: 2% done \n", paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "\n", sep = "")
  
  sink()
  
  # run the rest of the model while sinking
  for (i in 2:50) {
    jags_output <- update(jags_output, ni, nt)
    
    # update n.eff and Rhat
    n.eff <- n.eff + jags_output$BUGSoutput$summary[,"n.eff"]
    Rhat <- Rhat + jags_output$BUGSoutput$summary[,"Rhat"]
    
    sink("progress.txt")
    
    time_elapsed <-
      as.numeric(difftime(Sys.time(), as.POSIXct(start_time), units = "secs"))
    end_time <-
      format(as.POSIXct(start_time) + (time_elapsed / (i * 2) * 100),
             "%Y-%m-%d %H:%M")
    
    cat("Start time:",
        paste(start_time),
        "\nEstimated end time:",
        end_time,
        "\n\n")
    
    cat("Progress: ", i * 2, "% done \n", paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "\n", sep = "")
    
    sink()
  }
  
  # get the mean of Rhats
  Rhat <- Rhat/50
  
  ## 3.3 Save outputs ####
  
  jags_output_names <- c("jags_output", "scalar.dat", "Rhat", "n.eff")
  jags_outputs <-
    ls(pattern = paste0("^", paste(jags_output_names, collapse = "|")))
  save(list = jags_outputs,
       file = paste0(
         "output/jags_output_",
         format(Sys.time(), "%Y%b%d_%H%M"),
         ".rdata"
       ))
  
  files_to_keep <- c(jags_outputs, other_inputs)
  rm(list = setdiff(ls(), files_to_keep))
  
  # 4 OUTPUT -------------------------------------------------------------------
  
  ## 4.1 Load ####
  
  # Get summary data (i.e. standard estimates) from your excel file
  standard <- compile_sheets(file_path, "\\d{4} Summary") %>%
    rename(Standard = estimate)
  standard$EPU <- name_fixer(EPU_list, standard$EPU)
  
  ## 4.2 Clean ####
  
  ### 4.2.1 bayesian ####
  
  jags_table <-
    rjags_to_table(jags_output, scalar.dat, total.scalar.dat, year.ID, EPU.ID, stratum.ID)
  
  model_results <- jags_table %>%
    select(year:Model) %>%
    pivot_wider(names_from = stratum, values_from = Model) %>%
    inner_join(jags_table %>% filter(stratum=="total") %>% select(-stratum, -Model), by=c("year", "EPU"))
    
  
  ### 4.2.2 standard ####
  
  # Need to extract most recent target numbers
  target <- standard %>%
    group_by(EPU) %>%
    filter(year == max(year)) %>%
    select(EPU, target)
  
  standard <- standard %>%
    select(-target) %>%
    left_join(target, by = "EPU")
  
  ### 4.2.3 combine tables ####
  
  # create a dataframe that combines the important elements of all dataframes
  results.all <- left_join(model_results,
                           standard,
                           by = c("EPU", "year")) %>%
    left_join(observed, by=c("EPU", "year"))
  
  # calculate calf:100 cows and bull:100 cows ratios
  results.all <- results.all %>%
    mutate(
      Model = total, .before = lcl_95) %>%
    mutate(
      "calf_cow" = calf * 100 / cow,
      "bull_cow" = bull * 100 / cow,
      "percent_branched" = bull / (bull + spike) * 100
    ) %>%
    select(-any_of(c("cow","calf","bull","spike","UC","total")))
  
  results.long <- pivot_longer(results.all,
                               c(Model,
                                 Standard),
                               names_to = "method",
                               values_to = "estimate") %>%
    mutate(
      lcl_50 =
        if_else(method == "Model", lcl_50, as.double(NA)),
      ucl_50 =
        if_else(method == "Model", ucl_50, as.double(NA)),
      lcl_95 =
        if_else(method == "Model", lcl_95, as.double(NA)),
      ucl_95 =
        if_else(method == "Model", ucl_95, as.double(NA)),
      calf_cow =
        if_else(method == "Model", calf_cow, as.double(NA)),
      bull_cow =
        if_else(method == "Model", bull_cow, as.double(NA)),
      percent_branched =
        if_else(method == "Model", percent_branched, as.double(NA)),
      Rhat =
        if_else(method == "Model", Rhat, as.double(NA)),
      cv =
        if_else(method == "Model", cv, as.double(NA)),
      n.eff =
        if_else(method == "Model", n.eff, as.double(NA))
    ) %>%
    select(
      year,
      EPU,
      min_count,
      target,
      method,
      estimate,
      lcl_50,
      ucl_50,
      lcl_95,
      ucl_95,
      calf_cow,
      bull_cow,
      percent_branched,
      cv,
      Rhat,
      n.eff,
      cows_observed,
      bulls_observed,
      yearlings_observed,
      calves_observed,
      unclassified_observed
    )
  
  ## 4.3 Write CSV ####
  
  write_csv(results.long,
            paste0(
              getwd(),
              "/output/Results_",
              format(Sys.time(), "%Y%b%d_%H%M"),
              ".csv"
            ))
  
  # update progress
  sink("progress.txt", append = T)
  cat("\nModel finished!\n\nResults will start downloading shortly.\n",
      sep = "")
  sink()
  
  # mark as done
  sink("done.txt")
  cat("TRUE")
  sink()
  
  # wait a second, then mark running as "false"
  Sys.sleep(time = 3)
  sink("running.txt")
  cat("FALSE")
  sink()
  
  ## 4.4 Extras ####
  
  # UNCOMMENT BELOW TO TEST AGREEMENT BETWEEN METHODS
  # results.all.stats <- results.all %>%
  #   mutate(diff = Model-Standard,
  #          within_50 = if_else(Standard>=lcl_50 & Standard <=ucl_50, T, F),
  #          within_95 = if_else(Standard>=lcl_95 & Standard <=ucl_95, T, F)) %>%
  #   mutate(percent = abs(diff)/((Standard+Model)/2)*100)
  # 
  # library(SimplyAgree)
  # agreement = agree_test(x = results.all$Model,
  #                       y = results.all$Standard,
  #                       delta = 1)
  # print(agreement)
  # agreement_plot = plot(agreement, smooth_method = "lm")
  # agreement_plot
  # 
  # # TRACEPLOTS
  # library(mcmcplots)
  # mcmc_output <- as.mcmc(jags_output)
  # mcmcplot(mcmc_output, "total.tau.hat") # (for total estimates only)
  
}

output <- runModel(file_path)

