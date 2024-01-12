# Name_fixer converts misspelled or abbreviated EPU names to standard names
name_fixer <- function(x){
  output <- case_when(
    grepl("Rainy", x, ignore.case = TRUE) ~ "Rainy-Gray",
    grepl("Narrow", x, ignore.case = TRUE) ~ "Tzoonie-Narrows",
    grepl("Desert", x, ignore.case = TRUE) ~ "Deserted-Stakawus",
    grepl("Cheh", x, ignore.case = TRUE) ~ "Chehalis",
    grepl("Sech", x, ignore.case = TRUE) ~ "Sechelt Peninsula",
    grepl("Homa", x, ignore.case = TRUE) ~ "Homathko",
    grepl("Lois", x, ignore.case = TRUE) ~ "Lois",
    grepl("Eldr", x, ignore.case = TRUE) ~ "Eldred",
    grepl("Hasl", x, ignore.case = TRUE) ~ "Haslam",
    grepl("Dani", x, ignore.case = TRUE) ~ "Powell-Daniels",
    grepl("Quat", x, ignore.case = TRUE) ~ "Quatam",
    grepl("Lill", x, ignore.case = TRUE) ~ "Lower Lillooet",
    grepl("Van", x, ignore.case = TRUE) ~ "Vancouver",
    grepl("Squam", x, ignore.case = TRUE) ~ "Squamish",
    grepl("Indian", x, ignore.case = TRUE) ~ "Indian",
    grepl("Stave", x, ignore.case = TRUE) ~ "Stave",
    grepl("Theo", x, ignore.case = TRUE) ~ "Theodosia",
    grepl("Mcnab", x, ignore.case = TRUE) ~ "McNab",
    grepl("Bear", x, ignore.case = TRUE) ~ "Bear",
    grepl("Pit", x, ignore.case = TRUE) ~ "Pitt",
    grepl("Brem", x, ignore.case = TRUE) ~ "Brem",
    grepl("Brit", x, ignore.case = TRUE) ~ "Brittain",
    grepl("Clow", x, ignore.case = TRUE) ~ "Clowhom",
    grepl("Orf", x, ignore.case = TRUE) ~ "Orford",
    grepl("Skwa", x, ignore.case = TRUE) ~ "Skwawka",
    grepl("Southg", x, ignore.case = TRUE) ~ "Southgate",
    TRUE ~ x
  )
  return(output)
}

# Standard_survey standardizes survey types
standard_survey <- function(x){
  output <- case_when(
    grepl("incident", x, ignore.case = TRUE) ~ "Incidental",
    grepl("telem", x, ignore.case = TRUE) ~ "Telemetry",
    grepl("trans", x, ignore.case = TRUE) ~ "Inventory",
    grepl("invent", x, ignore.case = TRUE) ~ "Inventory",
    grepl("capt", x, ignore.case = TRUE) ~ "Capture",
    TRUE ~ "Other")
  return(output)
}

# compile_sheets binds all sheets in a file (filepath) that follow a certain naming patter (type)
# E.g. to bind survey data sheets from all years, type should be "Data"
compile_sheets <- function(filepath,type){
  sheets <- excel_sheets(filepath)
  sheets <- subset(sheets, str_detect(sheets, paste0(type)) ==T)
  output <- data.frame(matrix(ncol = 0, nrow = 0))
  for(i in 1:length(sheets))
  {
    output <- bind_rows(output, read_excel(filepath, sheet = paste0(sheets[i])))
  }
  return(output)
}

oper.datify <- function(df){
  transmute(.data = df,
            x = voc,
            ym1 = y-1,
            h = as.double(h),
            q = 1,
            z = 1,
            yr = year.ID,
            subunits = as.double(subunits),
            survey.type = NULL)
}

augment <- function(df){
  aug <- df %>%
    # need to determine max annual m of each h
  group_by(yr, h, subunits) %>%
    summarize(m = n()) %>%
    ungroup() %>%
    group_by(h, subunits) %>%
    reframe(yr = yr,
            subunits = subunits,
            m = m,
            m.max = max(m)) %>%
    ungroup() %>%
    mutate(b = 2*m.max,
           aug = b-m)
  # create augmented dataframe
  oper.dat.aug <- aug[rep(1:nrow(aug), aug$aug),] %>%
    mutate(x = NA, ym1 = NA, h = h, q = NA, z = 0, yr = yr, subunits = subunits, .keep="none") %>%
    ungroup()
  # combine dataframes
  output <- rbind(df, oper.dat.aug) %>%
    arrange(yr, h, subunits, desc(z))
  return(output)
}

plot.datify <- function(df){
  df %>%
    select(yr, h, subunits) %>%
    distinct() %>%
    mutate(h.plots = h, 
           yr.plots = yr,
           subunits.plots = subunits) %>%
    select(h.plots, yr.plots, subunits.plots) %>%
    arrange(yr.plots, h.plots, subunits.plots)
}

scalar.datify <- function(operdat, plotdat, sightdat){
  output <- as.data.frame(matrix(NA, 1, (nrow(plotdat))))
  i <- 1
  for(i in 1:nrow(plotdat)){
    output[,i] <- as.double(nrow(operdat %>% filter(yr == plotdat$yr.plots[i], h == plotdat$h.plots[i], subunits == plotdat$subunits.plots[i])))
    colnames(output)[i] <- paste0("h", plotdat$h.plots[i], "y", plotdat$yr.plots[i], "sub", plotdat$subunits.plots[i])
  }
  
  output <- output %>%
    mutate(R = as.double(nrow(sightdat)),
           Ngroups = as.double(nrow(operdat)),
           Nsubunits.yr = as.double(nrow(plotdat)))
  return(output)
}
scalar.sumsify <- function(plotdat, scalardat){
  # Create scalar.sums to ease modelling
  # tells us how many rows belong to each year/stratum/subunit combo
  output <- matrix(NA, nrow(plotdat), 2)
  for (i in 1:nrow(plotdat)){
    t <- i-1
    output[i, 1] <- sum(scalardat[,0:t], 1)
    output[i, 2] <- sum(scalardat[,0:i])
  }
  return(output)
}

rjags_to_table <- function(jagsoutput, scalardat, year_list, EPU_list, stratum_list){
  jags.summary <- as.data.frame(jagsoutput$BUGSoutput$summary)
  
  tau.jags <- matrix(NA,(nrow(jags.summary)-3),11)
  tau.jags <- as.data.frame(tau.jags)
  tau.jags[,1] <- as.numeric(str_extract(colnames(scalardat)[1:length(jagsoutput$BUGSoutput$median$tau.hat)], "(?<=sub)[:digit:]{1,2}"))
  tau.jags[,2] <- as.numeric(str_extract(colnames(scalardat)[1:length(jagsoutput$BUGSoutput$median$tau.hat)], "(?<=h)[:digit:]{1,2}"))
  tau.jags[,3] <- as.numeric(str_extract(colnames(scalardat)[1:length(jagsoutput$BUGSoutput$median$tau.hat)], "(?<=y)[:digit:]{1,2}"))
  tau.jags[,4] <- round(jags.summary$`50%`[4:nrow(jags.summary)])
  tau.jags[,5] <- round(jags.summary$`2.5%`[4:nrow(jags.summary)])
  tau.jags[,6] <- round(jags.summary$`97.5%`[4:nrow(jags.summary)])
  tau.jags[,7] <- round(jags.summary$`25%`[4:nrow(jags.summary)])
  tau.jags[,8] <- round(jags.summary$`75%`[4:nrow(jags.summary)])
  tau.jags[,9] <- round(jags.summary$Rhat[4:nrow(jags.summary)], 3)
  tau.jags[,10] <- round(jags.summary$sd[4:nrow(jags.summary)]/jags.summary$mean[4:nrow(jags.summary)], 3)
  tau.jags[,11] <- as.numeric(jags.summary$n.eff[4:nrow(jags.summary)])
  
  colnames(tau.jags) <- c("subunit.ID", "stratum.ID", "year.ID", "Model","lcl_95", "ucl_95", "lcl_50", "ucl_50", "Rhat", "cv", "n.eff") 
  output <- left_join(tau.jags, year_list, by="year.ID") %>%
    left_join(EPU_list, by=c("subunit.ID"="ID")) %>%
    left_join(stratum_list, by=c("stratum.ID"="h")) %>%
    select(-year.ID, -stratum.ID, -subunit.ID) %>%
    select(year, EPU, stratum, Model:n.eff)
  
  return(output)
}
