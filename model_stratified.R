# Elk Sightability Analysis

runModel <- function(file_path) {

# 1 Load and clean ####

# ## 1.1 LOAD DATA ####
# list.of.packages <- c("shiny", "shinyjs", "bslib", "DT", "outliers", "bayesplot", "tidyverse", "lubridate","chron","rgdal", "readxl", "Cairo", "rjags","coda","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags", "SimplyAgree")
# # Check you have them and load them
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# lapply(list.of.packages, require, character.only = TRUE)
# source("helpers_stratified.R")
# 
# # Set your working directory paths and survey data file path
# wd <- getwd()
# input_wd <- paste0(wd,"/input")
# output_wd <- paste0(wd,"/output")
# 
# # create input and output folders if you haven't already
# dir.create(input_wd)
# dir.create(output_wd)

file <- paste0(file_path())
setwd(input_wd)

# file <- "test_data.xlsx"

# Extract observations from all years
# If you didn't name your survey data sheets with "Data", replace below
obs.all <- compile_sheets(file, "Data")

# fix EPU names & survey types
obs.all$EPU <- name_fixer(obs.all$EPU)
obs.all$survey.type <- standard_survey(obs.all$survey.type) 

# Bring in summary data from each year
# This acts as a list of all the EPUs that were properly surveyed that year
eff <- compile_sheets(file, "Summary") %>%
  filter(!is.na(min_count))

eff$EPU <- name_fixer(eff$EPU)

# Save EPU names from reliable source
setwd(wd)
EPU.areas <- read_csv("www/EPU_areas.csv")
EPU.list <- as.character(EPU.areas$EPU)

## 1.4 SIGHTABILITY DATA ####

# we will have one sightability dataframe for all sexes/ages, which means we're assuming
# equal sightability of bulls, cows, and calves -> may not be true.

sight <- obs.all %>%
  # we'll create a field to tell us whether each year counts toward sightability data
  group_by(year) %>%
  mutate(sight = if_else(all(c("Inventory", "Telemetry") %in% unique(survey.type)), TRUE, FALSE)) %>%
  # we only want inventory and telemetry obs to keep things simple
  filter(survey.type == "Telemetry" | survey.type == "Inventory",
         # only keep years where sightability trials were done
         sight == TRUE,
         # we only want observations with collars
         collars>0
  )

# duplicate observations with >1 collars
sight.dup <- as.data.frame(matrix(NA, 0, ncol(sight)))
colnames(sight.dup) <- colnames(sight)

for(i in 1:nrow(sight))
{
  if(sight$collars[i]>1){
    sight.dup <- rbind(sight.dup, rep(sight[i,], sight$collars[i]-1))
    sight.dup$date <- as.Date(sight.dup$date)
  }
}
sight <- bind_rows(sight, sight.dup)

## 1.6 OBSERVATIONAL DATASET ####

# make sure we're only keeping data from EPUs with summary data that year
obs <- inner_join(obs.all, eff %>% select(year, EPU), by=c("EPU","year")) %>%
  mutate(
    subunit = EPU,
    # make sure there are no NAs in observation counts
    mutate(across(c(cow, calf, spike, bull, UC, total), ~if_else(is.na(.), 0, .))),
    voc = voc,
    .keep="unused") %>%
  # only keep inventory & capture surveys
  filter(survey.type=="Inventory" | survey.type=="Capture" | survey.type=="Telemetry") %>%
  # then turn any "capture" to "inventory"
  mutate(survey.type = if_else(survey.type=="Capture", "Inventory", survey.type))

# make sure all totals = sum of cows, calves, etc

obs <- obs %>%
  mutate(UC = if_else(
      total > (cow+calf+spike+bull+UC), as.numeric(total-(cow+calf+spike+bull)),
      as.numeric(UC)),
    total = as.numeric(cow+calf+spike+bull+UC))

## 1.6 EFFORT ####

# Amend EPU.list to only include surveyed EPUs, then assign ID numbers
EPU.list <- data.frame(EPU = unique(obs$subunit)) %>%
  mutate(ID = seq(1,length(EPU),1))

eff <- inner_join(eff, EPU.list, by="EPU")

# Add ID to obs

# convert sex/age to stratum
obs <- obs %>%
  pivot_longer(cow:total, names_to = "stratum", values_to = "y") %>%
  filter(y>0,
         stratum != "UC")

stratum.ID <- obs %>%
  select(stratum) %>%
  distinct()
stratum.ID$h <- seq(1:nrow(stratum.ID))

obs <- obs %>%
  left_join(stratum.ID, by="stratum")

obs <- inner_join(obs, eff %>% select(year, EPU, ID), by=c(c("subunit"="EPU"),"year")) %>%
  mutate(
    subunits = ID,
    .keep="unused")

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
  mutate(n_telem = replace_na(n_Telemetry, 0),
         n_nontelem = replace_na(n_Inventory, 0),
         count_telem = replace_na(count_Telemetry, 0),
         count_nontelem = replace_na(count_Inventory, 0),
         .keep="unused") %>%
  filter(count_telem>0) %>%
  mutate(avg_group_nontelem = (count_nontelem/n_nontelem),
         avg_group_telem = (count_telem+count_nontelem)/(n_telem+n_nontelem)) %>%
  left_join(group, by=c("subunit", "stratum")) %>%
  mutate(keep_telem= if_else(abs(avg_group_telem-avg_group)>=abs(avg_group_nontelem-avg_group), F, T))

# add telem direction to obs
obs <- left_join(obs, telem.stats %>% select(year, subunit, stratum, keep_telem), by=c("year", "subunit", "stratum")) %>%
  filter(!(survey.type=="Telemetry"& keep_telem==F))

# 2 Prepare data ####

## 2.1 SIGHT DAT ####

# s = habitat indicator ()
# x = visual obstrcution measurements associated with the test trial data used to develop the sightability model
# a = activity indicator (0 if bedded, 1 if standing/moving)
# z = detection indicator (1 if the group was observed, 0 otherwise)
# t = group size

sight.dat <- sight %>%
  mutate(
    observed = as.integer(if_else(survey.type=="Inventory", 1, 0)),
    grpsize = as.integer(total)
  )  %>%
  # standardize habitat
  mutate(
    # 1 - rock / other (gravel, landfill, road, slide, other)
    # 2 - meadow / riparian (field, meadow, riparian, wetland, river)
    # 3 - cutblock / powerline (block, powerline, NSR, FTG)
    # 4 - mature forest (mature, old)
    habitat = case_when(
      grepl("mature|old|conifer", habitat, ignore.case = TRUE) ~ 4,
      grepl("block|powerline|nsr|ftg", habitat, ignore.case = TRUE) ~ 3,
      grepl("field|meadow|riparian|wetland|river", habitat, ignore.case = TRUE) ~ 2,
      grepl("gravel|landfill|road|wtp|other|slide", habitat, ignore.case = TRUE) ~ 1
    ),
    # standardize activity
    activity = case_when(
      grepl("standing|moving|run", activity, ignore.case = TRUE) ~ 1,
      grepl("bed", activity, ignore.case = TRUE) ~ 0),
    a = as.double(activity),
    s = as.double(habitat),
    t = as.double(grpsize),
    x.tilde = as.double(voc),
    z.tilde = as.double(observed)) %>%
  select(a, s, t, x.tilde, z.tilde)

### 2.1.1 test correlations ####
# sight.dat %>% group_by(z.tilde) %>% summarize(mean = mean(x.tilde))
# 
# test <- "kendall"
# 
# x.z <- cor.test(sight.dat$z.tilde, sight.dat$x.tilde, method=test)
# a.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$a)], sight.dat$a[!is.na(sight.dat$a)], method=test)
# s.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$s)], sight.dat$s[!is.na(sight.dat$s)], method=test)
# t.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$t)], sight.dat$t[!is.na(sight.dat$t)], method=test)
# 
# Correlation <- as.data.frame(matrix(NA, 4, 3))
# Correlation[1,] <- c("VOC", x.z$estimate, x.z$p.value)
# Correlation[2,] <- c("Activity", a.z$estimate, a.z$p.value)
# Correlation[3,] <- c("Habitat", s.z$estimate, s.z$p.value)
# Correlation[4,] <- c("Group size", t.z$estimate, t.z$p.value)
# colnames(Correlation) <- c("Variable", "Correlation", "p")
# 
# write.csv(Correlation, "C:/Users/TBRUSH/R/SightabilityModels/output/Correlation.csv", row.names = FALSE)

### 2.1.2 finish sight.dat ####
# voc is the only factor significantly correlated with sightability -> select only voc
sight.dat <- sight.dat %>% select(x.tilde, z.tilde)

## 2.2 OPER DAT ####

### 2.2.1 Non-augmented data ####

# Get year ID
year.ID <- as.data.frame(matrix(NA, length(unique(obs$year)), 2))
colnames(year.ID) <- c("year", "year.ID")

year.ID[,1] <- unique(obs$year) %>% sort()
year.ID[,2] <- seq(1,length(unique(obs$year)))

# join to oper.dat
oper.dat <- left_join(obs, year.ID, by="year")


# get non-augmented data organized
oper.dat <- oper.dat %>%
  # model won't accept voc = 0 or 1, fix below
  mutate(voc = if_else(voc==1, 0.99,
                     if_else(voc==0, 0.01, voc)))

oper.dat <- oper.dat %>%
  oper.datify()

### 2.2.2 Augmented data ####

oper.dat <- oper.dat %>%
  augment()

## 2.3 PLOT DAT ####

plot.dat <- oper.dat %>%
  plot.datify()

## 2.4 SCALAR DAT ####
scalar.dat <- scalar.datify(oper.dat, plot.dat, sight.dat)

# Create scalar.sums to ease modelling
# tells us how many rows belong to each year/stratum combo
scalar.sums <- scalar.sumsify(plot.dat, scalar.dat)

## 2.5 SAVE INPUTS ####

setwd(input_wd)

# JAGS inputs
jags_input_names <- c("sight.dat", "oper.dat", "plot.dat", "scalar.dat", "eff", "scalar.sums")
jags_input <- ls(pattern = paste0("^", paste(jags_input_names, collapse = "|")))
save(list = jags_input, file = paste0("jags_input_", format(Sys.time(), "%Y%b%d_%H%M"), ".rdata"))
# other inputs
other_inputs <- c("compile_sheets", "name_fixer", "rjags_to_table", "wd", "input_wd","output_wd","file","EPU.list","year.ID", "stratum.ID", "eff")
save(list= other_inputs, file=paste0("other_inputs_", format(Sys.time(), "%Y%b%d_%H%M"), ".rdata"))

files_to_keep <- c(jags_input, other_inputs, "other_inputs")

rm(list = setdiff(ls(), files_to_keep))

# 3 Bayesian Analysis ####

## 3.1 SET PARAMETERS ####

# specify initial values
inits <-  function() list(bo=runif(1), bvoc=runif(1))

# Parameters monitored
params <- c("bo", "bvoc", "tau.hat")

# MCMC settings
ni <- 40000 
nt <- 2     
nb <- 20000
nc <- 3   

## 3.2 RUN THE MODEL ####
setwd(paste0(wd, "/www"))

# All data
bundle.dat <- list(x.tilde=sight.dat$x.tilde, z.tilde=sight.dat$z.tilde, #sight.dat
                   x=oper.dat$x, ym1=oper.dat$ym1, h=oper.dat$h, q=oper.dat$q, z=oper.dat$z, yr=oper.dat$yr, subunits=oper.dat$subunits, # oper.dat
                   h.plots=plot.dat$h.plots, yr.plots=plot.dat$yr.plots, # plot.dat
                   R=scalar.dat$R, Ngroups=scalar.dat$Ngroups, Nsubunits.yr=scalar.dat$Nsubunits.yr, scalars=scalar.sums, #scalar.dat
                   years=length(unique(plot.dat$yr.plots)), stratums=length(unique(plot.dat$h.plots)))

jags_output <- jags(bundle.dat, inits, params, "beta_binom_model_elk2022.txt", nc, ni, nb, nt)

## 3.4 SAVE OUTPUTS ####

setwd(output_wd)

jags_output_names <- c("jags_output", "scalar.dat")
jags_outputs <- ls(pattern = paste0("^", paste(jags_output_names, collapse = "|")))
save(list = jags_outputs, file=paste0("jags_output_", format(Sys.time(), "%Y%b%d_%H%M"), ".rdata"))

files_to_keep <- c(jags_outputs, other_inputs)
rm(list = setdiff(ls(), files_to_keep))

# 4 Plot & Report ####

## 4.1 LOAD DATA ####
setwd(input_wd)

# Get summary data (i.e. standard estimates) from your excel file
standard <- compile_sheets(file, "Summary") %>%
  rename(Standard = estimate)
standard$EPU <- name_fixer(standard$EPU)

## 4.3 CLEAN OUTPUTS ####

### 4.3.1 Bayesian ####

jags_table <- rjags_to_table(jags_output, scalar.dat, year.ID, EPU.list, stratum.ID)

total <- jags_table %>%
  filter(stratum=="total") %>%
  select(-stratum)

model_results <- jags_table %>%
  select(year:Model) %>%
  pivot_wider(names_from = stratum, values_from = Model) %>%
  select(-total) %>%
  left_join(total, by=c("year", "EPU"))

### 4.3.2 Standard ####

# Need to extract most recent target numbers
target <- standard %>%
  group_by(EPU) %>%
  filter(year == max(year)) %>%
  select(EPU, target)

standard <- standard %>%
  select(-target) %>%
  left_join(target, by="EPU")

### 4.3.3 Combine tables ####

# create a dataframe that combines the important elements of all dataframes
results.all <- left_join(model_results,
                         standard, 
                         by=c("EPU", "year"))

# calculate calf:100 cows and bull:100 cows ratios
results.all <- results.all %>%
  mutate("calf_cow" = calf*100/cow,
         "bull_cow" = bull*100/cow) %>%
  select(-cow, -calf, -bull, -spike)

# uncomment any commented sections below if you're including mHT estimates
results.long <- pivot_longer(results.all,
                             c(Model,
                               Standard),
                             names_to = "method",
                             values_to = "estimate") %>%
  mutate(lcl_50 =
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
         Rhat = 
           if_else(method == "Model", Rhat, as.double(NA)),
         cv = 
           if_else(method=="Model", cv, as.double(NA))) %>%
  select(year, 
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
         cv, 
         Rhat,
         n.eff)

results.long

## 4.5 EXTRAS ####

# results.all.stats <- results.all %>%
#   mutate(diff = Model-Standard,
#          within_50 = if_else(Standard>=lcl_50 & Standard <=ucl_50, T, F),
#          within_95 = if_else(Standard>=lcl_95 & Standard <=ucl_95, T, F)) %>%
#   mutate(percent = abs(diff)/((Standard+Model)/2)*100)

}

output <- runModel(file_path)

