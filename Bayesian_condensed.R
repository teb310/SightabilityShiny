# Elk Sightability Analysis

runModel <- function(file_path) {

# 1 Load and clean ####

## 1.1 LOAD DATA ####

file <- paste0(file_path())

setwd(input_wd)

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
EPU.areas <- read_csv("EPU_areas.csv")
EPU.list <- as.character(EPU.areas$EPU)

## 1.4 SIGHTABILITY DATA ####

sight <- obs.all

# filter out incidental observations
sight <- sight %>%
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
  filter(survey.type=="Inventory" | survey.type=="Capture")

# make sure all totals = sum of cows, calves, etc

obs1 <- obs %>%
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

obs <- inner_join(obs, eff %>% select(year, EPU, ID), by=c(c("subunit"="EPU"),"year")) %>%
  mutate(
    stratum = ID,
    .keep="unused")

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

glimpse(sight.dat) # check - looks the same as Fieberg's sight_dat csv

### 2.1.1 test correlations ####
# sight.dat %>% group_by(z.tilde) %>% summarize(mean = mean(x.tilde))
# 
# x.z <- cor.test(sight.dat$z.tilde, sight.dat$x.tilde, method="pearson")
# a.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$a)], sight.dat$a[!is.na(sight.dat$a)], method="pearson")
# s.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$s)], sight.dat$s[!is.na(sight.dat$s)], method="pearson")
# t.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$t)], sight.dat$t[!is.na(sight.dat$t)], method="pearson")
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
  transmute(x = voc,
            ym1 = total-1,
            h = as.double(stratum),
            q = 1,
            z = 1,
            yr = year.ID,
            subunits = as.double(stratum),
            survey.type = NULL) %>%
  # model won't accept x = 0 or 1, fix below
  mutate(x = if_else(x==1, 0.99,
                     if_else(x==0, 0.01, x))) %>%
  glimpse()

### 2.2.2 Augmented data ####
# need to determine max m of each h
aug <- oper.dat %>%
  group_by(yr, h) %>%
  summarize(m = n()) %>%
  ungroup() %>%
  group_by(h) %>%
  reframe(yr = yr,
          m = m,
          m.max = max(m)) %>%
  ungroup() %>%
  mutate(b = 2*m.max,
         aug = b-m)

oper.dat.aug <- aug[rep(1:nrow(aug), aug$aug),] %>%
  mutate(x = NA, ym1 = NA, h = h, q = NA, z = 0, yr = yr, subunits = h, .keep="none") %>%
  ungroup()

### 2.2.3 Combine ####

oper.dat <- rbind(oper.dat, oper.dat.aug) %>%
  arrange(yr, h, q)

glimpse(oper.dat) # check

## 2.3 PLOT DAT ####

plot.dat <- oper.dat %>%
  select(yr, h) %>%
  distinct() %>%
  mutate(h.plots = h, 
         yr.plots = yr) %>%
  select(h.plots, yr.plots) %>%
  arrange(yr.plots, h.plots)
glimpse(plot.dat)

## 2.4 SCALAR DAT ####

scalar.dat <- as.data.frame(matrix(NA, 1, 3))
colnames(scalar.dat) <- c("R", "Ngroups", "Nsubunits.yr")

scalar.dat <- as.data.frame(matrix(NA, 1, (nrow(plot.dat))))
i <- 1
for(i in 1:nrow(plot.dat)){
  scalar.dat[,i] <- as.double(nrow(oper.dat %>% filter(yr == plot.dat$yr.plots[i], h == plot.dat$h.plots[i])))
  colnames(scalar.dat)[i] <- paste("h", plot.dat$h.plots[i], "y", plot.dat$yr.plots[i], sep = "")
}

scalar.dat <- scalar.dat %>%
  mutate(R = as.double(nrow(sight.dat)),
         Ngroups = as.double(nrow(oper.dat)),
         Nsubunits.yr = as.double(nrow(plot.dat)))

# Create scalar.sums to ease modelling
# tells us how many rows belong to each year/stratum combo
scalar.sums <- matrix(NA, nrow(plot.dat), 2)
for (i in 1:nrow(plot.dat)){
  t <- i-1
  scalar.sums[i, 1] <- sum(scalar.dat[,0:t], 1)
  scalar.sums[i, 2] <- sum(scalar.dat[,0:i])
}

## 2.5 SAVE INPUTS ####
# JAGS inputs
jags_input <- c("sight.dat", "oper.dat", "plot.dat", "scalar.dat", "eff", "scalar.sums")
save(list = jags_input, file = paste0("jags_input_", format(Sys.time(), "%Y%b%d_%H%M"), ".rdata"))
# other inputs
other_inputs <- c("compile_sheets", "name_fixer", "input_wd","output_wd","file","EPU.list","year.ID", "eff")
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
ni <- 40000 # build to 40000
nt <- 2     # 50% thinning rate (discard every 2nd iteration)
nb <- 20000
nc <- 3

# Bundle data
bundle.dat <- list(x.tilde=sight.dat$x.tilde, z.tilde=sight.dat$z.tilde, #sight.dat
                   x=oper.dat$x, ym1=oper.dat$ym1, h=oper.dat$h, q=oper.dat$q, z=oper.dat$z, yr=oper.dat$yr, subunits=oper.dat$subunits, # oper.dat
                   h.plots=plot.dat$h.plots, yr.plots=plot.dat$yr.plots, # plot_dat
                   R=scalar.dat$R, Ngroups=scalar.dat$Ngroups, Nsubunits.yr=scalar.dat$Nsubunits.yr, scalars=scalar.sums, #scalar.dat
                   years=length(unique(plot.dat$yr.plots)), stratums=length(unique(plot.dat$h.plots)))

## 3.2 RUN THE MODEL ####
jags_output <- jags(bundle.dat, inits, params, "beta_binom_model_elk2022.txt", nc, ni, nb, nt)

setwd(output_wd)

## 3.4 SAVE OUTPUTS ####

jags_outputs <- c("jags_output", "scalar.dat")
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

jags.summary <- as.data.frame(jags_output$BUGSoutput$summary)

tau.jags <- matrix(NA,(nrow(jags.summary)-3),9)
tau.jags <- as.data.frame(tau.jags)
tau.jags[,1] <- as.numeric(str_extract(colnames(scalar.dat)[1:length(jags_output$BUGSoutput$median$tau.hat)], "(?<=h)[:digit:]{1,2}"))
tau.jags[,2] <- as.numeric(str_extract(colnames(scalar.dat)[1:length(jags_output$BUGSoutput$median$tau.hat)], "(?<=y)[:digit:]{1,2}"))
tau.jags[,3] <- round(jags.summary$`50%`[4:nrow(jags.summary)])
tau.jags[,4] <- round(jags.summary$`2.5%`[4:nrow(jags.summary)])
tau.jags[,5] <- round(jags.summary$`97.5%`[4:nrow(jags.summary)])
tau.jags[,6] <- round(jags.summary$`25%`[4:nrow(jags.summary)])
tau.jags[,7] <- round(jags.summary$`75%`[4:nrow(jags.summary)])
tau.jags[,8] <- round(jags.summary$Rhat[4:nrow(jags.summary)], 3)
tau.jags[,9] <- round(jags.summary$sd[4:nrow(jags.summary)]/jags.summary$`50%`[4:nrow(jags.summary)], 3)

rm(jags_output)

colnames(tau.jags) <- c("ID", "year.ID", "Model","JAGS_lcl_95", "JAGS_ucl_95", "JAGS_lcl_50", "JAGS_ucl_50", "Rhat", "cv") 
jags_output <- left_join(tau.jags, year.ID, by="year.ID") %>%
  left_join(EPU.list, by="ID") %>%
  select(-year.ID, -ID)

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
results.all <- left_join(jags_output %>% select(year, EPU, Model, JAGS_lcl_95, JAGS_ucl_95, JAGS_lcl_50, JAGS_ucl_50),
                         standard, 
                         by=c("EPU", "year"))

# uncomment any commented sections below if you're including mHT estimates
results.long <- pivot_longer(results.all,
                             c(Model,
                               Standard),
                             names_to = "method",
                             values_to = "estimate") %>%
  mutate(lcl_50 =
           if_else(method == "Model", JAGS_lcl_50, as.double(NA))) %>%

  mutate(ucl_50 =
           if_else(method == "Model", JAGS_ucl_50, as.double(NA))) %>%
  mutate(lcl_95 =
           if_else(method == "Model", JAGS_lcl_95, as.double(NA))) %>%
  mutate(ucl_95 =
           if_else(method == "Model", JAGS_ucl_95, as.double(NA))) %>%
  select(-JAGS_lcl_50,-JAGS_ucl_50, -JAGS_lcl_95,-JAGS_ucl_95)

results_filename <- paste0("Results_", format(Sys.time(), "%Y%b%d_%H%M"), ".csv")

setwd(output_wd)
write.csv(results.long, results_filename, row.names = F)

# 
# ## 4.5 EXTRAS ####
# 
# ### 4.5.1 Agreement ####
# 
# agree.BS = agree_test(x = results.all$JAGS,
#                       y = results.all$Standard, 
#                       delta = 1)
# print(agree.BS) # 65%
# agree.BS_plot = plot(agree.BS) +
#   scale_y_continuous(breaks = c(seq(-300, 400, by = 100)), limits = c(-300, 400))
# agree.BS_plot
# 
# ggsave("Method_agreement.jpeg", width = 10, height = 7, units="in")
# 
# # Agreement Table
# Agreement <- as.data.frame(matrix(NA, 3, 2))
# 
# Agreement[1,] <- c("Bayesian vs. Standard", agree.BS$ccc.xy[1])
# 
# colnames(Agreement) <- c("Test", "CCC")
# 
# write.csv(Agreement,"Agreement.csv", row.names = FALSE)

}

output <- runModel(file_path)

