# Agreement
library(tidyverse)
library(SimplyAgree)
library(ggeffects)
library(bayesplot)

results <- Results_2021_only

results <- filter(results, year>2020)

results_wide <- pivot_wider(
  results %>% select(year, EPU, method, estimate), 
  names_from = "method", 
  values_from = "estimate")

agreement = agree_test(x = results_wide$Model,
                      y = results_wide$Standard,
                      delta = 0)
print(agreement) # 86%
agreement_plot = plot(agreement, 
                      x_name = "'Model'",
                      y_name = "'Standard'",
                      smooth_method = "lm")
agreement_plot

# ggsave("Method_agreement.jpeg", width = 10, height = 7, units="in")

# by year

year.ID <- as.data.frame(matrix(c(unique(results_wide$year), seq(1:length(unique(results_wide$year)))), ncol = 2))

agreement_by_year <- as.data.frame(matrix(NA, nrow(year.ID), 5))

i <- 1
for(i in 1:max(year.ID$V2)) {
  tmp <- results_wide %>%
    filter(year == year.ID$V1[i])
  agree.tmp <- agree_test(x = tmp$Model,
                          y = tmp$Standard,
                          delta = 1)
  agreement_by_year[i,] <- c(year.ID$V1[i], 
                             agree.tmp[["ccc.xy"]][["est.ccc"]], 
                             agree.tmp[["ccc.xy"]][["lower.ci"]],
                             agree.tmp[["ccc.xy"]][["upper.ci"]],
                             agree.tmp[["loa"]][["estimate"]][[1]])
}

colnames(agreement_by_year) <- c("Year", "CCC", "LCL", "UCL", "bias")

# write.csv(agreement_by_year, "Agreement_by_year.csv", row.names=F)

# CV
cv.summary <- results %>%
  group_by(year) %>%
  summarize(cv = median(cv, na.rm=T))

print(median(results$cv[results$year==2021], na.rm=T))
summary(results$cv)

# model performance
summary(results$Rhat)
summary(results$n.eff)

# Agreement Table
Agreement <- as.data.frame(matrix(NA, 3, 2))

Agreement[1,] <- c("Bayesian vs. Standard", agree.BS$ccc.xy[1])

colnames(Agreement) <- c("Test", "CCC")

write.csv(Agreement,"Agreement.csv", row.names = FALSE)


# Stable GR statistic
library(stableGR)
library(coda)
library(stringr)
library(tidyverse)
mcmc_output <- as.mcmc(jags_output)
target <- target.psrf(2, 3)
print(target$psrf)
GR <- stable.GR(mcmc_output)
psrf <- GR$psrf
psrf.table <- as.data.frame(matrix(c(as.numeric(str_extract(names(GR$means), "\\d+")), psrf), ncol = 2, nrow=length(psrf)))
psrf.table <- arrange(psrf.table, V1)
psrf.table$ID <- colnames(scalar.dat)
psrf.table$sub <- as.numeric(str_extract(psrf.table$ID, "(?<=sub)[:digit:]{1,2}"))
psrf.table$h <- as.numeric(str_extract(psrf.table$ID, "(?<=h)[:digit:]{1,2}"))
psrf.table$yr <- as.numeric(str_extract(psrf.table$ID, "(?<=y)[:digit:]{1,2}"))
psrf.table <- inner_join(psrf.table, stratum.ID, by="h")
psrf.summary <- psrf.table %>%
  filter(stratum=="total") %>%
  mutate(converged = if_else(V2<=target$psrf, T, F))

n.eff(mcmc_output)

summary <- as.data.frame(as.array(summary(psrf.summary$V2)))

# run autojags until the model converges

recompile(jags_output)
auto_output <- autojags(jags_output, Rhat=target$psrf)
update(jags_output, n.iter = 5000)

