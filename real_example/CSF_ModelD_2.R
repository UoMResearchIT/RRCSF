
# Intro -------------------------------------------------------------------

# Author: Rowan Green, https://orcid.org/0000-0002-2147-9087, 02/2024
#
# This script creates an ODE model simulating growth and mutation in a 
# population of E. coli bacteria.
#
# - Parallel processing of the foreach instruction (line ~210) on the CSF
#   is helpful because we want to test a large number of different parameter 
#   sets for their effect on mutation rate plasticity.
#
# - In the final plot we use this large data set of many runs taking differing
#   parameter values to explore which parameters exert most control over our
#   output of interest (mutation rate plasticity)
#
# - The mutation rate plasticity we are looking at is the change in mutation
#   rates in populations provisioned with differing amounts of glucose as a 
#   food source.

# Opening packages -------------------------------------------------------
#NOTE TO SELF: CHECK YOU ARE IN THE CORRECT WORKING DIRECTORY

library(plyr)
library(deSolve)
library(magrittr)
library(parallel)
library(foreach)
library(doFuture)
library(tidyverse)
library(nlme)
library(RColorBrewer)
library(emdbook)

plan(multisession)

N_Runs <- 100 # number of sensitivity test runs, N_Runs = 5e4 takes 15 hours
theme_set(theme_bw(base_size = 24))


# Setting model parameters and starting variable values ------------------------------------------------

CellVol = 1.0325e-12 #kubitsheke & friske 1986 (in mL)

times <- seq(0, 1e5, by = 10)
parameters <- c(U1 = 0.2657562, #fitted to Ks Dykhuzien & Growth Jain
                M1 = 2.69e-04, #fitted to growth data
                Ks = 3.974e-5, #Dykhuzien
                I1 = 6.9e-3, #fitted to cytoplasmic dGTP conc of 92uM Buckstein
                D1 = 6.9e-3, #given the same value as I1
                O2 = 11.99985, #titrated to mutation rate of 2e-10 (lab data + Foster 2015)
                I2 = 6.9e-3 * (7.7e-8 / 2.1e-6), #relative efficiency of oG binding to A compared to G binding to c (I1) is 7.7e-8:2.1e-6 (Maki)
                D2 = 6.9e-3 * (0.029), #rate of c pairing opposite an oG relative to C:G is 6e-8:2.1e-6 (Maki)
                C1 = 2.8, #kcat measured ex vivo Xia et al, 2005
                C2 = 3.5e-4, #lu et al, 1996
                R1 = 6.9e-3 * (0.029), #rate of oG pairing with C is = D2 
                S = 0.02582221, #fitted to give mutS KO mut rate 40x wt (Swings et al)
                r = 17.3, #titrated to known H2O2 production rate of 14uM / second Seaver & Imlay - may be closer to 10, good enough 
                O3 = 56,  # fitted to give standing ROS conc of 1.9e-7 (Gonzalez - Fletcha)
                R2 = 6.9e-3 * (7.7e-8 / 2.1e-6), # relative efficiency of oG binding to A compared to G binding to c (I1) is 7.7e-8:2.1e-6 (Maki)
                Met1 = 1545,# fitted to reach expected carrying capacity from given glucose?
                CellVol = 1.0325e-12)#Kubitschek & Friske, 1986, mean of measurements in minimal media in exponential growth

log_glc <- lseq(3.1e-4, 62e-4, 5)

startsA <- c(iGlc_cyt = 0, dGTP_cyt = 0,  GDNA_cyt = 0, Gcell = 8.510873e-12, scale_Gcell2 = 0,
           cytVol = CellVol * 2175, ROS = 0, odGTP = 0, mGDNA = 0, mGcell = 0, scale_mGcell = 0)

parameters -> parA


# Defining the ODE model ------------------------------------------------------------------

modD2 <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    deGlc <- -U1 * (Gcell) * eGlc / (eGlc + Ks) #eGlc is mmol / ml total (molar conc)
    diGlc_cyt <- (U1 * (Gcell) * eGlc / (eGlc + Ks)) / cytVol - Met1 * M1 * iGlc_cyt
    ddGTP_cyt <- M1 * iGlc_cyt - I1 * dGTP_cyt - dGTP_cyt * ROS * O2
    dGDNA_cyt <- I1 * dGTP_cyt - D1 * GDNA_cyt + C2 * mGDNA + S * mGDNA + R1 * odGTP 
    dGcell <- (D1 * GDNA_cyt + R2 * mGDNA) * cytVol
    dscale_Gcell2 <- ((D1 * GDNA_cyt + R2 * mGDNA) * cytVol) * 6.02e20 /  2357528
    dcytVol <- ((D1 * GDNA_cyt + R2 * mGDNA) * cytVol * 6.02e20 /  2357528) * CellVol #number cells x ml / cell
    
    dROS <- -dGTP_cyt * ROS * O2 - O3 * ROS - kdiff * (ROS - ROS_ext)
    
    dROS_ext <-  ROSC2 + (ROS - ROS_ext) * kdiff * (cytVol / (1 - cytVol))
    
    dodGTP <- dGTP_cyt *  ROS * O2 - C1 * odGTP - I2 * odGTP - R1 * odGTP
    dmGDNA <- I2 * odGTP - D2 * mGDNA - C2 * mGDNA - S * mGDNA  - R2 * mGDNA
    dmGcell <- D2 * mGDNA * cytVol
    dscale_mGcell <- ((D2 * mGDNA) * cytVol) * 6.02e20 /  2357528
    list(c(deGlc, diGlc_cyt, ddGTP_cyt, dGDNA_cyt, dGcell, dscale_Gcell2, dcytVol, dROS, dROS_ext, dodGTP, dmGDNA, dmGcell, dscale_mGcell))
  })
}
startsD2 <- c(iGlc_cyt = 0, dGTP_cyt = 0,  GDNA_cyt = 0, Gcell = 8.510873e-12,
            scale_Gcell2 = 0, cytVol = CellVol * 2175, ROS = 0, ROS_ext = 0, odGTP = 0, mGDNA = 0, mGcell = 0,  
            scale_mGcell = 0)
parD2 <- c(parameters, ROSC2 = 6e-11)
parD2["kdiff"] <- 70
parD2["O2"] <- 4e+01


# Mutation Rate Estimating Function ---------------------------------------------

DAMP.slopeS <- function(CHOSENPARAMETERS, CHOSENMODEL, starts) {
  times <- seq(0, 1e5, by = 10)
  glc.data <- function(x) {as.data.frame(lsode(y = c(eGlc = x,  starts), 
                                                times = times, func = CHOSENMODEL, 
                                                parms = CHOSENPARAMETERS)) -> dataframe}
  data.1 <- try(glc.data(log_glc[1]), silent = TRUE)
  data.2 <- try(glc.data(log_glc[2]), silent = TRUE)
  data.3 <- try(glc.data(log_glc[3]), silent = TRUE)
  data.4 <- try(glc.data(log_glc[4]), silent = TRUE)
  data.5 <- try(glc.data(log_glc[5]), silent = TRUE)
  mutants <- function(dataframe) {(dataframe$mGcell[10001]) / (dataframe$mGcell[10001] + dataframe$Gcell[10001])}
  CELLS <- function(dataframe) {dataframe$Gcell[10001]}
  x <- c(CELLS(data.1), CELLS(data.2), CELLS(data.3), CELLS(data.4), CELLS(data.5))
  list(NA, NA) -> result
  if (as.numeric(is.na(x) == FALSE) %>% sum() > 4) {
    
    y <- c(mutants(data.1), mutants(data.2), mutants(data.3), mutants(data.4), mutants(data.5))
    stat <- function(dataframe) {
      lm(dataframe$scale_Gcell2[9000:10000]~c(9000:10000)) -> g
      return(as.numeric(g$coefficients[2]))
    }
    STAT <- as.list(c(stat(data.1), stat(data.2), stat(data.3), stat(data.4), stat(data.5)))
    DAMP.lm <- lm(log(y) ~ log(x + y))
    #result = lm, stationary, wt, mr
    list(DAMP.lm, STAT, as.list(x), as.list(y)) -> result}
  result
}

# Setting up Global Sensitivity analysis (GSA) by varying all variables from 0.1 to 10x given value --------

list(parD2) -> par_list
list(modD2) -> mod_list
list(startsD2) -> starts_list

rep(NA, 1) -> parN

for (i in 1:length(par_list)) {length(par_list[[i]]) -> parN[i]}
max(parN) -> maxPars
multipliers <- as.data.frame(matrix(nrow = N_Runs, ncol = maxPars))
multipliers[1,] <- 1
set.seed(1143)
for (i in 1:maxPars) {
  multipliers[2:N_Runs, i] <- sample(lseq(0.1, 10, N_Runs - 1))
}

output_list <- vector(mode = 'list', length = length(par_list))

format <- function(data) {
  for (i in c((ncol(data) - 14):ncol(data))) {
    list <- c(rep(NA, N_Runs))
      for (j in 1:N_Runs) {
      data[j, i] %>% unlist() %>% as.character() %>% as.numeric() -> val
      if (is_empty(val) == TRUE) {NA -> list[j]}
      else (val -> list[j])
    }
    list -> data[, i]
  }
  return(data)
}

length(par_list[[1]]) -> nPars
multipliers[, 1:nPars] -> nMult
nMult$ParSet <- 1:N_Runs
par_list[[1]] -> modPars
for (j in 1:nPars) {
    names(modPars)[j] -> names(nMult)[j]
    modPars[j] * nMult[, j] -> nMult[, j]
}
nMult %>% tibble() %>% rowwise() -> sensitivity
sensitivity %>% group_by(ParSet) %>% nest() -> sens.nest
  
sens.nest %>% add_column(Intercept = NA) -> sens.nest
sens.nest %>% add_column(Gradient = NA) -> sens.nest
sens.nest %>% add_column(Stationary = NA) -> sens.nest
sens.nest %>% add_column(Tried = NA) -> sens.nest  
sens.nest %<>% add_column(WT1 = NA)
sens.nest %<>% add_column(WT2 = NA)
sens.nest %<>% add_column(WT3 = NA)
sens.nest %<>% add_column(WT4 = NA)
sens.nest %<>% add_column(WT5 = NA)

sens.nest %<>% add_column(MR1 = NA)
sens.nest %<>% add_column(MR2 = NA)
sens.nest %<>% add_column(MR3 = NA)
sens.nest %<>% add_column(MR4 = NA)
sens.nest %<>% add_column(MR5 = NA)

sens.nest %<>% add_column(Stat1 = NA)
sens.nest %<>% add_column(Stat2 = NA)
sens.nest %<>% add_column(Stat3 = NA)
sens.nest %<>% add_column(Stat4 = NA)
sens.nest %<>% add_column(Stat5 = NA)
  

# Running GSA in parallel -------------------------------------------------

foreach(i = 1:N_Runs,
        .packages = c("tidyverse","magrittr","deSolve","emdbook"),
        .combine = 'c') %dofuture% {
    try(DAMP.slopeS(sens.nest[[2]][[i]],
                    mod_list[[1]], starts_list[[1]]), silent = TRUE) -> mod
    if (class(mod) == "try - error") {
      return(list(rep(NA, 3),"Yes", rep(NA, 15)))
      mod <- list(NA, NA)
      
      next}
    if (is.na(mod[1]) == TRUE) {
      return(list(rep(NA, 3),"Yes", rep(NA, 15)))
      mod <- list(NA, NA)
      
      next}
    mod[1] -> model
    return(list(c(model[[1]]$coefficients[1],
                model[[1]]$coefficients[2],
                mod[[2]] %>% as.numeric() %>% abs() %>% max(),
                "Yes",
                mod[[3]] %>% as.numeric(),
                mod[[4]] %>% as.numeric(),
                mod[[2]] %>% as.numeric()
                )))
    mod <- list(NA, NA)
} -> SensitivityOutput

for (i in 1:nrow(sens.nest)) {
  for (j in 1:19) {
    SensitivityOutput[[i]][j] -> sens.nest[i, j + 2]
  }
}

write.csv(sens.nest %>% unnest(data) %>% format(),"results/model_Dsens.csv")
sens.nest %>% unnest(data) %>% format() -> df

df[, c(1:23, 25:39)] <- lapply(df[, c(1:23, 25:39)], function(x) as.numeric(as.character(x)))


# Analysing sensitivity output (produces various plots & tables) -----------------------------

df$Model <- rep("D", each = N_Runs)

df %>% subset(!is.na(Gradient)) -> success
success %>% nrow() / (N_Runs)
table(success$Model) %>% t() %>% t() %>% write.csv("results/SuccessRuns.csv")#successfully computed parsets for each model
success %<>% mutate(ReachStat =
                    (Stationary < 1 & MR1 > 0 & MR2 > 0 & MR3 > 0 & MR4 > 0 & MR5 > 0))
ggplot(success, aes(Model, Gradient,
                   colour = ReachStat)) +
  geom_jitter(alpha = 0.5, size = 2, width = 0.2) + theme_bw() -> plot1
  ggsave("results/Filter1.png", plot1, width = 5, height = 5, units = "in")

success %>% subset(Stationary < 1 & MR1 > 0 & MR2 > 0 & MR3 > 0 & MR4 > 0 & MR5 > 0) -> stationary
stationary %>% nrow() / (N_Runs)
stationary %>% ggplot(aes(Model, WT1 * 6.02e20 / 2356491)) +
  geom_text(aes(label = ParSet), size = 2) +
  scale_y_log10() + theme_bw() + ylab("CFU.mL at 50mg.L glc") +
  geom_hline(yintercept = 1e7, colour = "red", lwd = 2) +
  geom_hline(yintercept = 1e10, colour = "red", lwd = 2) -> plot2
ggsave("results/Filter2.png", plot2, width = 5, height = 5, units = "in")

stationary %>% subset(WT1 * 6.02e20 / 2356491 > 1e7 &
                      WT1 * 6.02e20 / 2356491 < 1e10 &
                      WT2 * 6.02e20 / 2356491 > 1e7 &
                      WT2 * 6.02e20 / 2356491 < 1e10 &
                      WT3 * 6.02e20 / 2356491 > 1e7 &
                      WT3 * 6.02e20 / 2356491 < 1e10 &
                      WT4 * 6.02e20 / 2356491 > 1e7 &
                      WT4 * 6.02e20 / 2356491 < 1e10 &
                      WT5 * 6.02e20 / 2356491 > 1e7 &
                      WT5 * 6.02e20 / 2356491 < 1e10) -> goodNt

goodNt %>% nrow() / (N_Runs)

goodNt %>% ggplot(aes(Model, WT5 * 6.02e20 / 2356491)) +
  geom_jitter(alpha = 0.5, size = 2) +
  scale_y_log10(limits = c(1e5, 1e12)) + theme_bw() + ylab("CFU.mL at 1100mg.L glc") +
  geom_hline(yintercept = 1e7, colour = "red", lwd = 2) +
  geom_hline(yintercept = 1e10, colour = "red", lwd = 2) -> plot3
ggsave("results/Filter3.png", plot3, width = 5, height = 5, units = "in")
goodNt %>% filter(WT1 < WT2 & WT2 < WT3 & WT3 < WT4 & WT4 < WT5) -> orderNt
orderNt %>% nrow() / (N_Runs)

orderNt %>% filter(MR1 < 2e-8 & MR2 < 2e-8 &
                   MR3 < 2e-8 & MR4 < 2e-8 & MR5 < 2e-8) -> LowMR
LowMR %>% nrow() / (N_Runs)

LowMR %>% filter(MR1 > 2e-12 & MR2 > 2e-12 &
                 MR3 > 2e-12 & MR4 > 2e-12 & MR5 > 2e-12) -> HighMR
HighMR %>% nrow() / (N_Runs)

FIND_RSQ <- function(MR1, MR2, MR3, MR4, MR5,
                   WT1, WT2, WT3, WT4, WT5) {
  lm(log(as.numeric(c(MR1, MR2, MR3, MR4, MR5)))~
       log(as.numeric(c(WT1, WT2, WT3, WT4, WT5)))) -> mo
  return(summary(mo)$adj.r.squared)
}

HighMR$rsq <- NA
for (i in 1:nrow(HighMR)) {
  FIND_RSQ(HighMR$MR1[i], HighMR$MR2[i], HighMR$MR3[i], HighMR$MR4[i], HighMR$MR5[i],
           HighMR$WT1[i], HighMR$WT2[i], HighMR$WT3[i], HighMR$WT4[i], HighMR$WT5[i]) -> HighMR$rsq[i]
}

ggplot(HighMR, aes(Model, rsq, colour = Gradient)) +
  geom_jitter(alpha = 0.5) + theme_bw() + geom_hline(yintercept = 0.5, colour = "red") +
  scale_color_viridis_c() -> plot4
ggsave("results/Filter4.png", plot4, width = 5, height = 5, units = "in")


HighMR %>% filter(rsq > 0.5) -> rsqT

rsqT %>% nrow() / (N_Runs)

table(rsqT$Model) %>% t() %>% t() %>% write.csv("results/FilteredRuns.csv")#table of runs per model after filter

rsqT %>%
  ggplot(aes(Model, Gradient, colour = Stationary)) +
  geom_jitter(width = 0.2, alpha = 0.5) -> plot5
ggsave("results/Filter5.png", plot5, width = 5, height = 5, units = "in")


rsqT %>%
  group_by(Model) %>%
  dplyr::summarize(q25 = quantile(Gradient, probs = 0.25),
            q50 = quantile(Gradient, probs = 0.5),
            q75 = quantile(Gradient, probs = 0.75),
            max = max(Gradient),
            min = min(Gradient),
            count = n()) %>%
  as.data.frame() -> summaryt
write.csv(summaryt,"results/SensSummary.csv")

write.csv(rsqT,"results/CombinedModels.csv")


# Calculating Spearman's Rank Correlation between each parameter and mutation rate plasticity --------------------------------------------

#This creates the key output plot of interest
rsqT %>% subset(Model == "D") -> df_A
names(df_A)
df_A %<>% pivot_longer(cols = c(2:20), names_to = "Parameter", values_to = "Value")
df_A$Parameter %<>% as.factor()
df_A$Model %<>% as.factor()
df_A$Parameter %>% levels() %>% length() -> npar
rho <- as.data.frame(matrix(nrow = 1, ncol = 4))
names(rho) <- c("Parameter","Model","SpRho","pValue")
for (i in c(1:npar)) {
  df_A %>% subset(Parameter == levels(df_A$Parameter)[i]) -> df3
  corr <- cor.test(x = log(df3$Value), y = df3$Gradient, method = 'spearman',
                   alternative = "two.sided", exact = FALSE)
  c(levels(df_A$Parameter)[i],"A", corr$estimate, corr$p.value) -> l
  rho %<>% rbind(l)
}

rho %>% arrange(abs(as.numeric(SpRho))) -> ar
rho$Parameter <- factor(rho$Parameter, levels = ar$Parameter)
ggplot(rho[-1,], aes(Parameter, abs(as.numeric(SpRho)), colour = as.numeric(pValue) < 0.05,
                    fill = SpRho > 0)) +
  geom_point(size = 4.5, shape = 21, stroke = 1.5) +
  scale_colour_manual(values = c("white","black"), name = "P < 0.05") +
  theme_bw() +
  ylab("Absolute Spearman's Rho") +
  scale_fill_manual(name = "Correlation",
                    breaks = c(TRUE, FALSE),
                    labels = c("Positive","Negative"),
                    values = c(brewer.pal("Dark2", n = 8)[c(1, 2)])) +
  ggtitle("DAMP") -> DAMPplot

ggsave("results/OutputPlot.png", DAMPplot, units = "in", width = 8, height = 5)
