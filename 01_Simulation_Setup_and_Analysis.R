# 5300 report code. 
# Team 10 

############
#Q1 - S1 no effect
n <- 440
library(data.table);library(DT)
set.seed(329)
rev.dat_Q1_S1 <- data.table(Group = c(rep.int(x = "Alacarte", times = n/2), rep.int(x = "Buffet", times = n/2)))

rev.dat_Q1_S1[Group == "Alacarte", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 530), digits = 1)]
rev.dat_Q1_S1[Group == "Buffet", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 560), digits = 1)]
datatable(data = rev.dat_Q1_S1)

rev.dat_Q1_S1
# function:
analyze.experiment <- function(the.dat_Q1_S1) {
  require(data.table)
  setDT(the.dat_Q1_S1)
  
  the.test_Q1_S1 <- t.test(x = the.dat_Q1_S1[Group == "Alacarte", Rev], y = the.dat_Q1_S1[Group == "Buffet", Rev], alternative = "two.sided")
  
  the.effect_Q1_S1 <- the.test_Q1_S1$estimate[1] - the.test_Q1_S1$estimate[2]
  upper.bound <- the.test_Q1_S1$conf.int[2]
  p <- the.test_Q1_S1$p.value
  
  result_Q1_S1 <- data.table(effect = the.effect_Q1_S1, upper_ci = upper.bound, p = p)
  
  
  return(result_Q1_S1)}


# testing
analyze.experiment(the.dat_Q1_S1 = rev.dat_Q1_S1)

# 1000 repetition
B <- 1000
n <- 440
RNGversion(vstr = 3.6)
set.seed(329)
Experiment <- 1:B
Group <- c(rep.int( x = "Alacarte", times  = n/2), rep.int(x = "Buffet", times = n/2))

sim.dat_Q1_S1 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv( x = sim.dat_Q1_S1, cols = c("Experiment", "Group"), order = c(1,1))

sim.dat_Q1_S1[Group == "Buffet", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 530), digits = 1)]
sim.dat_Q1_S1[Group == "Alacarte", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 560), digits = 1)]
dim(sim.dat_Q1_S1)

# All Simulated Experiments
exp.results_Q1_S1 <- sim.dat_Q1_S1[, analyze.experiment(the.dat_Q1_S1 = .SD),
                       keyby = "Experiment"]

exp.results_Q1_S1

DT :: datatable(data = round(x = exp.results_Q1_S1[1:100,], digits = 3),
                rownames = F)

#Further Analyses
#assume effect size  (no effect)
# power
exp.results_Q1_S1[, mean(p < 0.05)]

# FP,TN

FP_Q1_S1 <- mean(exp.results_Q1_S1$p < 0.05)
TN_Q1_S1 <- mean(exp.results_Q1_S1$p >= 0.05)

FP_Q1_S1
TN_Q1_S1

effect_Q1_S1 <- exp.results_Q1_S1[, summary(effect)]
CI_effect_Q1_S1 <- exp.results_Q1_S1[, summary(upper_ci)]

effect_Q1_S1
CI_effect_Q1_S1

#Q1 S2 effect
n <- 440
library(data.table);library(DT)
set.seed(329)
rev.dat_Q1_S2 <- data.table(Group = c(rep.int(x = "Alacarte", times = n/2), rep.int(x = "Buffet", times = n/2)))

rev.dat_Q1_S2[Group == "Buffet", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 500), digits = 1)]
rev.dat_Q1_S2[Group == "Alacarte", Rev := round(x = rnorm(n = .N, mean = 5825, sd = 500), digits = 1)]
datatable(data = rev.dat_Q1_S2)

rev.dat_Q1_S2
# function:
analyze.experiment_Q1_S2 <- function(the.dat_Q1_S2) {
  require(data.table)
  setDT(the.dat_Q1_S2)
  
  the.test_Q1_S2 <- t.test(x = the.dat_Q1_S2[Group == "Alacarte", Rev], y = the.dat_Q1_S2[Group == "Buffet", Rev], alternative = "two.sided")
  
  the.effect_Q1_S2 <- the.test_Q1_S2$estimate[1] - the.test_Q1_S2$estimate[2]
  upper.bound_Q1_S2 <- the.test_Q1_S2$conf.int[2]
  p_Q1_S2 <- the.test_Q1_S2$p.value
  
  result_Q1_S2 <- data.table(effect = the.effect_Q1_S2, upper_ci = upper.bound_Q1_S2, p = p_Q1_S2)
  
  
  return(result_Q1_S2)}


# testing
analyze.experiment_Q1_S2(the.dat_Q1_S2 = rev.dat_Q1_S2)

# 1000 repetition
B <- 1000
n <- 440
RNGversion(vstr = 3.6)
set.seed(329)
Experiment <- 1:B
Group <- c(rep.int( x = "Alacarte", times  = n/2), rep.int(x = "Buffet", times = n/2))

sim.dat_Q1_S2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv( x = sim.dat_Q1_S2, cols = c("Experiment", "Group"), order = c(1,1))

sim.dat_Q1_S2[Group == "Buffet", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 500), digits = 1)]
sim.dat_Q1_S2[Group == "Alacarte", Rev := round(x = rnorm(n = .N, mean = 5825, sd = 500), digits = 1)]
dim(sim.dat_Q1_S2)

# All Simulated Experiments
exp.results_Q1_S2 <- sim.dat_Q1_S2[, analyze.experiment_Q1_S2(the.dat_Q1_S2 = .SD),
                             keyby = "Experiment"]

exp.results_Q1_S2

DT :: datatable(data = round(x = exp.results_Q1_S2[1:100,], digits = 3),
                rownames = F)

#Further Analyses
# Assume effect size  (effect)

assumed_effect_size_Q1_S2 <- 562.5


# power
exp.results_Q1_S2[, mean(p < 0.05)]

# FN , TP
FN_Q1_S2 <- mean(exp.results_Q1_S2$p >= 0.05 & abs(exp.results_Q1_S2$effect) < assumed_effect_size_Q1_S2)
TP_Q1_S2 <- mean(exp.results_Q1_S2$p < 0.05 & abs(exp.results_Q1_S2$effect) >= assumed_effect_size_Q1_S2)

FN_Q1_S2
TP_Q1_S2

effect_Q1_S2 <- exp.results_Q1_S2[, summary(effect)]

CI_effect_Q1_S2 <- exp.results_Q1_S2[, summary(upper_ci)]

effect_Q1_S2
CI_effect_Q1_S2


#Q2- S1 no effect
n <- 440
library(data.table);library(DT)
set.seed(329)
rev.dat_Q2_S1 <- data.table(Group = c(rep.int(x = "With Take-Out Revenue", times = n/2), rep.int(x = "Without Take-Out Revenue", times = n/2)))

rev.dat_Q2_S1[Group == "With Take-Out Revenue", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 500), digits = 1)]
rev.dat_Q2_S1[Group == "Without Take-Out Revenue", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 580), digits = 1)]
datatable(data = rev.dat_Q2_S1)

rev.dat_Q2_S1
###define functions:
analyze.experiment <- function(the.dat_Q2_S1) {
  require(data.table)
  setDT(the.dat_Q2_S1)
  
  the.test_Q2_S1 <- t.test(x = the.dat_Q2_S1[Group == "With Take-Out Revenue", Rev], y = the.dat_Q2_S1[Group == "Without Take-Out Revenue", Rev], alternative = "two.sided")
  
  the.effect_Q2_S1 <- the.test_Q2_S1$estimate[1] - the.test_Q2_S1$estimate[2]
  upper.bound_Q2_S1 <- the.test_Q2_S1$conf.int[2]
  p_Q2_S1 <- the.test_Q2_S1$p.value
  
  result_Q2_S1 <- data.table(effect = the.effect_Q2_S1, upper_ci = upper.bound_Q2_S1, p = p_Q2_S1)
  
  
  return(result_Q2_S1)}


# testing
analyze.experiment(the.dat_Q2_S1 = rev.dat_Q2_S1)

# 1000 repetition
B <- 1000
n <- 440
RNGversion(vstr = 3.6)
set.seed(329)
Experiment <- 1:B
Group <- c(rep.int( x = "With Take-Out Revenue", times  = n/2), rep.int(x = "Without Take-Out Revenue", times = n/2))

sim.dat_Q2_S1 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv( x = sim.dat_Q2_S1, cols = c("Experiment", "Group"), order = c(1,1))

sim.dat_Q2_S1[Group == "With Take-Out Revenue", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 500), digits = 1)]
sim.dat_Q2_S1[Group == "Without Take-Out Revenue", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 580), digits = 1)]
dim(sim.dat_Q2_S1)

# all simulated Experiments
exp.results_Q2_S1 <- sim.dat_Q2_S1[, analyze.experiment(the.dat_Q2_S1 = .SD),
                                   keyby = "Experiment"]
exp.results_Q2_S1

DT :: datatable(data = round(x = exp.results_Q2_S1[1:100,], digits = 3),
                rownames = F)

#Further Analyses
# assume effect size( no effect)

# power
exp.results_Q2_S1[, mean(p < 0.05)]

# FP,TN

FP_Q2_S1 <- mean(exp.results_Q2_S1$p < 0.05)
TN_Q2_S1 <- mean(exp.results_Q2_S1$p >= 0.05)

FP_Q2_S1
TN_Q2_S1

effect_Q2_S1 <- exp.results_Q2_S1[, summary(effect)]
CI_effect_Q2_S1 <- exp.results_Q2_S1[, summary(upper_ci)]

effect_Q2_S1
CI_effect_Q2_S1





#Q2 S2 effect
n <- 440
library(data.table);library(DT)
set.seed(329)
rev.dat_Q2_S2 <- data.table(Group = c(rep.int(x = "With Take-Out Revenue", times = n/2), rep.int(x = "Without Take-Out Revenue", times = n/2)))

rev.dat_Q2_S2[Group == "With Take-Out Revenue", Rev := round(x = rnorm(n = .N, mean = 6000, sd = 500), digits = 1)]
rev.dat_Q2_S2[Group == "Without Take-Out Revenue", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 580), digits = 1)]
datatable(data = rev.dat_Q2_S2)

rev.dat_Q2_S2
###define functions:
analyze.experiment_Q2_S2 <- function(the.dat_Q2_S2) {
  require(data.table)
  setDT(the.dat_Q2_S2)
  
  the.test_Q2_S2 <- t.test(x = the.dat_Q2_S2[Group == "With Take-Out Revenue", Rev], y = the.dat_Q2_S2[Group == "Without Take-Out Revenue", Rev], alternative = "two.sided")
  
  the.effect_Q2_S2 <- the.test_Q2_S2$estimate[1] - the.test_Q2_S2$estimate[2]
  upper.bound_Q2_S2 <- the.test_Q2_S2$conf.int[2]
  p_Q2_S2 <- the.test_Q2_S2$p.value
  
  result_Q2_S2 <- data.table(effect = the.effect_Q2_S2, upper_ci = upper.bound_Q2_S2, p = p_Q2_S2)
  
  
  return(result_Q2_S2)}


# testing
analyze.experiment_Q2_S2(the.dat_Q2_S2 = rev.dat_Q2_S2)

# 1000 repetition
B <- 1000
n <- 440
RNGversion(vstr = 3.6)
set.seed(329)
Experiment <- 1:B
Group <- c(rep.int( x = "With Take-Out Revenue", times  = n/2), rep.int(x = "Without Take-Out Revenue", times = n/2))

sim.dat_Q2_S2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv( x = sim.dat_Q2_S2, cols = c("Experiment", "Group"), order = c(1,1))

sim.dat_Q2_S2[Group == "With Take-Out Revenue", Rev := round(x = rnorm(n = .N, mean = 6000, sd = 500), digits = 1)]
sim.dat_Q2_S2[Group == "Without Take-Out Revenue", Rev := round(x = rnorm(n = .N, mean = 5625, sd = 500), digits = 1)]
dim(sim.dat_Q2_S2)

# all simulated Experiments
exp.results_Q2_S2 <- sim.dat_Q2_S2[, analyze.experiment_Q2_S2(the.dat_Q2_S2 = .SD),
                                   keyby = "Experiment"]

exp.results_Q2_S2

DT :: datatable(data = round(x = exp.results_Q2_S2[1:100,], digits = 3),
                rownames = F)

#Further Analyses
# assume effect size  (effect)
assumed_effect_size_Q2_S2 <- 562.5


# power
exp.results_Q2_S2[, mean(p < 0.05)]

# FN , TP

FN_Q2_S2 <- mean(exp.results_Q2_S2$p >= 0.05 & abs(exp.results_Q2_S2$effect) < assumed_effect_size_Q2_S2)
TP_Q2_S2 <- mean(exp.results_Q2_S2$p < 0.05 & abs(exp.results_Q2_S2$effect) >= assumed_effect_size_Q2_S2)

FN_Q2_S2
TP_Q2_S2

effect_Q2_S2 <- exp.results_Q2_S2[, summary(effect)]
CI_effect_Q2_S2 <- exp.results_Q2_S2[, summary(upper_ci)]

effect_Q2_S2
CI_effect_Q2_S2



# Form
results_df <- data.frame(
  Research_Question = c("Question 1", "Question 1", "Question 2", "Question 2"),
  Scenario = c("No Effect", "Effect: (Size)", "No Effect", "Effect: (Size)"),
  Mean_Effect = c(effect_Q1_S1["Mean"], effect_Q1_S2["Mean"], effect_Q2_S1["Mean"], effect_Q2_S2["Mean"]), 
  CI_Mean = c(CI_effect_Q1_S1["Mean"], CI_effect_Q1_S2["Mean"], CI_effect_Q2_S1["Mean"], CI_effect_Q2_S2["Mean"]), 
  Percentage_FP = c(FP_Q1_S1, NA, FP_Q2_S1, NA), 
  Percentage_TN = c(TN_Q1_S1, NA, TN_Q2_S1, NA),
  Percentage_FN = c(NA, FN_Q1_S2, NA, FN_Q2_S2  ), 
  Percentage_TP = c(NA, TP_Q1_S2, NA, TP_Q2_S2  )  
)

print(results_df)









