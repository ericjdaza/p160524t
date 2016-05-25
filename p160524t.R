# ----- template.git.R.v160524t ----- #

# ========================================================= #
# ========================================================= #
# ===== Preliminary code (e.g., packages, libraries). ===== #
# ========================================================= #
# ========================================================= #
# install.packages("aod")
# install.packages("ggplot2")
library(aod)
library(ggplot2)
library(gam)
library(splines)
library(CausalGAM)
library(plyr)
library(ipw)
library(plotrix)
require(gridExtra)
library(twang)
library(MatchIt)
library(weights)
library(sandwich)
library(CBPS)
library(boot)
library(foreign)
.pardefault <- par(no.readonly = T) # save default graphics settings

# Random seed for bootstrap procedures.
scalar.seed <- 1511020118







# ========================== #
# ========================== #
# ===== Documentation. ===== #
# ========================== #
# ========================== #
#
# 160524t [Daza_EJ] --- Created program from IMPACT.R, which was itself created on 151006t.
#







# ============================ #
# ============================ #
# ===== Data Management. ===== #
# ============================ #
# ============================ #





##### Path and variable names: Define.
# pathfile.rawdatasource <- 
# var.reemployment12mo <- 
# var.smoking <- 
# var.timeunemployed <- 
# var.age <- 
# var.education <- 
# var.race <- 
# var.healthstatus <- 
# var.sex <- Gender
# var.drugalcohol <- 
# var.criminalhistory <- 
# var.transportation <- 
# var.housing <- 
# pathfile.analdata <- 





##### Raw datasets: Read, import, load, etc.
rawdata <- read.spss(pathfile.rawdatasource, to.data.frame=TRUE)





##### Keep only variables to use.
attach(rawdata)

# Variables for causal inference analysis.
preanaldata1 <- data.frame(
  var.reemployment12mo,
  var.smoking,
  var.timeunemployed,
  var.age,
  var.education,
  var.race,
  var.healthstatus,
  var.sex,
  var.drugalcohol,
  var.criminalhistory,
  var.transportation,
  var.housing
)

detach(rawdata)





##### Preliminary data management and diagnostics.



### Analysis data.

# Check the outcome and treatment variable modes (i.e., types). These should both be binary (i.e., factor).
for ( i in 1:dim(preanaldata1)[2] ) {
  
  print(colnames(preanaldata1)[i])
  print(class(preanaldata1[,i]))
  print(table(preanaldata1[,i]))
  
}

# Code template to use:
#
# class(var)
# table(var)
# table(c(var))
# # rm(new_var)
# new_var <- as.vector(rep(NA, length(var)))
# for ( j in 1:length(var) ) {
# 
#   ifelse(
#     var[j] == "?",
#     new_var[j] <- 1,
#     ifelse(
#       var[j] == "?",
#       new_var[j] <- 2,
#       new_var[j] <- 0
#     )
#   )
# 
# }
# class(new_var)
# table(new_var)
# new_var <- as.factor(new_var)
# new_var <- as.logical(new_var)
# class(new_var)
# table(new_var)
# table(c(var))
# table(var)

attach(preanaldata1)


## Outcome.
class(var.reemployment12mo)
table(var.reemployment12mo)
table(c(var.reemployment12mo))
# rm(new_var.reemployment12mo)
new_var.reemployment12mo <- as.vector(rep(NA, length(var.reemployment12mo)))
for ( j in 1:length(var.reemployment12mo) ) {
  
  ifelse(
    var.reemployment12mo[j] == "Yes",
    new_var.reemployment12mo[j] <- 1,
    new_var.reemployment12mo[j] <- 0
  )
  
}
class(new_var.reemployment12mo)
table(new_var.reemployment12mo)
aslogical_new_var.reemployment12mo <- as.logical(new_var.reemployment12mo)
class(aslogical_new_var.reemployment12mo)
table(aslogical_new_var.reemployment12mo)


## Exposure.
class(var.smoking)
table(var.smoking)
table(c(var.smoking))
# rm(new_var.smoking)
new_var.smoking <- as.vector(rep(NA, length(var.smoking)))
for ( j in 1:length(var.smoking) ) {
  
  ifelse(
    var.smoking[j] == "No",
    new_var.smoking[j] <- 1,
    new_var.smoking[j] <- 0
  )
  
}
class(new_var.smoking)
table(new_var.smoking) # 0 = smoker, 1 = non-smoker
aslogical_new_var.smoking <- as.logical(new_var.smoking)
class(aslogical_new_var.smoking)
table(aslogical_new_var.smoking)


## Covariates.

class(var.timeunemployed)
summary(var.timeunemployed)
par(.pardefault, mfrow=c(1,1))
hist(var.timeunemployed)
hist(log(var.timeunemployed))

class(var.age)
summary(var.age)
hist(var.age)

class(var.education)
summary(var.education)
hist(var.education)

class(var.race)
table(var.race)
table(c(var.race))
# rm(new_var.race)
new_var.race <- as.vector(rep(NA, length(var.race)))
for ( j in 1:length(var.race) ) {
  
  ifelse(
    var.race[j] == "white",
    new_var.race[j] <- 1,
    ifelse(
      var.race[j] == "black",
      new_var.race[j] <- 2,
      new_var.race[j] <- 0
    )
  )
  
}
class(new_var.race)
table(new_var.race)
asfactor_new_var.race <- as.factor(new_var.race)
class(asfactor_new_var.race)
table(asfactor_new_var.race)

class(var.healthstatus)
table(var.healthstatus)
table(c(var.healthstatus))
# rm(new_var.healthstatus)
new_var.healthstatus <- as.vector(rep(NA, length(var.healthstatus)))
for ( j in 1:length(var.healthstatus) ) {
  
  ifelse(
    var.healthstatus[j] == "Fair",
    new_var.healthstatus[j] <- 1,
    ifelse(
      var.healthstatus[j] == "Good",
      new_var.healthstatus[j] <- 2,
      ifelse(
        var.healthstatus[j] == "Very good",
        new_var.healthstatus[j] <- 3,
        ifelse(
          var.healthstatus[j] == "Excellent",
          new_var.healthstatus[j] <- 4,
          new_var.healthstatus[j] <- 0
        )
      )
    )
  )
  
}
class(new_var.healthstatus)
table(new_var.healthstatus)
asfactor_new_var.healthstatus <- as.factor(new_var.healthstatus)
class(asfactor_new_var.healthstatus)
table(asfactor_new_var.healthstatus)

class(var.sex)
table(var.sex)
table(c(var.sex))
# rm(new_var.sex)
new_var.sex <- as.vector(rep(NA, length(var.sex)))
for ( j in 1:length(var.sex) ) {
  
  ifelse(
    var.sex[j] == "Male",
    new_var.sex[j] <- 1,
    new_var.sex[j] <- 0
  )
  
}
class(new_var.sex)
table(new_var.sex)
aslogical_new_var.sex <- as.logical(new_var.sex)
class(aslogical_new_var.sex)
table(aslogical_new_var.sex)

class(var.drugalcohol)
table(var.drugalcohol)
table(c(var.drugalcohol))
# rm(new_var.drugalcohol)
new_var.drugalcohol <- as.vector(rep(NA, length(var.drugalcohol)))
for ( j in 1:length(var.drugalcohol) ) {
  
  ifelse(
    var.drugalcohol[j] == "yes",
    new_var.drugalcohol[j] <- 1,
    new_var.drugalcohol[j] <- 0
  )
  
}
class(new_var.drugalcohol)
table(new_var.drugalcohol)
aslogical_new_var.drugalcohol <- as.logical(new_var.drugalcohol)
class(aslogical_new_var.drugalcohol)
table(aslogical_new_var.drugalcohol)

class(var.criminalhistory)
table(var.criminalhistory)
table(c(var.criminalhistory))
# rm(new_var.criminalhistory)
new_var.criminalhistory <- as.vector(rep(NA, length(var.criminalhistory)))
for ( j in 1:length(var.criminalhistory) ) {
  
  ifelse(
    var.criminalhistory[j] == "Yes",
    new_var.criminalhistory[j] <- 1,
    new_var.criminalhistory[j] <- 0
  )
  
}
class(new_var.criminalhistory)
table(new_var.criminalhistory)
aslogical_new_var.criminalhistory <- as.logical(new_var.criminalhistory)
class(aslogical_new_var.criminalhistory)
table(aslogical_new_var.criminalhistory)

class(var.transportation)
table(var.transportation)
table(c(var.transportation))
# rm(new_var.transportation)
new_var.transportation <- as.vector(rep(NA, length(var.transportation)))
for ( j in 1:length(var.transportation) ) {
  
  ifelse(
    var.transportation[j] == "Yes",
    new_var.transportation[j] <- 1,
    new_var.transportation[j] <- 0
  )
  
}
class(new_var.transportation)
table(new_var.transportation)
aslogical_new_var.transportation <- as.logical(new_var.transportation)
class(aslogical_new_var.transportation)
table(aslogical_new_var.transportation)

class(var.housing)
table(var.housing)
table(c(var.housing))
# rm(new_var.housing)
new_var.housing <- as.vector(rep(NA, length(var.housing)))
for ( j in 1:length(var.housing) ) {
  
  ifelse(
    var.housing[j] == "other",
    new_var.housing[j] <- 1,
    new_var.housing[j] <- 0
  )
  
}
class(new_var.housing)
table(new_var.housing)
aslogical_new_var.housing <- as.logical(new_var.housing)
class(aslogical_new_var.housing)
table(aslogical_new_var.housing)

detach(preanaldata1)



### Keep only variables to use.

# Analysis.
preanaldata1_keepvars <- preanaldata1[
  c(
    "var.age",
    "var.education",
    "var.timeunemployed"
  )
  ]
preanaldata2 <- data.frame(
  cbind(
    preanaldata1_keepvars,
    aslogical_new_var.reemployment12mo,
    aslogical_new_var.smoking,
    asfactor_new_var.race,
    new_var.healthstatus,
    aslogical_new_var.sex,
    aslogical_new_var.drugalcohol,
    aslogical_new_var.criminalhistory,
    aslogical_new_var.transportation,
    aslogical_new_var.housing
  )
)




#### Remove all rows with an NA value for any variable, for proper balance.IPW processing below.


## Analysis.

# Any NA's?
any(is.na(preanaldata2)) # = TRUE

# Count the NA's per variable.
apply(preanaldata2, 2, function(x) table(is.na(x)))
#   equivalent code: for elements "x" defined as column variables in a data frame
#   sapply(preanaldata2, function(x) table(is.na(x)))

# Remove rows.
rowhasanyNA <- matrix(
  apply(preanaldata2, 1, function(x) any(is.na(x))),
  ncol = 1,
  nrow = dim(preanaldata2)[1]
)
preanaldata2a <- cbind(preanaldata2, rowhasanyNA)
table(preanaldata2a$rowhasanyNA) # This should match the output above from "apply(preanaldata2, 2, function(x) table(is.na(x)))".
preanaldata3 <- subset(
  preanaldata2a,
  rowhasanyNA == FALSE
)
preanaldata3$rowhasanyNA <- NULL # Drop temporary variable "rowhasanyNA".
#   equivalent code: for multiple drop variables
#   dropvarsindicators <- names(preanaldata3) %in% c("rowhasanyNA") 
#   preanaldata3 <- preanaldata3[!dropvarsindicators] # Drop temporary variable "rowhasanyNA".

# Any NA's? Should be FALSE.
any(is.na(preanaldata3)) # = FALSE





##### Analysis datasets: Construct, export, etc.
analdata <- preanaldata3
write.table(analdata, pathfile.analdata, sep="\t", row.names=FALSE, col.names=TRUE)







# ===================== #
# ===================== #
# ===== Analyses. ===== #
# ===================== #
# ===================== #





##### Path and variable names: Define.
# path.psmodeldiag <- 
# pathfile.plot_untrimmed_model05model10.pdf <- 
# pathfile.plot_trimmed_model05model10.pdf <- 





##### Causal logit regression: production code.

# Set x-axis limits for IPW plots. These are based on manually checking the range of all IPW plots, and manually picking an appropriate inclusive range.
xlim.ipwplots <- c(0, 30)




#### Estimate PS's.



### Function to produce PS model selection diagnostics: psmodeldiag.
psmodeldiag <- function(
  dataset,
  var.trt,
  formula.ps,
  modelname
)
{
  
  
  ## Calculate quantities needed.
  
  # Calculate PS; i.e., IPW and sIPW denominator.
  glm.ps <- glm(data = dataset, formula = formula.ps, family = "binomial")
  var.predict.pshat <- predict(glm.ps, newdata = dataset, type = "response")
  
  # Calculate sIPW numerator.
  glm.psmarginal <- glm(data = dataset, formula = var.trt ~ 1, family = "binomial")
  var.predict.psmarginal <- predict(glm.psmarginal, newdata = dataset, type = "response")
  
  # Calculate IPWs and sIPWs.
  var.ipwhat_trt0 <- 1 / ( 1 - var.predict.pshat ) # = 1 / Pr(D=0|S)
  var.sipwhat_trt0 <- ( 1 - var.predict.psmarginal ) / ( 1 - var.predict.pshat ) # = Pr(D=0) / Pr(D=0|S)
  var.ipwhat_trt1 <- 1 / var.predict.pshat # = 1 / Pr(D=1|S)
  var.sipwhat_trt1 <- var.predict.psmarginal / var.predict.pshat # = Pr(D=1) / Pr(D=1|S)
  
  # Create the PS and IPW plot data.
  var.predict.pshat_trt0 <- ifelse(var.trt == 0, var.predict.pshat, NA)
  var.predict.pshat_trt1 <- ifelse(var.trt == 1, var.predict.pshat, NA)
  
  # Create the PS plot data.
  data.frame.ps_trt0 <- data.frame(var.predict.pshat_trt0, row.names = NULL)
  names(data.frame.ps_trt0) <- c("propensityscore")
  data.frame.ps_trt1 <- data.frame(var.predict.pshat_trt1, row.names = NULL)
  names(data.frame.ps_trt1) <- c("propensityscore")
  data.frame.ps_trt0$exposuregroup <- "Smoker"
  data.frame.ps_trt1$exposuregroup <- "Non-Smoker"
  data.frame.propensityscore_exposuregroup <- rbind(data.frame.ps_trt0, data.frame.ps_trt1)
  
  # Create the IPW plot data.
  data.frame.ipw_1minusps <- data.frame(1/(1-var.predict.pshat_trt0), row.names = NULL)
  names(data.frame.ipw_1minusps) <- c("ipw")
  data.frame.ipw_ps <- data.frame(1/var.predict.pshat_trt1, row.names = NULL)
  names(data.frame.ipw_ps) <- c("ipw")
  data.frame.ipw_1minusps$exposuregroup <- "Smoker"
  data.frame.ipw_ps$exposuregroup <- "Non-Smoker"
  data.frame.ipw_exposuregroup <- rbind(data.frame.ipw_1minusps, data.frame.ipw_ps)
  
  
  ## Make the plots.
  
  # Prepare PS overlap plot.
  plot.psoverlap <- ggplot(data.frame.propensityscore_exposuregroup, aes(propensityscore, fill = exposuregroup)) + geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.05) + scale_fill_manual(values=c("green", "red")) + xlim(c(0, 1)) + ylim(c(0, 30)) # ylim is manually determined by examining all relevant output figures
  
  # Prepare IPW distribution plot.
  plot.ipwdistribution <- ggplot(data.frame.ipw_exposuregroup, aes(ipw, fill = exposuregroup)) + geom_bar(alpha = 1.0, position = "dodge", binwidth = 2) + scale_fill_manual(values=c("green", "red")) + xlim(xlim.ipwplots) + ylim(c(0, 90)) # xlim and ylim are manually determined by examining all relevant output figures
  
  # Side-by-side plots.
  pdf(
    file = paste0(
      path.psmodeldiag,
      modelname,
      ".pdf"
    ),
    height=2.75,
    width=8,
    onefile=T
  )
  plot(plot.psoverlap)
  dev.off()


  ## Return calculated quantities.
  return(
    cbind(
      var.predict.pshat,
      var.predict.psmarginal,
      var.ipwhat_trt0,
      var.sipwhat_trt0,
      var.ipwhat_trt1,
      var.sipwhat_trt1,
      var.predict.pshat_trt0,
      var.predict.pshat_trt1
    )
  )
  
}



### Treatment variable to use.
rm(var.trt)
table(analdata$aslogical_new_var.smoking) # FALSE = smoker, TRUE = non-smoker
var.trt <- analdata$aslogical_new_var.smoking



### Check various model balance and fit diagnostics.


## Plots.

# Define models, indexed by number of covariates included.
formula.covariates05 <- as.formula(
  var.trt ~ 
    var.timeunemployed +
    var.age +
    var.education +
    asfactor_new_var.race +
    new_var.healthstatus
)
formula.covariates10 <- as.formula(
  var.trt ~ 
    var.timeunemployed +
    var.age +
    var.education +
    asfactor_new_var.race +
    new_var.healthstatus +
    aslogical_new_var.sex +
    aslogical_new_var.drugalcohol +
    aslogical_new_var.criminalhistory +
    aslogical_new_var.transportation +
    aslogical_new_var.housing
)

# Run analyses.
matrix.covariates05 <- psmodeldiag(
  analdata,
  analdata$aslogical_new_var.smoking,
  formula.covariates05,
  "covariates05"
)
matrix.covariates10 <- psmodeldiag(
  analdata,
  analdata$aslogical_new_var.smoking,
  formula.covariates10,
  "covariates10"
)



### Estimate ATE for models 05 and 10.


## Model 05.

# Code to identify PS-distribution overlap for trimming observations (i.e., generally exclude observations outside of PS overlap regions).
var.pshat_trt0_covariates05 <- as.data.frame(matrix.covariates05)$var.predict.pshat_trt0
var.pshat_trt1_covariates05 <- as.data.frame(matrix.covariates05)$var.predict.pshat_trt1
vector.minmax_pshat_trt0_covariates05 <- c(
  min(var.pshat_trt0_covariates05[is.na(var.pshat_trt0_covariates05) == FALSE]),
  max(var.pshat_trt0_covariates05[is.na(var.pshat_trt0_covariates05) == FALSE])
)
vector.minmax_pshat_trt1_covariates05 <- c(
  min(var.pshat_trt1_covariates05[is.na(var.pshat_trt1_covariates05) == FALSE]),
  max(var.pshat_trt1_covariates05[is.na(var.pshat_trt1_covariates05) == FALSE])
)
vector.overlapboundaries_covariates05 <- c(
  max(vector.minmax_pshat_trt0_covariates05[1], vector.minmax_pshat_trt1_covariates05[1]),
  min(vector.minmax_pshat_trt0_covariates05[2], vector.minmax_pshat_trt1_covariates05[2])
); vector.overlapboundaries_covariates05

# ATE estimation: glm. Mean estimates are equal to svyglm and hand-calculation results; SEs differ.
analdata_covariates05 <- analdata
analdata_covariates05$var.predict.pshat <- as.data.frame(matrix.covariates05)$var.predict.pshat
analdata_covariates05$var.predict.pshat_trt0 <- as.data.frame(matrix.covariates05)$var.predict.pshat_trt0
analdata_covariates05$var.predict.pshat_trt1 <- as.data.frame(matrix.covariates05)$var.predict.pshat_trt1
analdata_covariates05$var.sipw05 <- ifelse(var.trt == 1, as.data.frame(matrix.covariates05)$var.sipwhat_trt1, as.data.frame(matrix.covariates05)$var.sipwhat_trt0)
pre1_analdata_covariates05_trimmed <- analdata_covariates05[which(analdata_covariates05$var.predict.pshat >= vector.overlapboundaries_covariates05[1] & analdata_covariates05$var.predict.pshat <= vector.overlapboundaries_covariates05[2]),]

# Re-estimate and attach PS without trimmed observations.
rm(var.trt_covariates05_trimmed)
var.trt_covariates05_trimmed <- pre1_analdata_covariates05_trimmed$aslogical_new_var.smoking
formula.covariates05_trimmed <- as.formula(
  var.trt_covariates05_trimmed ~ 
    pre1_analdata_covariates05_trimmed$var.timeunemployed +
    pre1_analdata_covariates05_trimmed$var.age +
    pre1_analdata_covariates05_trimmed$var.education +
    pre1_analdata_covariates05_trimmed$asfactor_new_var.race +
    pre1_analdata_covariates05_trimmed$new_var.healthstatus
)
matrix.covariates05_trimmed <- psmodeldiag(
  pre1_analdata_covariates05_trimmed,
  pre1_analdata_covariates05_trimmed$aslogical_new_var.smoking,
  formula.covariates05_trimmed,
  "covariates05"
)
analdata_covariates05_trimmed <- pre1_analdata_covariates05_trimmed[c("aslogical_new_var.reemployment12mo", "aslogical_new_var.smoking")]
analdata_covariates05_trimmed$var.predict.pshat <- as.data.frame(matrix.covariates05_trimmed)$var.predict.pshat
analdata_covariates05_trimmed$var.predict.pshat_trt0 <- as.data.frame(matrix.covariates05_trimmed)$var.predict.pshat_trt0
analdata_covariates05_trimmed$var.predict.pshat_trt1 <- as.data.frame(matrix.covariates05_trimmed)$var.predict.pshat_trt1
analdata_covariates05_trimmed$var.sipw05 <- ifelse(var.trt_covariates05_trimmed == 1, as.data.frame(matrix.covariates05_trimmed)$var.sipwhat_trt1, as.data.frame(matrix.covariates05_trimmed)$var.sipwhat_trt0)

# 05-covariate trimmed sample sizes
table(var.trt_covariates05_trimmed)

# Run main-model PS-IPW-adjusted analysis.
glm.covariates05 <- glm(
  data = analdata_covariates05_trimmed,
  formula = aslogical_new_var.reemployment12mo ~ aslogical_new_var.smoking,
  weights = var.sipw05
)
summary(glm.covariates05)
coef(glm.covariates05)

# bootstrapped SE's
set.seed(scalar.seed)
boot.fun_covariates05 <- function(dat, index){ coef(glm.covariates05)[2] }
boot.fun_covariates05 <- function(dat, index){
  coef(
    glm(
      data = dat[index,],
      formula = aslogical_new_var.reemployment12mo ~ aslogical_new_var.smoking,
      weights = var.sipw05
    )
  )[2]
}
bootres_covariates05 <- boot(
  data = analdata_covariates05_trimmed,
  boot.fun_covariates05,
  R = 501
); bootres_covariates05
boot.ci(bootres_covariates05, type = "basic")


## Model 10.

# Code to identify PS-distribution overlap for trimming observations (i.e., generally exclude observations outside of PS overlap regions).
var.pshat_trt0_covariates10 <- as.data.frame(matrix.covariates10)$var.predict.pshat_trt0
var.pshat_trt1_covariates10 <- as.data.frame(matrix.covariates10)$var.predict.pshat_trt1
vector.minmax_pshat_trt0_covariates10 <- c(
  min(var.pshat_trt0_covariates10[is.na(var.pshat_trt0_covariates10) == FALSE]),
  max(var.pshat_trt0_covariates10[is.na(var.pshat_trt0_covariates10) == FALSE])
)
vector.minmax_pshat_trt1_covariates10 <- c(
  min(var.pshat_trt1_covariates10[is.na(var.pshat_trt1_covariates10) == FALSE]),
  max(var.pshat_trt1_covariates10[is.na(var.pshat_trt1_covariates10) == FALSE])
)
vector.overlapboundaries_covariates10 <- c(
  max(vector.minmax_pshat_trt0_covariates10[1], vector.minmax_pshat_trt1_covariates10[1]),
  min(vector.minmax_pshat_trt0_covariates10[2], vector.minmax_pshat_trt1_covariates10[2])
); vector.overlapboundaries_covariates10

# ATE estimation: glm. Mean estimates are equal to svyglm and hand-calculation results; SEs differ.
analdata_covariates10 <- analdata
analdata_covariates10$var.predict.pshat <- as.data.frame(matrix.covariates10)$var.predict.pshat
analdata_covariates10$var.predict.pshat_trt0 <- as.data.frame(matrix.covariates10)$var.predict.pshat_trt0
analdata_covariates10$var.predict.pshat_trt1 <- as.data.frame(matrix.covariates10)$var.predict.pshat_trt1
analdata_covariates10$var.sipw10 <- ifelse(var.trt == 1, as.data.frame(matrix.covariates10)$var.sipwhat_trt1, as.data.frame(matrix.covariates10)$var.sipwhat_trt0)
pre1_analdata_covariates10_trimmed <- analdata_covariates10[which(analdata_covariates10$var.predict.pshat >= vector.overlapboundaries_covariates10[1] & analdata_covariates10$var.predict.pshat <= vector.overlapboundaries_covariates10[2]),]

# Re-estimate and attach PS without trimmed observations.
rm(var.trt_covariates10_trimmed)
var.trt_covariates10_trimmed <- pre1_analdata_covariates10_trimmed$aslogical_new_var.smoking
formula.covariates10_trimmed <- as.formula(
  var.trt_covariates10_trimmed ~ 
    pre1_analdata_covariates10_trimmed$var.timeunemployed +
    pre1_analdata_covariates10_trimmed$var.age +
    pre1_analdata_covariates10_trimmed$var.education +
    pre1_analdata_covariates10_trimmed$asfactor_new_var.race +
    pre1_analdata_covariates10_trimmed$new_var.healthstatus +
    pre1_analdata_covariates10_trimmed$aslogical_new_var.sex +
    pre1_analdata_covariates10_trimmed$aslogical_new_var.drugalcohol +
    pre1_analdata_covariates10_trimmed$aslogical_new_var.criminalhistory +
    pre1_analdata_covariates10_trimmed$aslogical_new_var.transportation +
    pre1_analdata_covariates10_trimmed$aslogical_new_var.housing
)
matrix.covariates10_trimmed <- psmodeldiag(
  pre1_analdata_covariates10_trimmed,
  pre1_analdata_covariates10_trimmed$aslogical_new_var.smoking,
  formula.covariates10_trimmed,
  "covariates10"
)
analdata_covariates10_trimmed <- pre1_analdata_covariates10_trimmed[c("aslogical_new_var.reemployment12mo", "aslogical_new_var.smoking")]
analdata_covariates10_trimmed$var.predict.pshat <- as.data.frame(matrix.covariates10_trimmed)$var.predict.pshat
analdata_covariates10_trimmed$var.predict.pshat_trt0 <- as.data.frame(matrix.covariates10_trimmed)$var.predict.pshat_trt0
analdata_covariates10_trimmed$var.predict.pshat_trt1 <- as.data.frame(matrix.covariates10_trimmed)$var.predict.pshat_trt1
analdata_covariates10_trimmed$var.sipw10 <- ifelse(var.trt_covariates10_trimmed == 1, as.data.frame(matrix.covariates10_trimmed)$var.sipwhat_trt1, as.data.frame(matrix.covariates10_trimmed)$var.sipwhat_trt0)

# 10-covariate trimmed sample sizes
table(var.trt_covariates10_trimmed)

# Run main-model PS-IPW-adjusted analysis.
glm.covariates10 <- glm(
  data = analdata_covariates10_trimmed,
  formula = aslogical_new_var.reemployment12mo ~ aslogical_new_var.smoking,
  weights = var.sipw10
)
summary(glm.covariates10)
coef(glm.covariates10)

# bootstrapped SE's
set.seed(scalar.seed)
boot.fun_covariates10 <- function(dat, index){ coef(glm.covariates10)[2] }
boot.fun_covariates10 <- function(dat, index){
  coef(
    glm(
      data = dat[index,],
      formula = aslogical_new_var.reemployment12mo ~ aslogical_new_var.smoking,
      weights = var.sipw10
    )
  )[2]
}
bootres_covariates10 <- boot(
  data = analdata_covariates10_trimmed,
  boot.fun_covariates10,
  R = 501
); bootres_covariates10
boot.ci(bootres_covariates10, type = "basic")



### Plot untrimmed PS plots.
pdf(
  file = pathfile.plot_untrimmed_model05model10.pdf,
  height=5.5,
  width=8,
  onefile=T
)

# Model 05.
data.frame.ps_trt0_covariates05 <- data.frame(analdata_covariates05$var.predict.pshat_trt0, row.names = NULL)
names(data.frame.ps_trt0_covariates05) <- c("propensityscore")
data.frame.ps_trt1_covariates05 <- data.frame(analdata_covariates05$var.predict.pshat_trt1, row.names = NULL)
names(data.frame.ps_trt1_covariates05) <- c("propensityscore")
data.frame.ps_trt0_covariates05$exposuregroup <- "Smoker"
data.frame.ps_trt1_covariates05$exposuregroup <- "Non-Smoker"
data.frame.propensityscore_exposuregroup_covariates05 <- rbind(data.frame.ps_trt0_covariates05, data.frame.ps_trt1_covariates05)
plot.psoverlap_covariates05 <- ggplot(data.frame.propensityscore_exposuregroup_covariates05, aes(propensityscore, fill = exposuregroup)) + geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.05) + scale_fill_manual(values=c("green", "red")) + xlim(c(0, 1)) + ylim(c(0, 30)) # ylim is manually determined by examining all relevant output figures

# Model 10.
data.frame.ps_trt0_covariates10 <- data.frame(analdata_covariates10$var.predict.pshat_trt0, row.names = NULL)
names(data.frame.ps_trt0_covariates10) <- c("propensityscore")
data.frame.ps_trt1_covariates10 <- data.frame(analdata_covariates10$var.predict.pshat_trt1, row.names = NULL)
names(data.frame.ps_trt1_covariates10) <- c("propensityscore")
data.frame.ps_trt0_covariates10$exposuregroup <- "Smoker"
data.frame.ps_trt1_covariates10$exposuregroup <- "Non-Smoker"
data.frame.propensityscore_exposuregroup_covariates10 <- rbind(data.frame.ps_trt0_covariates10, data.frame.ps_trt1_covariates10)
plot.psoverlap_covariates10 <- ggplot(data.frame.propensityscore_exposuregroup_covariates10, aes(propensityscore, fill = exposuregroup)) + geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.05) + scale_fill_manual(values=c("green", "red")) + xlim(c(0, 1)) + ylim(c(0, 30)) # ylim is manually determined by examining all relevant output figures

grid.arrange(plot.psoverlap_covariates05, plot.psoverlap_covariates10, nrow=2)
dev.off()



### Plot trimmed PS plots.
pdf(
  file = pathfile.plot_trimmed_model05model10.pdf,
  height=5.5,
  width=8,
  onefile=T
)

# Model 05.
data.frame.ps_trt0_covariates05 <- data.frame(analdata_covariates05_trimmed$var.predict.pshat_trt0, row.names = NULL)
names(data.frame.ps_trt0_covariates05) <- c("propensityscore")
data.frame.ps_trt1_covariates05 <- data.frame(analdata_covariates05_trimmed$var.predict.pshat_trt1, row.names = NULL)
names(data.frame.ps_trt1_covariates05) <- c("propensityscore")
data.frame.ps_trt0_covariates05$exposuregroup <- "Smoker"
data.frame.ps_trt1_covariates05$exposuregroup <- "Non-Smoker"
data.frame.propensityscore_exposuregroup_covariates05 <- rbind(data.frame.ps_trt0_covariates05, data.frame.ps_trt1_covariates05)
plot.psoverlap_covariates05 <- ggplot(data.frame.propensityscore_exposuregroup_covariates05, aes(propensityscore, fill = exposuregroup)) + geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.05) + scale_fill_manual(values=c("green", "red")) + xlim(c(0, 1)) + ylim(c(0, 30)) # ylim is manually determined by examining all relevant output figures

# Model 10.
data.frame.ps_trt0_covariates10 <- data.frame(analdata_covariates10_trimmed$var.predict.pshat_trt0, row.names = NULL)
names(data.frame.ps_trt0_covariates10) <- c("propensityscore")
data.frame.ps_trt1_covariates10 <- data.frame(analdata_covariates10_trimmed$var.predict.pshat_trt1, row.names = NULL)
names(data.frame.ps_trt1_covariates10) <- c("propensityscore")
data.frame.ps_trt0_covariates10$exposuregroup <- "Smoker"
data.frame.ps_trt1_covariates10$exposuregroup <- "Non-Smoker"
data.frame.propensityscore_exposuregroup_covariates10 <- rbind(data.frame.ps_trt0_covariates10, data.frame.ps_trt1_covariates10)
plot.psoverlap_covariates10 <- ggplot(data.frame.propensityscore_exposuregroup_covariates10, aes(propensityscore, fill = exposuregroup)) + geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.05) + scale_fill_manual(values=c("green", "red")) + xlim(c(0, 1)) + ylim(c(0, 30)) # ylim is manually determined by examining all relevant output figures

grid.arrange(plot.psoverlap_covariates05, plot.psoverlap_covariates10, nrow=2)
dev.off()