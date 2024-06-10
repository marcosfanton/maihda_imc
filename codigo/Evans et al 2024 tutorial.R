#  *****************************************************************************
#  ***																			 
#  ***		A Tutorial for Conducting  										 
#  ***		Intersectional Multilevel Analysis of Individual 				 
#  *** 		Heterogeneity and Discriminatory Accuracy (MAIHDA)				 
#  ***																			 
#  ***		MLE Estimation Approach											 
#  ***																			 
#  ***		(Evans, Leckie, Subramanian, Bell, & Merlo, 2024)
#  ***																			 
#  *****************************************************************************
#  *****************************************************************************
  
#   If using this code, cite as:
#  /*
#  Evans, C.R., G. Leckie, S.V. Subramanian, A. Bell, & J. Merlo. (2024.).
#  A Tutorial for Conducting Intersectional Multilevel Analysis of Individual
#  Heterogeneity and Discriminatory Accuracy (MAIHDA). SSM - Population Health.
#  https://doi.org/10.1016/j.ssmph.2024.101664
#  */

#  # NOTES
#  ****************************************************************************
#  
#  What follows is R code for fitting all models discussed in the MAIHDA 
#  Tutorial (Evans, Leckie, Subramanian, Bell, & Merlo, 2024).

#  Be sure to download the simulated data set named: TutorialData.dta

#  Examples are provided for linear (model 1) and logistic (model 2) MAIHDA
#  models. 

#  For each, there are two versions: 
#  (A) a null (empty) model, and 
#  (B) an additive main effects model. 

#  linear models are fit using restricted maximum likelihood
#  estimation (RMLE) using the function lmer
#  logistic models are fitted using a Laplace Approximatyetion of maximum 
#  likelihood estimation using the funtion glmer.
#  For alternate versions of the code 
#  (e.g., Bayesian estimation, or using other software) 
#  visit: https://osf.io/dtvc3/ 

# GENERAL SET UP

# to start with, in RStudio, create a project pointing at a file that contains
# the dataset TutorialData.dta. this will ensure you are starting R with a 
# clean slate without any additional objects in the working environment

# load packages used in this tutorial
install.packages("haven", "tidyverse", "ggeffects", "merTools", "labelled", 
                 "sjPlot", "Metrics")
library(haven)
library(tidyverse)
library(ggeffects)
library(lme4)
library(merTools)
library(labelled)
library(sjPlot)
library(Metrics)


# load the data
tut <- read_dta("dados/TutorialData.dta")
# if TutorialData.dta is not located in the project file / working directory, 
# put the full pathway link to the file within the quote marks

# Generate the stratum ID
tut$stratum <- 10000*tut$sex + 1000*tut$race + 100*tut$education + 
                10*tut$income + 1*tut$age

# Turn the stratum identifier into a factor variable
tut$stratum <- as.factor(tut$stratum)

# reattach factor labels from original Stata dataset
tut <- unlabelled(tut)

# sort data by stratum
tut <- tut[order(tut$stratum),]

# tabulate stratum
table(tut$stratum)

# Generate a new variabe which records stratum size
tut <- tut %>%
  group_by(stratum) %>%
  mutate(strataN = n())



##############################################
# Estimate linear model 1A
##############################################

# Fit the two-level linear regression with no covariates
model1A_tut <- lmer(HbA1c ~ (1|stratum), data=tut)
summary(model1A_tut)

# This produces a model output showing the level 2 variance (or, alternatively,
# it's standard deviation) and the same for level 1 (labelled 
# "stratum (Intercept)" and "Residual", respectively)

# predict the mean outcome
tut$m1Am_tut <- predict(model1A_tut)
# Calculated as the fixed-portion linear prediction plus contributions based on 
# predicted random effects.


############################
# Estimate Linear model 1B
##########################

# Fit the two-level linear regression with covariates
model1B_tut <- lmer(HbA1c ~ sex + race + education + income + age + (1|stratum), 
                data=tut)
summary(model1B_tut)

# predict the mean outcome and confidence intervals of prediction
m1Bm_tut <- predictInterval(model1B_tut, level=0.95, include.resid.var=FALSE)
# note that, unlike the stata code, this code combines the fixed and random part
# uncertainty. This saves a step later in comparison to the stata code. It also
# creates a new dataframe for these predictions, with one row per stratum.

# Create an identifier variable in the new dataframe, called "id"
m1Bm_tut <- mutate(m1Bm_tut, id=row_number())

# predict the stratum random effects and associated SEs
m1Bu_tut <- REsim(model1B_tut)

# Note that, unlike the Stata code, and as above this creates a separate 
# dataframe with one line per stratum.

##################################
# Estimate Logistic model 2A
##################################

# Fit the two-level logistic regression with no covariates
model2A_tut <- glmer(diabetic ~ (1|stratum), data=tut, family=binomial)
summary(model2A_tut)

# Get the estimates as Odds ratios (and SEs on the odds scale)
tab_model(model2A_tut, show.se=T)

# Predict the fitted linear predictor (on the probability scale)
tut$m2Axbu <- predict(model2A_tut, type="response")

# Predict the linear predictor for the fixed portion of the model only
# (only the intercept, so this is just the weighted grand mean probability)
tut$m2Axb <- predict(model2A_tut, type="response", re.form=NA)


#####################################
# Estimate Logistic model 2B
####################################

# Fit the two-level linear regression with covariates
model2B_tut <- glmer(diabetic ~ sex + race + education + income + age + 
                   (1|stratum), data=tut, family=binomial)
summary(model2B_tut)

# Get the estimates as Odds ratios (and SEs on the odds scale)
tab_model(model2B_tut, show.se=T)

# predict the fitted linear predictor, and confidence intervals, on the logit 
# scale
m2Bm_tut <- predictInterval(model2B_tut, level=0.95, include.resid.var=FALSE)
# create a new id variable for this newly created dataframe
m2Bm_tut <- mutate(m2Bm_tut, id=row_number())

# on the logit scale, predict the linear predictor for the fixed portion of the
# model only
tut$m2BmF <- predict(model2B_tut, re.form=NA)

# predict the fitted linear predictor, and confidence intervals, on the 
# probability scale
m2Bm_prob_tut <- predictInterval(model2B_tut, level=0.95, include.resid.var=FALSE, 
                             type="probability") 
# note that, unlike the stata code, this code combines the uncertainty from the
# fixed and random parts. This saves a step later compared to the Stata code.

# create a new id variable for this newly created dataframe
m2Bm_prob_tut <- mutate(m2Bm_prob_tut, id=row_number())

# predict the fitted linear predictor, on the probability scale, for the fixed 
# portion of the model only
tut$m2Bxb <- predict(model2B_tut, re.form=NA, type="response")

#predict the stratum random effects and associated standard errors
m2Bu_tut <- REsim(model2B_tut)

###################################
# create a data frame at the strata level (with the means of each variable)
##################################

# first, merge predictions with original data

# create an id variable for merging in the tut dataframe
tut$id <- seq.int(nrow(tut))

# create a new dataframe, tut2, that merges tut and m1Bm
tut2 <- merge(tut, m1Bm, by="id")

# rename the variables from m1Bm
tut2 <- tut2 %>%
  rename(
    m1Bmfit=fit,
    m1Bmupr= upr,
    m1Bmlwr=lwr
  )

# merge in m2bm_prob
tut2 <- merge(tut2, m2Bm_prob, by="id")

tut2 <- tut2 %>%
  rename(
    m2Bmfit=fit,
    m2Bmupr= upr,
    m2Bmlwr=lwr
  )

# merge in m2Bm
tut2 <- merge(tut2, m2Bm, by="id")

tut2 <- tut2 %>%
  rename(
    m2BmfitL=fit,
    m2BmuprL= upr,
    m2BmlwrL=lwr
  )

# Collapse the data down to a stratum-level dataset 
stratum_level <- aggregate(x=tut2[c("HbA1c", "diabetic")], 
                  by=tut2[c("sex", "race", "education", "income", "age",
                  "stratum", "strataN", "m1Am_tut", "m1Bmfit", "m1Bmupr", "m1Bmlwr",
                  "m2Bmfit", "m2Bmupr", "m2Bmlwr", 
                  "m2BmfitL", "m2BmuprL", "m2BmlwrL",
                  "m2BmF")],
                  FUN=mean)

# convert the outcome from a proportion to a percentage
stratum_level$diabetic <- stratum_level$diabetic*100

#############################
# Table 1
#############################

# Tabulate each individual characteristic
table(tut$sex)
table(tut$race)
table(tut$education)
table(tut$income)
table(tut$age)

summary(tut$HbA1c)
sd(tut$HbA1c)
# Note: the mean of the individual outcomes = "sample mean"

##############
# Table 2: Calculate stratum-level descriptive statistics
##############

# generate binary indicators for whether each stratum has more than X 
# individuals
summary(stratum_level$strataN)
stratum_level$n100plus <- ifelse(stratum_level$strataN>=100, 1,0)
stratum_level$n50plus <- ifelse(stratum_level$strataN>=50, 1,0)
stratum_level$n30plus <- ifelse(stratum_level$strataN>=30, 1,0)
stratum_level$n20plus <- ifelse(stratum_level$strataN>=20, 1,0)
stratum_level$n10plus <- ifelse(stratum_level$strataN>=10, 1,0)
stratum_level$nlessthan10 <- ifelse(stratum_level$strataN<10, 1,0)

# tabulate the binary indicators
table(stratum_level$n100plus)
table(stratum_level$n50plus)
table(stratum_level$n30plus)
table(stratum_level$n20plus)
table(stratum_level$n10plus)
table(stratum_level$nlessthan10)

# summmarise the observed stratum means
summary(stratum_level$HbA1c)
sd(stratum_level$HbA1c)
#Note: mean of the observed stratum means = "grand mean"

# summarise the predicted stratum means
summary(stratum_level$m1Am)
sd(stratum_level$m1Am)
#Note: mean of the predicted stratum means = "precision weighted grand mean"

############
# Table 3
############

# Create a table that includes all model estimates, including the Variance 
# Partitioning Coefficients (VPC)
tab_model(model1A_tut, model1B_tut, model2A_tut, model2B_tut, p.style="stars")

# Calculate the Proportional Change in Variance (PCV) (as a percentage) for 
# models 1 and 2

# first extract variance matrices from the model objects
vc1a_tut <-as.data.frame(VarCorr(model1A_tut))
vc1b_tut <-as.data.frame(VarCorr(model1B_tut))
vc2a_tut <-as.data.frame(VarCorr(model2A_tut))
vc2b_tut <-as.data.frame(VarCorr(model2B_tut))

# calculate PCVs using components of these variance matrices (as percentages)
PCV1_tut <- ((vc1a_tut[1,4] - vc1b_tut[1,4]) / vc1a_tut[1,4])*100
PCV1_tut
PCV2_tut <- ((vc2a_tut[1,4] - vc2b_tut[1,4]) / vc2a_tut[1,4])*100
PCV2_tut

# Calculate the area under the receiver operating characteristic (ROC) curve
# for model 2a - based on intercept and stratum random effects
AUC2A_tut <- auc(tut2$diabetic, tut2$m2Axbu)

#for model 2A - based on only the fixed portion of the model
AUC2AF_tut <- auc(tut2$diabetic, tut2$m2Axb)

#for model 2b - based on intercept, main effects, and stratum random effects
AUC2B_tut <- auc(tut2$diabetic, tut2$m2Bmfit)
#for model 2b - based on the fixed portion of the model (main effects)
AUC2BF_tut <- auc(tut2$diabetic, tut2$m2Bxb) 

#output the AUC calculations
AUC2A_tut
AUC2AF_tut
AUC2B_tut
AUC2BF_tut

############
#Figure 1
###########

# panel A - histogram of the individual outcome
ggplot(tut, aes(x=HbA1c)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth=2, 
                 boundary=22) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent of Individuals") +
  geom_vline(aes(xintercept=40.22)) + #this is the sample mean
  annotate("text", x=53, y=0.1, label="Sample Mean=40.2 mmol/mol") +
  annotate("text", x=11, y=0.01, label="Min =
           10.3 mmol/mol") +
  annotate("text", x=80, y=0.01, label="Max =
           101.2 mmol/mol")


# Panel B - histogram of the observed stratum means
ggplot(stratum_level, aes(x=HbA1c)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth=2, 
                 boundary=22) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent of Strata") +
  geom_vline(aes(xintercept=40.80)) + #this is the grand mean
  annotate("text", x=45, y=0.2, label="Grand Mean=40.8 mmol/mol") +
  annotate("text", x=25, y=0.02, label="Min =
           23.5 mmol/mol") +
  annotate("text", x=52, y=0.02, label="Max =
           53.8 mmol/mol")

#PanelC  - histogram of the predicted stratum means
ggplot(tut, aes(x=m1Am_tut)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth=2, 
                 boundary=22) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent of Strata") +
  xlab("Predicted HbA1c") +
  geom_vline(aes(xintercept=40.79))+ #this is the precision weighted grand mean
  annotate("text", x=43.2, y=0.26, 
           label="Precision Weighted Grand Mean=40.8 mmol/mol") +
  annotate("text", x=34, y=0.03, label="Min =
           34.2 mmol/mol") +
  annotate("text", x=48.2, y=0.03, label="Max =
           48.1 mmol/mol")

##############
#Figure 2 / table 4
#############

# Rank the predicted stratum means
stratum_level <- stratum_level %>%
  mutate(rank=rank(m1Bmfit))

# Panel A - Plot the caterpillar plot of the predicted stratum means
# use predictions and uncertainty generated previously
ggplot(stratum_level, aes(y=m1Bmfit, x=rank)) +
  geom_point() +
  geom_pointrange(aes(ymin=m1Bmlwr, ymax=m1Bmupr)) +
  ylab("Predicted HbA1c, Model 1B") +
  xlab("Stratum Rank") + 
  theme_bw()

# Generate list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level <- stratum_level[order(stratum_level$rank),]
head(stratum_level)
tail(stratum_level)

# Panel B

# note that (compared to Stata code), our predictions are already on the
# probability scale

# Rank the predicted stratum probabilities
stratum_level <- stratum_level %>%
  mutate(rank2=rank(m2Bmfit))

# convert probabilities to percentages
stratum_level$m2Bmfit <- stratum_level$m2Bmfit * 100
stratum_level$m2Bmupr <- stratum_level$m2Bmupr * 100
stratum_level$m2Bmlwr <- stratum_level$m2Bmlwr * 100

# Plot the caterpillar plot of the predicted stratum means
ggplot(stratum_level, aes(y=m2Bmfit, x=rank2)) +
  geom_point() +
  geom_pointrange(aes(ymin=m2Bmlwr, ymax=m2Bmupr)) +
  ylab("Predicted Percent Diabetic, Model 2B") +
  xlab("Stratum Rank") + 
  theme_bw()


# Generate list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level <- stratum_level[order(stratum_level$rank2),]
head(stratum_level)
tail(stratum_level)

###############
# Figure 3
###############

# Panel A

# repeating step from above - generate u0j values and uncertainty
m1Bu <- REsim(model1B)

# Plot the caterpillar plot of the predicted stratum random effects
p <-plotREsim(m1Bu) +
  xlab("Stratum Rank") +
  ylab("Predicted stratum Random Effect in HbA1c (mmol/mol)") +
  ggtitle("") +
  theme(
    strip.background=element_blank(),
    strip.text.x = element_blank()
  )
p

# Panel B

# Here we use the data embedded in the ggplot p object above, since it has 
# already created what we need to subset the data.

m1Bucut <- p[["data"]]

# filter the data based on significance
m1Bucut <- m1Bucut %>%
  filter(sig=="TRUE") %>%
  mutate(xvar=as.factor(xvar))

# plot the caterpillar plot of the significant predicted stratum random effects
ggplot(m1Bucut, aes(x=xvar, y=median, label=groupID)) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=ymin, ymax=ymax)) + 
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_text(hjust=0, vjust=5) +
  xlab("Stratum Rank") +
  ylab("Predicted stratum Random Effect in HbA1c (mmol/mol)") +
  theme_bw()

# Panel C

# create figure but on the log-odds scale
m2Bu <- REsim(model2B)
plotREsim(m2Bu)

# This is more difficult to do on the probability scale

# For this plot we follow a simulation-based approach to 
# calculating the limits of the approximate 95% confidence intervals of our 
# predictions. This involves simulating 1000 values for each predicted value.

# Approximate as the model assumes no sampling covariability between the 
# regression coefficients and the stratum random effect

# duplicate stratum data *1000 (creating a new dataframe, stratumsim)
stratumsim <- rbind(stratum_level, 
                    stratum_level[rep(1:nrow(stratum_level),999),])

# Generate the approximate standard error for the linear prediction on the
# logit scale. We do this based on the difference between one estimated
# confidence interval and the estimated fit, on the logit scale
stratumsim$m2Bmse <- (stratumsim$m2BmfitL - stratumsim$m2BmlwrL)/1.96

# specify initial value of the random-number seed (for replication purposes)
set.seed(354612)

# Generate the predicted stratum percentages based on the regression 
# coefficients and the predicted stratum random effect and factoring in 
# prediction uncertainty
stratumsim$m2Bpsim <- 100*invlogit(stratumsim$m2BmfitL + 
                                  rnorm(384000, mean=0, sd=stratumsim$m2Bmse))

# Generate the predicted stratum percentages ignoring the predicted stratum 
# effect
stratumsim$m2BpAsim <- 100*invlogit(stratumsim$m2BmF)

# Generate the difference in the predicted stratum percentages due to the 
# predicted stratum effect
stratumsim$m2BpBsim <- stratumsim$m2Bpsim - stratumsim$m2BpAsim

# sort the data by strata
stratumsim <- stratumsim[order(stratumsim$stratum),]

# collapse the data down to stratum level, generate mean and SE variables,
# then use these to generate rank and lower and upper limits of the approximate
# 95% confidence intervals of the difference in predicted stratum percentates 
# due to interaction variables.
stratumsim2 <- stratumsim %>%
  group_by(stratum) %>%
  summarise(mean=mean(m2BpBsim), std=sd(m2BpBsim)) %>%
  mutate(rank=rank(mean)) %>%
  mutate(hi=(mean + 1.96*std)) %>%
  mutate(lo=(mean - 1.96*std))

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(stratumsim2, aes(x=rank, y=mean)) +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lo, ymax=hi)) + 
  xlab("Stratum Rank") +
  ylab("Difference in predicted percent diabetic due to interactions") +
  theme_bw()
