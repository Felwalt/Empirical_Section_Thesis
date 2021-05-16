
install.packages("huxtable")
install.packages("flextable", type = "binary")
install.packages("table1")
install.packages("dplyr")


library ("jtools")
library ("flextable")
library("table1")
library("dplyr")


## The Data is prepared for analyis in the excel Raw_data_Thesis. For analyisis in R the excel sheet "Import to R" has to be imported. 

# In a first step, the binary variable "Cleaned" indicates, which variables are to be removed from the full data-set. 
# Duplicate answers, one outlier, and participants who took less than 5 seconds for the insurance task were removed. 

Raw_data_full <- Raw_data_Thesis
Raw_data_cleaned <- subset(Raw_data_full, Cleaned == 0)

# create dummmies for large overconfidence and underconfidence

Raw_data_cleaned$Un <- ifelse(Raw_data_cleaned$Overc < -2,  1, 0)
Raw_data_cleaned$Ov <- ifelse(Raw_data_cleaned$Overc > 2,  1, 0)

# split data in in conditional and standard contracts

Cond_data_cleaned = subset(Raw_data_cleaned, Cond == 1)
standard_data_cleaned = subset(Raw_data_cleaned, Cond == 0)

## Table 7 in the thesis entails summary statistics of the data set as well as a balance check between groups. 
## The code below generates the statistics and was manually transferred in the formmat of the thesis.

# The order of the colummns is adapted in written Master thesis. 
# Note that, compared to table 7 in the thesis, in the R output below Column 1 = SI, Column 3 = SU, Column 2 = CI, Column 4 = CU.

table1::label(Raw_data_cleaned$RiskQ) <- "RiskQ"
table1::label(Raw_data_cleaned$ProbQ) <- "ProbQ"
table1::label(Raw_data_cleaned$InsQ) <- "InsQ"
table1::label(Raw_data_cleaned$Risk) <- "Risk"
table1::label(Raw_data_cleaned$Estim) <- "Estim"
table1::label(Raw_data_cleaned$Overc) <- "Overc"
table1::label(Raw_data_cleaned$Take) <- "Take"

Descriptive_stats = table1::table1(~RiskQ + ProbQ + InsQ + Estim + Risk + Overc + Take | Group, data = Raw_data_cleaned)
Descriptive_stats

# Table 7 involves a balance check. The code below conducts the respective t-tests between groups.
# Separate data into the 4 groups for t-tests

Group_1M = subset(Raw_data_cleaned, Group == 1)
Group_2M = subset(Raw_data_cleaned, Group == 2)
Group_3M = subset(Raw_data_cleaned, Group == 3)
Group_4M = subset(Raw_data_cleaned, Group == 4)

# t-test 2 and 4 i.e. CI vs CU 

RiskQ_2 = t.test(Group_4M$RiskQ, Group_2M$RiskQ, alternative = "two.sided", var.equal = FALSE)
ProbQ_2 = t.test(Group_4M$ProbQ, Group_2M$ProbQ, alternative = "two.sided", var.equal = FALSE)
InsQ_2 = t.test(Group_4M$InsQ, Group_2M$InsQ, alternative = "two.sided", var.equal = FALSE)
Risk_2 = t.test(Group_4M$Risk, Group_2M$Risk, alternative = "two.sided", var.equal = FALSE)
Estim_2 = t.test(Group_4M$Estim, Group_2M$Estim, alternative = "two.sided", var.equal = FALSE)
Overc_2 = t.test(Group_4M$Overc, Group_2M$Overc, alternative = "two.sided", var.equal = FALSE)
Take_2 = t.test(Group_4M$Take, Group_2M$Take, alternative = "two.sided", var.equal = FALSE)

collect = rbind(RiskQ_2$p.value, ProbQ_2$p.value, InsQ_2$p.value, Risk_2$p.value ,Estim_2$p.value, Overc_2$p.value, Take_2$p.value)

colnames(collect) = c("Treatment vs. Control (Conditional)")
rownames(collect) = c("RiskQ","ProbQ", "InsQ","risk", "Estimation", "overconfidence", "Take")

collect = as.table(collect)
collect

# t-test 1 and 3 i.e. SI vs SU

RiskQ_2 = t.test(Group_3M$RiskQ, Group_1M$RiskQ, alternative = "two.sided", var.equal = FALSE)
ProbQ_2 = t.test(Group_3M$ProbQ, Group_1M$ProbQ, alternative = "two.sided", var.equal = FALSE)
InsQ_2 = t.test(Group_3M$InsQ, Group_1M$InsQ, alternative = "two.sided", var.equal = FALSE)
Risk_2 = t.test(Group_3M$Risk, Group_1M$Risk, alternative = "two.sided", var.equal = FALSE)
Estim_2 = t.test(Group_3M$Estim, Group_1M$Estim, alternative = "two.sided", var.equal = FALSE)
Overc_2 = t.test(Group_3M$Overc, Group_1M$Overc, alternative = "two.sided", var.equal = FALSE)
Take_2 = t.test(Group_3M$Take, Group_1M$Take, alternative = "two.sided", var.equal = FALSE)

collect = rbind(RiskQ_2$p.value, ProbQ_2$p.value, InsQ_2$p.value, Risk_2$p.value ,Estim_2$p.value, Overc_2$p.value, Take_2$p.value)

colnames(collect) = c("Treatment vs. Control (Standard)")
rownames(collect) = c("RiskQ","ProbQ", "InsQ","risk", "Estimation", "overconfidence", "Take")
table
collect = as.table(collect)
collect

## Table 8: Hypothesis 1 regression outputs (2 OLS  and 2 Probits)
# Table 8 entails 4 regressions that are run with and without covariates and for the standard and conditional insurance contracts.

# Run the regressions and store output

regr1 = lm(Take ~ Overc + Info + Info:Overc, standard_data_cleaned)
regr2 = lm(Take ~ RiskQ + ProbQ + InsQ + Risk + Overc + Info + Info:Overc, standard_data_cleaned)
regr3 = glm(Take ~ Overc + Info + Info:Overc, standard_data_cleaned, family = binomial(link = "probit"))
regr4 = glm(Take ~ RiskQ + ProbQ + InsQ + Risk + Overc + Info + Info:Overc, standard_data_cleaned, family = binomial(link = "probit"))

# The table in the thesis is a screenshot from the html output produced by the function export_summs.

export_summs(regr1,regr2, regr3,regr4, to.file = "html",  model.names = c("LPM (1)","LPM (2)", "Probit (1)","Probit (2)"), robust ="HC0", digits = 3, statistics = c(N = "nobs", R2 = "r.squared", `Pseudo R2` = "pseudo.r.squared"), error_pos = c( "right"),   model.info = getOption("summ-model.info", TRUE))

## Figure 2 computations
# The marginal effect of treatment for the Probit estimator is computed. 
# In a first step, the non-treatment probability of insurance participation for differing overconfidence scores is estimated and converted to a z-score.

predictions = predict(regr4, newdata = data.frame(RiskQ = mean(standard_data_cleaned$RiskQ),ProbQ = mean(standard_data_cleaned$ProbQ), InsQ = mean(standard_data_cleaned$InsQ), Risk = mean(standard_data_cleaned$Risk), Info = c(0),Overc = c(-5:5)), type ="response")

zscore = qnorm(predictions)

# In a second step, the probability of participation for the treatment group is estimated.
# For this, the estimamted treatment effect is added to the z-score of non-treatment. Here, only the coefficient of the interaction term is significant. 

# extract the coefficients from the Probit output with covariates

coefficients_probit = as.data.frame(regr4$coefficients)

# The predicted probability of insurance participation in the treatmment group can be computed by adding the treatment effect to the non-treatment z-score.
# The treatment effect is computed by variying the overconfidence variable from -5 to 5. 
# Formally: zscore + Overc*0.212.

treatment_predict = pnorm(zscore+(c(-5:5))*coefficients_probit[8,1])

# The  marginal effect of treatment is computed by taking the difference of predicted probability in treatment and control group.

Marginal = cbind(treatment_predict) - cbind(predictions)

# The treatment effect of the linear model (OLS model) is directly interpretable from the significant coefficient of interaction term, hence the marginal effect of treatment 
# can be computed by multiplying the coefficient with the overconfidence score.

# Create variable for differing overconfidence scores.

overc1 = c(-5:5)

# extract the coefficients from the Probit output with covariates.

coefficients_linear = as.data.frame(regr2$coefficients)

# compute linear treatment effect 

lin = overc1*coefficients_linear[8,1]

## Figure 2: Plot linear and Probit treatment effect
# The treatment effects are plotted with overconfidence on the x-axis and the treatment effect on the y-axis

plot(overc1, Marginal, type = "l", col = "red")
lines(overc1, lin, type = "l", col = "green")

plot(overc1, Marginal, xlab="Overconfidence (in %-points)", ylab="Treatment effect (change in probability)", lty=1, type = "l", cex.axis=0.6, cex.lab = 0.8)
lines(overc1, lin, lty=2)

legend("topleft", legend=c("Marginal Effect Probit","Marginal Effect LPM"), lty=c(1,2), cex=0.8)


## Table 9: Hypothesis 2 regression outputs (2 OLS  and 2 Probits)
# Table 9 entails 4 regressions that are run with and without covariates and for the standard and conditional insurance contracts.

# Run the regressions and store output

regr1 = lm(Take ~ Overc + Info + Info:Overc, Cond_data_cleaned)
regr2 = lm(Take ~ RiskQ + ProbQ + InsQ + Risk + Overc +  Info + Info:Overc, Cond_data_cleaned)
regr3 = glm(Take ~ Overc + Info + Info:Overc, Cond_data_cleaned, family = binomial(link = "probit"))
regr4 = glm(Take ~ RiskQ + ProbQ + InsQ + Risk + Overc +  Info + Info:Overc, Cond_data_cleaned, family = binomial(link = "probit"))

# The table in the thesis is a screenshot from the html output produced by the function export_summs.

export_summs(regr1,regr2, regr3,regr4, to.file = "html",  model.names = c("LPM (1)","LPM (2)", "Probit (1)","Probit (2)"), robust ="HC0", digits = 3, statistics = c(N = "nobs", R2 = "r.squared", `Pseudo R2` = "pseudo.r.squared"), error_pos = c( "right"),   model.info = getOption("summ-model.info", TRUE))


## Table 10: Robustness tests (2 OLS with alternative specification and 2 OLS for reduced sample)
# Table 10 entails 4 regressions that are run with covariates for the standard and conditional insurance contracts. 
# Further, the regressions are run for a random sub-sample and an alternative econometric specification.

# First, OLS regressions are computed with an alternative specification using underconfidence and overconfidence dummmies. 
# Run the regressions and store output

regr1 = lm(Take ~ Info*Un + Info*Ov + RiskQ + ProbQ + InsQ + Risk , standard_data_cleaned)
regr2 = lm(Take ~ Info*Un + Info*Ov + RiskQ + ProbQ + InsQ + Risk, Cond_data_cleaned)

# Seccond, a sub-sample of the redcued sample is taken (Note that, since the sub-sample produces a new sample each time the code is run, the results are different each time).

Standard_data_sub = sample_n(standard_data_cleaned, 75, replace = FALSE, prob = NULL)
Cond_data_sub = sample_n(Cond_data_cleaned, 75, replace = FALSE, prob = NULL)

# Run the regressions for the sub-sample and store output

regr3 = lm(Take ~ Overc + Info + Info:Overc + RiskQ + ProbQ + InsQ + Risk, Standard_data_sub)
regr4 = lm(Take ~ Overc + Info + Info:Overc + RiskQ + ProbQ + InsQ + Risk, Cond_data_sub)

# The table in the thesis is a screenshot from the html output produced by the function export_summs.

export_summs(regr1,regr2,regr3, regr4, to.file = "html",  model.names = c("LPM standard alt","LPM conditional alt", "LPM standard sub", "LPM conditional sub"), robust ="HC0", digits = 3, statistics = c(N = "nobs", R2 = "r.squared"), error_pos = c( "right"))

## Figure 1 computations 
# In Figure 1 the willingness to pay for insurance is computed and plotted along-side the insurance premium of standard and conditional insurance contracts.

# create  probability vectors for for the loss state and the non-loss state
# non-loss state

prob_vector <- cbind(1 - seq(1:20)/100)

# loss state
loss_prob <- 1 - prob_vector

# Compute insurance premium for each contract design. For the standard contract the premium is constant at 25 cents, while the conditional premium is proportional the loss probability.

Conditional_premium = loss_prob*3.5*100
Standard_premium = rep(25,20)

# Compute the maximum willingness to pay for insurance for differing levels of the loss probability and CRRA utility function

# compute the non-insurance utility for differing levels of the loss probability

Non_ins_Utility = prob_vector*(100^(1-0.75))*1/(1-0.75)

# compute the certainty equivalent 

CE = (Non_ins_Utility*(1-0.75))^(1/0.25)

# the endowment minus the certainty equivalent = willingness to pay for insurance

WTP = 100 - CE

# In Figure 1, the WTP for insurance and the insurance premium for the conditional and standard contract are plotted against the loss probability.

plot(loss_prob, WTP, xlab="Loss probability", ylab="WTP or Insurance Premium (in cents)", lty=1, type = "l", cex.axis=0.6, cex.lab = 0.8)
lines(loss_prob, Conditional_premium, lty=2)
lines(loss_prob, Standard_premium, lty=6)

legend("topleft", legend=c("Willingness to pay","Premium Conditional", "Premium Standard"), lty=c(1,2,6), cex=0.8)


