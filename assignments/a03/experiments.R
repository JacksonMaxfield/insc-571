#### experiments.R
#### INSC 571
#### Assignment 03
#### Winter 2022
#### Jackson Brown and Ruotong Wang
#### jmxbrown@uw.edu and ruotong@uw.edu

library(plyr)
library(car)
library(ez)
library(MASS)
library(coin)
library(reshape2)
library(lme4)
library(lmerTest)
library(car)
library(multcomp)
library(emmeans)
library(ARTool)
library(dplyr)
library(ordinal)
library(RVAideMemoire)
library(ggplot2)

####
## EXPERIMENT 01 SETUP ##
####

# set working directory
setwd("//home/maxfield/active/personal/insc-571/assignments/a03/")
df <- read.csv("experiment_01.csv")

# encode nominal and ordinal factors (numeric variables are set by default)
df$Subject <- factor(df$Subject)
df$Engine <- factor(df$Engine)
df$Order <- ordered(df$Order)

# encode sum-to-zero factor contrasts
contrasts(df$Engine) <- "contr.sum"
contrasts(df$Order) <- "contr.sum"




####
# Step 1. Examine the expertiment_01.csv data table and write
# a precise description conveying what you think this study was about.
# Be specific!

# The results stored in experiment_01.csv detail a study that measured
# how long it took in minutes to find and answer a set of questions
# relating to different topics using different search engines.
#
# The experiment was a within-subjects study, and 18 subjects
# participated in the experiment.
#
# Each participant used three different search engines to complete the task,
# Google, Bing, and Yahoo, and the entire study counterbalanced for order
# effects. This counterbalancing was possible because the total number of
# possible different orderings was 3! (3! = 6) and 18 participants is a
# multiple of 6, therefore, each ordering was following by three different
# participants.




####
# Step 2. Conduct exploratory data analysis (EDA) using (a)
# descriptive statistics and (b) plots.
#         (Hint: At a minimum, explore central tendency and
#         variation numerically and graphically.)
print("Step 2")

hist(df$Minutes)
print(mean(df$Minutes))
print(sd(df$Minutes))

# Looking at the overall mean and standard deviation for the whole dataset
# we see that on average, regardless of search engine, participants
# completed the task in 22.38 minutes with a standard deviation of
# 15.22 minutes. However, we notice from our histogram that it seems our
# distribution is right-skewed or log-normal distributed which makes sense
# because it is very common for datasets that measure response times or
# completion times to be log-normal distributed.

plot(Minutes ~ Engine, data = df)
print(
    ddply(
        df,
        ~Engine,
        summarise,
        Minutes.mean = round(mean(Minutes), 2),
        Minutes.sd = round(sd(Minutes), 2)
    )
)

# Looking at how long each subject took to complete the task
# in minutes by each search engine, we see that Google has a much lower
# mean and third quartile values than the other two search engines.
# Yahoo has the next lowest mean but it appears to have a similar distribution
# to Bing.
#
# The standard deviation for the time it took to complete the task using
# Google is much lower than the standard deviation for the time it took
# to complete the task using Yahoo or Bing - the standard
# deviation for Google is less than half what it is for Yahoo or Bing.
#
# Not only is the mean less for Google but it's standard deviation is also
# much less as well. This seems to indicate that Google may result in a
# significant difference in the time it takes to complete the task.

plot(Minutes ~ as.factor(Order), data = df)
print(
    ddply(
        df,
        ~ as.factor(Order),
        summarise,
        Minutes.mean = round(mean(Minutes), 2),
        Minutes.sd = round(sd(Minutes), 2)
    )
)

# Looking at how long each subject took to complete the task
# in minutes by order, there are no clear order effects present
# given that there seems to be similar distributions
# (and means and standard deviations) for each order.

print("----------------------------------------------------------------------")




####
# Step 3. Based on your EDA, report what you think will
# be the findings from the statistical tests that you will
# perform. (Do not revise your predictions after moving past
# this step. The correctness of your predictions is not
# being graded, only their completeness and articulation.)

# We will find that the time it takes to complete the task using
# Google is statistically significantly less than the time it takes
# to complete the task on either Yahoo or Bing.
# We will find no statistically significant difference in the time
# it takes to complete the task between Yahoo and Bing.
# We will find that order does not have a statistically significant
# effect on the time it takes to complete the task, therefore,
# there were no order effects present in the study.




####
# Step 4. Conduct tests for normality and sphericity.
# (Hint: When testing for normality, just test for the
# normality of the D.V. per level of the I.V.; do not
# worry about the normality of residuals. See this post
# by Karen Grace-Martin:
# http://www.theanalysisfactor.com/checking-normality-anova-model/.)
# Beneath your code, formally report the results of
# this step using comments. To give you an example, one report
# for a single normality test is shown below:
# A Shapiro-Wilk test of normality shows no violation for
# Bing (W=.960, p=.608).
print("Step 4")

# Construct subsets by search engine
google <- df[df$Engine == "Google", ]
yahoo <- df[df$Engine == "Yahoo", ]
bing <- df[df$Engine == "Bing", ]

# Shapiro-Wilk for normality
print(shapiro.test(google$Minutes))
print(shapiro.test(yahoo$Minutes))
print(shapiro.test(bing$Minutes))
# google: W = 0.89608, p-value = 0.04911 (not-normal)
# yahoo: W = 0.8779, p-value = 0.02407 (not-normal)
# bing: W = 0.96032, p-value = 0.6078 (normal)

# Mauchly sphericity violation test
m <- ezANOVA(
    data = df,
    dv = Minutes,
    within = c(Engine),
    wid = Subject,
    type = 3
)
print(m$Mauchly)
# Mauchly sphericity: W = 0.978147, p-value = 0.8379789 (no violation)

# We tested each level of the "Engine" factor (Google, Yahoo, Bing) for
# violations of normality using Shapiro-Wilk tests. Google and Yahoo
# show statistically significant deviations from normality
# (Google: W=.896, p<.05; Yahoo: W=.878, p<.05).
# However, Bing showed no statistically significant deviation from normality
# (W=.960, p=.608).
#
# We further, evaluated the within-subjects "Engine" factor with Mauchly's
# test of sphericity. Our test found no violation of sphericity
# (W=.978, p=.838).

print("----------------------------------------------------------------------")




####
# Step 5. Examine the conditional distribution of your response.
# Determine statistically whether you should transform your response.
# If so, transform it and then retest its normality.
# Beneath your code, formally report the results of this step using comments.
print("Step 5")

# Create fits for each engine
fGoogle <- fitdistr(google$Minutes, "lognormal")$estimate
fYahoo <- fitdistr(yahoo$Minutes, "lognormal")$estimate
fBing <- fitdistr(bing$Minutes, "lognormal")$estimate

# Run all tests
print(
    ks.test(
        google$Minutes,
        "plnorm",
        meanlog = fGoogle[1],
        sdlog = fGoogle[2],
        exact = TRUE
    )
)
print(
    ks.test(
        yahoo$Minutes,
        "plnorm",
        meanlog = fYahoo[1],
        sdlog = fYahoo[2],
        exact = TRUE
    )
)
print(
    ks.test(
        bing$Minutes,
        "plnorm",
        meanlog = fBing[1],
        sdlog = fBing[2],
        exact = TRUE
    )
)
# Google: D = 0.11737, p-value = 0.9408
# Yahoo: D = 0.097717, p-value = 0.9886
# Bing: D = 0.13272, p-value = 0.8687
# All levels non-significant

# Transforming the `Minutes` response into `logMinutes`:
df$logMinutes <- log(df$Minutes)

# Reconstruct the data subsets
google <- df[df$Engine == "Google", ]
yahoo <- df[df$Engine == "Yahoo", ]
bing <- df[df$Engine == "Bing", ]

# Shapiro-Wilk for normality
print(shapiro.test(google$logMinutes))
print(shapiro.test(yahoo$logMinutes))
print(shapiro.test(bing$logMinutes))
# google: W = 0.9647, p-value = 0.6941 (normal)
# yahoo: W = 0.96827, p-value = 0.7646 (normal)
# bing: W = 0.94563, p-value = 0.3606 (normal)

# To test if each level of the Engine factor's Minutes continuous response
# was log-normal, a Kolmogorov-Smirnov was run on all levels of the Engine
# factor. All factors were found to be statistically non-significant
# indicating non-detectable deviations from log-normal distributions.
#
# After applying a log transform to the continuous Minutes response to the
# data and running Shapiro-Wilk tests for normality on all levels of the
# Engine factor with the new logMinutes, we found that all levels
# were non-significant indicating compliance with the normality assumption
# (Google: W=.965, p=.694, Yahoo: W=.968, p=.765, Bing: W=.946, p=.361).

print("----------------------------------------------------------------------")




####
# Step 6. Proceeding with whichever response you settled upon in Step 5,
# conduct an appropriate omnibus parametric test to rule out any order
# effects.
# (Hint: Before doing so, you should test for normality and sphericity
# for Order as your I.V.) Beneath your code, formally report the results
# of this step using comments.
print("Step 6")

# Create order subsets
orderOne <- df[df$Order == 1, ]
orderTwo <- df[df$Order == 2, ]
orderThree <- df[df$Order == 3, ]

# Shapiro-Wilk for normality
print(shapiro.test(orderOne$logMinutes))
print(shapiro.test(orderTwo$logMinutes))
print(shapiro.test(orderThree$logMinutes))
# orderOne: W = 0.9364, p-value = 0.2512
# orderTwo: W = 0.94372, p-value = 0.3351
# orderThree: W = 0.96091, p-value = 0.6192

# Mauchly sphericity violation test
m <- ezANOVA(
    data = df,
    dv = logMinutes,
    within = c(Order),
    wid = Subject,
    type = 3
)
print(m$Mauchly)
# Order: W=0.948256 p=0.6537397 (no violation)
# Run ANOVA

print(m$ANOVA)
#   Effect DFn DFd        F         p p<.05        ges
# 2  Order   2  34 1.642182 0.2085388       0.05839094

# We tested each level of the "Order" factor (1, 2, 3) for
# violations of normality using Shapiro-Wilk tests. No level of Order
# shows statistically significant deviation from normality
# (One: W=.936, p =.251; Two: W=.944, p=.335, Three: W=.961, p=.619).
#
# We further, evaluated the "Order" factor for violations of sphericity.
# Our test was not statistically significant for the within-subjects
# factor, indicating no violations of sphericity (W=.948, p=.654).
#
# Because we found no sphericity violation,
# we conducted a one-way repeated measures ANOVA against Order
# to test for statistically significant differences between the levels.
# Our one-way repeated measures ANOVA showed no statistically significant
# differences (F(2, 34) = 1.642, n.s.).

print("----------------------------------------------------------------------")




####
# Step 7. Conduct an appropriate omnibus parametric test for your factor
# of interest. (Hint: You tested conditional normality for each level
# of Engine in Step 5, but you still need to test for sphericity.
# Do that, and then test for the Engine main effect.) Beneath your
# code, formally report the results of this step using comments.
print("Step 7")

# Mauchly sphericity violation test
m <- ezANOVA(
    data = df,
    dv = logMinutes,
    within = c(Engine),
    wid = Subject,
    type = 3
)
print(m$Mauchly)
#   Effect         W         p p<.05
# 2 Engine 0.9945278 0.9570519

# No sphericity violation, simply use ANOVA
print(m$ANOVA)
#   Effect DFn DFd        F            p p<.05       ges
# 2 Engine   2  34 11.52435 0.0001509957     * 0.2678067


# Looking at the main effect of Engine on the logMinutes response,
# Mauchly's test of sphericity indicated no sphericity violation
# (W=.995, p=.957), allowing for an uncorrected repeated measures ANOVA,
# which, showed statistically significant differences between the levels
# of Engine (F(2, 34) = 11.524, p<.0005).

print("----------------------------------------------------------------------")




####
# Step 8. Conduct an analogous omnibus nonparametric test on your
# factor of interest. Beneath your code, formally report the results
# of this step using comments.
print("Step 8")

print(
    friedman_test(
        logMinutes ~ Engine | Subject,
        data = df,
        distribution = "asymptotic"
    )
)
# chi-squared = 8.4444, df = 2, p-value = 0.01467


# A Friedman test found that the differences between levels of Engine were
# statistically significant (χ2(2, N=54) = 8.44, p<.05))

print("----------------------------------------------------------------------")




####
# Step 9. In Steps 7 and 8, you had a chance to discover whether
# Engine had a significant effect on search time. But in neither
# step did you discover which search engine(s) differed significantly
# from which other(s). For neither, one of, or both of
# Steps 7 and 8 in which a statistically significant main effect of
# Engine was found, conduct post hoc pairwise comparisons among levels
# of Engine, adjusted with Holm’s sequential Bonferroni procedure.
# Beneath your code, formally report the results of this step using
# comments. (Hint: If Step 7 was statistically significant, use three
# paired-samples t-tests for your post hoc pairwise comparisons.
# If Step 8 was statistically significant, use three Wilcoxon
# signed-rank tests for your post hoc pairwise comparisons. Don’t
# forget to correct the p-values for multiple comparisons using
# Holm’s sequential Bonferroni procedure.) Finally, describe how your
# findings compare to the predictions you made in Step 3.
print("Step 9")

# Make wide format table
wideFormatDf <- dcast(df, Subject ~ Engine, value.var = "logMinutes")

# Parametric paired sample t-tests
googleYahoo <- t.test(wideFormatDf$Google, wideFormatDf$Yahoo, paired = TRUE)
googleBing <- t.test(wideFormatDf$Google, wideFormatDf$Bing, paired = TRUE)
yahooBing <- t.test(wideFormatDf$Yahoo, wideFormatDf$Bing, paired = TRUE)
print(googleYahoo)
print(googleBing)
print(yahooBing)

# Adjust all
print(
    p.adjust(
        c(
            googleYahoo$p.value,
            googleBing$p.value,
            yahooBing$p.value
        ),
        method = "holm"
    )
)
# [1] 0.0071881866 0.0007386976 0.2660674077

# We conducted post-hoc paired-samples t-tests, corrected with Holm's
# sequential Bonferroni procedure, for all pairs of the Engine factor
# to test for statistically significant deviations between the
# different pairs of levels.
#
# We found a statistically significant difference between
# Google and Yahoo (t(17) = 3.375, p<.01) and Google and Bing
# (t(17) = 4.617, p <.001) but did not find a statistically
# significant difference between Yahoo and Bing (t(17) = 1.15, n.s.).
#
# These post-hoc results match our prediction from step 3.

print("----------------------------------------------------------------------")




####
## EXPERIMENT 02 SETUP ##
####
df <- read.csv("experiment_02.csv")

# encode nominal and ordinal factors (numeric variables are set by default)
df$Subject <- factor(df$Subject)
df$Engine <- factor(df$Engine)
df$Device <- factor(df$Device)
df$Satisfaction <- ordered(df$Satisfaction)
df$logMinutes <- log(df$Minutes) # re-create the logMinutes response

# encode sum-to-zero factor contrasts
contrasts(df$Engine) <- "contr.sum"
contrasts(df$Device) <- "contr.sum"




####
# Step 10. Now examine the experiment_02.csv data table.
# (Note that the Subject, Engine, Minutes, and logMinutes
# columns are unchanged from experiment_01.csv.)
# Write a precise description conveying what you think this
# study was about. Be specific!

# The results stored in experiment_02.csv detail a study that measured
# how long it took participants to complete a search task in minutes
# using different search engines on a single device
# (with different subjects randomly provided a
# different device, either mobile or desktop).
# Additionally, the experiment measured each participant's
# satisfaction with their experience with each search engine.
#
# In this case, search engine was a within-subjects factor
# (with levels: Google, Yahoo, and Bing),
# while device was a between-subjects factor (with levels: mobile and desktop).
#
# Each participant was tested on each search engine, while each device
# was tested by 9 participants each.


####
# Step 11. Conduct exploratory data analysis (EDA) using (a)
# descriptive statistics and (b) plots. (Hint: At a minimum,
# for descriptive statistics, include measures of central tendency
# and variation for each of the six Engine × Device combinations.
# For plots, create boxplots of Engine, Device, and
# Engine × Device for both logMinutes and Satisfaction.
# Use boxplot() to do so. Since Satisfaction is ordinal,
# you will need to wrap it in as.numeric() when plotting.
# Also, create two interaction plots using
# with(df, interaction.plot(...)), one for logMinutes and and
# one for Satisfaction.)
print("Step 11")

hist(df$logMinutes)
print(mean(df$logMinutes))
print(sd(df$logMinutes))

hist(as.numeric(df$Satisfaction))
print(mean(as.numeric(df$Satisfaction)))
print(sd(as.numeric(df$Satisfaction)))

print(
    ddply(
        df,
        ~ Engine * Device,
        summarize,
        logMinutes.mean = round(mean(logMinutes), 2),
        logMinutes.sd = round(sd(logMinutes), 2),
        Satisfaction.mean = round(mean(as.numeric(Satisfaction)), 2),
        Satisfaction.sd = round(sd(as.numeric(Satisfaction)), 2)
    )
)

# Plot boxplot by engine (logMinutes)
ggplot(df, aes(x = Engine, y = logMinutes)) +
    geom_boxplot()

# Plot boxplot by device (logMinutes)
ggplot(df, aes(x = Device, y = logMinutes)) +
    geom_boxplot()

# Plot boxplot by engine (satisfaction)
ggplot(df, aes(x = Engine, y = as.numeric(Satisfaction))) +
    geom_boxplot()

# Plot boxplot by device (satisfaction)
ggplot(df, aes(x = Device, y = as.numeric(Satisfaction))) +
    geom_boxplot()

# Plot boxplot by engine x device (logMinutes)
ggplot(df, aes(x = Engine, y = logMinutes, fill = Device)) +
    geom_boxplot()

# Plot boxplot by engine x device (satisfaction)
ggplot(df, aes(x = Engine, y = as.numeric(Satisfaction), fill = Device)) +
    geom_boxplot()

# Interaction plot for logMinutes
with(df, interaction.plot(Engine, Device, logMinutes))

# Interaction plot for satisfaction
with(df, interaction.plot(Engine, Device, as.numeric(Satisfaction)))

# Looking at the whole dataset, we see that there was a mean logMinutes of
# 2.869 with a standard deviation of 0.722. The mean of Satisfaction was
# 4.519 with a standard deviation of 1.657. A table of all combinations
# of device and search engine can be seen below.
#
#   Engine  Device logMinutes.mean logMinutes.sd Satisfaction.mean Satisfaction.sd
# 1   Bing desktop            2.98          0.63              5.33            1.32
# 2   Bing  mobile            3.48          0.48              3.22            1.30
# 3 Google desktop            1.98          0.45              5.78            0.83
# 4 Google  mobile            2.74          0.51              5.33            1.41
# 5  Yahoo desktop            2.71          0.52              4.33            1.22
# 6  Yahoo  mobile            3.33          0.70              3.11            1.76
#
# The overall trends seem to show that users on desktop devices complete the
# search task in less time regardless of search engine. Further,
# on average users report being more satisfied on desktop devices than on
# mobile devices across the search engines.
#
# From the interaction plots, there doesn't seem to be a interaction effect
# for Device x Engine against logMinutes. Similarly, Device x Engine
# interaction doesn't seem to have an effect on satisfaction.

print("----------------------------------------------------------------------")




####
# Step 12. Based on your EDA, report what you think will be your results
# from tests for main effects and interactions for logMinutes and
# Satisfaction. (You do not need to make predictions about any post hoc
# pairwise comparisons. Do not revise your predictions after moving past
# this step. The correctness of your predictions is not being graded,
# only their completeness and articulation.) (Hint: Remember to make
# predictions about significant differences for both D.V.s, logMinutes
# and Satisfaction, and to do so for an Engine main effect, Device
# main effect, and Engine × Device interaction. So, you should have six
# predictions in all, although your "prediction" for Engine concerning
# search time should not be a "prediction" at all, given your analysis
# of experiment_01.csv.)

# ENGINE
# Completion time of the task in logMinutes will be significantly lower
# when the user used Google vs Yahoo or Bing (logMinutes will be
# significantly different for Google vs other levels of Engine). Yahoo
# and Bing will not be statistically different from each other
# in logMinutes.
#
# User reported satisfaction will be statistically significantly higher
# for Google than Yahoo or Bing. While Yahoo and Bing, will not be
# statistically significantly different from each other.
#
# DEVICE
# Completion time in logMinutes will not be statistically significantly
# different by Device. We believe this because in the
# logMinutes by Device boxplots the two IQRs overlap quite a bit.
#
# User reported satisfaction will be statistically significantly
# higher on desktop devices than on mobile devices. While there is
# overlap in the IQRs from the boxplot, it seems like the total range
# (standard deviation) is lower on desktop devices.
#
# ENGINE x DEVICE
# We will find no statistically significant interaction effects on
# logMinutes for Engine x Device.
#
# We will find no statistically significant interaction effects on
# Satisfaction for Engine x Device.




####
# Step 13. Ignoring assumption tests or tests for order effects,
# conduct a two-way fixed-effects mixed factorial ANOVA of
# logMinutes by Engine and Device. Beneath your code, formally
# report the results of this step using comments. (Hint: USe ezANOVA().)
print("Step 13")

m <- ezANOVA(
    data = df,
    dv = logMinutes,
    within = c(Engine),
    between = c(Device),
    wid = Subject,
    type = 3
)

print(m$Mauchly)
print(m$ANOVA)

# Mauchly's test of sphericity was run on mixed factorial ANOVA model with a
# between-subjects factor Device and a within-subjects factor Engine.
# The test was statistically non-significant for both the Engine main effect
# (W=.994, p=.958) and the Device x Engine interaction effect (W=.994, p=.958)
# indicating no sphericity violations.
#
# Because there were no sphericity violations found, we can use a repeated
# measures mixed factorial ANOVA to test for statistical differences in
# main and interaction effects.
#
# We found a statistically significant effect on logMinutes from Device
# (F(1, 16) = 21.487, p<.0005) and from Engine (F(2, 32) = 10.994, p<.005)
# but not from the Device x Engine interaction effect
# (F(2, 32) = 0.217, n.s.).

print("----------------------------------------------------------------------")




####
# Step 14. Repeat the analysis of variance you did in Step 13 but
# using a linear mixed model (LMM). (Hint: After using lmer() to
# build a model m, use Anova(m, type=3, test.statistic="F") to show
# the ANOVA table. Ignore singularity warnings when building the model.
# Also, recall that a p-value between .05 and .10 is considered a
# "marginal result" or "trend," and should be reported to 3-digits.)
# As before, beneath your code, formally report the results of this step using
# comments. Also, indicate whether the statistical conclusions change from the
# fixed-effects analysis you did in Step 13.
print("Step 14")

m <- lmer(logMinutes ~ Engine * Device + (1 | Subject), data = df)
print(Anova(m, type = 3, test.statistic = "F"))

# A linear mixed model analysis of variance indicated a statistically
# significant effect on logMinutes from Engine (F(2, 32) = 12.04, p<.0005)
# and Device (F(1, 16) = 17.38, p<.0005) but not from the Engine x Device
# interaction effect (F(2, 32) = 0.238, n.s.).
#
# None of the effects marked as statistically significant on Step 13
# changed in this step (14). In this step (14), the main effect of Engine
# had a lower statistically significant p-value, but this doesn't
# change the overall reporting.

print("----------------------------------------------------------------------")




####
# Step 15. You have a hypothesis that each search engine's search time,
# in logMinutes, is significantly different between their desktop and
# mobile versions. Conduct three pairwise comparisons to test this
# hypothesis and report your results. (Hint: Use glht() on the model
# you built in Step 14 to carry out all Engine × Device pairwise
# comparisons, but do not correct for multiple comparisons so as to get
# the raw p-values. Then, hand-select just the three pairwise
# comparisons your hypothesis requires, manually correcting their p-values
# with p.adjust() using Holm's sequential Bonferroni procedure.)
print("Step 15")

print(summary(glht(m, emm(pairwise ~ Engine * Device))))
# Linear Hypotheses:
#                                     Estimate Std. Error t value Pr(>|t|)
# Bing desktop - Google desktop == 0   0.99657    0.26132   3.814  0.00566 **
# Bing desktop - Yahoo desktop == 0    0.26830    0.26132   1.027  0.90621
# Bing desktop - Bing mobile == 0     -0.50560    0.26132  -1.935  0.39661
# Bing desktop - Google mobile == 0    0.23624    0.26132   0.904  0.94326
# Bing desktop - Yahoo mobile == 0    -0.35279    0.26132  -1.350  0.75559
# Google desktop - Yahoo desktop == 0 -0.72827    0.26132  -2.787  0.07993 .
# Google desktop - Bing mobile == 0   -1.50217    0.26132  -5.748  < 0.001 ***
# Google desktop - Google mobile == 0 -0.76033    0.26132  -2.910  0.06038 .
# Google desktop - Yahoo mobile == 0  -1.34936    0.26132  -5.164  < 0.001 ***
# Yahoo desktop - Bing mobile == 0    -0.77390    0.26132  -2.962  0.05330 .
# Yahoo desktop - Google mobile == 0  -0.03205    0.26132  -0.123  1.00000
# Yahoo desktop - Yahoo mobile == 0   -0.62109    0.26132  -2.377  0.18831
# Bing mobile - Google mobile == 0     0.74185    0.26132   2.839  0.07108 .
# Bing mobile - Yahoo mobile == 0      0.15282    0.26132   0.585  0.99151
# Google mobile - Yahoo mobile == 0   -0.58903    0.26132  -2.254  0.23631

print(
    p.adjust(
        c(
            0.39661, # bing
            0.06038, # google
            0.18831 # yahoo
        ),
        method = "holm"
    )
)

# After evaluating all pairwise comparisons of Engine x Device against
# logMinutes using Z-tests, to specifically test our hypothesis that the
# time it takes to complete the task, is lower on the desktop device than
# the mobile device for each search engine, we then select the three
# pairwise comparisons we are interested in and correct their p-values
# with Holm’s sequential Bonferroni procedure. We find that each individual
# search engine does not have a statistically significant difference between
# their desktop and mobile device counterparts in terms of completion time
# in logMinutes.
#
# Specifically for the comparison of device for Google we find no
# statistically significant effect (t(41) = 2.91, n.s.), for Yahoo
# we find no statistically significant effect (t(41) = 2.377, n.s.),
# and for Bing we find no statistically significant effect as well
# (t(41) = 1.935, n.s.).

print("----------------------------------------------------------------------")




####
# Step 16. Rather than using parametric tests on logMinutes, we can
# use nonparametric tests on the raw Minutes response, as we did in
# Step 8. However, typical nonparametric tests such as Wilcoxon signed-rank,
# Friedman, Mann-Whitney, and Kruskal-Wallis all only handle a single factor.
# Since we have two factors, Engine and Device, we need to use something else.
# The Aligned Rank Transform (ART) procedure provides such an option.
# Use the ART procedure to conduct a nonparametric analysis of variance on
# Minutes by Engine and Device. Compare your conclusions to those you obtained
# in Step 14. (Hint: The model is similar to the one you built in Step 14,
# except the response is Minutes instead of logMinutes. The model-building
# function is art() instead of lmer(). And the ANOVA call is anova() instead
# of Anova().)
print("Step 16")

m <- art(Minutes ~ Engine * Device + (1 | Subject), data = df)
print(anova(m))

# A nonparametric analysis of variance based on the Aligned Rank Transform
# indicated statistically significant effects on Minutes from Engine
# (F(2, 32) = 10.251, p<.0005) and Device (F(1, 16) = 14.484, p<.005) but
# not from the Engine x Device interaction effect (F(2, 32) = 0.413, n.s.).
#
# Comparing this to the parametric analysis of variance we conducted in
# step 14, we see that we are drawing the same statistical difference
# conclusions for which effects are statistically different.
# The only difference between the non-parameteric and parametric results
# are the fact that the tests results in different p-values with the
# parametric tests having lower p-values than their non-parametric
# counterparts.

print("----------------------------------------------------------------------")




####
# Step 17. Repeat Step 15 but using the ART model you built in Step 16.
# Report your findings as usual, and compare the conclusions to those reached
# in Step 15. (Hint: Use art.con() to perform the ART-C procedure, which stands
# for "ART contrasts." Do not correct for multiple comparisons, but hand-select
# just the three hypothesized comparisons and manually correct their p-values
# using p.adjust().)
print("Step 17")

print(
    art.con(m, ~ Engine * Device) %>%
        summary() %>%
        mutate(sig. = symnum(p.value,
            corr = FALSE, na = FALSE,
            cutpoints = c(0, .001, .01, .05, .10, 1),
            symbols = c("***", "**", "*", ".", " ")
        ))
)
#                  contrast    estimate       SE df     t.ratio      p.value sig.
# Bing,desktop - Bing,mobile -10.7777778 5.781516 48 -1.86417853 4.360724e-01
# Bing,desktop - Google,desktop  20.8888889 5.781516 32  3.61304705 1.200258e-02    *
# Bing,desktop - Google,mobile   5.6666667 5.781516 48  0.98013510 9.220374e-01
# Bing,desktop - Yahoo,desktop   6.0000000 5.781516 32  1.03779011 9.015093e-01
# Bing,desktop - Yahoo,mobile  -8.1111111 5.781516 48 -1.40293848 7.250573e-01
# Bing,mobile - Google,desktop  31.6666667 5.781516 48  5.47722558 2.228368e-05  ***
# Bing,mobile - Google,mobile  16.4444444 5.781516 32  2.84431363 7.570102e-02    .
# Bing,mobile - Yahoo,desktop  16.7777778 5.781516 48  2.90196864 5.869680e-02    .
# Bing,mobile - Yahoo,mobile   2.6666667 5.781516 32  0.46124005 9.971416e-01
# Google,desktop - Google,mobile -15.2222222 5.781516 48 -2.63291194 1.089372e-01
# Google,desktop - Yahoo,desktop -14.8888889 5.781516 32 -2.57525694 1.330105e-01
# Google,desktop - Yahoo,mobile -29.0000000 5.781516 48 -5.01598553 1.070946e-04  ***
# Google,mobile - Yahoo,desktop   0.3333333 5.781516 48  0.05765501 9.999999e-01
# Google,mobile - Yahoo,mobile -13.7777778 5.781516 32 -2.38307358 1.924290e-01
# Yahoo,desktop - Yahoo,mobile -14.1111111 5.781516 48 -2.44072859 1.630131e-01

print(
    p.adjust(
        c(
            0.4360724, # bing
            0.1089372, # google
            0.1630131 # yahoo
        ),
        method = "holm"
    )
)

# After evaluating all pairwise comparisons of Engine x Device against
# Minutes using the ART-C procedure, to specifically test our hypothesis
# that the time it takes to complete the task, is lower on the desktop device
# than the mobile device for each search engine, we then select the three
# pairwise comparison we are interested in and correct their p-values with
# Holm’s sequential Bonferroni procedure. We find that each individual
# search engine does not have a statistically significant difference
# between their desktop and mobile device counterparts in terms of completion
# time in Minutes.
#
# Specifically for the comparison of device for Google we find no
# statistically significant effect (t(48) = 2.633, n.s.), for Yahoo
# we find no statistically significant effect (t(41) = 2.441, n.s.),
# and for Bing we find no statistically significant effect as well
# (t(48) = 1.864, n.s.).
#
# Comparing these results against our results in step 15, we find no
# differences. Each search engine individually does not have a
# statistically significantly lower time for task completion on the
# desktop device than on the mobile device. The only difference between
# the parametric and non-parametric tests was the difference in
# result p-values.

print("----------------------------------------------------------------------")




####
# Step 18. Now you will analyze the Satisfaction response, which is a 1-7
# Likert scale rating for search satisfaction. As Satisfaction is an ordinal
# response, we can analyze it using mixed ordinal logistic regression, which is
# a form of generalized linear mixed model (GLMM). Analyze Satisfaction by
# Engine and Device. As clmm() is a bit finnicky, most of the code is given
# to you below, but you need to fill in the statistical model. As usual,
# formally report your results using comments.
#
#   df2 <- as.data.frame(df) # copy the data frame to appease clmm()
#   m = clmm(...put statistical model here..., data=df2, link="probit")
#   Anova.clmm(m)
print("Step 18")

df2 <- as.data.frame(df)
m <- clmm(Satisfaction ~ Engine * Device + (1 | Subject), data = df2, link = "probit")
print(Anova.clmm(m))

# An analysis of variance based on mixed ordinal logistic regression indicated
# a statistically significant effect on Satisfaction of Engine
# (χ2(2, N=32) = 12.044, p<.0005) and of Device (χ2(1, N=16) = 17.382, p<.001)
# but there was no statistically significant effect detected for the
# Engine x Device interaction (χ2(2, N=32) = 0.238, n.s.).

print("----------------------------------------------------------------------")




####
# Step 19. You have a hypothesis that satisfaction with each search engine
# is significantly different between their desktop and mobile versions.
# Using your mixed ordinal logistic statistical model from Step 18,
# conduct pairwise comparisons to test this hypothesis and report your
# results. (Hint: This step is to Satisfaction what Steps 15 and 17 were
# to logMinutes.) As models built with clmm() can be a bit finnicky,
# the first part of the necessary code is given to you below. (You still
# need to hand-correct the hypothesized p-values as you did in Steps 15 and
# 17.) Be sure to formally report your results below your code.
#
#   summary(as.glht(pairs(emmeans(m, ~ Engine*Device))), test=adjusted(type="none"))
#   # hand-select and correct hypothesized p-values for Bing, Google, and Yahoo
print("Step 19")

print(
    summary(
        as.glht(pairs(emmeans(m, ~ Engine * Device))),
        test = adjusted(type = "none")
    )
)
# Linear Hypotheses:
#                                     Estimate Std. Error z value Pr(>|z|)
# Bing desktop - Google desktop == 0   -0.3457     0.4977  -0.695 0.487251
# Bing desktop - Yahoo desktop == 0     0.8104     0.4950   1.637 0.101590
# Bing desktop - Bing mobile == 0       1.6360     0.5166   3.167 0.001541 **
# Bing desktop - Google mobile == 0    -0.0141     0.4931  -0.029 0.977197
# Bing desktop - Yahoo mobile == 0      1.7874     0.5218   3.425 0.000614 ***
# Google desktop - Yahoo desktop == 0   1.1562     0.5071   2.280 0.022599 *
# Google desktop - Bing mobile == 0     1.9817     0.5334   3.715 0.000203 ***
# Google desktop - Google mobile == 0   0.3316     0.4982   0.666 0.505591
# Google desktop - Yahoo mobile == 0    2.1331     0.5371   3.972 7.13e-05 ***
# Yahoo desktop - Bing mobile == 0      0.8256     0.4940   1.671 0.094711 .
# Yahoo desktop - Google mobile == 0   -0.8245     0.4953  -1.665 0.095943 .
# Yahoo desktop - Yahoo mobile == 0     0.9770     0.5020   1.946 0.051655 .
# Bing mobile - Google mobile == 0     -1.6501     0.5165  -3.195 0.001399 **
# Bing mobile - Yahoo mobile == 0       0.1514     0.4909   0.308 0.757706
# Google mobile - Yahoo mobile == 0     1.8015     0.5217   3.453 0.000554 ***

print(
    p.adjust(
        c(
            0.001541, # bing
            0.505591, # google
            0.051655 # yahoo
        ),
        method = "holm"
    )
)

# Pairwise comparisons using Z-tests, corrected with Holm's sequential
# Boneferroni procedure, indicated that users that used Bing on desktop
# devices reported statistically significantly higher satisfaction
# than users on mobile devices (Z = 3.167, p<.005), however users that
# used Google or Yahoo on desktop or mobile devices did not report
# statistically significant differences in satisfaction
# (Google: Z = 0.666, n.s.; Yahoo: Z = 1.946, n.s.).

print("----------------------------------------------------------------------")




####
# Step 20. Repeat Step 18 but now using the Aligned Rank Transform
# (ART) procedure. As usual, formally report your statistical findings
# beneath your code. Also, compare your statistical conclusions to those
# from Step 18.
print("Step 20")

m <- art(Satisfaction ~ Engine * Device + (1 | Subject), data = df)
print(anova(m))

# A nonparametric analysis of variance based on the Aligned Rank Transform
# indicated statistically significant effects on Satisfaction from Engine
# (F(2, 32) = 8.598, p<.005) and on Satisfaction from Device
# (F(1, 16) = 12.506, p<.005) but not from the Engine x Device interaction
# effect (F(2, 32) = 2.072, n.s.).
#
# Comparing this to the parametric analysis of variance we conducted in
# step 18, we see that we are drawing the same statistical difference
# conclusions for which effects are statistically different.
# The only difference between the non-parameteric and parametric results
# are the fact that the tests results in different p-values with the
# parametric tests having lower p-values than their non-parametric
# counterparts.

print("----------------------------------------------------------------------")




####
# Step 21. Repeat step 19 but now using the Aligned Rank Transform
# contrasts (ART-C) procedure. As usual, formally report your statistical
# findings beneath your code. Also, compare your statistical conclusions
# to those from Step 19.
print("Step 21")

print(
    art.con(m, ~ Engine * Device) %>%
        summary() %>%
        mutate(sig. = symnum(p.value,
            corr = FALSE, na = FALSE,
            cutpoints = c(0, .001, .01, .05, .10, 1),
            symbols = c("***", "**", "*", ".", " ")
        ))
)
#                   contrast    estimate       SE df     t.ratio     p.value sig.
# Bing,desktop - Bing,mobile  19.7777778 5.958814 48  3.31907982 0.020197127    *
# Bing,desktop - Google,desktop  -4.6666667 5.958814 32 -0.78315367 0.968429883
# Bing,desktop - Google,mobile  -0.1111111 5.958814 48 -0.01864652 1.000000000
# Bing,desktop - Yahoo,desktop  10.0000000 5.958814 32  1.67818643 0.555377719
# Bing,desktop - Yahoo,mobile  19.3333333 5.958814 48  3.24449376 0.024655305    *
# Bing,mobile - Google,desktop -24.4444444 5.958814 48 -4.10223348 0.002069047   **
# Bing,mobile - Google,mobile -19.8888889 5.958814 32 -3.33772633 0.023987140    *
# Bing,mobile - Yahoo,desktop  -9.7777778 5.958814 48 -1.64089339 0.576360915
# Bing,mobile - Yahoo,mobile  -0.4444444 5.958814 32 -0.07458606 0.999999640
# Google,desktop - Google,mobile   4.5555556 5.958814 48  0.76450715 0.972139401
# Google,desktop - Yahoo,desktop  14.6666667 5.958814 32  2.46134009 0.166158255
# Google,desktop - Yahoo,mobile  24.0000000 5.958814 48  4.02764742 0.002602696   **
# Google,mobile - Yahoo,desktop  10.1111111 5.958814 48  1.69683294 0.540541681
# Google,mobile - Yahoo,mobile  19.4444444 5.958814 32  3.26314027 0.028772875    *
# Yahoo,desktop - Yahoo,mobile   9.3333333 5.958814 48  1.56630733 0.624064446

print(
    p.adjust(
        c(
            0.020197127, # bing
            0.972139401, # google
            0.624064446 # yahoo
        ),
        method = "holm"
    )
)

# Post-hoc pairwise comparisons conducted with the ART-C procedure, and corrected
# with Holm’s sequential Bonferroni procedure, indicated a trend for Bing users
# reporting higher satisfaction on desktop devices than on mobile devices
# (t(48) = 3.319, p=.061), however users that used Google or Yahoo on desktop
# or mobile devices did not report statistically significant differences in
# satisfaction (Google: t(48) = 0.765, n.s.; Yahoo: t(48) = 1.566, n.s.).
#
# Comparing this to the parametric tests we conducted in
# step 19, we see that our result for Bing device satisfaction difference
# has moved from statistically significant to a trend. However,
# Google and Yahoo, continue to be non-significant.

print("----------------------------------------------------------------------")




####
# Step 22. Stepping back , compare your statistical conclusions from your
# analysis of experiment_02.csv with the predictions you made in Step 12.
# For each main effect or interaction effect (you should have six in all),
# indicate whether the various statistical tests you conducted confirmed
# or failed to confirm your prediction. (Hint: As a reminder, you had two
# main effect predictions and one interaction prediction for each of the
# logMinutes and Satisfaction responses. For logMinutes, you performed
# analyses with a fixed-effects ANOVA, a linear mixed model (LMM), and
# the Aligned Rank Transform (ART) procedure.[1] For Satisfaction,
# you performed analyses with mixed ordinal logistic regression
# (a GLMM) and the Aligned Rank Transform (ART) procedure.)
#
#   [1] Technically, you performed the ART procedure on Minutes, not
#       logMinutes, but since the ART is a rank-based procedure,
#       the difference is negligible.

# ENGINE
# Our prediction for the main effect of Engine was that the task
# completion time would be statistically significantly lower when users
# completed the task with Google vs the other two search engines.
# Our tests from step 13 and step 14 show that there is a
# statistically significant difference between the levels, however
# no post-hoc analysis was conducted to confirm our specific
# hypothesis that Google was statistically significantly different
# from the other two levels and that Yahoo and Bing were not
# statistically significantly different from each other.
# As such, we cannot entirely confirm our prediction.
#
# Our prediction for the main effect of Engine was that the
# user reported satisfaction would be statistically significantly
# higher for Google than for Yahoo or Bing. And Yahoo and Bing
# would not have a statistically significant difference.
# Our tests from step 18 and step 20 show that there is a statistically
# significant difference between the levels, however post-hoc analysis
# was conducted to confirm our specific hypothesis that Google
# was statistically significant different from the other two levels
# and that Yahoo and Bing were not statistically significantly different
# from each other. As such, we cannot entirely confirm our prediction.
#
# DEVICE
# Our prediction for the main effect of Device was that the task
# completion time would not be statistically significantly different
# between the two devices. However, the results from our tests
# from steps 13 and 14 show that the main effect of Device is
# significant which means that users of Desktop devices spent
# less time completing the task than their mobile device user
# counterparts. Our prediction was incorrect.
#
# Our prediction for the main effect of Device was that the
# user reported satisfaction would be statistically significantly
# higher from users that used desktop devices than users that used
# mobile devices. Our results from steps 18 and 20, confirm our
# prediction, and show that users who used desktop devices
# reported higher satisfaction levels than users of mobile devices.
#
# ENGINE x DEVICE
# We predicted that there would be no interaction effects found
# for Engine x Device on the logMinutes response. Steps 13 and 14
# show that there are no overall statistically significant
# interaction effects. Further, utilizing post-hoc analyses
# from steps 15 and 17, we found that on an individual level,
# users of the search engine options were not statistically
# significantly different for their pairwise device comparisons
# (Google.desktop vs Google.mobile = n.s.,
# Yahoo.desktop vs Yahoo.mobile = n.s.,
# Bing.desktop vs Bing.mobile = n.s.). These findings confirm
# our prediction.
#
# We predicted that there would be no interaction effects found
# for Engine x Device on the user reported Satisfaction response.
# Steps 18 and 20 show that there are no overall statistically
# significant interaction effects. However, utilizing post-hoc
# analyses from steps 19 and 21, we found that on an individual level,
# users of the search engine options were not statistically
# significantly different for their pairwise device comparisons
# except in the case of Bing where we found that users of desktop
# devices reported statistically significantly higher
# (or trended higher) satisfaction than their mobile device
# counterparts. This partially confirms our prediction.