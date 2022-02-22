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
hist(df$Minutes)
print(mean(df$Minutes))
print(sd(df$Minutes))

# Looking at the overall mean and standard deviation for the whole dataset
# we see that on average, regardless of search engine, participants
# completed the task in 22.38 minutes with a standard deviation of
# 15.22 minutes. However, we notice from our histogram that it seems our
# distribution is right-skewed or gamma distributed which makes sense
# because it is very common for datasets that measure response times or
# completion times to be gamma distributed.

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




####
# Step 3. Based on your EDA, report what you think will be the findings from the statistical tests
#         that you will perform. (Do not revise your predictions after moving past this step. The
#         correctness of your predictions is not being graded, only their completeness and
#         articulation.)




####
# Step 4. Conduct tests for normality and sphericity. (Hint: When testing for normality, just test
#         for the normality of the D.V. per level of the I.V.; do not worry about the normality of
#         residuals. See this post by Karen Grace-Martin:
#         http://www.theanalysisfactor.com/checking-normality-anova-model/.) Beneath your code,
#         formally report the results of this step using comments. To give you an example, one
#         report for a single normality test is shown below:
#
#         # A Shapiro-Wilk test of normality shows no violation for Bing (W=.960, p=.608).




####
# Step 5. Examine the conditional distribution of your response. Determine statistically whether you
#         should transform your response. If so, transform it and then retest its normality. Beneath
#         your code, formally report the results of this step using comments.




####
# Step 6. Proceeding with whichever response you settled upon in Step 5, conduct an appropriate
#         omnibus parametric test to rule out any order effects. (Hint: Before doing so, you should
#         test for normality and sphericity for Order as your I.V.) Beneath your code, formally report
#         the results of this step using comments.




####
# Step 7. Conduct an appropriate omnibus parametric test for your factor of interest. (Hint: You
#         tested conditional normality for each level of Engine in Step 5, but you still need to
#         test for sphericity. Do that, and then test for the Engine main effect.) Beneath your
#         code, formally report the results of this step using comments.




####
# Step 8. Conduct an analogous omnibus nonparametric test on your factor of interest. Beneath
#         your code, formally report the results of this step using comments.




####
# Step 9. In Steps 7 and 8, you had a chance to discover whether Engine had a significant effect
#         on search time. But in neither step did you discover which search engine(s) differed
#         significantly from which other(s). For neither, one of, or both of Steps 7 and 8 in which
#         a statistically significant main effect of Engine was found, conduct post hoc pairwise
#         comparisons among levels of Engine, adjusted with Holm’s sequential Bonferroni procedure.
#         Beneath your code, formally report the results of this step using comments. (Hint: If
#         Step 7 was statistically significant, use three paired-samples t-tests for your post hoc
#         pairwise comparisons. If Step 8 was statistically significant, use three Wilcoxon
#         signed-rank tests for your post hoc pairwise comparisons. Don’t forget to correct the
#         p-values for multiple comparisons using Holm’s sequential Bonferroni procedure.) Finally,
#         describe how your findings compare to the predictions you made in Step 3.




####
## EXPERIMENT 02 SETUP ##
####

# you might want to set your working directory where your experiment_02.csv data file is
# setwd("C:\\example\\insc 571 wi22\\assignments\\A03") # example directory path
# df <- read.csv("experiment_02.csv")

# # encode nominal and ordinal factors (numeric variables are set by default)
# df$Subject <- factor(df$Subject)
# df$Engine <- factor(df$Engine)
# df$Device <- factor(df$Device)
# df$Satisfaction <- ordered(df$Satisfaction)
# df$logMinutes <- log(df$Minutes) # re-create the logMinutes response

# # encode sum-to-zero factor contrasts
# contrasts(df$Engine) <- "contr.sum"
# contrasts(df$Device) <- "contr.sum"




####
# Step 10. Now examine the experiment_02.csv data table. (Note that the Subject, Engine, Minutes,
#          and logMinutes columns are unchanged from experiment_01.csv.) Write a precise description
#          conveying what you think this study was about. Be specific!




####
# Step 11. Conduct exploratory data analysis (EDA) using (a) descriptive statistics and (b) plots.
#          (Hint: At a minimum, for descriptive statistics, include measures of central tendency
#          and variation for each of the six Engine × Device combinations. For plots, create boxplots
#          of Engine, Device, and Engine × Device for both logMinutes and Satisfaction. Use boxplot()
#          to do so. Since Satisfaction is ordinal, you will need to wrap it in as.numeric() when
#          plotting. Also, create two interaction plots using with(df, interaction.plot(...)), one
#          for logMinutes and and one for Satisfaction.)




####
# Step 12. Based on your EDA, report what you think will be your results from tests for main effects
#          and interactions for logMinutes and Satisfaction. (You do not need to make predictions
#          about any post hoc pairwise comparisons. Do not revise your predictions after moving past
#          this step. The correctness of your predictions is not being graded, only their completeness
#          and articulation.) (Hint: Remember to make predictions about significant differences for
#          both D.V.s, logMinutes and Satisfaction, and to do so for an Engine main effect, Device
#          main effect, and Engine × Device interaction. So, you should have six predictions in all,
#          although your "prediction" for Engine concerning search time should not be a "prediction"
#          at all, given your analysis of experiment_01.csv.)




####
# Step 13. Ignoring assumption tests or tests for order effects, conduct a two-way fixed-effects mixed
#          factorial ANOVA of logMinutes by Engine and Device. Beneath your code, formally report the
#          results of this step using comments. (Hint: USe ezANOVA().)




####
# Step 14. Repeat the analysis of variance you did in Step 13 but using a linear mixed model (LMM).
#          (Hint: After using lmer() to build a model m, use Anova(m, type=3, test.statistic="F") to show
#          the ANOVA table. Ignore singularity warnings when building the model. Also, recall that a p-
#          value between .05 and .10 is considered a "marginal result" or "trend," and should be reported
#          to 3-digits.) As before, beneath your code, formally report the results of this step using
#          comments. Also, indicate whether the statistical conclusions change from the fixed-effects
#          analysis you did in Step 13.




####
# Step 15. You have a hypothesis that each search engine's search time, in logMinutes, is significantly
#          different between their desktop and mobile versions. Conduct three pairwise comparisons to
#          test this hypothesis and report your results. (Hint: Use glht() on the model you built in
#          Step 14 to carry out all Engine × Device pairwise comparisons, but do not correct for
#          multiple comparisons so as to get the raw p-values. Then, hand-select just the three pairwise
#          comparisons your hypothesis requires, manually correcting their p-values with p.adjust()
#          using Holm's sequential Bonferroni procedure.)




####
# Step 16. Rather than using parametric tests on logMinutes, we can use nonparametric tests on the raw
#          Minutes response, as we did in Step 8. However, typical nonparametric tests such as Wilcoxon
#          signed-rank, Friedman, Mann-Whitney, and Kruskal-Wallis all only handle a single factor. Since
#          we have two factors, Engine and Device, we need to use something else. The Aligned Rank
#          Transform (ART) procedure provides such an option. Use the ART procedure to conduct a
#          nonparametric analysis of variance on Minutes by Engine and Device. Compare your conclusions
#          to those you obtained in Step 14. (Hint: The model is similar to the one you built in Step 14,
#          except the response is Minutes instead of logMinutes. The model-building function is art()
#          instead of lmer(). And the ANOVA call is anova() instead of Anova().)




####
# Step 17. Repeat Step 15 but using the ART model you built in Step 16. Report your findings as usual,
#          and compare the conclusions to those reached in Step 15. (Hint: Use art.con() to perform
#          the ART-C procedure, which stands for "ART contrasts." Do not correct for multiple
#          comparisons, but hand-select just the three hypothesized comparisons and manually correct
#          their p-values using p.adjust().)




####
# Step 18. Now you will analyze the Satisfaction response, which is a 1-7 Likert scale rating for
#          search satisfaction. As Satisfaction is an ordinal response, we can analyze it using mixed
#          ordinal logistic regression, which is a form of generalized linear mixed model (GLMM).
#          Analyze Satisfaction by Engine and Device. As clmm() is a bit finnicky, most of the code
#          is given to you below, but you need to fill in the statistical model. As usual, formally
#          report your results using comments.
#
#            df2 <- as.data.frame(df) # copy the data frame to appease clmm()
#            m = clmm(...put statistical model here..., data=df2, link="probit")
#            Anova.clmm(m)




####
# Step 19. You have a hypothesis that satisfaction with each search engine is significantly different
#          between their desktop and mobile versions. Using your mixed ordinal logistic statistical
#          model from Step 18, conduct pairwise comparisons to test this hypothesis and report your
#          results. (Hint: This step is to Satisfaction what Steps 15 and 17 were to logMinutes.) As
#          models built with clmm() can be a bit finnicky, the first part of the necessary code is
#          given to you below. (You still need to hand-correct the hypothesized p-values as you did
#          in Steps 15 and 17.) Be sure to formally report your results below your code.
#
#            summary(as.glht(pairs(emmeans(m, ~ Engine*Device))), test=adjusted(type="none"))
#            # hand-select and correct hypothesized p-values for Bing, Google, and Yahoo




####
# Step 20. Repeat Step 18 but now using the Aligned Rank Transform (ART) procedure. As usual,
#          formally report your statistical findings beneath your code. Also, compare your
#          statistical conclusions to those from Step 18.




####
# Step 21. Repeat step 19 but now using the Aligned Rank Transform contrasts (ART-C) procedure.
#          As usual, formally report your statistical findings beneath your code. Also, compare
#          your statistical conclusions to those from Step 19.




####
# Step 22. Stepping back , compare your statistical conclusions from your analysis of
#          experiment_02.csv with the predictions you made in Step 12. For each main effect or
#          interaction effect (you should have six in all), indicate whether the various
#          statistical tests you conducted confirmed or failed to confirm your prediction. (Hint:
#          As a reminder, you had two main effect predictions and one interaction prediction for
#          each of the logMinutes and Satisfaction responses. For logMinutes, you performed
#          analyses with a fixed-effects ANOVA, a linear mixed model (LMM), and the Aligned Rank
#          Transform (ART) procedure.[1] For Satisfaction, you performed analyses with mixed
#          ordinal logistic regression (a GLMM) and the Aligned Rank Transform (ART) procedure.)
#
#          [1] Technically, you performed the ART procedure on Minutes, not logMinutes, but
#              since the ART is a rank-based procedure, the difference is negligible.