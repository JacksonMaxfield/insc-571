#### survey.R
#### INSC 571
#### Assignment 02
#### Winter 2022
#### Jackson Brown and Ruotong Wang
#### jmxbrown@uw.edu and ruotongw@uw.edu

library(plyr)
library(XNomial)
library(RVAideMemoire)
library(coin)
library(MASS)

# Set working dir
setwd("//home/maxfield/active/personal/insc-571/assignments/a02/")
df <- read.csv("survey.csv")

# encode nominal and ordinal factors (numeric variables are set by default)
df$PId <- factor(df$PId)
df$Gender <- factor(df$Gender)
df$MajorField <- factor(df$MajorField)
df$UsedPhysical <- factor(df$UsedPhysical)
df$UsedOnline <- factor(df$UsedOnline)
df$Device <- factor(df$Device)
df$AccessPref <- factor(df$AccessPref)
df$ResourceSeek <- factor(df$ResourceSeek)
df$HowOftenFind <- ordered(df$HowOftenFind)
df$SatisfiedPhysical <- ordered(df$SatisfiedPhysical)
df$SatisfiedOnline <- ordered(df$SatisfiedOnline)




####
# DEMOGRAPHICS
####
# Question 1. How many undergraduates responded to the survey?
print(levels(df$PId)) # 250
# There were 250 undergraduates who responded to this survey.




####
# Question 2. What was the average age of all respondents?
# Include standard deviation.
print(mean(df$Age))
print(sd(df$Age))
# The mean of all respondents ages was 19.84 with a
# standard deviation of 1.7898.




####
# Question 3. Plot the distribution of gender responses.
# How many respondents indicated each gender response?
plot(~Gender, data = df)
print(table(df$Gender))
# There were 109 respondents that self-identified as men,
# there were 108 respondents that self identified as women,
# there were 19 respondents that self-identified as non-binary,
# 5 respondents preferred not to disclose their gender identity,
# and 9 respondents preferred to self-describe their gender identity.




####
# Question 4. What was the average matriculation year of respondents?
# Include standard deviation. (Note: Treat “5th+” as a “5”.)
print(mean(df$Year))
print(sd(df$Year))
# The mean of all respondents matriculation years was 2.93 with a
# standard deviation of 1.3911.




####
# Question 5. Plot the distribution of major fields of study.
# How many respondents indicated each major field of study?
plot(~MajorField, data = df)
print(table(df$MajorField))
# Of the respondents, 56 were enrolled in a business major,
# 36 were enrolled in a computing major,
# 17 were enrolled in an engineering major,
# 62 were enrolled in a humanities major,
# 25 were enrolled in a natural science major,
# 39 were enrolled in a social science major,
# and 15 were enrolled in other majors.




####
# Question 6. Plot a histogram of GPA. Describe what you see.
hist(df$GPA)
# The most common GPA for a respondent falls between 3.5 and 4.0 GPA,
# with a left-skew with fewer and fewer respondents having lower GPAs.



####
# Question 7. What was the average GPA (and standard deviation),
# rounded to the nearest hundredth:
#
#     7a. Of all respondents?
print(round(mean(df$GPA), 2))
print(round(sd(df$GPA), 2))
#       The average GPA of all respondents was 3.05 with a
#       standard deviation of 0.85.



#     7b. Of respondents by year? Feel free to show a table.
#         Also, make a plot to show this. (Hint: You will want to treat
#         df$Year as a factor when plotting by wrapping it in as.factor().)
plot(GPA ~ as.factor(Year), data = df)
print(
    ddply(
        df,
        ~ as.factor(Year),
        summarise,
        GPA.mean = round(mean(GPA), 2),
        GPA.sd = round(sd(GPA), 2)
    )
)
#       The average GPA of year ones was 3.20 with a
#       standard deviation of 0.80.
#       The average GPA of year twos was 3.03 with a
#       standard deviation of 0.88.
#       The average GPA of year threes was 3.01 with a
#       standard deviation of 0.85.
#       The average GPA of year fours was 3.11 with a
#       standard deviation of 0.84.
#       The average GPA of year fives+ was 2.86 with a
#       standard deviation of 0.91.



#     7c. Of respondents by major field of study? Feel free to show a table.
#         Also, make a plot to show this.
plot(GPA ~ MajorField, data = df)
print(
    ddply(
        df,
        ~MajorField,
        summarise,
        GPA.mean = round(mean(GPA), 2),
        GPA.sd = round(sd(GPA), 2)
    )
)
#        MajorField GPA.mean GPA.sd
# 1        business     2.93   0.94
# 2       computing     2.72   0.99
# 3     engineering     3.07   0.69
# 4      humanities     3.24   0.78
# 5 natural science     2.97   0.82
# 6           other     3.02   0.60
# 7  social science     3.27   0.76




# Question 8. Is there a statistically significant difference in GPA by
# major field of study?
model <- aov(GPA ~ MajorField, data = df)
print(anova(model))
# There was a statistically significant difference in GPA by major
# field of study according to a one-way ANOVA
# (F(6, 243) = 2.1573, p < .05).




####
# LIBRARY USAGE
####
# Question 9. How many respondents had ever used:
#
#     9a. The physical library? Is this proportion statistically
#         significantly different from half?
xt <- xtabs(~UsedPhysical, data = df)
print(xt)
print(binom.test(xt, p = (1 / 2), alternative = "two.sided"))
#       Out of 250 respondents, 230 of them have used the physical library.
#       A two-sided exact binomial test indicated that these proportions were
#       statistically significantly different from chance/half (p < 0.0001).



#     9b. The online library system? Is this proportion statistically
#         significantly different from half?
xt <- xtabs(~UsedOnline, data = df)
print(xt)
print(binom.test(xt, p = (1 / 2), alternative = "two.sided"))
#       Out of 250 respondents, 234 of them used the online library resources.
#       A two-sided exact binomial test indicated that these proportions were
#       statistically significantly different from chance/half (p < 0.0001).



# Question 10. Plot the proportion of respondents who ever used the physical
# library and online library system side-by-side. (Hint: Use barplot() with
# two values combined with c().) Is the proportion of respondents who ever
# used the physical library statistically significantly different from the
# percentage of respondents who used the online library system?
# (Hint: In question 9a, you compared a proportion to 50%. This only
# requires a simple change to that.)
barplot(c(230, 234), ylim = c(0, 250), names.arg = c("Physical", "Online"))
print(binom.test(c(230, 234), p = (1 / 2), alternative = "two.sided"))
#       A two-sided exact binomial test indicated that the proportion of
#       respondents who had ever used the physical library resources and
#       the proportion of respondents who have ever used the online library
#       resources was not statistically significantly different from
#       chance/half (n.s.).




# Question 11. Is there a statistically significant difference in GPA:
#
#    11a. By whether respondents ever used the physical library?
print(t.test(GPA ~ UsedPhysical, data = df, var.equal = TRUE))
#   There was no statistically significant difference found for the GPA between
#   respondents who used the physical library and who didn't use the physical
#   library (t(248) = 0.07761, n.s.).



#    11b. By whether respondents ever used the online library system?
print(t.test(GPA ~ UsedOnline, data = df, var.equal = TRUE))
#   There was no statistically significant difference for found for the GPA
#   between respondents who used the online library resources and who didn't
#   use the online library resources (t(248) = 0.81506, n.s.).




####
# Question 12. What was the average hours per week (and standard deviation)
# spent by respondents:
#
#    12a. In the physical library?
print(mean(df$HoursPhysical))
print(sd(df$HoursPhysical))
#       The average hours per week spent in the physical library by
#       all respondents was 7.632 hours
#       with a standard deviation of 4.325 hours.



#    12b. Accessing the online library system?
print(mean(df$HoursOnline))
print(sd(df$HoursOnline))
#       The average hours per week spent using the online library system
#       by all respondents was 9.264 hours
#       with a standard deviation of 6.23 hours.



#    12c. On social media?
print(mean(df$HoursSocial))
print(sd(df$HoursSocial))
#       The average hours per week spent using social media by
#       all respondents was 10.384 hours
#       with a standard deviation of 6.818 hours.




####
# Question 13. What is the statistical correlation between the following?
# Also, is the correlation statistically significant? Be sure to interpret
# each result. (Hint: Use cor.test(). For this and other correlation
# questions, interpret 0.10 <= |r| < .20 as "weak,"
# 0.20 <= |r| < 0.40 as "mild," 0.40 <= |r| < 0.60 as "moderate,"
# 0.60 <= |r| < 0.80 as "strong," and |r| >= 0.80 as "very strong.")
#
#    13a. Hours spent in the physical library and hours spent accessing
#         the online library system?
print(cor.test(df$HoursPhysical, df$HoursOnline))
#    A Pearson's r test found no correlation between respondents
#    number of hours spent using the physical library and the number
#    of hours spent using the online library system
#    (r=-0.005918667, n.s.).



#    13b. Hours spent using the physical library and hours spent on
#         social media?
print(cor.test(df$HoursPhysical, df$HoursSocial))
#    A Pearson's r test found no correlation between respondents
#    number of hours spent using the physical library and the number
#    of hours spent using the social media
#    (r=-0.001998112, n.s.).



#    13c. Hours spent accessing the online library system and hours spent
#         on social media?
print(cor.test(df$HoursOnline, df$HoursSocial))
#    A Pearson's r test found no correlation between respondents
#    number of hours spent using the online library system and the number
#    of hours spent using social media
#    (r=0.03145147, n.s.).



#    13d. Hours spent in the physical library and GPA?
print(cor.test(df$HoursPhysical, df$GPA))
#    A Pearson's r test found a week correlation between respondents
#    number of hours spent using the physical library and GPA
#    (r=0.1624735, p < .05).



#    13e. Hours spent accessing the online library system and GPA?
print(cor.test(df$HoursOnline, df$GPA))
#    A Pearson's r test found no correlation between respondents
#    number of hours spent using the online library system and GPA
#    (r=0.08158198, n.s.).



#    13f. Hours spent on social media and GPA?
print(cor.test(df$HoursSocial, df$GPA))
#    A Pearson's r test found a week correlation between respondents
#    number of hours spent using social media and GPA
#    (r=-0.1788291, p < .005).




####
# LIBRARY ACCESS
####
# Question 14. Concerning the distribution of computing devices typically
# used to access the online library system:
#
#    14a. Plot the distribution of devices used. How many respondents
#         indicated each device?
plot(df$Device)
print(table(df$Device))
# The number of devices used to access the online library system.
#    desktop     laptop      other smartphone     tablet
#        116         54         19         22         39



#    14b. Is this distribution of devices significantly different from uniform?
xt <- xtabs(~Device, data = df)
print(xt)
print(chisq.test(xt))
#   A one-sample Pearson Chi-Squared test indicated that these proportions were
#   statistically significantly different from chance
#   (χ2(4, N=250) = 124.76, p < .0001).




####
# Question 15. Concerning the distribution of respondents’ preferences for
# accessing the university library’s resources:
#
#    15a. Plot the distribution of respondents’ preferences.
#         How many respondents indicated each preference?
plot(df$AccessPref)
print(table(df$AccessPref))
# The number of respondent preferences for using the library resources.
# in-person no preference        online
#        69            15           166



#    15b. Is this distribution of preferences significantly different from
#         uniform?
xt <- xtabs(~AccessPref, data = df)
print(xt)
print(chisq.test(xt))
#   A one-sample Pearson Chi-Squared test indicated that these proportions were
#   statistically significantly different from chance
#   (χ2(2, N=250) = 140.5, p < .0001).



#    15c. Is there a significant association between respondents’ major
#         field of study and how they prefer to access the university library’s
#         resources? (Hint: You have learned three different two-sample tests
#         of association[1] pick one to use. Ignore any warnings. Also, plot
#         the crosstabs created with xtabs() to help you see any differences in
#         proportion, or lack thereof.)
#
#         [1] These are chisq.test(), G.test(), and fisher.test(). Whichever
#         test you choose should be used consistently throughout this
#         assignment. If you choose fisher.test(), you will need to set the
#         simulate.p.value parameter to TRUE.
xt <- xtabs(~ MajorField + AccessPref, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#   A two-sample Pearson Chi- Squared test indicated a statistically
#   significant association between respondents major field and their
#   access preference (χ2(12, N=250) = 21.489, p < .05).




####
# Question 16. For respondents that expressed a preference for
# in-person or online library access (N=235), was the preference for
# in-person access (N=69) significantly different than the preference
# for online access (N=166)? (Hint: What you did for question 10 should
# be relevant here.)
print(binom.test(c(69, 166), p = (1 / 2), alternative = "two.sided"))
#       A two-sided exact binomial test indicated that the proportion of
#       respondents who preferred physical library resources and
#       the proportion of respondents who preferred the online library
#       resources was statistically significantly different from
#       chance/half (p < 0.0001).




####
# Question 17. Concerning the types of resources sought by respondents:
#
#    17a. Plot the distribution of resources sought.
#         How many of each type of resource is sought?
plot(df$ResourceSeek)
print(table(df$ResourceSeek))
# The number of respondent sought each type of resource
# books     conference proceedings          journals             multimedia                  other
# 84                     62                     77                     23                      4



#    17b. Is this distribution of resources significantly different
#         from uniform?
xt <- xtabs(~ResourceSeek, data = df)
print(xt)
print(chisq.test(xt))
#   A one-sample Pearson Chi-Squared test indicated that these proportions were
#   statistically significantly different from chance
#   (χ2(4, N=250) = 97.48, p < .0001).



#    17c. Is there a significant association between how respondents
#         prefer to access the library’s resources and the type of resource
#         they most often seek? (Hint: Hint: For this and any following
#         two-sample tests of association, refer back to what you did for
#         question 15c, including plotting the crosstabs to aid interpretation.)
xt <- xtabs(~ ResourceSeek + AccessPref, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#   A two-sample Pearson Chi-Squared test indicated there was no statistically
#   significant association between how respondents prefer to access the
#   library’s resources and the type of resource they most often seek
#   (χ2(8, N=250) = 12.098, n.s.).



#    17d. Is there a significant association between respondents’ major
#         field of study and the type of resource they most often seek?
xt <- xtabs(~ MajorField + ResourceSeek, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#   A two-sample Pearson Chi- Squared test indicated a statistically
#   significant association between respondents major field of study and
#   the type of resource they most often seek
#   (χ2(24, N=250) = 95.273, p < .0001).




####
# Question 18. Concerning how often respondents find the item they seek in the
# university library:
#
#    18a. Plot the distribution of responses. How many respondents
#         indicated each response (1-7)? For ease of presentation,
#         you can show a simple table.
plot(df$HowOftenFind)
print(table(df$HowOftenFind))
# How often respondents found what they were looking for.
# Never or almost never   Rarely  Less than half the time     About half the time     More than half the time     Often   Always or almost never
# 2                       5           26                       65                         78                       50      24



#    18b. Is this distribution of responses significantly different
#         from uniform? (Hint: xmulti() will produce a warning due to
#         potential running time; use xmonte() instead. The syntax is
#         otherwise the same as xmulti().)
xt <- xtabs(~HowOftenFind, data = df)
print(xmonte(xt, rep(1 / length(xt), length(xt)), statName = "Prob"))
# A Multinomial Goodness of Fit (monte carlo) test indicated that
# these proportions were statistically significantly different from
# chance (p < .0001).



#    18c. Is there a significant association between the computing device
#         respondents typically use to access the online library system and
#         how often they find what they seek?
xt <- xtabs(~ HowOftenFind + Device, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi- Squared test indicated there was no statistically
#  significant association between the computing device respondents
#  typically use to access the online library system and how often they find
#  what they seek (χ2(24, N=250) = 25.353, n.s.).



#    18d. Is there a significant association between how respondents prefer
#         to access the library’s resources and how often they find the
#         library item they seek?
xt <- xtabs(~ HowOftenFind + AccessPref, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi-Squared test indicated there was no statistically
#  significant association between how respondents prefer to access the
#  library’s resources and how often they found item they were looking for
#  (χ2(12, N=250) = 9.2245, n.s.).



#    18e. Is there a significant association between what type of resource
#         respondents most often seek and how often they find what they seek?
xt <- xtabs(~ HowOftenFind + ResourceSeek, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#   A two-sample Pearson Chi-Squared test indicated a statistically significant
#   association between what type of resource respondents most often seek
#   and how often they find what they seek (χ2(24, N=250) = 116.56, p < .0001).



#    18f. What is the statistical correlation between weekly hours spent in the
#         physical library and how often respondents find what they seek?
#         (Hint: Consider which type of correlation is appropriate here;
#         look at the parameters for cor.test(). Also, you will need to use
#         as.numeric() to wrap df$HowOftenFind. Ignore any warnings.)
print(
    cor.test(
        df$HoursPhysical,
        as.numeric(df$HowOftenFind),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was a statistically significant
# correlation between weekly hours spent in the physical library and how
# often respondents find what they seek (rho = 0.15667, p < .05)



#    18g. What is the statistical correlation between weekly hours spent
#         accessing the online library system and how often respondents
#         find what they seek?
print(
    cor.test(
        df$HoursOnline,
        as.numeric(df$HowOftenFind),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was a statistically significant
# correlation between weekly hours spent accessing the online library
# system and how often respondents find what they seek
# (rho = 0.1428439, p < .05)



#    18h. What is the statistical correlation between how often respondents
#         find what they seek in the library and GPA?
print(
    cor.test(
        df$GPA,
        as.numeric(df$HowOftenFind),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was a statistically significant
# correlation between how often respondents find what they seek in the
# library and GPA (rho = 0.1393051, p < .05)




####
# COLLECTION SATISFACTION
####
# Question 19. Concerning respondents’ satisfaction with the library’s physical
# collection:
#
#    19a. Plot the distribution of responses. How many respondents indicated
#         each response (1-7)? For ease of presentation, you can show a simple
#         table.
plot(df$SatisfiedPhysical)
print(table(df$SatisfiedPhysical))
# Satisfaction (Very Dissatisfied - Very Satisfied)
#               1  2  3  4  5  6  7
# Count         15 24 45 44 58 35 29



#    19b. Is this distribution of responses significantly different from
#         uniform? (Hint: Recall what you did for question 18b.)
xt <- xtabs(~SatisfiedPhysical, data = df)
print(xt)
print(chisq.test(xt))
#   A one-sample Pearson Chi-Squared test indicated that these proportions were
#   statistically significantly different from chance
#   (χ2(6, N=250) = 35.376, p < .0001).



#    19c. Is there a significant association between how respondents prefer to
#         access the university library and their satisfaction with the
#         library’s physical collection?
xt <- xtabs(~ AccessPref + SatisfiedPhysical, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi- Squared test indicated there was no statistically
#  significant association between the access preference and how satisfied a
#  respondent was to the library's physical collection
#  (χ2(12, N=250) = 10.243, n.s.).



#    19d. Is there a significant association between what type of resource
#         respondents most often seek and their satisfaction with the
#         library’s physical collection?
xt <- xtabs(~ ResourceSeek + SatisfiedPhysical, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi- Squared test indicated there was no statistically
#  significant association between what type of resource each respondent was
#  trying to find and their satisfaction with the library's physical collection
#  (χ2(24, N=250) = 13.058, n.s.).



#    19e. Is there a significant association between how often respondents find
#         what they seek in the library and their satisfaction with the
#         library’s physical collection?
xt <- xtabs(~ HowOftenFind + SatisfiedPhysical, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi- Squared test indicated there was no statistically
#  significant association between how often each respondent found what they
#  were looking for and their satisfaction with the library's physical
#  collection (χ2(36, N=250) = 45.297, n.s.).



#    19f. What is the statistical correlation between how often respondents
#         find what they seek in the library and their satisfaction with the
#         library’s physical collection?
print(
    cor.test(
        as.numeric(df$HowOftenFind),
        as.numeric(df$SatisfiedPhysical),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was a statistically significant
# correlation between how often each respondent found what they were
# looking for and how satisfied they were with the library's physical
# resources (rho = 0.2369941, p < .001)



#    19g. What is the statistical correlation between respondents’ weekly
#         hours spent in the physical library and their satisfaction with
#         the library’s physical collection?
print(
    cor.test(
        df$HoursPhysical,
        as.numeric(df$SatisfiedPhysical),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was a statistically significant
# correlation between how many hours each respondent spent in the physical
# library and how satisfied they were with the library's physical resources
# (rho = 0.3420017, p < .00001)



#    19h. What is the statistical correlation between respondents’ GPA and
#         their satisfaction with the library’s physical collection?
print(
    cor.test(
        df$GPA,
        as.numeric(df$SatisfiedPhysical),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was no statistically significant
# correlation between a respondents GPA and how satisfied they were with
# the library's physical resources (rho = 0.05917, n.s.).



#    19i. Is there a statistically significant difference in satisfaction
#         with the library’s physical collection by whether respondents ever
#         used the physical library? (Hint: SatisfiedPhysical is an ordinal
#         response so you will need to wrap it in as.numeric() and consider
#         carefully what type of test to use. Make a plot to help interpret
#         the results.)
m <- aov(as.numeric(SatisfiedPhysical) ~ UsedPhysical, data = df) # fit model
plot(df$SatisfiedPhysical, df$UsedPhysical)
print(anova(m))
# The difference in satisfaction between respondents who have used the physical
# library resources vs not, was statistically significant according to a
# one-way ANOVA (F(1, 248) = 6.4107, p < .05).



#    19j. Is there a statistically significant difference in satisfaction with
#         the library’s physical collection by major field of study? Make a
#         plot to help interpret the results.
m <- aov(as.numeric(SatisfiedPhysical) ~ MajorField, data = df) # fit model
plot(df$SatisfiedPhysical, df$MajorField)
print(anova(m))
# The difference in satisfaction with the physical library resources
# between all respondents major fields of study,
# were not statistically significant according to a one-way ANOVA
# (F(6, 243) = 2.0173, n.s.).




####
# Question 20. Concerning respondents’ satisfaction with the library’s online
# collection:
#
#    20a. Plot the distribution of responses. How many respondents indicated
#         each response (1-7)? For ease of presentation, you can show a simple
#         table.
plot(df$SatisfiedOnline)
print(table(df$SatisfiedOnline))
# Satisfaction (Very Dissatisfied - Very Satisfied)
#               1  2  3  4  5  6  7
# Count         4 24 63 75 51 18 15



#    20b. Is this distribution of responses significantly different from
#         uniform?
xt <- xtabs(~SatisfiedOnline, data = df)
print(xt)
print(chisq.test(xt))
#   A one-sample Pearson Chi-Squared test indicated that these proportions were
#   statistically significantly different from chance
#   (χ2(6, N=250) = 123.41, p < .0001).



#    20c. Is there a significant association between respondents’ typically
#         used computing devices and their satisfaction with the library’s
#         online collection?
xt <- xtabs(~ Device + SatisfiedOnline, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi- Squared test indicated there was no statistically
#  significant association between the respondents device used and how
#  satisfied a respondent was to the library's online collection
#  (χ2(24, N=250) = 22.214, n.s.).



#    20d. Is there a significant association between how respondents prefer
#         to access the university library and their satisfaction with the
#         library’s online collection?
xt <- xtabs(~ AccessPref + SatisfiedOnline, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi- Squared test indicated there was no statistically
#  significant association between respondent access preference and their
#  satisfaction with the library's online collection
#  (χ2(12, N=250) = 17.134, n.s.).



#    20e. Is there a significant association between what type of resource
#         respondents most often seek and their satisfaction with the
#         library’s online collection?
xt <- xtabs(~ ResourceSeek + SatisfiedOnline, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi- Squared test indicated there was no statistically
#  significant association between the resource respondents most often tried
#  to seek and their satisfaction with the library's online collection
#  (χ2(24, N=250) = 31.901, n.s.).



#    20f. Is there a significant association between how often respondents
#         find what they seek in the library and their satisfaction with the
#         library’s online collection?
xt <- xtabs(~ HowOftenFind + SatisfiedOnline, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi- Squared test indicated there was a statistically
#  significant association between how often respondents were able to find the
#  resource they were seeking and their satisfaction with the library's online
#  collection (χ2(36, N=250) = 57.539, p<0.05).



#    20g. What is the statistical correlation between how often respondents
#         find what they seek in the library and their satisfaction with the
#         library’s online collection?
print(
    cor.test(
        as.numeric(df$HowOftenFind),
        as.numeric(df$SatisfiedOnline),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was a statistically significant
# correlation between how often each respondent found what they were
# looking for and how satisfied they were with the library's online
# resources (rho = 0.3499714, p < .00001).



#    20h. What is the statistical correlation between respondents’ weekly
#         hours spent accessing the online library system and their
#         satisfaction with the library’s online collection?
print(
    cor.test(
        df$HoursOnline,
        as.numeric(df$SatisfiedOnline),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was a statistically significant
# correlation between how many hours respondents spent online and how
# satisfied they were with the library's online resources
# (rho = 0.3790529, p < .00001).



#    20i. Is there a significant association between respondents’ satisfaction
#         with the library’s physical and online collections?
xt <- xtabs(~ SatisfiedPhysical + SatisfiedOnline, data = df)
print(xt)
plot(xt)
print(chisq.test(xt))
#  A two-sample Pearson Chi- Squared test indicated there was no statistically
#  significant association between respondents satisfaction with the library's
#  physical and online collections (χ2(36, N=250) = 39.223, n.s.).



#    20j. What is the statistical correlation between respondents’
#         satisfaction with the library’s physical and online collections?
print(
    cor.test(
        as.numeric(df$SatisfiedPhysical),
        as.numeric(df$SatisfiedOnline),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was a statistically significant
# correlation between respondents satisfaction with the library's
# physical and online collections (rho = 0.1387893, p < .05).



#    20k. What is the statistical correlation between respondents’ GPA and their
#         satisfaction with the library’s online collection?
print(
    cor.test(
        df$GPA,
        as.numeric(df$SatisfiedOnline),
        method = "spearman",
        exact = FALSE
    )
)
# A spearmen rho test shows that there was no statistically significant
# correlation between respondents GPA and their satisfaction with the
# library's online collections (rho = 0.05356785, n.s.).



#    20l. Is there a statistically significant difference in satisfaction
#         with the library’s online collection by whether respondents ever
#         used the online library system? Make a plot to help interpret the
#         results. (Hint: Recall what you did for question 19i.)
m <- aov(as.numeric(SatisfiedOnline) ~ UsedOnline, data = df) # fit model
plot(df$SatisfiedOnline, df$UsedOnline)
print(anova(m))
# The difference in satisfaction between respondents who have used the online
# library resources vs not, was not statistically significant according to a
# one-way ANOVA (F(1, 248) = 2.1176, n.s.).



#    20m. Is there a statistically significant difference in satisfaction
#         with the library’s online collection by major field of study?
#         Make a plot to help interpret the results. (Hint: Recall what you
#         did for question 19j.)
m <- aov(as.numeric(SatisfiedOnline) ~ MajorField, data = df) # fit model
plot(df$SatisfiedOnline, df$MajorField)
print(anova(m))
# The difference in satisfaction with the online library resources
# between respondents major field, was not statistically significant
# according to a one-way ANOVA (F(6, 243) = 1.0581, n.s.).



####
# MODEL BUILDING
####
# Question 21. The library staff has a hypothesis that the time spent
# utilizing the physical and online university libraries, and a lack of time
# spent on social media, predicts an increase in undergraduate GPA. Build a
# linear regression model to test this hypothesis. Write out the fitted model,
# rounding model coefficients to the nearest thousandths (3 digits). What do
# you find? Is this hypothesis supported, not supported, or both?
m <- lm(GPA ~ HoursPhysical + HoursOnline + HoursSocial, data = df)
print(m)
print(anova(m))
# Model:
#   GPA = 2.930 + 0.032*HoursPhysical + 0.012*HoursOnline - 0.023*HoursSocial
#
# An analysis of variance indicated a statistically significant effect on
# GPA for HoursPhysical (F(1, 246) = 6.9530, p < .01) and HoursSocial
# (F(1, 246) = 8.6472, p < .005), but not for HoursOnline
# (F(1, 246) = 1.7947, n.s.).




####
# Question 22. Build a full linear regression model and then use stepwise
# regression to build two reduced models, one that minimizes Akaike's
# Information Criterion (AIC) and one that minimizes Schwarz's Bayesian
# Information Criterion (BIC). (Hint: When building the full model, be sure
# to exclude the participant ID: the full model formula is therefore
# (GPA ~ . -PId). For building and assessing the reduced models, refer to
# Kassambara (2018a, 2018b). Use MASS::stepAIC() to build these models with
# direction="both". For AIC, use k=2, the default.
# For BIC, use k=log(nrow(df))).
m <- lm(GPA ~ . - PId, data = df)
ma <- stepAIC(m, direction = "both", trace = FALSE, k = 2) # AIC
mb <- stepAIC(m, direction = "both", trace = FALSE, k = log(nrow(df))) # BIC



#    22a. Including the model intercept, how many terms are in each model?
#         What are the R^2 and Adjusted R^2 model fits for each model?
print(summary(m))
print(summary(ma))
print(summary(mb))
# There are 45 terms in the base model the R-squared statistic is 0.2687 and
# the adjusted R-squared statistic is 0.1073.
# There are 15 terms in the AIC model, the R-squared statistic is 0.175 and the
# adjusted R-squared statistic is 0.1221.
# There are only 2 terms in the BIC model, the R-squared statistic is
# 0.05826 and the adjusted R-squared statistic is 0.05064.



#    22b. What is the AIC of each model? Explain your results.
#         (Hint: Use AIC() on each model. Lower is better.)
print(AIC(m))
print(AIC(ma))
print(AIC(mb))
# AIC is meant to help determine how well fit a model is to your data.
# In this case, the base model has an AIC of 643.79, the stepwise AIC
# model has an AIC of 613.92, and the stepwise BIC model has an AIC
# of 621.00. Under AIC, our "best fit" model is the stepwise AIC
# model.



#    22c. What is the BIC of each model? Explain your results.
#         (Hint: Use BIC() on each model. Lower is better.)
print(BIC(m))
print(BIC(ma))
print(BIC(mb))
# BIC is meant to help determine how well fit a model is to your data.
# In this case, the base model has a BIC of 809.304, the stepwise AIC
# model has a BIC of 673.79, and the stepwise BIC model has an BIC
# of 635.09. Under BIC, our "best fit" model is the stepwise BIC
# model.



#    22d. Besides R^2, Adj. R^2, AIC, and BIC, one way to assess how well a
# model fits the data from which it was built is to compare model predictions,
# in this case for GPA, to actual GPA.[2] Calculate and report the mean
# absolute error (MAE) for each model. Which model is the least "far off" from
# actual GPA? Why? (Hint: The MAE is the average absolute difference between
# observations and model predictions. So, for a model m, your code would be
# mean(abs(df$GPA - predict(m,df))). Be sure you understand what this line of
# code is doing.)
#
#         [2] A better approach to evaluating such models is to predict GPA
#             from new data that was not used to build the models in the first
#             place. If we lack such data, we can rebuild our current models on
#             only a subset of our 250 respondents, say on 225 of them, and then
#             compute MAE for the other 25. In fact, we could do this many times
#             over, using a new sample of 225 rows for model building (90%) and
#             leaving out 25 rows for model testing (10%). Repeatedly evaluating
#             our model in this way is called k-fold cross-validation and is a
#             common practice for evaluating machine learning models. For more,
#             see Kassambara (2018c).
print(mean(abs(df$GPA - predict(m, data = df))))
print(mean(abs(df$GPA - predict(ma, data = df))))
print(mean(abs(df$GPA - predict(mb, data = df))))
# The base model MAE is 0.601881, the stepwise AIC model MAE is 0.6384049, and
# the stepwise BIC model MAE is 0.6869124. Our best performing model in this
# case is the base model (with all the 45 parameters), but considering that
# we get nearly as good of performance from just 2 parameters (with the
# stepwise BIC model), it seems like those extra 43 parameters are only
# contributing ~0.08 MAE improvement (reduction).


#    22e. Stepping back and looking at each model overall, which would you
#         settle on as your preferred model for predicting GPA from answers
#         to a structured survey? Why? (Note: This question does not require
#         any R code, only R comments.)
# We could recommend selecting the stepwise BIC model (or simply only
# providing the survey results for GPA and HoursSocial) as this will be the
# shortest and cheapest survey to send out with decently low MAE.




####
# ZOOMING OUT
####
# Question 23. The library wants to know whether it should continue to
# solicit ACME University for funding to maintain (or even expand) its
# physical resources and collection. Looking at your findings taken
# together, what would you advise the library’s leadership? Do your
# findings support maintaining (or even expanding) the physical collection
# of the library? Compose a brief argument for or against grounded in your
# statistically significant findings. Remember, non-significant statistical
# tests cannot be taken as proof of "equality" or "no difference."
# They can only be taken as tests from which there resulted no
# "detectable difference."
#
#
# We would recommend continued solicitation for funding for the library's
# physical collection for the following reasons:
#
# We found weak positive correlation between respondents number of
# hours spent using the library's online resources and their GPA
# (r=0.1624735, p < .05).




####
# Question 24. Repeat question 23 for ACME University’s online library system
# and collection.
#
#
# We would recommend continued solicitation for funding for the library's
# online collection for the following reasons:
#
# More respondents preferred accessing library resources online.



####
# Question 25. Now that you have analyzed the data from this structured survey,
# step back and look at the survey itself. What is one thing you would add,
# remove, or change to improve the survey? Why? How would your change improve
# the survey? Be specific.

# We would add more questions to clarify the activities people are doing in
# all capacities at the library. For example,
# "what are you typically doing in the physical library?" options
# (studying, reading, homework, meeting, socializing, etc.)
# And more specifically adding questions about their social media usage:
# "what are you typically doing on social media?" options
# (communicating and sharing work, homework, chatting, etc.)
# This way we can make a more informed decision about the effect of social
# media usage in a respondents GPA, and what type of activities the library
# may want to better support in the future.