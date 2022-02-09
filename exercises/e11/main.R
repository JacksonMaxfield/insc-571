## Exercise 11
## Jackson Maxfield Brown

# Installs
# install.packages("plyr")
# install.packages("reshape2")
# install.packages("coin")
# install.packages("ez")

# Load data
websearch <- read.csv(
    "/home/maxfield/active/personal/insc-571/exercises/materials/websearch2.csv"
)
websearch$Subject <- factor(websearch$Subject)
websearch$Engine <- factor(websearch$Engine)
websearch$Order <- factor(websearch$Order)
print("Injested data head")
print(head(websearch))
print("--------------------------------------------------------")


# Question 1:
# Download the file websearch2.csv from the course materials.
# This file describes a study in which participants were asked
# to find 100 distinct facts on the web using different search engines.
# The number of searches required and a subjective effort rating for
# each search engine were recorded. How many participants took part in this
# experiment?
print(length(unique(websearch$Subject)))
print("--------------------------------------------------------")

# Question 2:
# To the nearest hundredth (two digits), what was the average number
# of searches required for the search engine that had the
# greatest average overall?
library(plyr)
print("Summary for each Search Engine")
print(ddply(websearch, ~Engine, function(data) summary(data$Searches)))
print("--------------------------------------------------------")

# Question 3:
# Conduct an order effect test on Searches using a paired-samples
# t-test assuming equal variances. To the nearest ten-thousandths (four digits),
# what is the p-value from such a test? Hint: Use the reshape2 library
# and the dcast function to create a wide-format table with columns for
# each level of Order.
library(reshape2)
websearchWideForOrder <- dcast(websearch, Subject ~ Order, value.var = "Searches")
print(
    t.test(
        websearchWideForOrder$"1",
        websearchWideForOrder$"2",
        paired = TRUE,
        var.equal = TRUE
    )
)
print("--------------------------------------------------------")

# Question 4:
# Conduct a paired-samples t-test, assuming equal variances,
# on Searches by Engine. To the nearest hundredth (two digits),
# what is the absolute value of the t statistic for such a test?
# Hint: Use the reshape2 library and the dcast function to create
# a wide-format table with columns for each level of Engine.
websearchWideForEngine <- dcast(websearch, Subject ~ Engine, value.var = "Searches")
print(
    t.test(
        websearchWideForEngine$Google,
        websearchWideForEngine$Bing,
        paired = TRUE,
        var.equal = TRUE
    )
)
print("--------------------------------------------------------")

# Question 5
# Conduct a nonparametric Wilcoxon signed-rank test on the
# Effort Likert-type ratings. Calculate an exact p-value.
# To the nearest ten-thousandth (four digits), what is the p-value
# from such a test? Hint: Use the coin library and its
# wilcoxsign_test function with distribution=“exact”.
library(coin)
print(
    wilcoxsign_test(
        Effort ~ Engine | Subject,
        data = websearch,
        distribution = "exact"
    )
)
print("--------------------------------------------------------")

# Question 6
# Download the file websearch3.csv from the course materials.
# This file describes a study just like the one from websearch2.csv,
# except that now three search engines were used instead of two.
# Once again, the number of searches required and a subjective effort
# rating for each search engine were recorded. How many subjects took
# part in this new experiment?
websearch <- read.csv(
    "/home/maxfield/active/personal/insc-571/exercises/materials/websearch3.csv"
)
websearch$Subject <- factor(websearch$Subject)
websearch$Engine <- factor(websearch$Engine)
websearch$Order <- factor(websearch$Order)
print("Injested data head")
print(head(websearch))
print(length(unique(websearch$Subject)))
print("--------------------------------------------------------")

# Question 7
# To the nearest hundredth (two digits), what was the average
# number of searches required for the search engine that had the
# greatest average overall?
print("Summary for each Search Engine")
print(ddply(websearch, ~Engine, function(data) summary(data$Searches)))
print("--------------------------------------------------------")

# Question 8
# Conduct a repeated measures ANOVA to determine if there was
# an order effect on Searches. First determine whether there
# is a violation of sphericity. To the nearest ten-thousandth
# (four digits), what is the value of Mauchly’s W criterion?
# Hint: Use the ez library and its ezANOVA function passing
# within=Order, among other things, to test for order effects.
library(ez)
m <- ezANOVA(
    data = websearch,
    dv = Searches,
    within = Order,
    wid = Subject
)
print(m$Mauchly)
print("--------------------------------------------------------")

# Question 9
# Interpret the result of Mauchly’s test of sphericity,
# and then interpret the appropriate repeated measures ANOVA result.
# To the nearest ten-thousandth (four digits),
# what is the p-value from the appropriate F-test?
print(m$ANOVA)
print("--------------------------------------------------------")

# Question 10
# Conduct a repeated measures ANOVA on Searches by Engine.
# First determine whether there is a violation of sphericity.
# To the nearest ten-thousandth (four digits), what is the value
# of Mauchly’s W criterion? Hint: Use the ez library and its
# ezANOVA function passing within=Engine, among other things,
# to test for a significant main effect.
m <- ezANOVA(
    data = websearch,
    dv = Searches,
    within = Engine,
    wid = Subject
)
print(m$Mauchly)
print("--------------------------------------------------------")

# Question 11
# Interpret the result of Mauchly’s test of sphericity, and then
# interpret the appropriate repeated measures ANOVA result.
# To the nearest ten-thousandth (four digits), what is the
# p-value from the appropriate F-test?
print(m$ANOVA)
print("--------------------------------------------------------")

# Question 12
# Strictly speaking, given the result of the repeated measures
# ANOVA examining Searches by Engine, are post hoc pairwise
# comparisons among levels of Engine warranted? (Mark one)

# Answer: No

# Question 13
# Whatever your previous answer, proceed to do post hoc
# pairwise comparisons. Conduct manual pairwise comparisons
# of Searches among levels of Engine using paired-samples
# t-tests, assuming equal variances and using Holm’s sequential
# Bonferroni procedure to correct for multiple comparisons.
# To the nearest ten-thousandth (four digits), what is the
# smallest corrected p-value resulting from this set of tests?
# Hint: Use the reshape2 library and dcast function to
# create a wide-format table.
websearchWideForEngine <- dcast(
    Subject ~ Engine,
    data = websearch,
    value.var = "Searches"
)
googleBing <- t.test(
    websearchWideForEngine$Google,
    websearchWideForEngine$Bing,
    paired = TRUE,
    var.equal = TRUE
)
googleYahoo <- t.test(
    websearchWideForEngine$Google,
    websearchWideForEngine$Yahoo,
    paired = TRUE,
    var.equal = TRUE
)
bingYahoo <- t.test(
    websearchWideForEngine$Bing,
    websearchWideForEngine$Yahoo,
    paired = TRUE,
    var.equal = TRUE
)
print(
    p.adjust(
        c(
            googleBing$p.value,
            googleYahoo$p.value,
            bingYahoo$p.value
        ),
        method = "holm"
    )
)
print("--------------------------------------------------------")

# Question 14
# Conduct a nonparametric Friedman test on the Effort Likert-type
# ratings. Calculate an asymptotic p-value. To the nearest
# ten-thousandth (four digits), what is the Chi-Square statistic
# from such a test? Hint: Use the coin library and the
# friedman_test function.
print(
    friedman_test(
        Effort ~ Engine | Subject,
        data = websearch,
        distribution = "asymptotic"
    )
)
print("--------------------------------------------------------")

# Question 15
# Strictly speaking, given the result of the Friedman test
# examining Effort by Engine, are post hoc pairwise comparisons
# among levels of Engine warranted? (Mark one)

# Yes (p-value is significant)

# Question 16
# Whatever your previous answer, proceed to do post hoc pairwise
# comparisons. Conduct manual pairwise comparisons of
# Effort among levels of Engine with Wilcoxon signed-rank tests,
# using Holm’s sequential Bonferroni procedure to correct for
# multiple comparisons. To the nearest ten-thousandth (four digits),
# what is the smallest corrected p-value resulting from
# this set of tests? Hint: Use the reshape2 library and dcast
# function to create a wide-format table. Then use the wilcox.test
# function with paired=TRUE (and to avoid warnings, exact=FALSE).
websearchWideForEngineWithEffort <- dcast(
    Subject ~ Engine,
    data = websearch,
    value.var = "Effort"
)
googleBing <- wilcox.test(
    websearchWideForEngineWithEffort$Google,
    websearchWideForEngineWithEffort$Bing,
    paired = TRUE,
    exact = FALSE
)
googleYahoo <- wilcox.test(
    websearchWideForEngineWithEffort$Google,
    websearchWideForEngineWithEffort$Yahoo,
    paired = TRUE,
    exact = FALSE
)
bingYahoo <- wilcox.test(
    websearchWideForEngineWithEffort$Bing,
    websearchWideForEngineWithEffort$Yahoo,
    paired = TRUE,
    exact = FALSE
)
print(
    p.adjust(
        c(
            googleBing$p.value,
            googleYahoo$p.value,
            bingYahoo$p.value
        ),
        method = "holm"
    )
)
print("--------------------------------------------------------")