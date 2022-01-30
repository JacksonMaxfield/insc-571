## Exercise 09
## Jackson Maxfield Brown

## Installs
# install.packages("pylr")
# install.packages("car")
# install.packages("multcomp")
# install.packages("coin")

# Read and convert to factors
alphaWPM <- read.csv(
    "/home/maxfield/active/personal/insc-571/exercises/materials/alphabets.csv"
)
alphaWPM$Subject <- factor(alphaWPM$Subject)
alphaWPM$Alphabet <- factor(alphaWPM$Alphabet)
print("Injested data head")
print(head(alphaWPM))
print("--------------------------------------------------------")

# Question 1
# This file describes a study in which people used a pen-based
# stroke alphabet to enter a set of text phrases. How many
# different stroke alphabets are being compared?
# Print summary stats
print("Summary stats for site alphabets")
print(summary(alphaWPM))
print("--------------------------------------------------------")

# Question 2
# To the nearest hundredth (two digits), what was the average
# text entry speed in words per minute (WPM) of the EdgeWrite alphabet?
# Get summary for each alphabet
library(plyr)
print("Summary for each alphabet")
print(ddply(alphaWPM, ~Alphabet, function(data) summary(data$WPM)))
print("--------------------------------------------------------")

# Question 3
# Conduct Shapiro-Wilk normality tests on the WPM response
# for each Alphabet. Which of the following, if any,
# violate the normality test? (violates normality if p>0.05)
# Run shaprio for WPM by Alphabet
ewGroup <- alphaWPM[alphaWPM$Alphabet == "EdgeWrite", ]
gGroup <- alphaWPM[alphaWPM$Alphabet == "Graffiti", ]
uGroup <- alphaWPM[alphaWPM$Alphabet == "Unistrokes", ]
print("EdgeWrite Shapiro Test")
print(shapiro.test(ewGroup$WPM))
print("Graffiti Shapiro Test")
print(shapiro.test(gGroup$WPM))
print("Unistrokes Shapiro Test")
print(shapiro.test(uGroup$WPM))
print("--------------------------------------------------------")

# Question 4
# Conduct a Shapiro-Wilk normality test on the residuals of a
# WPM by Alphabet model. To the nearest ten-thousandth (four digits),
# what is the p-value from such a test?
# Run shapiro against residuals
model <- aov(WPM ~ Alphabet, data = alphaWPM)
print("WPM by Alphabet Residuals Shapiro")
print(shapiro.test(residuals(model)))
print("--------------------------------------------------------")

# Question 5
# Conduct a Brown-Forsythe homoscedasticity test on WPM by Alphabet.
# To the nearest ten-thousandth (four digits),
# what is the p-value from such a test? (BF test is a levene but center=median)
# Check homogeneity of variance
library(car)
print("Homogeneity of variance (Brown-Forsythe Test)")
print(leveneTest(WPM ~ Alphabet, data = alphaWPM, center = median))
print("--------------------------------------------------------")

# Question 6
# Conduct a one-way ANOVA on WPM by Alphabet. To the nearest
# hundredth (two digits), what is the F statistic from such a test?
# Reusing the generated aov model for WPM by Alphabet from above
print("One-way ANOVA for WPM ~ Alphabet")
print(anova(model))
print("--------------------------------------------------------")

# Question 7
# Perform simultaneous pairwise comparisons among levels of Alphabet
# using the Tukey approach. Adjust for multiple comparisons using
# Holm’s sequential Bonferroni procedure. To the nearest
# ten-thousandth (four digits), what is the corrected p-value
# for the comparison of Unistrokes to Graffiti?

# Question 8
# According to the results of the simultaneous pairwise comparisons,
# which of the following levels of Alphabet are
# significantly different in terms of WPM?
# Reusing the generated aov model from WPM by Alphabet from above
library(multcomp)
print("Pairwise comparison of alphabets")
print(
    summary(
        glht(model, mcp(Alphabet = "Tukey")),
        test = adjusted(type = "holm")
    )
)
print("--------------------------------------------------------")

# Question 9
# Conduct a Kruskal-Wallis test on WPM by Alphabet.
# To the nearest ten-thousandth (four digits),
# what is the p-value from such a test?
library(coin)
print("Kruskal-Wallis test for WPM by Alphabet")
print(
    kruskal_test(
        WPM ~ Alphabet,
        data = alphaWPM,
        distribution = "asymptotic"
    )
)
print("--------------------------------------------------------")

# Question 10
# Conduct nonparametric post hoc pairwise comparisons of WPM
# among all levels of Alphabet manually using three separate
# Mann-Whitney U tests. Adjust the p-values using Holm’s sequential
# Bonferroni procedure. To the nearest ten-thousandth (four digits),
# what is the corrected p-value for Unistrokes vs. Graffiti?
# Reusing level subsets from above
print("Individual post hoc Mann-Whitney U tests for each alphabet")
graffiti_vs_edgewrite <- wilcox.test(
    gGroup$WPM,
    ewGroup$WPM,
    paired = FALSE,
    exact = FALSE
)
unistrokes_vs_edgewrite <- wilcox.test(
    uGroup$WPM,
    ewGroup$WPM,
    paired = FALSE,
    exact = FALSE
)
unistrokes_vs_graffiti <- wilcox.test(
    uGroup$WPM,
    gGroup$WPM,
    paired = FALSE,
    exact = FALSE
)
# Adjust all
print(p.adjust(
    c(
        graffiti_vs_edgewrite$p.value,
        unistrokes_vs_edgewrite$p.value,
        unistrokes_vs_graffiti$p.value
    ),
    method = "holm"
))
print("--------------------------------------------------------")