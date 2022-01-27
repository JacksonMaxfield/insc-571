## Exercise 07
## Jackson Maxfield Brown

## Installs
# install.packages("car")
# install.packages("MASS")
# install.packages("plyr")
# install.packages("coin")

# Read and convert to factors
designTime <- read.csv(
    "/home/maxfield/active/personal/insc-571/exercises/materials/designtime.csv"
)
designTime$Subject <- factor(designTime$Subject)
designTime$Tool <- factor(designTime$Tool)
print("Injested data head")
print(head(designTime))
print("--------------------------------------------------------")

# Print summary stats
print("Summary stats for site experiment")
print(summary(designTime))
print("--------------------------------------------------------")

# Gen boxplot for the two groups
plot(Time ~ Tool, data = designTime)

# Run basic shapiro test
illustratorGroup <- designTime[designTime$Tool == "Illustrator", ]
inDesignGroup <- designTime[designTime$Tool == "InDesign", ]
print("Illustrator Shapiro Test")
print(shapiro.test(illustratorGroup$Time))
print("InDesign Shapiro Test")
print(shapiro.test(inDesignGroup$Time))
print("--------------------------------------------------------")

# Run shapiro against residuals
model <- aov(Time ~ Tool, data = designTime)
print("Time by Tool Residuals Shapiro")
print(shapiro.test(residuals(model)))
print("--------------------------------------------------------")

# Check homogeneity of variance
library(car)
print("Homogeneity of variance (Brown-Forsythe Test)")
print(leveneTest(Time ~ Tool, data = designTime, center = median))
print("--------------------------------------------------------")

# Fit log-normal distribution and run K-S test
library(MASS)
fitIllustrator <- fitdistr(illustratorGroup$Time, "lognormal")$estimate
print("KS test for Illustrator subset")
print(ks.test(
    illustratorGroup$Time,
    "plnorm",
    meanlog = fitIllustrator[1],
    sdlog = fitIllustrator[2],
    exact = TRUE
))
fitInDesign <- fitdistr(inDesignGroup$Time, "lognormal")$estimate
print("KS test for InDesign subset")
print(ks.test(
    inDesignGroup$Time,
    "plnorm",
    meanlog = fitInDesign[1],
    sdlog = fitInDesign[2],
    exact = TRUE
))
print("--------------------------------------------------------")

# Add log time as column to data
designTime$LogTime <- log(designTime$Time)

# Get mean of LogTime for subsets
library(plyr)
print("Summary, Mean, and SD of LogTime by Tool")
print(ddply(designTime, ~Tool, function(data) summary(data$LogTime)))
print(
    ddply(
        designTime,
        ~Tool,
        summarise,
        Time.mean = mean(LogTime),
        Time.sd = sd(LogTime)
    )
)
print("--------------------------------------------------------")

# Run welch t-test
print("Welch t-test for LogTime by Tool")
print(t.test(LogTime ~ Tool, data = designTime, var.equal = FALSE))
print("--------------------------------------------------------")

# Run Mann-Whitney U Test
library(coin)
print("Mann-Whitney U for Time by Tool")
print(wilcox_test(Time ~ Tool, data = designTime, distribution = "exact"))
print("--------------------------------------------------------")