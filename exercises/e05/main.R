## Exercise 05
## Jackson Maxfield Brown

## Installs
# install.packages("plyr")

# Read and convert to factors
timeOnSite <- read.csv(
    "/home/maxfield/active/personal/insc-571/exercises/materials/timeonsite.csv"
)
timeOnSite$Subject <- factor(timeOnSite$Subject)
timeOnSite$Site <- factor(timeOnSite$Site)
print("Injested data head")
print(head(timeOnSite))
print("--------------------------------------------------------")

# Print summary stats
print("Summary stats for site experiment")
print(summary(timeOnSite))
print("--------------------------------------------------------")

# Show summary stats from subsets
library(plyr)
print(ddply(timeOnSite, ~Site, function(data) summary(data$Time)))
print(
    ddply(
        timeOnSite,
        ~Site,
        summarise,
        Time.mean = mean(Time),
        Time.sd = sd(Time)
    )
)
print("--------------------------------------------------------")

# Run a independent samples t test on Time by Site
print(t.test(Time ~ Site, data = timeOnSite, var.equal = TRUE))
print("--------------------------------------------------------")