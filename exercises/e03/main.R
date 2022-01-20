## Exercise 03
## Jackson Maxfield Brown

## Installs
# install.packages("XNomial")
# install.packages("RVAideMemoire")

# Read and convert to factors
devicePrefs <- read.csv(
    "/home/maxfield/active/personal/insc-571/exercises/materials/deviceprefs.csv"
)
devicePrefs$Subject <- factor(devicePrefs$Subject)
devicePrefs$Pref <- factor(devicePrefs$Pref)
devicePrefs$Disability <- factor(devicePrefs$Disability)
print("Injested data head")
print(head(devicePrefs))
print("--------------------------------------------------------")

# Print summary stats
print("Summary stats for device preferences")
print(summary(devicePrefs))
print("--------------------------------------------------------")


# Run Pearson chi-square test
print("xtabs and chi-square for device preferences")
prfs <- xtabs(~Pref, data = devicePrefs)
print(prfs)
print(chisq.test(prfs))
print("--------------------------------------------------------")


# Binomial tests by user group
print("binom tests for users without and users with disability and their device preferences")
usersWithoutDisability <- binom.test(
    sum(devicePrefs[devicePrefs$Disability == 0, ]$Pref == "touchpad"),
    nrow(devicePrefs[devicePrefs$Disability == 0, ]),
    p = 1 / 2
)
usersWithDisability <- binom.test(
    sum(devicePrefs[devicePrefs$Disability == 1, ]$Pref == "touchpad"),
    nrow(devicePrefs[devicePrefs$Disability == 1, ]),
    p = 1 / 2
)
print("uncorrected")
print(c(usersWithoutDisability$p.value, usersWithDisability$p.value))
print("corrected")
print(p.adjust(
    c(usersWithoutDisability$p.value, usersWithDisability$p.value),
    method = "holm"
))
print("--------------------------------------------------------")


# Run two-sample Pearson chi-square test
print("Two sample xtabs and chi-square for device preferences")
twoSamplePrfs <- xtabs(~ Pref + Disability, data = devicePrefs)
print(twoSamplePrfs)
print(chisq.test(twoSamplePrfs))
print("--------------------------------------------------------")

# Run two-sample G-test
# library(RVAideMemoire)

print("Two sample g-test for device preferences")
# print(G.test(twoSamplePrfs))
print("--------------------------------------------------------")

# Run Fisher's test on preferences
print("Fisher's exact")
print(fisher.test(twoSamplePrfs))
print("--------------------------------------------------------")