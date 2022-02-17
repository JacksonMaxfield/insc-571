## Exercise 11
## Jackson Maxfield Brown

# Installs
# install.packages("plyr")
# install.packages("ez")
# install.packages("ARTool")

# Load data
avatars <- read.csv(
    "/home/maxfield/active/personal/insc-571/exercises/materials/avatars.csv"
)
avatars$Subject <- factor(avatars$Subject)
avatars$Height <- factor(avatars$Height)
avatars$Avatar <- factor(avatars$Avatar)
print("Injested data head")
print(head(avatars))
print("--------------------------------------------------------")

# Question 1
# Download the file avatars.csv from the course materials.
# This file describes a study in which tall and short participants were
# shown a single virtual human avatar that was itself rendered as either
# tall or short. Participants were asked to craft a persona and write a
# day-in-the-life scenario for that avatar. The number of positive sentiments
# expressed were counted by a panel of judges who knew neither the height of
# the participant nor of the avatar. Examine the data. What kind of
# experiment design was this? (Mark one)
print(length(unique(avatars$Height)))
print(length(unique(avatars$Avatar)))
print(nrow(avatars))
print(length(unique(avatars$Subject)))
print("--------------------------------------------------------")
# nrow and len unique are the same, indicates between subjects
# Answer: A 2×2 between-subjects design with factors for Height (tall, short) and
# Avatar (tall, short).

# Question 2
# How many subjects took part in this experiment?
print(length(unique(avatars$Subject)))
print("--------------------------------------------------------")
# Answer: 60

# Question 3
# To the nearest hundredth (two digits), on average how many
# positive sentiments were expressed for the most positive
# combination of Height and Avatar?
library(plyr)
print(ddply(avatars, ~ Height + Avatar, function(data) summary(data$Positives)))
print("--------------------------------------------------------")
# Answer: 100.73

# Question 4
# Create an interaction plot with Height on the X-axis and
# Avatar as the traces. Do the lines cross?
with(avatars, interaction.plot(Height, Avatar, Positives))
# Answer: The lines cross (Yes)

# Question 5
# Create an interaction plot with Avatar on the X-axis and
# Height as the traces. Do the lines cross?
with(avatars, interaction.plot(Avatar, Height, Positives))
# Answer: No

# Question 6
# Conduct a factorial ANOVA on Positives by Height and
# Avatar. To the nearest hundredth (two digits), what is
# the largest F statistic from such a test?
# Hint: Use the ez library and its ezANOVA function.
# Pass both Height and Avatar as the between parameter
# using a vector created with the “c” function. If you
# aren’t sure how to use the “c” function, look it up,
# as with any function, by using the question-mark
# operator, like so: ?c
library(ez)
print(ezANOVA(
    data = avatars,
    dv = Positives,
    between = c(Height, Avatar),
    wid = Subject
))
print("--------------------------------------------------------")
# Answer: 17.04

# Question 7
# Which effects are statistically significant in the
# factorial ANOVA of Positives by Height and Avatar?
# Answer: main effect of height and Height:Avatar interaction

# Question 8
# Conduct two planned pairwise comparisons using
# independent-samples t-tests. The first question is
# whether short participants produced different numbers
# of positive sentiments for tall avatars versus short
# avatars. The second question is whether tall participants
# produced different numbers of positive sentiments for tall
# avatars versus short avatars. Assuming equal variances,
# and using Holm’s sequential Bonferroni procedure to correct
# for multiple comparisons, what to within a ten-thousandth
# (four digits) is the lowest corrected p-value from these
# tests?
#
# Hint: You will need conjunctions with ampersands
# (&) to select the necessary rows for your t.test functions.
# By now, you have seen certain responses (Y) obtained by
# selecting rows with a single factor (X) matching a given
# level value:
#
# df[df$X == "value",]$Y
#
# Now, you can see how to request responses (Y) from rows
# having two factors (X1, X2) that match certain level
# values. And with more ampersands (&), you can easily
# expand to three factors, four factors, etc.:
#
# df[df$X1 == "value1" & df$X2 == "value2",]$Y
shortPTallA <- avatars[avatars$Height == "short" & avatars$Avatar == "tall", ]
shortPShortA <- avatars[avatars$Height == "short" & avatars$Avatar == "short", ]
tallPTallA <- avatars[avatars$Height == "tall" & avatars$Avatar == "tall", ]
tallPShortA <- avatars[avatars$Height == "tall" & avatars$Avatar == "short", ]
short_p <- t.test(
    shortPTallA$Positives,
    shortPShortA$Positives,
    paired = FALSE,
    var.equal = TRUE
)
tall_p <- t.test(
    tallPTallA$Positives,
    tallPShortA$Positives,
    paired = FALSE,
    var.equal = TRUE
)
print(p.adjust(c(short_p$p.value, tall_p$p.value), method = "holm"))
print("--------------------------------------------------------")
# Answer: 0.0193

# Question 9
# Which of the following conclusions are supported by the
# planned pairwise comparisons just conducted?
# Answer:
# shorter participants made more positive statements about tall avatars
# than they did about short avatars

# Question 10
# Download the file notes.csv from the course materials.
# This file describes a study in which iPhone and Android
# smartphone owners used their phone’s built-in note-taking
# app and then switched to an add-on third-party app, or
# vice-versa. The number of words they wrote in their notes
# apps over the course of a week was recorded. Examine the data
# and indicate what kind of experiment design this was. (Mark one)
# Load data
notes <- read.csv(
    "/home/maxfield/active/personal/insc-571/exercises/materials/notes.csv"
)
notes$Subject <- factor(notes$Subject)
notes$Phone <- factor(notes$Phone)
notes$Notes <- factor(notes$Notes)
notes$Order <- factor(notes$Order)
print("Injested data head")
print(head(notes))
print("--------------------------------------------------------")
# Answer: 2 x 2 mixed factor. phone is between and notes is within

# Question 11
# How many subjects took part in this experiment?
print(length(unique(notes$Subject)))
print("--------------------------------------------------------")
# Answer: 20

# Question 12
# To the nearest whole number, on average how many words were
# recorded with the most heavily used combination of Phone and Notes?
print(ddply(notes, ~ Phone + Notes, function(data) summary(data$Words)))
print("--------------------------------------------------------")
# Answer: 534

# Question 13
# Create an interaction plot with Phone on the X-axis and
# Notes as the traces. Do the lines cross?
with(notes, interaction.plot(Phone, Notes, Words))
# Answer: No

# Question 14
# Create an interaction plot with Notes on the X-axis and Phone
# as the traces. Do the lines cross?
with(notes, interaction.plot(Notes, Phone, Words))
# Answer: No

# Question 15
# Conduct a factorial ANOVA to test for any order effect
# that the presentation order of the Notes factor may have had.
# To the nearest ten-thousandth (four digits), what is the
# p-value for the Order factor from such a test? Hint: Use
# the ez library and its ezANOVA function, passing one
# between parameter and Order as the within parameter.
print(ezANOVA(
    data = notes,
    dv = Words,
    between = Phone,
    within = Order,
    wid = Subject
))
print("--------------------------------------------------------")
# Answer: 4.684e-01 --> 0.4684

# Question 17
# Conduct a factorial ANOVA on Words by Phone and Notes.
# To the nearest hundredth (two digits), what is the largest
# F statistic produced by such a test? Hint: Use the ez
# library and its ezANOVA function, passing one between
# parameter and one within parameter.
print(ezANOVA(
    data = notes,
    dv = Words,
    within = Notes,
    between = Phone,
    wid = Subject
))
print("--------------------------------------------------------")
# Answer: 43.56

# Question 18
# Conduct two planned pairwise comparisons using
# paired-samples t-tests. The first question is whether
# iPhone users entered different numbers of words using
# the built-in notes app versus the add-on notes app.
# The second question is whether Android users entered
# different numbers of words using the built-in notes app
# versus the add-on notes app. Assuming equal variances,
# and using Holm’s sequential Bonferroni procedure to
# correct for multiple comparisons, what to within a
# ten-thousandth (four digits) is the lowest p-value
# from these tests? Hint: Use the reshape2 library and
# its dcast function to make a wide-format table with columns
# for Subject, Phone, Add-on, and Built-in, and then within
# each Phone type, do a paired-samples t-test between the
# Add-on and Built-in columns.
library(reshape2)
wideformNotes <- dcast(
    data = notes,
    formula = Subject + Phone ~ Notes,
    value.var = "Words"
)
print(head(wideformNotes))
iphone <- wideformNotes[wideformNotes$Phone == "iPhone", ]
android <- wideformNotes[wideformNotes$Phone == "Android", ]
iphone_p <- t.test(
    iphone$"Add-on",
    iphone$"Built-in",
    paired = TRUE,
    var.equal = TRUE
)
android_p <- t.test(
    android$"Add-on",
    android$"Built-in",
    paired = TRUE,
    var.equal = TRUE
)
print(p.adjust(c(iphone_p$p.value, android_p$p.value), method = "holm"))
print("--------------------------------------------------------")
# Answer: 0.1961

# Question 20
# Download the file socialvalue.csv from the course materials.
# This file describes a study of people viewing a positive or
# negative film clip before going onto social media and then
# judging the value of the first 100 posts they see there.
# The number of valued posts was recorded. Examine the data
# and indicate what kind of experiment design this was.
social <- read.csv(
    "/home/maxfield/active/personal/insc-571/exercises/materials/socialvalue.csv"
)
social$Subject <- factor(social$Subject)
social$Clip <- factor(social$Clip)
social$ClipOrder <- factor(social$ClipOrder)
social$Social <- factor(social$Social)
social$SocialOrder <- factor(social$SocialOrder)
print("Injested data head")
print(head(social))
print("--------------------------------------------------------")

# Question 21
# How many subjects took part in this experiment?
print(length(unique(social$Subject)))
print("--------------------------------------------------------")
# Answer: 16

# Question 22
# To the nearest hundredth (two digits), on average how many
# posts out of 100 were valued for the most valued combination
# of Clip and Social?
print(ddply(social, ~ Clip + Social, function(data) summary(data$Valued)))
print("--------------------------------------------------------")
# Answer: 68.75

# Question 23
# Create an interaction plot with Social on the X-axis and
# Clip as the traces. Do the lines cross?
with(social, interaction.plot(Social, Clip, Valued))
# Answer: No

# Question 24
# Create an interaction plot with Clip on the X-axis and
# Social as the traces. Do the lines cross?
with(social, interaction.plot(Clip, Social, Valued))
# Answer: Yes

# Question 25
# Conduct a factorial ANOVA to test for any order effects
# that the presentation order of the Clip factor and/or the
# Social factor may have had. To the nearest ten-thousandth
# (four digits), what is the p-value for the ClipOrder main
# effect? Hint: Use the ez library and its ezANOVA function.
# Pass both ClipOrder and SocialOrder as the within
# parameter using a vector created with the “c” function.
print(ezANOVA(
    data = social,
    dv = Valued,
    within = c(ClipOrder, SocialOrder),
    wid = Subject
))
print("--------------------------------------------------------")
# Answer: 0.3484

# Question 26
# Conduct a factorial ANOVA on Valued by Clip and Social.
# To the nearest hundredth (two digits), what is the largest
# F statistic produced by such a test? Hint: Use the ez
# library and its ezANOVA function. Pass both Clip and
# Social as the within parameter using a vector created
# with the “c” function.
print(ezANOVA(
    data = social,
    dv = Valued,
    within = c(Clip, Social),
    wid = Subject
))
print("--------------------------------------------------------")
# Answer: 6.995 - > 7.00

# Question 27
# Conduct two planned pairwise comparisons using paired-samples
# t-tests. The first question is whether on Facebook, the
# number of valued posts was different after people saw a positive
# film clip versus a negative film clip. The second question is
# whether on Twitter, the number of valued posts was different
# after people saw a positive film clip versus a negative film
# clip. Assuming equal variances, and using Holm’s sequential
# Bonferroni procedure to correct for multiple comparisons,
# what to within a ten-thousandth (four digits) is the lowest
# p-value from these tests? Hint: Use the reshape2 library
# and its dcast function to make a wide-format table with
# columns for Subject and the combination of Social × Clip,
# and then do a paired-samples t-test between columns with the
# same Social level.
wideformSocial <- dcast(
    data = social,
    formula = Subject ~ Social + Clip,
    value.var = "Valued"
)
print(head(wideformSocial))
facebook_p <- t.test(
    wideformSocial$Facebook_positive,
    wideformSocial$Facebook_negative,
    paired = TRUE,
    var.equal = TRUE
)
twitter_p <- t.test(
    wideformSocial$Twitter_positive,
    wideformSocial$Twitter_negative,
    paired = TRUE,
    var.equal = TRUE
)
print(p.adjust(c(facebook_p$p.value, twitter_p$p.value), method = "holm"))
print("--------------------------------------------------------")
# Answer: 0.0408

# Question 29
# Continue using the file socialvalue.csv from the course materials.
# Conduct a nonparametric Aligned Rank Transform procedure on
# Valued by Clip and Social. To the nearest hundredth (two digits),
# what is the largest F statistic produced by this procedure?
# Hint: Use the  ARTool library and its art function with the formula:
#
# Valued ~ Clip * Social + (1|Subject)
#
# The above formula indicates that Subject is to be treated as a
# random effect. (Random effects will be covered later in the course.)
library(ARTool)
m <- art(
    formula = Valued ~ Clip * Social + (1 | Subject),
    data = social
)
print(anova(m))
print("--------------------------------------------------------")
# Answer: 17.13

# Question 30
# For question 23, you created an interaction plot with Social
# on the X-axis, Clip as the traces, and Valued on the Y-axis.
# Create this plot again. Is the number of valued social
# media posts after viewing positive and negative film clips
# closer for Facebook or Twitter? (Mark one)
with(social, interaction.plot(Social, Clip, Valued))

# Question 31
# Compare the number of valued posts on Facebook after viewing
# positive and negative film clips using post hoc contrast
# tests. To the nearest ten-thousandth (four digits), what is
# the p-value for this comparison? Correct for multiple
# comparisons in the family of contrasts using Holm’s
# sequential Bonferroni procedure. Hint: Use the Aligned Rank
# Transform contrast testing function art.con. Assuming “m”
# is the ART model you built in question 29, this is the
# code to use:
#
# art.con(m, ~ Clip*Social, adjust="holm")
print(art.con(m, ~ Clip * Social, adjust = "holm"))
print("--------------------------------------------------------")
# Answer: 0.0031

# Question 32
# Compare the number of valued posts on Twitter after viewing
# positive and negative film clips using post hoc contrast tests.
# To the nearest ten-thousandth (four digits), what is the p-value
# for this comparison? Correct for multiple comparisons in the
# family of contrasts using Holm’s sequential Bonferroni
# procedure. Hint: Use the Aligned Rank Transform contrast
# testing function art.con. Assuming “m” is the ART model
# you built in question 29, this is the code to use:
#
# art.con(m, ~ Clip*Social, adjust="holm")
print(art.con(m, ~ Clip * Social, adjust = "holm"))
print("--------------------------------------------------------")
# Answer: 0.4905