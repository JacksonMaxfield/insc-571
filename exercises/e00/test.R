hello <- c(1, 2, 3)
print("this is awesome")

library(ggplot2)
library(apstyle)

ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_point() +
    theme_ap()