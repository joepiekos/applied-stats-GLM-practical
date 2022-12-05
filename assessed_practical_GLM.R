library(readr)
library(MASS)
library(leaps)
library(devtools)
library(gridExtra)
library(ggplot2)
library(dplyr)

docvis <- read_csv("docvis.csv")
attach(docvis)
plot(visits)

docvis <- mutate_at(docvis,c(4,5,6,7,8),as.factor) #change final 5 columns data type to factor.


ggplot(docvis,aes(x=visits, fill = female)) + geom_histogram(binwidth = 0.5,position = position_fill(),alpha = 1) + scale_fill_brewer(palette = "Paired") + scale_x_continuous(name = "Number of visits", breaks = seq(0,9,1)) + ylab("Proportion")
ggplot(docvis,aes(x=visits, fill = private)) + geom_histogram(binwidth = 0.5,position = position_fill(),alpha = 1) + scale_fill_brewer(palette = "Paired") + scale_x_continuous(name = "Number of visits", breaks = seq(0,9,1))

docvis_no0 <- as_tibble(filter(docvis,visits != 0 & visits != 1))
ggplot(docvis_no0,aes(x=visits)) + geom_histogram(binwidth = 1) 
#+ geom_point(aes(x=jitter(docvis$age),y=jitter(docvis$vsits))) 
