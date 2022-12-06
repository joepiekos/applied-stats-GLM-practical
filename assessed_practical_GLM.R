library(readr)
library(MASS)
library(leaps)
library(devtools)
library(gridExtra)
library(tidyverse) #tidyverse!
library(inspectdf) #easy summary of data frames with plots
library(ggmosaic) #ggplot mosaic plots
library(RColorBrewer) #colour palette visualisation
library(rsq) #r squared for GLM
library(formattable) #visually appealing tables
library(gt)

docvis <- read_csv("docvis.csv")
attach(docvis)
plot(visits)

docvis.all.numerical <- read_csv("docvis.csv") #read in from csv, has categorical variables as numerical 
docvis <- mutate_at(docvis,c(4,5,6,7,8),as.factor) #change final 5 columns data type to factor.
docvis.factor <- mutate_at(docvis,c(1:8),as.factor)

#numerical summaries
visits.freq <- round(table(visits)/length(visits),digits = 3)
visits.freq

means.and.sd <- mutate_at(inspect_num(docvis.all.numerical)[,c(1,5,8)],c(2,3),round,digits = 3) #computes means and stanard devs of all variables, then rounds to 3 decimal places
formattable(means.and.sd)

test.poisson <- as_tibble(rpois(5190,mean(visits)))
test.poisson <- (test.poisson %>% bind_cols(rep(0,times = 5190)) %>% rename(is_real=...2))
visits.real <- (visits %>% as_tibble() %>% bind_cols(rep(1,times = 5190)) %>% rename(is_real=...2))
real.model.comparison <- bind_rows(test.poisson,visits.real)
real.model.comparison <- mutate_at(real.model.comparison,c(2),as.factor)



ggplot(real.model.comparison) + geom_bar(aes(x=value, fill = is_real),alpha=0.8, position = position_dodge()) + scale_fill_brewer(palette = "Paired", labels = c('Male','Female')) + scale_x_continuous(name = "Number of visits", breaks = seq(0,9,1))


docvis %>% inspect_num() %>% show_plot()

docvis %>% inspect_cat()
docvis.factor %>% inspect_cat() %>% show_plot()
#plots
  
age.vis.mosaic <- ggplot(data = docvis.factor) + geom_mosaic(aes(x = product(age), fill=age), divider = "vspine") + labs(title = 'Age|Number of Visits') + facet_grid(~visits) + theme(aspect.ratio = 3,axis.text.x = element_blank(),axis.ticks.x = element_blank()) + guides(fill = guide_legend(reverse=TRUE))
ggsave(age.vis.mosaic, filename = "age_vis_mosaic.png",height = 4,width = 13)

docvis %>% inspect_num() %>% show_plot() #histograms of each numerical column

ggplot(docvis,aes(x=visits)) + geom_histogram(binwidth=1, fill = "#1F78B4", col = "grey", alpha = 0.7) + scale_x_continuous(name = "Number of visits", breaks = seq(2,9,1)) + ylab("Count") + stat_bin(binwidth=1, geom="text", colour="black", size=3.5, aes(label=..count..),vjust = "inward", position = position_dodge()) 

ggplot(docvis,aes(x=visits, fill = female)) + geom_histogram(binwidth = 0.5,position = position_fill(),alpha = 0.8) + scale_fill_brewer(palette = "Paired", labels = c('Male','Female')) + scale_x_continuous(name = "Number of visits", breaks = seq(0,9,1)) + ylab("Proportion") + stat_bin(binwidth=1, geom="text", colour="white", size=3.5, aes(label=..count.., group=female), position = position_fill(vjust = 0.5))
ggplot(docvis,aes(x=visits, fill = private)) + geom_histogram(binwidth = 0.5,position = position_fill(),alpha = 0.8) + scale_fill_brewer(palette = "Paired", labels = c('No Private Healthcare','Private Healthcare')) + scale_x_continuous(name = "Number of visits", breaks = seq(0,9,1)) + ylab("Proportion") + stat_bin(binwidth=1, geom="text", colour="white", size=3.5, aes(label=..count.., group=private), position = position_fill(vjust = 0.5))

ggplot(docvis,aes(x=age,y=income,group=age)) + geom_boxplot(alpha=0.5,fill = "blue") + scale_x_continuous(name = "Age", breaks = unique(age)) + ylab("Income ($10,000s)") #boxplots of age vs income
ggplot(docvis,aes(x=private,y=visits)) + geom_boxplot()
docvis_no0 <- as_tibble(filter(docvis,visits != 0 & visits != 1))
ggplot(docvis_no0,aes(x=visits)) + geom_histogram(binwidth = 1) 
#+ geom_point(aes(x=jitter(docvis$age),y=jitter(docvis$vsits))) 


#q2 - glm fitting
#dont have data on patients recent/long term health measures
null.model <- glm(visits ~ 1, data = docvis, family = "poisson")
full.model <- glm(visits ~ age + income + private + freepoor + freerepat + lchronic + female*. ,data = docvis, family = "poisson")
summary(full.model)
rsq(full.model)

reduced.model <- glm(visits ~ age + income + freepoor + lchronic + female + age:female, data = docvis, family = "poisson")
summary(reduced.model)
rsq(reduced.model, type = "kl")

step(full.model, scope = null.model, direction = "backward")
### mention Wald tests
### also do chi squared stuff (is it still valid for poisson with low counts, maybe only breaks for saturated model).
#q3 - diagnostics




#q4 - interpreation + confidence intervals
