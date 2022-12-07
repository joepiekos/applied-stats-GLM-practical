library(readr)
library(MASS)
library(leaps)
library(devtools)
library(gridExtra) #arrange ggplots
library(tidyverse) #tidyverse!
library(inspectdf) #easy summary of data frames with plots
library(ggmosaic) #ggplot mosaic plots
library(RColorBrewer) #colour palette visualisation
library(rsq) #r squared for GLM
library(gt) #nice tables!
library(webshot2)
library(vcd) #Ord_plot()
library(car) #influence plot

docvis <- read_csv("docvis.csv")
attach(docvis)
plot(visits)

docvis.all.numerical <- read_csv("docvis.csv") #read in from csv, has categorical variables as numerical 
docvis <- mutate_at(docvis,c(4,5,6,7,8),as.factor) #change final 5 columns data type to factor.
docvis.factor <- mutate_at(docvis,c(1:8),as.factor)

#numerical summaries
visits.freq <- as_tibble(round(table(visits)/length(visits),digits = 4))
visits.freq <- visits.freq %>% mutate_at(c(2),as.double) %>% mutate_at(c(1),as.integer) %>% mutate_at(c(1),round,digits = 1) %>% rename(proportion = n) #formatting data a bit
visits.freq <- visits.freq %>% rownames_to_column()  %>% pivot_longer(, cols = -rowname) %>% pivot_wider(, names_from = rowname) %>% rename("lll" = 1) %>% as_tibble() #transposes tibble for better viewing

visits.freq.table <- visits.freq %>% gt() %>% tab_options(column_labels.hidden = TRUE) %>% fmt_number(columns = c(2:11), rows = c(1),drop_trailing_zeros = TRUE) #create nice visual table and format.
gtsave(visits.freq.table, filename = "Visit_relative_frequency.png")

means.and.sd <- mutate_at(inspect_num(docvis.all.numerical)[,c(1,5,8)],c(2,3),round,digits = 3) #computes means and stanard devs of all variables, then rounds to 3 decimal places
means.and.sd.table <- means.and.sd %>% rename(variable = col_name) %>% gt()
gtsave(means.and.sd.table, filename = "Variable_means_sdev.png")

test.poisson <- as_tibble(rpois(5190,mean(visits)))
test.poisson <- (test.poisson %>% bind_cols(rep(0,times = 5190)) %>% rename(is_real=...2))
visits.real <- (visits %>% as_tibble() %>% bind_cols(rep(1,times = 5190)) %>% rename(is_real=...2))
real.model.comparison <- bind_rows(test.poisson,visits.real)
real.model.comparison <- mutate_at(real.model.comparison,c(2),as.factor)
poisson.hist.comparison <- ggplot(real.model.comparison) + geom_bar(aes(x=value, fill = is_real),alpha=0.8, position = position_dodge()) + scale_fill_brewer(palette = "Paired", labels = c('0','1')) + scale_x_continuous(name = "Number of visits", breaks = seq(0,9,1))
ggsave(poisson.hist.comparison, filename = "Data_vs_Poisson_histogram.png")

age.income.hist <- docvis[,c(2,3)] %>% inspect_num() %>% show_plot() + theme(axis.title = element_text(size = rel(2)),axis.text=element_text(size=12))
ggsave(age.income.hist,filename = "age_and_income_histograms.png")

factor.levels.freq <- docvis.factor %>% inspect_cat() %>% show_plot()  + theme(axis.title = element_text(size = 13.5),axis.text=element_text(size=13.5))
ggsave(factor.levels.freq, filename = "factor_levels_frequencies.png")
  
age.vis.mosaic <- ggplot(data = docvis.factor) + geom_mosaic(aes(x = product(age), fill=age), divider = "vspine") + labs(title = 'Age|Number of Visits') + facet_grid(~visits) + theme(aspect.ratio = 3,axis.text.x = element_blank(),axis.ticks.x = element_blank()) + guides(fill = guide_legend(reverse=TRUE))
ggsave(age.vis.mosaic, filename = "age_vis_mosaic.png",height = 4,width = 13)

visits.histogram <- ggplot(docvis,aes(x=visits)) + geom_histogram(binwidth=1, fill = "#1F78B4", col = "grey", alpha = 0.7) + scale_x_continuous(name = "Number of visits", breaks = seq(0,9,1)) + ylab("Count") + stat_bin(binwidth=1, geom="text", colour="black", size=3.5, aes(label=..count..),vjust = "inward", position = position_dodge()) + theme(axis.text=element_text(size=10))
ggsave(visits.histogram, filename = "visits_histogram.png")

male.female.visit.split <- ggplot(docvis,aes(x=visits, fill = female)) + geom_histogram(binwidth = 0.5,position = position_fill(),alpha = 0.8) + scale_fill_brewer(palette = "Paired", labels = c('Male','Female')) + scale_x_continuous(name = "Number of visits", breaks = seq(0,9,1)) + ylab("Proportion") + stat_bin(binwidth=1, geom="text", colour="white", size=3.5, aes(label=..count.., group=female), position = position_fill(vjust = 0.5))
private.visit.split <- ggplot(docvis,aes(x=visits, fill = private)) + geom_histogram(binwidth = 0.5,position = position_fill(),alpha = 0.8) + scale_fill_brewer(palette = "Paired", labels = c('No Private Healthcare','Private Healthcare')) + scale_x_continuous(name = "Number of visits", breaks = seq(0,9,1)) + ylab("Proportion") + stat_bin(binwidth=1, geom="text", colour="white", size=3.5, aes(label=..count.., group=private), position = position_fill(vjust = 0.5))

ggsave("visits_split_by_factors.png", arrangeGrob(male.female.visit.split,private.visit.split))

ggplot(docvis,aes(x=age,y=income,group=age)) + geom_boxplot(alpha=0.5,fill = "blue") + scale_x_continuous(name = "Age", breaks = unique(age)) + ylab("Income ($10,000s)") #boxplots of age vs income

#q2 - glm fitting
#dont have data on patients recent/long term health measures
null.model <- glm(visits ~ 1, data = docvis, family = "poisson")
full.model <- glm(visits ~ age + income + private + freepoor + freerepat + lchronic + female*. ,data = docvis, family = "poisson")
full.model.summary <- summary(full.model)
full.model.signif <- c("***","***","**","","**","","***","***","***","","","","","")
coefficients.table.full <- rownames_to_column(data.frame(full.model.summary$coefficients),var = "Variables") %>% as_tibble() %>% mutate_at(c(2,3,4),round,digits=5) %>% rename(z_Prob=Pr...z..) %>% add_column(full.model.signif, .after = "z_Prob") %>% rename(Significance=full.model.signif) %>% gt()


rsq(full.model, type = "kl")
Lambda.full <- reduced.model$null.deviance - reduced.model$deviance
1 - pchisq(Lambda.full, 13) #reject null hypothesis

reduced.model <- glm(visits ~ age + income + freepoor + lchronic + female + age:female, data = docvis, family = "poisson")
reduced.model.summary <- summary(reduced.model)
reduced.model.signif <- c("***","***","***","**","***","***","***")
coefficients.table.reduced <- rownames_to_column(data.frame(reduced.model.summary$coefficients),var = "Variables") %>% as_tibble() %>% mutate_at(c(2,3,4),round,digits=5) %>% rename(z_Prob=Pr...z..) %>% add_column(reduced.model.signif, .after = "z_Prob") %>% rename(Significance=reduced.model.signif) %>% gt()

rsq(reduced.model, type = "kl") #rsq must increase with more predictors, hence why reduced one is lower. However it is still very close to the full model rsquared.


step(full.model, scope = null.model, direction = "backward") #gives same results as just removing based on Wald test.

Lambda.reduced <- reduced.model$null.deviance - reduced.model$deviance
1 - pchisq(Lambda.reduced, 6) #reject null hypothesis too, still better than null!

#can't do chi-squared test for goodness of it i.e. using just the residual deviance and seeing if it's big. This would be for comparing our model to the saturated model. But due to lack of normality from poisson with low counts then we can't use it.
#no need to compare nested models. I'm happy with the reduced one chosen by step and wald tests.
### mention Wald tests
### also do chi squared stuff (is it still valid for poisson with low counts, maybe only breaks for saturated model).
#q3 - diagnostics

deviance.residuals <- residuals(reduced.model, type = "deviance")
fitted.eta <- predict(reduced.model, type = "link")
fitted.mu <- predict(reduced.model, type = "response")
residual.fittedval <- data.frame(deviance.residuals,fitted.eta,fitted.mu) %>% as_tibble() 
fittedval.deviance.plot <- ggplot(residual.fittedval) + geom_point(aes(x=fitted.eta,y=deviance.residuals)) #pretty bad, as expected for poisson with low count models
ggsave(fittedval.deviance.plot, filename = "dev_residuals_against_fitted_eta.png")

png(filename = "qqplot_standard_dev_residuals.png",width = 6,height = 6,units="in",res = 400)
qqnorm(rstandard(reduced.model),pch=19, main = "")
qqline(rstandard(reduced.model))
dev.off()

png(filename = "influence_plot.png",width = 6,height = 6,units="in",res = 400)
influencePlot(reduced.model) #points 115, 630, 198 have high influence but seem to just be normal people with lots of doctor visits. None have chronic condition but know nothing else about their previous health. No need to remove as outliers.
dev.off()

#q4 - interpreation + confidence intervals
#value of coefficient gives percentage change in output per unit change of corresponding regressor
#exponential of binary variable's coefficient gives multiplicative factor that output is bigger by if treatment is 1
model.covmatrix <- vcov(reduced.model)
model.coefficients <- summary(reduced.model)$coefficients[c(1,3,4,5),1]
model.coefficients <- rownames_to_column(as.data.frame(model.coefficients),var = "Variable") %>% as_tibble() %>% rename(Effect = model.coefficients) %>% add_row(Variable = c("age|female = 0","age|female = 1","female"), Effect = c(summary(reduced.model)$coefficients[2,1], summary(reduced.model)$coefficients[2,1] + summary(reduced.model)$coefficients[7,1], summary(reduced.model)$coefficients[6,1] + summary(reduced.model)$coefficients[7,1]))
model.coefficients <- model.coefficients %>% add_column(Std_Error = sqrt(c(model.covmatrix[1,1],model.covmatrix[3,3],model.covmatrix[4,4],model.covmatrix[5,5],model.covmatrix[2,2],model.covmatrix[2,2]+model.covmatrix[7,7]+(2*model.covmatrix[7,2]),model.covmatrix[6,6]+model.covmatrix[7,7]+model.covmatrix[7,1])))
model.coefficients <- model.coefficients %>% mutate(Lower = Effect - 1.96*Std_Error, Upper = Effect + 1.96*Std_Error) 
model.coeff.table <- model.coefficients %>% gt()
gtsave(model.coeff.table, filename = "Effects_with_Intervals.png")

#q5 - dispersion and alternative models
dispersion.param <- sum(residuals(reduced.model,type ="pearson")^2)/reduced.model$df.residual

png(filename = "Ord_plot.png",width = 6,height = 6,units="in",res = 400)
Ord_plot(docvis$visits) #suggests logarithmic series distribution
dev.off()

