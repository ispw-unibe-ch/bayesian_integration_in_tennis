#---------------------------------------------------------------
# VR-Tennis logarithmic multilevel regression analysis of the shift differences
# of the gaze fixations and shift differences of the explicit point estimations

# Author: Damian Beck
# Date: November 2023
# Based on r version 4.3.2
#---------------------------------------------------------------

#Please note that if you want to analyse the first experiment, you have to change 
#the datafile name in the import data section and the plot names in the plots section.
#Do this in the following lines (103-109), 173

# Packages ----
#---------------------------------------------------------------
#install packages recommended by Field (2012)
#install.packages("car", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("nlme", dependencies = TRUE)
#install.packages("reshape", dependencies = TRUE)
#install.packages("tidyverse", dependencies = TRUE)
#install.packages("sjPlot", dependencies = TRUE)
#install.packages("broom.mixed", dependencies = TRUE)
#install.packages("modi", dependencies = TRUE)
#install.packages("robustlmm", dependencies = TRUE)
#install.packages("readxl", dependencies = TRUE)
#install.packages("dplyr", dependencies = TRUE)
#install.packages("DescTools", dependencies = TRUE)
#install.packages("lme4")
#install.packages("simr")


library(car)
library(ggplot2)
library(nlme)
library(reshape)
library(lme4)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(modi)
library(robustlmm)
library(readxl)
library(dplyr)
library(DescTools)
library(lme4)
library(simr)

# Import data ----
#---------------------------------------------------------------
datafile_for_r_exp1 <- read_excel("data/FixationShiftDifferencesexp1.xlsx")
datafile_for_r_exp2 <- read_excel("data/FixationShiftDifferencesexp2.xlsx")

# Remove outliers ----
# To remove outliers take the cooks distance (no package to do it hierarchical)
exp1_valid_trials <- datafile_for_r_exp1
#make a new column counting row number modulo 20
exp1_valid_trials$trial <- 1:nrow(exp1_valid_trials)
exp1_valid_trials$trial <- exp1_valid_trials$trial %% 20
# change 0 to 20 in trial
exp1_valid_trials$trial[exp1_valid_trials$trial == 0] <- 20
View(exp1_valid_trials)

#Calculate the cooks distance
model2_cooks <- lm(y ~ log(block), data = exp1_valid_trials)
summary(model2_cooks)
tab_model(model2_cooks)
plot(model2_cooks,4)
cd <- cooks.distance(model2_cooks)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers_exp1_trials_trough_the_middle <- exp1_valid_trials[names_of_outlier,]
# export the outliers exp1
#write.csv(outliers_exp1_trials_trough_the_middle, "outliers_exp1_trials_trough_the_middle.csv")

exp1_valid_trials <- exp1_valid_trials %>% anti_join(outliers_exp1_trials_trough_the_middle)


exp2_valid_trials <- datafile_for_r_exp2
#make a new column counting row number modulo 20
exp2_valid_trials$trial <- 1:nrow(exp2_valid_trials)
exp2_valid_trials$trial <- exp2_valid_trials$trial %% 20
# change 0 to 20 in trial
exp2_valid_trials$trial[exp2_valid_trials$trial == 0] <- 20

#Calculate the cooks distance
model2_cooks <- lm(y ~ log(block), data = exp2_valid_trials)
summary(model2_cooks)
tab_model(model2_cooks)
plot(model2_cooks,4)
cd <- cooks.distance(model2_cooks)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers_exp2_trials_trough_the_middle <- exp2_valid_trials[names_of_outlier,]
# export the outliers exp2
#write.csv(outliers_exp2_trials_trough_the_middle, "outliers_exp2_trials_trough_the_middle.csv")

exp2_valid_trials <- exp2_valid_trials %>% anti_join(outliers_exp2_trials_trough_the_middle)

# Choose the data for the analysis (exp1 or exp2) ----
##to analyse the first experiment
df <- exp1_valid_trials
plot_name <- "plots/GazeShiftDifferencesRandInterceptExp1.svg"
plot_name2 <- "plots/GazeShiftDifferencesRandInterceptExp1_individuals.svg"

#to analyse the second experiment
#df <- exp2_valid_trials
#plot_name <- "plots/GazeShiftDifferencesRandInterceptExp2.svg"
#plot_name2 <- "plots/GazeShiftDifferencesRandInterceptExp2_individuals.svg"


# View the data
df$new_index <- as.numeric(as.factor(df$index))
View(df)
summary(df)

# Calculate models ----
#Instructions according to Andy Field 
#Field, A. P., Miles, J., & Field, Z. (2012). Discovering statistics using 
#R: And sex and drugs and rock ‘n’ roll. London: Sage.
#---------------------------------------------------------------
model1 <- nlme::gls(y ~ 1,
                    data = df,
                    method = "ML",
                    na.action = na.exclude)
tab_model(model1)

#correlation = corAR1(form = ~1 |index) for growth models (Field, 2012)
log_model2 <- nlme::gls(y ~ I(log(block)),
                        data = df,
                        correlation = corAR1(form = ~ 1 | index),
                        method = "ML",
                        na.action = na.exclude)
tab_model(log_model2)
summary(log_model2)

log_model3  <- nlme::lme(y ~ I(log(block)),
                         random = ~1 |index,
                         data = df,
                         correlation = corAR1(form = ~ 1 | index),
                         method = "ML",
                         na.action = na.exclude)
tab_model(log_model3)
summary(log_model3)

log_model4 <- nlme::lme(y ~ I(log(block)),
                        random = ~ I(log(block)) |index,
                        data = df,
                        correlation = corAR1(form = ~ block | index),
                        method = "ML",
                        na.action = na.exclude)
tab_model(log_model4)

#compare models
anova(model1,log_model2, log_model3, log_model4)

#check assumptions descriptive
plot(log_model3) 
plot(log_model3$residuals)
hist(log_model3$residuals)
boxplot(log_model3$residuals)
shapiro.test(log_model3$residuals)#does not fit for repeated measures. 
#why significant?! significant, but almost always significant for large samples, 
#therefore only graphical check
ks.test(log_model3$residuals, "pnorm") #also significant, 
#but almost always significant in large samples, therefore only graphical check
qqnorm(log_model3$residuals)


#Explicit effects
# Import data ----
#---------------------------------------------------------------
datafile_for_r <- read_excel("data/JudgementsDifferencesexp1.xlsx") #To analyse the first experiment change the name "data/JudgementsDifferencesexp1.xlsx
View(datafile_for_r)
df_explicit <- datafile_for_r
View(df_explicit)
summary(df_explicit)

# Remove outlier ----
# To remove outliers take the cooks distance (not possible to do it hirarchical)
#---------------------------------------------------------------
model2_cooks <- lm(y ~ I(log(block)), data = df_explicit)
tab_model(model2_cooks)
plot(model2_cooks,4)
cd <- cooks.distance(model2_cooks)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- df_explicit[names_of_outlier,]
df_explicit <- df_explicit %>% anti_join(outliers)
view(df_explicit)

# Calculate models ----
#Instructions according to Andy Field 
#Field, A. P., Miles, J., & Field, Z. (2012). Discovering statistics using 
#R: And sex and drugs and rock ‘n’ roll. London: Sage.
#---------------------------------------------------------------
model1_explicit <- nlme::gls(y ~ 1,
                             data = df_explicit,
                             method = "ML",
                             na.action = na.exclude)
tab_model(model1_explicit)

#correlation = corAR1(form = ~1 |index) for growth models (Field, 2012)
log_model2_explicit <- nlme::gls(y ~ I(log(block)),
                                 data = df_explicit,
                                 method = "ML",
                                 correlation = corAR1(form = ~ 1 | index),
                                 na.action = na.exclude)
tab_model(log_model2_explicit)

log_model3_explicit  <- nlme::lme(y ~ I(log(block)),
                                  random = ~1 |index,
                                  data = df_explicit,
                                  correlation = corAR1(form = ~ 1 | index),
                                  method = "ML",
                                  na.action = na.exclude)
tab_model(log_model3_explicit)
summary(log_model3_explicit)

#not converging for exp1
log_model4_explicit <- nlme::lme(y ~ I(log(block)),
                                 random = ~ I(log(block)) |index,
                                 data = df_explicit,
                                 correlation = corAR1(form = ~ 1 | index),
                                 method = "ML",
                                 na.action = na.exclude)
tab_model(log_model4_explicit)

#compare models
anova(model1_explicit, log_model2_explicit, log_model3_explicit) 
anova(model1_explicit, log_model2_explicit, log_model3_explicit, log_model4_explicit) 

#check assumptions descriptiv
plot(log_model3) 
plot(log_model3$residuals)
hist(log_model3$residuals)
boxplot(log_model3$residuals)
shapiro.test(log_model3$residuals)#does not fit for repeated measures. 
#why significant?! significant, but almost always significant for large samples, 
#therefore only graphical check
ks.test(log_model3$residuals, "pnorm") #also significant, 
#but almost always significant in large samples, therefore only graphical check
qqnorm(log_model3$residuals)



#Plots ----
#explicit data
df_explicit$block_plot <- df_explicit$block + 0.1 # such that explicit and implicit is side by side
p <- ggplot(df_explicit, aes(x = block_plot, y = y, color = factor(index))) +
  geom_point(shape = 19, color = "black", fill = "black", size = 1) +
  labs(x = "Block (#)", y = "Δright-Δleft (°)" ) +
  ylim(-15, 15)

#implicit data
df$block_plot <- df$block - 0.1
p <- p + geom_point(data = df, aes(x = block_plot, y = y, color = factor(index)), shape = 19, color = "orange", fill = "orange", size = 1) 
p

# Define the log function for explicit
intercept_explicit <- broom.mixed::tidy(log_model3_explicit, effects = "fixed")$estimate[1]
slope_explicit <- broom.mixed::tidy(log_model3_explicit, effects = "fixed")$estimate[2]

log_func_exp <- function(x) intercept_explicit + slope_explicit * log(x)
p <- p + stat_function(
  fun = log_func_exp,
  colour = "black",
  lwd = 0.2)
p

# Define the log function
intercept <- broom.mixed::tidy(log_model3, effects = "fixed")$estimate[1]
slope <- broom.mixed::tidy(log_model3, effects = "fixed")$estimate[2]

log_func <- function(x) intercept + slope * log(x)
p <- p + stat_function(
  fun = log_func,
  colour = "orange",
  lwd = 0.2)
p

#Aesthetics
p <- p +geom_point( aes(x = 14, y = -16), size = 1, colour ="orange") +
  geom_point( aes(x = 14, y = -18), size = 1, colour ="black")  +
  geom_point( aes(x = 14, y = 16), size = 0, colour ="orange", alpha = 0) +
  scale_y_continuous(breaks = c(-10, 0, 3.2, 10), labels = c("-10","0","","10")) +theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 7, family = "Arial"),
    axis.text.y = element_text(size = 7, family = "Arial"),
    axis.title.x = element_text(size = 7, family = "Arial"),
    axis.title.y = element_text(size = 7, family = "Arial"),
    text = element_text(size = 7, family = "Arial") 
  )
p

#save the plot
ggsave(plot_name, p, device = "svg",width = 9, height = 6, units = "cm")


# Plot individual regression lines for implicit and explicit data ----
# implicit
p <- ggplot(df, aes(x = block_plot, y = y, color = factor(new_index))) +
  geom_point(shape = 19, size = 1) +
  labs(x = "Block (#)", y = "Δright-Δleft (°)" ) +
  ylim(-15, 15)
p

# Define the log function
intercept <- broom.mixed::tidy(log_model3, effects = "fixed")$estimate[1]
slope <- broom.mixed::tidy(log_model3, effects = "fixed")$estimate[2]


log_func <- function(x) intercept + slope * log(x)
p <- p + stat_function(
  fun = log_func,
  colour = "black",
  lwd = 5)
custom_functions <- list(
  log_func1 <- function(x) coef(log_model3)[1, 1] + coef(log_model3)[1, 2] * log(x),
  log_func2 <- function(x) coef(log_model3)[2, 1] + coef(log_model3)[2, 2] * log(x),
  log_func3 <- function(x) coef(log_model3)[3, 1] + coef(log_model3)[3, 2] * log(x),
  log_func4 <- function(x) coef(log_model3)[4, 1] + coef(log_model3)[4, 2] * log(x),
  log_func5 <- function(x) coef(log_model3)[5, 1] + coef(log_model3)[5, 2] * log(x),
  log_func6 <- function(x) coef(log_model3)[6, 1] + coef(log_model3)[6, 2] * log(x),
  log_func7 <- function(x) coef(log_model3)[7, 1] + coef(log_model3)[7, 2] * log(x),
  log_func8 <- function(x) coef(log_model3)[8, 1] + coef(log_model3)[8, 2] * log(x),
  log_func9 <- function(x) coef(log_model3)[9, 1] + coef(log_model3)[9, 2] * log(x),
  log_func10 <- function(x) coef(log_model3)[10, 1] + coef(log_model3)[10, 2] * log(x),
  log_func11 <- function(x) coef(log_model3)[11, 1] + coef(log_model3)[11, 2] * log(x),
  log_func12 <- function(x) coef(log_model3)[12, 1] + coef(log_model3)[12, 2] * log(x),
  log_func13 <- function(x) coef(log_model3)[13, 1] + coef(log_model3)[13, 2] * log(x),
  log_func14 <- function(x) coef(log_model3)[14, 1] + coef(log_model3)[14, 2] * log(x),
  log_func15 <- function(x) coef(log_model3)[15, 1] + coef(log_model3)[15, 2] * log(x),
  log_func16 <- function(x) coef(log_model3)[16, 1] + coef(log_model3)[16, 2] * log(x),
  log_func17 <- function(x) coef(log_model3)[17, 1] + coef(log_model3)[17, 2] * log(x),
  log_func18 <- function(x) coef(log_model3)[18, 1] + coef(log_model3)[18, 2] * log(x),
  log_func19 <- function(x) coef(log_model3)[19, 1] + coef(log_model3)[19, 2] * log(x),
  log_func20 <- function(x) coef(log_model3)[20, 1] + coef(log_model3)[20, 2] * log(x),
  log_func21 <- function(x) coef(log_model3)[21, 1] + coef(log_model3)[21, 2] * log(x),
  log_func22 <- function(x) coef(log_model3)[22, 1] + coef(log_model3)[22, 2] * log(x),
  log_func23 <- function(x) coef(log_model3)[23, 1] + coef(log_model3)[23, 2] * log(x),
  log_func24 <- function(x) coef(log_model3)[24, 1] + coef(log_model3)[24, 2] * log(x),
  log_func25 <- function(x) coef(log_model3)[25, 1] + coef(log_model3)[25, 2] * log(x),
  log_func26 <- function(x) coef(log_model3)[26, 1] + coef(log_model3)[26, 2] * log(x),
  log_func27 <- function(x) coef(log_model3)[27, 1] + coef(log_model3)[27, 2] * log(x),
  log_func28 <- function(x) coef(log_model3)[28, 1] + coef(log_model3)[28, 2] * log(x),
  log_func29 <- function(x) coef(log_model3)[29, 1] + coef(log_model3)[29, 2] * log(x),
  log_func30 <- function(x) coef(log_model3)[30, 1] + coef(log_model3)[30, 2] * log(x),
  log_func31 <- function(x) coef(log_model3)[31, 1] + coef(log_model3)[31, 2] * log(x),
  log_func32 <- function(x) coef(log_model3)[32, 1] + coef(log_model3)[32, 2] * log(x))


for (i in 1:length(custom_functions)) {
  function_name <- names(custom_functions)[i]
  p <- p + stat_function(fun = custom_functions[[i]], color = i)
}

p <- p + theme(text = element_text(size = 32))
p <- p + guides(color = "none")

p <- p + theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(size = 7, family = "Arial"),
  axis.text.y = element_text(size = 7, family = "Arial"),
  axis.title.x = element_text(size = 7, family = "Arial"),
  axis.title.y = element_text(size = 7, family = "Arial"),
  text = element_text(size = 7, family = "Arial") 
)
p

#save the plot
ggsave(plot_name2, p, device = "svg",width = 9, height = 6, units = "cm")





# analyse experiment differences -----------------------------------------------
#add a column exp to the data
exp1_valid_trials$exp <- 0
exp2_valid_trials$exp <- 1


#add 100 to the index of experiment 2
exp2_valid_trials$index <- exp2_valid_trials$index + 100

# merge the data from exp one and two
df_merged <- rbind(exp1_valid_trials, exp2_valid_trials)

check_variances <- df_merged
# Ensure 'exp' is a categorical (factor) variable
check_variances$exp <- as.factor(df_merged$exp)
#Check if variances are equal in both experiments
leveneTest(y ~ exp, data = check_variances)
var(df_merged$y[df_merged$exp == 0], na.rm = TRUE)
var(df_merged$y[df_merged$exp == 1], na.rm = TRUE)


# Calculate the 95% confidence interval for the variance
ci_variance <- VarCI(df_merged$y[df_merged$exp == 0], conf.level = 0.95, na.rm = TRUE)
print(ci_variance) #     var   lwr.ci   upr.ci 
# 8.670366 7.715955 9.814476 
ci_variance2 <- VarCI(df_merged$y[df_merged$exp == 1], conf.level = 0.95, na.rm = TRUE)
print(ci_variance2) #    var   lwr.ci   upr.ci 
#  16.65857 14.72936 18.99540


log_df_merged <- nlme::lme(y ~ I(log(block))+exp+I(log(block))*exp,
                           random = ~ I(log(block)) |index,
                           weights = varIdent(form = ~ 1 | exp), #because of different variances
                           data = df_merged,
                           correlation = corAR1(form = ~ 1 | index),
                           method = "ML",
                           na.action = na.exclude)
tab_model(log_df_merged)
summary(log_df_merged)

plot(log_df_merged) 
plot(log_df_merged$residuals)
hist(log_df_merged$residuals) #quite nice
boxplot(log_df_merged$residuals)
shapiro.test(log_df_merged$residuals)#does not fit for repeated measures. 
#why significant?! significant, but almost always significant for large samples, 
#therefore only graphical check
ks.test(log_df_merged$residuals, "pnorm") #also significant, 
#but almost always significant in large samples, therefore only graphical check
qqnorm(log_df_merged$residuals)


log_df_merged <- lmer(y ~ I(log(block)) + exp + I(log(block)) * exp + 
                        (1 | exp:index), 
                      data = df_merged,                       
                      REML = FALSE,   # Equivalent to "method = 'ML'" in nlme
                      na.action = na.exclude)
tab_model(log_df_merged) #not possible to consider the different variances, therefore slightly different from above


# Power-Analyse für die "Days"-Variable
powerSim(log_df_merged, test = fixed("I(log(block)):exp"), nsim = 5000)

#Power is only 23.60% (95% CI: 21.00, 26.36)

#Power for predictor 'I(log(block)):exp', (95% confidence interval):===|
#  19.70% (17.28, 22.30)
#
#Test: Kenward Roger (package pbkrtest)
#Effect size for I(log(block)):exp is 0.34
#
#Based on 1000 simulations, (297 warnings, 0 errors)
#alpha = 0.05, nrow = 1233
#
#Time elapsed: 0 h 4 m 8 s
#
#nb: result might be an observed power calculation
#Warning message:
#  In observedPowerWarning(sim) :
#  This appears to be an "observed power" calculation