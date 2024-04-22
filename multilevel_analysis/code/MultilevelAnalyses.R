#---------------------------------------------------------------
# VR-Tennis logarithmic multilevel regression analysis of the shift differences
# of the gaze fixations and shift differences of the explicit point estimations

# Author: Damian Beck
# Date: November 2023
# Based on r version 4.3.2
#---------------------------------------------------------------

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

# Import data ----
#---------------------------------------------------------------
datafile_for_r <- read_excel("data/FixationShiftDifferencesexp2.xlsx") #To analyse the first experiment change the name in "data/FixationShiftDifferencesexp1.xlsx"
View(datafile_for_r)
df <- datafile_for_r
summary(df)

# Remove outlier ----
# To remove outliers take the cooks distance (no package to do it hierarchical)
#---------------------------------------------------------------
model2_cooks <- lm(y ~ log(block), data = df)
summary(model2_cooks)
tab_model(model2_cooks)
plot(model2_cooks,4)
cd <- cooks.distance(model2_cooks)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- df[names_of_outlier,]
df <- df %>% anti_join(outliers)
summary(df)
View(df)

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
datafile_for_r <- read_excel("data/JudgementsDifferencesexp2.xlsx") #To analyse the first experiment change the name "data/JudgementsDifferencesexp1.xlsx
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
p <- ggplot(df_explicit, aes(x = block, y = y, color = factor(index))) +
  geom_point(shape = 19, color = "black", fill = "black", size = 1) +
  labs(x = "Block (#)", y = "Δright-Δleft (°)" ) +
  ylim(-15, 15)

#implicit data
p <- p + geom_point(data = df, aes(x = block, y = y, color = factor(index)), shape = 19, color = "orange", fill = "orange", size = 1) 
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
ggsave("plots/GazeShiftDifferencesRandInterceptExp2.svg", p, device = "svg",width = 9, height = 6, units = "cm")