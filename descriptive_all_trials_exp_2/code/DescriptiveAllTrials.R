#---------------------------------------------------------------
# VR-Tennis descriptive analysis of the gaze fixations of all trials
# Author: Damian Beck
# Date: November 2023
# Based on r version 4.3.2
#---------------------------------------------------------------

# Packages ----
#---------------------------------------------------------------
#install recommended packages from andy field
#install.packages("car"); 
#install.packages("ggplot2"); 
#install.packages("nlme"); 
#install.packages("reshape")
#install.packages("readxl")
#install.packages("lsr")
#install.packages("dplyr")
#install.packages("mgcv")
#install.packages("tidyverse")
#install.packages("sjPlot")
#install.packages("broom.mixed")
#install.packages("modi")
#install.packages("svglite")
#install.packages("showtext")
#install.packages("lme4")  
#install.packages("boot")  

library(car)
library(ggplot2)
library(nlme)
library(reshape)
library(readxl)
library(lsr)
library(dplyr)
library(mgcv)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(modi)
library(svglite)
library(showtext)
library(lme4)
library(boot)

# Functions ----
#---------------------------------------------------------------
#Function for calculating the mahalanobis distance and remove outliers
remove_outliers <- function(data) {
  data_clean <- data[complete.cases(data),] #delete missing values in order to calculate the mahalanobis distance
  data_clean <- data_clean[!is.na(data_clean$y), ]
  mahalanobis(data_clean, colMeans(data_clean), cov(data_clean)) #calculate mahalanobis distance for detecting outliers
  MDmiss(data_clean, colMeans(data_clean), cov(data_clean))
  data_clean$mahal <- mahalanobis(data_clean, colMeans(data_clean), cov(data_clean))
  data_clean$p <- pchisq(data_clean$mahal, df=1, lower.tail=FALSE)
  cutoff <- qchisq(1-.05, ncol(data_clean)) #set the cutoff for the mahalanobis distance to 5%
  data_clean <- data_clean[data_clean$mahal<cutoff ,]
  return(data_clean)
}

#start with the warm up trials----
#The two warm up blocks contain each 30 trials
start_index <- 0
end_index <- 60

#Bounce positions 0 to 9 in the warm Up trials with the condition afterwards of a central tendency to the left / right
conditions <- c("left", "right")
for (condition in conditions) {
  for (bounce_positions in 0:9) {
    data <- read_excel(paste0("data/datafileforR_", bounce_positions, "_", condition, "WarmUp.xlsx"))
    data_clean <- remove_outliers(data)
    subset_warm_up <- subset(data_clean, block > start_index) #select the warm up trials
    data_warm_up_clean <- subset(subset_warm_up, block < end_index)
    model_mean_data_warm_up_clean <- nlme::lme(y ~ 1, #calculate the hierarchical mean of the warm up trials
                               random = ~1 |index,
                               data = data_warm_up_clean,
                               method = "ML",
                               na.action = na.exclude)
    mean <- model_mean_data_warm_up_clean$coefficients$fixed #extract the mean
    assign(paste0("mean_warm_up_", condition, "_", bounce_positions), mean) #assign the mean to the bouncing_position
    conf_int_from_model <- intervals(model_mean_data_warm_up_clean, level = 0.95, "fixed") #calculate the confidence intervals
    ci <- data.frame()
    ci[1,1] <- conf_int_from_model$fixed[1,1]
    ci[1,2] <- conf_int_from_model$fixed[1,3]
    assign(paste0("ci_warm_up_", condition, "_", bounce_positions), ci) #assign the confidence interval to the bouncing_position
  }
}

#Summarize all confidence intervals in the left condition
ci_warm_up_left_lower <- c(ci_warm_up_left_0[1,1],ci_warm_up_left_1[1,1],ci_warm_up_left_2[1,1],ci_warm_up_left_3[1,1],ci_warm_up_left_4[1,1],ci_warm_up_left_5[1,1], ci_warm_up_left_6[1,1], ci_warm_up_left_7[1,1], ci_warm_up_left_8[1,1], ci_warm_up_left_9[1,1])
ci_warm_up_left_upper <- c(ci_warm_up_left_0[1,2],ci_warm_up_left_1[1,2],ci_warm_up_left_2[1,2],ci_warm_up_left_3[1,2],ci_warm_up_left_4[1,2],ci_warm_up_left_5[1,2], ci_warm_up_left_6[1,2], ci_warm_up_left_7[1,2], ci_warm_up_left_8[1,2], ci_warm_up_left_9[1,2])

#Summarize all confidence intervals in the right condition
ci_warm_up_right_lower <- c(ci_warm_up_right_0[1,1], ci_warm_up_right_1[1,1], ci_warm_up_right_2[1,1], ci_warm_up_right_3[1,1],ci_warm_up_right_4[1,1],ci_warm_up_right_5[1,1],ci_warm_up_right_6[1,1],ci_warm_up_right_7[1,1],ci_warm_up_right_8[1,1], ci_warm_up_right_9[1,1])
ci_warm_up_right_upper <- c(ci_warm_up_right_0[1,2], ci_warm_up_right_1[1,2], ci_warm_up_right_2[1,2], ci_warm_up_right_3[1,2],ci_warm_up_right_4[1,2],ci_warm_up_right_5[1,2],ci_warm_up_right_6[1,2],ci_warm_up_right_7[1,2],ci_warm_up_right_8[1,2], ci_warm_up_right_9[1,2])

#Save essential warm_updata for the plot
bouncing_position_angles <- c(-20.68, -16.08, -11.49, -6.89, -2.30, 2.30, 6.89, 11.49, 16.08, 20.68) #angles of the bouncing positions from participant perspective
fixation_right_name <- rep("fixation_right", 10)
probability_right <- c(NaN, NaN, NaN, 1.25, 3.75, 9.06, 18.13, 26.88, 27.19, 13.44) #probabilities of the bouncing positions in the right condition
shift_rightwarm_up <- c(mean_warm_up_right_0, mean_warm_up_right_1, mean_warm_up_right_2, mean_warm_up_right_3, mean_warm_up_right_4, mean_warm_up_right_5, mean_warm_up_right_6, mean_warm_up_right_7, mean_warm_up_right_8, mean_warm_up_right_9)
bouncing_position_shifted_rightwarm_up <- bouncing_position_angles - shift_rightwarm_up
data2_warm_up_right <- data.frame(probability_right, fixation_right_name, bouncing_position_shifted_rightwarm_up)
fixation_left_name <- rep("fixation_left", 10)
probability_left <- c(13.44, 27.19, 26.88, 18.13, 9.06, 3.75, 1.25, NaN, NaN, NaN) #probabilities of the bouncing positions in the left condition
shift_leftwarm_up <- c(mean_warm_up_left_0, mean_warm_up_left_1, mean_warm_up_left_2, mean_warm_up_left_3, mean_warm_up_left_4, mean_warm_up_left_5, mean_warm_up_left_6, NaN, NaN, NaN)
bouncing_position_shifted_leftwarm_up <- bouncing_position_angles - shift_leftwarm_up
data2_warm_up_left <- data.frame(probability_left, fixation_left_name, bouncing_position_shifted_leftwarm_up)

xmin <- bouncing_position_shifted_leftwarm_up - (shift_leftwarm_up - ci_warm_up_left_lower)
xmax <- bouncing_position_shifted_leftwarm_up + (ci_warm_up_left_upper - shift_leftwarm_up)
data1_warm_up_confidence <- data.frame(xmin, xmax)

xmin <- bouncing_position_shifted_rightwarm_up - (shift_rightwarm_up - ci_warm_up_right_lower)
xmax <- bouncing_position_shifted_rightwarm_up + (ci_warm_up_right_upper - shift_rightwarm_up)
data2_warm_up_confidence <- data.frame(xmin, xmax)


#Last 4 blocks ----
#each block contains 32 trials, therefore 4 blocks contain trials 192 to 320
#In the last 4 blocks, we saw that participants have learnt the prior distribution already good
#and the prior is stable and not changing significant anymore
start_index <- 192
end_index <- 320

#bouncing_position 0 to 6 (other have no values) in the condition with a central tendency to the left
for (bouncing_position in 0:6) {
  data <- read_excel(paste0("data/datafileforR_", bouncing_position, "_left.xlsx"))
  data_clean <- remove_outliers(data)
  subset_last4_blocks <- subset(data_clean, block > start_index) #select the Last4blocks
  data_clean <- subset(subset_last4_blocks, block < end_index)
  model_mean_clean <- nlme::lme(y ~ 1, #calculate the hierarchical mean of the Last4blocks
                               random = ~1 |index,
                               data = data_clean,
                               method = "ML",
                               na.action = na.exclude)
  mean <- model_mean_clean$coefficients$fixed #extract the mean
  assign(paste0("mean_left_", bouncing_position), mean) #assign the mean to the bouncing_position
  conf_int_from_model <- intervals(model_mean_clean, level = 0.95, "fixed") #calculate the confidence intervals
  ci <- data.frame()
  ci[1,1] <- conf_int_from_model$fixed[1,1]
  ci[1,2] <- conf_int_from_model$fixed[1,3]
  assign(paste0("ci_left_", bouncing_position), ci) #assign the confidence interval to the bouncing_position
}

#Summarize all confidence intervals in the left condition
ci_left_lower <- c(ci_left_0[1,1],ci_left_1[1,1],ci_left_2[1,1],ci_left_3[1,1],ci_left_4[1,1],ci_left_5[1,1], ci_left_6[1,1], NaN, NaN, NaN)
ci_left_upper <- c(ci_left_0[1,2],ci_left_1[1,2],ci_left_2[1,2],ci_left_3[1,2],ci_left_4[1,2],ci_left_5[1,2], ci_left_6[1,2], NaN, NaN, NaN)

#bouncing_position 3 to 9 (other have no values) in the condition with a central tendency to the right
for (bouncing_position in 3:9) {
  data <- read_excel(paste0("data/datafileforR_", bouncing_position, "_right.xlsx"))
  data_clean <- remove_outliers(data)
  subset_last4_blocks <- subset(data_clean, block > start_index) #select the Last4blocks
  data_clean <- subset(subset_last4_blocks, block < end_index)
  model_mean_clean <- nlme::lme(y ~ 1, #calculate the hierarchical mean of the Last4blocks
                               random = ~1 |index,
                               data = data_clean,
                               method = "ML",
                               na.action = na.exclude)
  mean <- model_mean_clean$coefficients$fixed #extract the mean
  assign(paste0("mean_right_", bouncing_position), mean) #assign the mean to the bouncing_position
  conf_int_from_model <- intervals(model_mean_clean, level = 0.95, "fixed") #calculate the confidence intervals
  ci <- data.frame()
  ci[1,1] <- conf_int_from_model$fixed[1,1]
  ci[1,2] <- conf_int_from_model$fixed[1,3]
  assign(paste0("ci_right_", bouncing_position), ci) #assign the confidence interval to the bouncing_position
}

#Summarize all confidence intervals in the right condition
ci_right_lower <- c(NaN, NaN, NaN, ci_right_3[1,1],ci_right_4[1,1],ci_right_5[1,1],ci_right_6[1,1],ci_right_7[1,1],ci_right_8[1,1], ci_right_9[1,1])
ci_right_upper <- c(NaN, NaN, NaN, ci_right_3[1,2],ci_right_4[1,2],ci_right_5[1,2],ci_right_6[1,2],ci_right_7[1,2],ci_right_8[1,2], ci_right_9[1,2])



#Plot all together----
#prepare data for the last4blocks right
bouncing_position_angles <- c(-20.68, -16.08, -11.49, -6.89, -2.30, 2.30, 6.89, 11.49, 16.08, 20.68) #angles of the bouncing positions from participant perspective
fixation_right_name <- rep("fixation_right", 10)
right_positions_name <- rep("right hit position", 10)
right_positions_name <- as.factor(right_positions_name)
probability_right <- c(NaN, NaN, NaN, 1.25,3.75, 9.06, 18.13, 26.88, 27.19, 13.44) #probabilities of the bouncing positions in the right condition
shift_right <- c(NaN, NaN, NaN, mean_right_3, mean_right_4, mean_right_5, mean_right_6, mean_right_7, mean_right_8, mean_right_9)
bouncing_position_shifted_right <- bouncing_position_angles - shift_right
data1_right <- data.frame(probability_right, right_positions_name, bouncing_position_angles)
data2_right <- data.frame(probability_right, fixation_right_name, bouncing_position_shifted_right)

#prepare data for the last4blocks left
bouncing_position_angles <- c(-20.68, -16.08, -11.49, -6.89, -2.30, 2.30, 6.89, 11.49, 16.08, 20.68) #angles of the bouncing positions from participant perspective
fixation_left_name <- rep("fixation_left", 10)
left_positions_name <- rep("left hit position", 10)
left_positions_name <- as.factor(left_positions_name)
probability_left <- c(13.44, 27.19, 26.88, 18.13, 9.06, 3.75, 1.25, NaN, NaN, NaN) #probabilities of the bouncing positions in the left condition
shift_left <- c(mean_left_0, mean_left_1, mean_left_2, mean_left_3, mean_left_4, mean_left_5, mean_left_6, NaN, NaN, NaN)
bouncing_position_shifted_left <- bouncing_position_angles - shift_left
data1_left <- data.frame(probability_left, left_positions_name, bouncing_position_angles)
data2_left <- data.frame(probability_left, fixation_left_name, bouncing_position_shifted_left)


# Plot right distribution
plot <- ggplot(data1_right, aes(x = bouncing_position_angles, y = probability_right)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k=7), colour = "blue", size = 0.2) 
print(plot)


# Plot left distribution
plot <- plot  +
  geom_smooth(data = data1_left, aes(x = bouncing_position_angles, y = probability_left), method = "gam", formula = y ~ s(x, k=7), colour = "red", size = 0.2)
print(plot)


#add arrows from the distribution to aggregated fixations of the last4blocks
arrows_left <- merge(data2_left, data2_warm_up_left)[1:7,]
arrows_right <- merge(data2_right, data2_warm_up_right)[1:7,]
plot <- plot +
  geom_segment(data = arrows_left, 
               aes(x = c(6.89, -20.68, -6.89, -11.49, -16.08, 2.30, -2.30), xend = bouncing_position_shifted_left, y = probability_left, yend = probability_left),
               arrow = arrow(length = unit(0.05, "inches")), size = 0.2, color = "black") +
  geom_segment(data = arrows_right, 
               aes(x = c(-6.89, 20.68, 6.89, 11.49, 16.08, -2.30, 2.30), xend = bouncing_position_shifted_right, y = probability_right, yend = probability_right),
               arrow = arrow(length = unit(0.05, "inches")), size = 0.2, color = "black") 
print(plot) 

#plot warm up trials
windowsFonts(sans = windowsFont("Arial"))
colnames(data2_warm_up_right)[colnames(data2_warm_up_right) == 'bouncing_position_shifted_right'] <- 'bouncing_position_shifted_rightwarm_up'
colnames(data2_warm_up_left)[colnames(data2_warm_up_left) == 'bouncing_position_shifted_left'] <- 'bouncing_position_shifted_leftwarm_up'

# Add the fixation points of the left Warm up trials 
plot <- plot +
  geom_point(data = data2_warm_up_left, aes(x = bouncing_position_shifted_leftwarm_up, y = probability_left), colour = "salmon", size = 1) +
  geom_errorbar(data = data1_warm_up_confidence, aes(xmin = xmin, xmax = xmax, y = probability_left),
              width = 0.2, colour = "salmon", size = 0.4)
print(plot)

# Add the fixation points of the right Warm up trials 
plot <- plot +
  geom_point(data = data2_warm_up_right, aes(x = bouncing_position_shifted_rightwarm_up, y = probability_right), colour =  "light blue", size = 1) +
  geom_errorbar(data = data2_warm_up_confidence, aes(xmin = xmin, xmax = xmax, y = probability_right),
                width = 0.2, colour = "light blue", size = 0.4)
print(plot)


# Add data2_right (last4blocks) to the plot
plot <- plot +
  geom_point(data = data2_right, aes(x = bouncing_position_shifted_right, y = probability_right), size = 1, colour = "blue") +
  geom_errorbar(data = data2_right, aes(xmin = bouncing_position_shifted_right - (shift_right - ci_right_lower), xmax = bouncing_position_shifted_right + (ci_right_upper - shift_right), y = probability_right),
                width = 0.2, colour = "blue", size = 0.4) 
print(plot)

# Add data2_left (last4blocks) to the plot
plot <- plot +
  geom_point(data = data2_left, aes(x = bouncing_position_shifted_left, y = probability_left), size = 1, colour ="red") +
  geom_errorbar(data = data2_left, aes(xmin = bouncing_position_shifted_left - (shift_left - ci_left_lower), xmax = bouncing_position_shifted_left + (ci_left_upper - shift_left), y = probability_left),
                width = 0.2, colour = "red", size = 0.4) 
print(plot)



  
#Aesthetics  
plot <- plot +  
 
  geom_point( aes(x = -22, y = 3), size = 1, colour ="salmon") +
  geom_point( aes(x = -22, y = 1.5), size = 1, colour ="red") +
  geom_point( aes(x = 14, y = 3), size = 1, colour ="light blue") +
  geom_point( aes(x = 14, y = 1.5), size = 1, colour ="blue") +
  
  labs(x = "Angle (°)", y = "Probability of Biased Condition (%)") +theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 7, family = "sans"),
    axis.text.y = element_text(size = 7, family = "sans"),
    axis.title.x = element_text(size = 7, family = "sans"),
    axis.title.y = element_text(size = 7, family = "sans"),
    text = element_text(size = 7, family = "sans") 
  ) +
  scale_x_continuous(limits = c(-24, 24))  # Set x-axis limits
print(plot)

# save the plot
ggsave("plots/GazeShiftDifferencesRandInterceptExp2.svg", plot, device = "svg",
       width = 9, height = 6, units = "cm")



# shift size inference statistics --------------------------------------------
# Take it positive if the shift is in the curve and negativ outside the curve
# The peak of the curve (modus) is is left condition between the second and third position from the edge
# Exclude these two positions as we have no exact hypotheses what should happen close to the peak 
# Take on the left side the first value negative and the fourth or higher positive
# As the hypothesis is that the shift is in the direction of the distribution's peak's area
# As the right distribution is mirrored to the left distribution, we can use the same logic
#shift size hierarchical ----------------------------------------------------
## merge all data from the last4blocks to one dataframe
start_index <- 192
end_index <- 320

#initialize list
all_data_left_exp2 <- list()

#left: summing up all deviation in the direction within the distribution
for (bouncing_position in c(0, 3, 4, 5, 6)) {
  data <- read_excel(paste0("data/datafileforR_", bouncing_position, "_left.xlsx"))
  data_clean <- remove_outliers(data)
  subset_last4_blocks <- subset(data_clean, block > start_index) #select the Last4blocks
  data_clean <- subset(subset_last4_blocks, block < end_index)
  # if bouncing_position is 0, 1 change sign (positiv if within the distribution)
  if (bouncing_position == 0 | bouncing_position == 1) {
    data_clean$y <- -data_clean$y
  }
  # Store cleaned data in the list
  all_data_left_exp2[[bouncing_position + 1]] <- data_clean  # Index starts from 1 in R
}

# Merge all data into one final data frame
final_data_left_exp2 <- do.call(rbind, all_data_left_exp2)

#add column left_right
final_data_left_exp2$left_right <- "left"

#initialize list for right distribution
all_data_right_exp2 <- list()

#right: summing up all deviation in the direction within the distribution
for (bouncing_position in c(3, 4, 5, 6, 9)) {
  data <- read_excel(paste0("data/datafileforR_", bouncing_position, "_right.xlsx"))
  data_clean <- remove_outliers(data)
  subset_last4_blocks <- subset(data_clean, block > start_index) #select the Last4blocks
  data_clean <- subset(subset_last4_blocks, block < end_index)  
  # if not bouncing_position is 8, 9 change signsuch that it is positiv if it is within the distribution)
  if (!(bouncing_position %in% c(8, 9))) {
    data_clean$y <- -data_clean$y
  }
  # Store cleaned data in the list
  all_data_right_exp2[[bouncing_position + 1]] <- data_clean  # Index starts from 1 in R
}

# Merge all data into one final data frame
final_data_right_exp2 <- do.call(rbind, all_data_right_exp2)
#add column left_right
final_data_right_exp2$left_right <- "right"

#merge datafile left right
final_data_all_exp2 <- rbind(final_data_left_exp2, final_data_right_exp2)

# Fit the hierarchical linear mixed-effects model using lme4 left and right
model_mean_exp2 <- lmer(y ~ 1 + (1 | index), 
                        data = final_data_all_exp2, REML = FALSE)
tab_model(model_mean_exp2)
summary(model_mean_exp2)

#check normality 
hist(final_data_all_exp2$y)
hist(resid(model_mean_exp2)) #normality is not violated (bootstrapp bias 0)

boot_lme_exp2 <- function(data, indices) {
  resampled_data <- data[indices, ]  # Bootstrap sample
  
  new_model <- try(lme(y ~ 1,
                       random = ~1 | index, 
                       data = final_data_all_exp2), silent = TRUE)
  
  if (inherits(new_model, "try-error")) {
    return(rep(NA, length(fixef(model_mean_left_exp2))))  # Return NA if model fails
  } else {
    return(fixef(new_model))  # Extract fixed effects (coefficients)
  }
}
# Perform bootstrapping with 5000 resamples
set.seed(123)
boot_results_exp2 <- boot(data = final_data_all_exp2, statistic = boot_lme_exp2, R = 5000)

# Print bootstrapped results
print(boot_results_exp2)
#Bootstrap Statistics :
#  original  bias    std. error
#t1* 1.885599       0           0

# Compute confidence intervals for bootstrapped estimates
boot.ci(boot_results_exp2, type = "perc") #CI are smaller but now differences in interpretation
#[1] "All values of t are equal to  1.88559862720519 \n Cannot calculate confidence intervals"
#NULL




### now the same for the warm up trials ----
#initialize list
all_data_exp2_warm_up <- list()
start_index <- 0
end_index <- 60

#Bounce positions 0 to 9 in the warm Up trials with the condition afterwards of a central tendency to the left / right
conditions <- c("left", "right")
for (condition in conditions) {
  for (bounce_positions in c(0,3,4,5,6,9)) {
    data <- read_excel(paste0("data/datafileforR_", bounce_positions, "_", condition, "WarmUp.xlsx"))
    data_clean <- remove_outliers(data)
    subset_warm_up_blocks <- subset(data_clean, block > start_index) #select the warm up blocks
    data_clean <- subset(subset_warm_up_blocks, block < end_index)
    data_clean$left_right <- condition
    if (condition == "left") {
      if (bounce_positions == 0 ) {
        data_clean$y <- -data_clean$y
      }
    } else {
      if (!(bounce_positions ==  9)) {
        data_clean$y <- -data_clean$y
      }
    }
    # Store cleaned data in the list
    all_data_exp2_warm_up[[bouncing_position + 1]] <- data_clean  # Index starts from 1 in R
  }}

# Merge all data into one final data frame
final_data_exp2_warm_up <- do.call(rbind, all_data_exp2_warm_up)

# Fit the hierarchical linear mixed-effects model using lme4
model_mean_exp2_warm_up <- lmer(y ~ 1 + (1 | index), 
                                data = final_data_exp2_warm_up, REML = FALSE)
tab_model(model_mean_exp2_warm_up)
summary(model_mean_exp2_warm_up)

#check normality 
hist(final_data_exp2_warm_up$y)
hist(resid(model_mean_exp2_warm_up)) #-> normality is violated

# Define a function for bootstrapping to check if the model is robust
boot_lmer_exp2 <- function(data, indices) {
  resampled_data <- data[indices, ]  # Bootstrap sample
  
  new_model <- try(lmer(y ~ 1 + (1 | index), 
                        data = resampled_data, REML = FALSE), silent = TRUE)
  
  if (inherits(new_model, "try-error")) {
    return(rep(NA, length(fixef(model_mean_left_exp2))))  # Return NA if model fails
  } else {
    return(fixef(new_model))  # Extract fixed effects (coefficients)
  }
}

# Perform bootstrapping with 5000 resamples
set.seed(123)
boot_results_exp2_warm_up <- boot(data = final_data_exp2_warm_up, statistic = boot_lmer_exp2, R = 5000)

# Print bootstrapped results
print(boot_results_exp2_warm_up)
#Bootstrap Statistics :
#  original     bias    std. error
#t1* -0.8519375 0.05624249   0.3495322

# Compute confidence intervals for bootstrapped estimates
boot.ci(boot_results_exp2_warm_up, type = "perc") #CI are smaller but now differences in interpretation
#Intervals : 
#  Level     Percentile     
#95%   (-1.4875, -0.1201 )  






# Inferential statistical differences of exp1 and exp2 ------------------------------------------------
#take the data from experiment 1
#read in "shift_values" csv file from Exp1 in folder data
final_data_all_exp1 <- read.csv("data/final_data_all_exp1.csv")
#add a new comuln exp1
final_data_all_exp1$exp <- 0

#add a new column exp2 to shift_values
final_data_all_exp2$exp <- 1
#add 100 to the index values of Exp2
final_data_all_exp2$index <- final_data_all_exp2$index + 100

#combine the shift values from Exp1 and Exp2 in long format
final_data_all <- rbind(final_data_all_exp2, final_data_all_exp1[,-1])
View(final_data_all)

#check variances
check_variances <- final_data_all
check_variances$exp <- as.factor(check_variances$exp)
leveneTest(y ~ exp,check_variances) #not equal variances
var(check_variances$y[check_variances$exp == 0], na.rm = TRUE)
var(check_variances$y[check_variances$exp == 1], na.rm = TRUE)

# Define the function for bootstrapping
boot_lmer <- function(data, indices) {
  resampled_data <- data[indices, ]  # Bootstrap sample
  new_model <- try(lmer(y ~ exp + (1 | exp:index), 
                        data = resampled_data, REML = FALSE), silent = TRUE)
  
  if (inherits(new_model, "try-error")) {
    return(rep(NA, length(nlme::fixef(model_mean_all))))  # Return NA if model fails
  } else {
    return(setNames(fixef(new_model), names(fixef(new_model))))  # Extract fixed effects (coefficients)
  }
}

model_mean_both_experiments <- lmer(y ~ exp + (1 | exp:index), 
                                         data = final_data_all, REML = FALSE)
tab_model(model_mean_both_experiments)
summary(model_mean_both_experiments)

#check normality 
hist(final_data_all$y)
hist(resid(model_mean_both_experiments)) #-> normality is violated

# Perform bootstrapping with 5000 resamples for left
set.seed(123)
boot_results_all <- boot(data = final_data_all, statistic = boot_lmer, R = 5000)

# Print bootstrapped results
print(boot_results_all)
#Bootstrap Statistics :
#  original        bias    std. error
#t1* 1.5480484  0.0031308655   0.1132933
#t2* 0.3346917 -0.0002491617   0.1678354
boot.ci(boot_results_all, type = "perc", index = 1)
#Intervals : 
#  Level     Percentile     
#95%   ( 1.324,  1.774 ) 
boot.ci(boot_results_all, type = "perc", index = 2)
#Intervals : 
#  Level     Percentile     
#95%   ( 0.0068,  0.6644 )  


#Considering different variances, however exactly the same interpretation of the results
# Define the function for bootstrapping using lme with heteroscedasticity
boot_lme <- function(data, indices) {
  resampled_data <- data[indices, ]  # Bootstrap sample
  
  # Fit the model with heteroscedasticity using varIdent
  new_model <- try(lme(y ~ exp, 
                       random = ~1 | index, 
                       weights = varIdent(form = ~1 | exp),  # Allows different variances per exp
                       data = resampled_data, method = "ML"), silent = TRUE)
  
  if (inherits(new_model, "try-error")) {
    return(rep(NA, length(fixef(model_mean_all))))  # Return NA if model fails
  } else {
    return(setNames(fixef(new_model), names(fixef(new_model))))  # Extract fixed effects (coefficients)
  }
}

# Fit the hierarchical linear mixed-effects model using nlme::lme for left side
model_mean_both_experiments <- lme(y ~ exp, 
                                        random = ~1 | index, 
                                        weights = varIdent(form = ~1 | exp), 
                                        data = final_data_all, method = "ML")

tab_model(model_mean_both_experiments)
summary(model_mean_both_experiments)

# Check normality 
hist(final_data_all$y)
hist(resid(model_mean_both_experiments)) #-> normality is violated

# Perform bootstrapping with 5000 resamples for left
set.seed(123)
boot_results <- boot(data = final_data_all, statistic = boot_lme, R = 5000)

# Print bootstrapped results
print(boot_results)
#Bootstrap Statistics :
#  original        bias    std. error
#t1* 1.5480523  4.352470e-03   0.1138556
#t2* 0.3341075 -9.179265e-05   0.1687615

boot.ci(boot_results, type = "perc", index = 1)
#Intervals : 
#  Level     Percentile     
#95%   ( 1.322,  1.776 )
boot.ci(boot_results, type = "perc", index = 2)
#Intervals : 
#  Level     Percentile     
#95%   ( 0.0038,  0.6681 )  

