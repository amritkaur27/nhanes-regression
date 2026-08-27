library(dplyr)
library(psych)
library(knitr)
library(ggplot2)
library(tidyr)
library(car)
library(MASS)
library(NHANES)


data("NHANES")
View(NHANES)

Cleaned <- NHANES %>% 
  dplyr::select(SleepHrsNight, Gender, Age, Poverty, Depressed, PhysActiveDays, AlcoholYear, 
                HealthGen) %>%
  dplyr::filter(Age >= 18)

Cleaned <- na.omit(Cleaned)
summary(Cleaned)


new_model_proposed <- lm(SleepHrsNight ~ Gender + Age + Poverty + Depressed + PhysActiveDays 
                         + AlcoholYear + HealthGen, data = Cleaned)


n <- nrow(Cleaned)
cooks_d <- cooks.distance(new_model_proposed)
influential_threshold <- 4 / n

Cleaned_final <- Cleaned[cooks_d <= influential_threshold, ]


p <- length(coef(new_model_proposed))
leverage <- hatvalues(new_model_proposed)
rstudent <- rstudent(new_model_proposed)
leverage_threshold <- 2 * p / n

Cleaned_problematic <- Cleaned[leverage <= leverage_threshold & 
                                 abs(rstudent) <= 3 & cooks_d <= influential_threshold, ]

model_preliminary <- lm(SleepHrsNight ~ Gender + Age + Poverty + Depressed + PhysActiveDays 
                        + AlcoholYear + HealthGen, data = Cleaned_final)


par(mfrow = c(2, 3)) 
crPlots(model_preliminary, main = "Component-Residual Plot for Preliminary Model")


plot(model_preliminary, which = 1, 
     main = "Residual-Fitted Plot for Prelim")
abline(h = 0, col = "pink", lwd = 2, lty = 2)

complete_table_preliminary <- cbind(summary(model_preliminary)$coefficients, confint(model_preliminary))
print(complete_table_preliminary)


Cleaned_final$log_AlcoholYear <- log(Cleaned_final$AlcoholYear + 1)
Cleaned_problematic$log_AlcoholYear <- log(Cleaned_problematic$AlcoholYear + 1)


full_model <- lm(SleepHrsNight ~ Gender + Age + Poverty + Depressed + PhysActiveDays 
                 + log_AlcoholYear + HealthGen, data = Cleaned_final)

full_model_problematic <- lm(SleepHrsNight ~ Gender + Age + Poverty + Depressed + PhysActiveDays 
                             + log_AlcoholYear + HealthGen, data = Cleaned_problematic)

summary(full_model)
summary(full_model_problematic)


vif_results <- vif(full_model)
print(vif_results)

stepwise_model <- step(full_model, direction = "both")
summary(stepwise_model)

reduced_final_model <- lm(SleepHrsNight ~ Gender + Age + Depressed + log_AlcoholYear 
                          + HealthGen, data = Cleaned_final)

crPlots(reduced_final_model, "log_AlcoholYear")


partial_f_test <- anova(reduced_final_model, full_model)
print(partial_f_test)


complete_table_final <- cbind(summary(reduced_final_model)$coefficients, confint(reduced_final_model))


model_proposal <- lm(SleepHrsNight ~ Gender + Age + Poverty + Depressed + PhysActiveDays 
                     + AlcoholYear, data = Cleaned_final)


final_metrics <- data.frame(
  Model = "Reduced Final Model",
  R_squared_proposal = summary(reduced_final_model)$r.squared,
  Adjusted_R_squared_proposal = summary(reduced_final_model)$adj.r.squared,
  AIC = AIC(reduced_final_model),
  BIC = BIC(reduced_final_model)
)
print(final_metrics)


proposed_metrics <- data.frame(
  Model = "Proposed Model",
  R_squared_proposal = summary(model_proposal)$r.squared,
  Adjusted_R_squared_proposal = summary(model_proposal)$adj.r.squared,
  AIC = AIC(model_proposal),
  BIC = BIC(model_proposal)
)
print(proposed_metrics)

par(mfrow = c(1, 3))
plot(reduced_final_model, which = 1, main="Final: Residuals vs. Fitted")
plot(reduced_final_model, which = 2, main="Final: QQ Plot")
hist(rstandard(reduced_final_model), xlab = "Standardized Residuals", 
     main = "Final: Std Residuals Hist.")
par(mfrow = c(1, 1))


bc <- boxcox(full_model)
optimal_lambda <- bc$x[which.max(bc$y)]
Cleaned_final$transformed_sleep <- (Cleaned_final$SleepHrsNight^optimal_lambda - 1) / optimal_lambda

transformed_final_model <- lm(transformed_sleep ~ Gender + Depressed + Age+ 
                                log_AlcoholYear + HealthGen, 
                              data = Cleaned_final)

summary(transformed_final_model)

par(mfrow = c(1, 3))
plot(transformed_final_model, which = 1, main="Transform: Residuals-Fitted")
plot(transformed_final_model, which = 2, main="Transform: QQ Plot")
hist(rstandard(transformed_final_model), xlab = "Standardized Residuals", 
     main = "Transform: Std Residuals Hist.")
par(mfrow = c(1, 1))
