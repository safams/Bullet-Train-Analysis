options(warn=-1)

install.packages("MASS")

library(ggplot2)
library(dplyr)
library(reshape2)
library(glmnet)
library(MASS)

# Reading data

travel <- read.csv("TravelTrain.csv", header=T, sep=",")
survey <- read.csv("SurveyTrain.csv", header=T, sep=",")
full <- merge(survey,travel,by.x="ID",by.y="ID")
nonfactors = c("ID", "Age", "Travel_Distance", "DepartureDelay_in_Mins", "ArrivalDelay_in_Mins")
factors = -which(names(full) %in% nonfactors)
full[, factors] = lapply(full[, factors], as.factor)
head(full)

num_rows_with_na <- sum(apply(full, 1, function(row) any(is.na(row))))
num_rows_with_na

full_complete <- na.omit(full)
head(full_complete)

# Removing rows with missing values

num_rows_with_na <- sum(apply(full_complete, 1, function(row) any(is.na(row))))
num_rows_with_na

full_complete <- full_complete[ , !(names(full_complete) %in% c("ID"))]
head(full_complete)

empty_vals <- sapply(full_complete, function(x) x == "")
rows_to_keep <- apply(empty_vals, 1, function(row) !any(row))
                      
shinkansen_data <- full_complete[rows_to_keep, ]

experience <- ggplot(shinkansen_data, aes(x = Overall_Experience)) +
  geom_bar()
experience

proportions <- prop.table(table(shinkansen_data$Overall_Experience))
proportions

# Visualizations

plot1 <- ggplot(shinkansen_data, aes(x = Age)) + geom_bar() 
plot2 <- ggplot(shinkansen_data, aes(x = Travel_Distance)) + geom_bar()
plot3 <- ggplot(shinkansen_data, aes(x = ArrivalDelay_in_Mins)) + 
  geom_bar() + 
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 5000))
plot4 <- ggplot(shinkansen_data, aes(x = DepartureDelay_in_Mins)) + 
  geom_bar() + 
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 5000))

plot1
plot2
plot3
plot4

delays <- ggplot(shinkansen_data, aes(x = ArrivalDelay_in_Mins, y = DepartureDelay_in_Mins)) +
  geom_point()
delays

# Checking counts of gender
gender <- ggplot(shinkansen_data, aes(x = Gender)) +
  geom_bar()
gender

# checking counts of customer type
customer_type <- ggplot(shinkansen_data, aes(x = CustomerType)) +
  geom_bar()
customer_type

# plotting proportions of loyal and disloyal customers in the responses
ggplot(shinkansen_data, aes(x = Overall_Experience,fill = CustomerType)) + 
    geom_bar(position = "fill")

set.seed(123) # For reproducibility
sample_frac <- 0.1
full_sampled <- full_complete[sample(nrow(full_complete), size = floor(nrow(full_complete) * sample_frac)), ]

head(full_sampled)

# Function to calculate Chi-squared test and Cramér's V for all pairs of categorical variables
association_test <- function(data) {
  cat_vars <- sapply(data, is.factor) 
  cat_combinations <- combn(names(cat_vars)[cat_vars], 2)
  
  results <- data.frame(Var1 = character(), Var2 = character(), Chi_Squared = numeric(), P_Value = numeric(), Cramers_V = numeric(), stringsAsFactors = FALSE)
  
  for(i in 1:ncol(cat_combinations)) {
    var1 <- cat_combinations[1, i]
    var2 <- cat_combinations[2, i]
    
    table <- table(data[[var1]], data[[var2]])
    test <- tryCatch(chisq.test(table), error = function(e) return(e))

    if(!inherits(test, "error")) {
      v <- sqrt(test$statistic / (sum(table) * (min(nrow(table), ncol(table)) - 1)))
    } else {
      v <- NA
    }
    results <- rbind(results, data.frame(Var1 = var1, Var2 = var2, Chi_Squared = if(!is.na(v)) test$statistic else NA, P_Value = if(!is.na(v)) test$p.value else NA, Cramers_V = v))
  }
  return(results)
}

association_results <- association_test(full_sampled)

association_results

association_results <- association_results[!is.nan(association_results$Cramers_V) & association_results$Cramers_V >= 0.25, ]
association_results

numerical_data <- full_sampled[c("Age", "Travel_Distance", "DepartureDelay_in_Mins", "ArrivalDelay_in_Mins")]
correlation_matrix <- cor(numerical_data, use="complete.obs")
correlation_matrix

numcols = full_complete[, c("Age", "Travel_Distance", "DepartureDelay_in_Mins", "ArrivalDelay_in_Mins")]
pca <- prcomp(numcols, scale=T)
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var) * 100, 2)
barplot(pca.var.per, xlab="Principal Component", ylab="Percentage Variation")

full_model <- glm(Overall_Experience ~ Onboard_entertainment + Onlinebooking_Ease + Baggage_handling + CustomerType + Age + Travel_Distance + DepartureDelay_in_Mins, data = full_complete, family = binomial())
summary(full_model)

# Lasso Regression
x <- model.matrix(Overall_Experience ~ Onboard_entertainment + Onlinebooking_Ease + Baggage_handling + CustomerType + Age + Travel_Distance + DepartureDelay_in_Mins - 1, data=full_complete)
y <- full_complete$Overall_Experience

cv_fit <- cv.glmnet(x, y, family="binomial", alpha=1)
plot(cv_fit)
coef(cv_fit, s="lambda.1se")

incremented_lambda <- cv_fit$lambda.1se + 0.03
coef(cv_fit, s=incremented_lambda)

predictors <- c("Onboard_entertainment", "Onlinebooking_Ease", "Baggage_handling", 
                "CustomerType")
combinations <- combn(predictors, 3)

aic_values <- sapply(1:ncol(combinations), function(i) {
  formula_str <- paste("Overall_Experience ~", paste(combinations[, i], collapse = " + "))
  formula <- as.formula(formula_str)
  model <- glm(formula, data = full_complete, family = "binomial")
  AIC(model)
})

# Find the index of the combination with the lowest AIC
best_model_index <- which.min(aic_values)

best_combination <- combinations[, best_model_index]
best_combination
