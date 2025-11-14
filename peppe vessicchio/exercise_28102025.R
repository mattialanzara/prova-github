# Exercise on subset selection for linear regression

### The Hitters data will be used in this lab #####
# For this exercise we use the Hitters dataset from the ISLR library. We wish to predict a baseball player's
# Salary on the basis of various statistics associated with performance in the previous year.
# For the sake of this exercise, remove from the dataset the features that are not numeric.
# NOTE: the Salary variable is missing for some of the players, so omit the pertinent entries of the dataset.

# a. Split the dataset in a training part and a test part. Use 75% of the observations for training, and the
#    remaining 25% for test.

# b. Compute the regression model, using the training set, for the Salary variable using best subset selection.
#    Use the "regsubsets" function, which is part of the leaps library. Check the summary to understand how
#    the features are selected.

# c. Choose the best subset using estimates of the test error based on C_p, AIC, BIC and adjusted R^2).
#    Produce suitable plots showing the value of the performance indicators vs the number of features.
#    Additionally, use also the builtin "plot.regsubsets" function and comment on the plot it produces.

# d. Compute the regression model using also the forward stepwise selection. Compare the obtained model against
#    the best subset one, using again the test MSE estimates, and verifying if there are any differences in the
#    models for a given number of predictors (use the "coef" function to look at the coefficients of the model).

# e. Compare the results obtained in points c and d using the BIC as indicator, with the ones you can obtain
#    using the "step" function in place of regsubsets (use "extractAIC" to compute the BIC of the selected model).

# f. Compute the RSS on the test set and compare the result with the ones from the analysis using the test MSE
#    indicators.

# HOMEWORK: add to the comparison the backward stepwise method, and use also the function "stepAIC". 

# dajeromadaje

install.packages('ISLR')
install.packages('leaps')
library('ISLR')
library('leaps')

df = Hitters
df = na.omit(df)
df = df[, sapply(df, is.numeric)]

plot(df)

n = nrow(df)
c = ncol(df) - 1
train_size = round(0.75 * n)          # 75% train, 25% test

train_set = df[1:train_size, ]
test_set = df[(train_size + 1):n, ]

model_subset = regsubsets(train_set$Salary~., data=train_set, nbest=1, nvmax=c, method = "exhaustive")
model_summary = summary(model_subset)
model_summary$which

names(model_summary)

model_summary$cp        # C_p index
model_summary$bic       # BIC index
model_summary$rsq       # R^2 index
model_summary$adjr2     # adjR^2 index

par(mfrow = c(2, 2))
plot(model_subset, scale = "Cp")
plot(model_subset, scale = "bic")
plot(model_subset, scale = "adjr2")
plot(model_subset, scale = "r2")
     
par(mfrow = c(2, 2))  # Griglia 2x2
# Plot 1: Cp
plot(model_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b", col = "blue", pch = 19)
points(which.min(model_summary$cp), min(model_summary$cp), col = "red", cex = 2, pch = 20)

# Plot 2: BIC
plot(model_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "b", col = "blue", pch = 19)
points(which.min(model_summary$bic), min(model_summary$bic), col = "red", cex = 2, pch = 20)

# Plot 3: R²
plot(model_summary$rsq, xlab = "Number of Variables", ylab = "R²", type = "b", col = "blue", pch = 19)
points(which.max(model_summary$rsq), max(model_summary$rsq), col = "red", cex = 2, pch = 20)

# Plot 4: Adjusted R²
plot(model_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "b", col = "blue", pch = 19)
points(which.max(model_summary$adjr2), max(model_summary$adjr2), col = "red", cex = 2, pch = 20)


# Exhaustive
model_subset = regsubsets(train_set$Salary~., data=train_set, nbest=1, nvmax=c, method = "forward")
model_summary = summary(model_subset)
model_summary$which

names(model_summary)

model_summary$cp        # C_p index
model_summary$bic       # BIC index
model_summary$rsq       # R^2 index
model_summary$adjr2     # adjR^2 index

coef(model_subset, 9)
model_summary$which[9, ]

final_model = lm(Salary~AtBat+Hits+Walks+CAtBat+CRuns+CRBI+CWalks+PutOuts+Assists, data = train_set)
summary(final_model)

# Forward
model_subsett = regsubsets(train_set$Salary~., data=train_set, nbest=1, nvmax=c, method = "forward")
model_summaryy = summary(model_subset)
model_summaryy$which

par(mfrow = c(2, 2))
plot(model_subsett, scale = "Cp")
plot(model_subsett, scale = "bic")
plot(model_subsett, scale = "adjr2")
plot(model_subsett, scale = "r2")

# Plot 1: Cp
plot(model_summaryy$cp, xlab = "Number of Variables", ylab = "Cp", type = "b", col = "blue", pch = 19)
points(which.min(model_summaryy$cp), min(model_summaryy$cp), col = "red", cex = 2, pch = 20)

# Plot 2: BIC
plot(model_summaryy$bic, xlab = "Number of Variables", ylab = "BIC", type = "b", col = "blue", pch = 19)
points(which.min(model_summaryy$bic), min(model_summaryy$bic), col = "red", cex = 2, pch = 20)

# Plot 3: R²
plot(model_summaryy$rsq, xlab = "Number of Variables", ylab = "R²", type = "b", col = "blue", pch = 19)
points(which.max(model_summaryy$rsq), max(model_summaryy$rsq), col = "red", cex = 2, pch = 20)

# Plot 4: Adjusted R²
plot(model_summaryy$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "b", col = "blue", pch = 19)
points(which.max(model_summaryy$adjr2), max(model_summaryy$adjr2), col = "red", cex = 2, pch = 20)

names(model_summaryy)

model_summaryy$cp        # C_p index
model_summaryy$bic       # BIC index
model_summaryy$rsq       # R^2 index
model_summaryy$adjr2     # adjR^2 index

coef(model_subsett, 9)
model_summaryy$which[9, ]

final_model = lm(Salary~AtBat+Hits+Walks+CAtBat+CRuns+CRBI+CWalks+PutOuts+Assists, data = train_set)
summary(final_model)














