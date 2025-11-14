# In this exercise you will estimate the coefficients of a linear regression
# model by using the "Boston Housing dataset". You can have access to this
# dataset after loading the library MASS. The dataset originally has 14 columns.
# For this exercise, focus only on the first ten columns, plus the last one
# (called medb), which will represent our output variable "y".

# 1. Check the dataset content with the summary function
# 2. Comment on the degree of correlation between madv and other features in dataset.
# 3. Find the coefficients of a simple linear regression using the lm function. Use only nox as regressor.
# 4. Solve a multiple linear regression problem using three regressors: nox, crim, rm.
# 5. Remove one of the regressors from the previous point and plot the regression hyperplane.
#
# Check the documentation on scatterplot3d for help. Plot anything useful.

install.packages("MASS")
install.packages("corrplot")
install.packages("scatterplot3d")
library("MASS")
library("corrplot")
library("scatterplot3d")

df = Boston[c(1:10, 14)]
df = na.omit(df)

summary(df)
plot(df)

correlations = cor(df)
corrplot(correlations, method="ellipse")

#      crim                zn             indus            chas              nox               rm             age        
# Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000   Min.   :0.3850   Min.   :3.561   Min.   :  2.90  
# 1st Qu.: 0.08205   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000   1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02  
# Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000   Median :0.5380   Median :6.208   Median : 77.50  
# Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917   Mean   :0.5547   Mean   :6.285   Mean   : 68.57  
# 3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000   3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08  
# Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000   Max.   :0.8710   Max.   :8.780   Max.   :100.00  
#      dis              rad              tax             medv      
# Min.   : 1.130   Min.   : 1.000   Min.   :187.0   Min.   : 5.00  
# 1st Qu.: 2.100   1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.02  
# Median : 3.207   Median : 5.000   Median :330.0   Median :21.20  
# Mean   : 3.795   Mean   : 9.549   Mean   :408.2   Mean   :22.53  
# 3rd Qu.: 5.188   3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:25.00  
# Max.   :12.127   Max.   :24.000   Max.   :711.0   Max.   :50.00  

# From the cor() command between "medv" variable and other variables, we can note
# that there is a certain degree of correlation with:
#  - rm     (positive linear correlation,  0.69)
#  - zn     (positive linear correlation,  0.36)
#  - indus  (negative linear correlation, -0.48)
#  - crim   (negative linear correlation, -0.38)
#  - nox    (negative linear correlation, -0.42)
#  - age    (negative linear correlation, -0.37)
#  - rad    (negative linear correlation, -0.38)
#  - tax    (negative linear correlation, -0.46)

simple_lr = lm(medv~nox, data=df)
simple_lr

plot(simple_lr)

plot(df$nox, df$medv, main = "Regressione lineare", xlab = "medv", ylab = "nox")
abline(simple_lr, col = "red", lwd = 2)

multi_lm = lm(medv~nox+crim+rm, data=df)
multi_lm

Mx = as.matrix(df[c("nox", "crim", "rm")])
Mx = t(Mx)  # necessario, altrimenti dobbiamo applicare l'altra convenzione per il calcolo delle beta

vy = as.numeric(df[["medv"]])

Mx <- rbind(1, Mx)   # necessario, altrimenti non riusciamo a calcolare b0 (che sarebbe quindi considerata implicitamente =0)

betas <- solve(Mx %*% t(Mx)) %*% Mx %*% vy
betas # si trova, daje roma

reduced_lm = lm(medv~crim+rm, data=df)

s3d <- scatterplot3d(df$crim, df$rm, df$medv,
                     pch = 19, color = "gray30", angle = 45,
                     xlab = "crim", ylab = "rm", zlab = "medv",
                     main = "Iperpiano del modello di regressione")

# Aggiunge il piano di regressione
s3d$plane3d(reduced_lm, draw_polygon = TRUE,
            polygon_args = list(col = rgb(0.2, 0.4, 0.9, 0.4)))

