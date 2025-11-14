#
# Exercise on R basic commands
#
# Load the Auto dataset using read.table
# Verify that therea re no missing data or NA data in the dataset (you can use 
# the na functionalities). Check the contents of the dataset in terms of its
# features (try to use the function summary).
#
#  1. Produce a plot showing the miles per gallon vs the number of cylinders
#  2. Show the histogram of the miles per gallon together with the est. density
#     of the miles per gallon (use the function hist and density)
#  3. Check what is a boxplot and produce a boxplot for the miles per gallon
#  4. Use the pairs command to plot pairwise scatter plots among the dataset
#     features.
#
#  Comment on that you observe.

setwd("-------/Data Science/Exercises/.")
df = read.csv("./Auto.csv", header=T, na.strings="?")

# to get dataset columns without dollar notation (i.e. mpg instead of df$mpg)
# attach(df)

df = na.omit(df)
summary(df)

# Point 1.
mpg = df$mpg
cyl = df$cylinders

plot.new()
plot(cyl, mpg)


# Point 2.
hist(mpg,
     breaks = 20,               # number of bins
     col = "lightgray",
     border = "white",
     probability = TRUE,        # scale y-axis to density - otteniamo una distribuzione di probabilità empirica
     main = "Histogram and Density of Miles per Gallon",
     xlab = "Miles per Gallon")

# Add estimated density line
lines(density(mpg),
      col = "steelblue",
      lwd = 2)


# Point 3.
plot.new()
boxplot(mpg,
        main = "Leclerc of Miles per Gallon",
        ylab = "Miles per Gallon",
        col = "lightblue",
        border = "gray40")

# Point 4.
plot.new()
plot(df)

plot.new()
pairs(df[c(1, 3, 4, 5, 6, 7)]) # removed "useless" variables and string vars.




