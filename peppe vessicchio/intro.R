# Create an array (combination of values):
arr = c(1, 2, 3, 7, 11)
arr   # print values on console

# Defining sequences:
y = seq(from=4, length=4, by=1)
y

z = 4:7
z

# To get some help on a command:
?seq


## Matrices

# Let's create a 2x2 matrix
x = matrix(data=c(1,2,3,4), nrow=2 , ncol=2)
x

# Same thing, with positional parameters
x = matrix(data=c(1,2,3,4), 2 ,2)
x

# Let's lay out the given data by rows
x = matrix(data=c(1,2,3,4), 2 ,2, byrow=T)
x

x%*%x             # matrix product
crossprod(x, x)   # matrix cross product
tcrossprod(x, x)   # matrix product with the second one transposed
cl



## Examples!

A = matrix(1:16, 4, 4)
A[2, 3]           # take element at row 2 column 2
A[1:3, 2:4]       # submatrix from starting to ending defined indexes
A[1:2,]           # first two rows with all the columns
A[1,]             # first row with all columns
A[c(1,3),c(2,4)]  # first and third row, with second and fourth column

test1 <- A[1,1:4, drop=FALSE]   # this would be still a matrix
dim(test1)
test1

test2 <- A[1,1:4, drop=TRUE]    # this will become a vector
dim(test2)
test2

A[,c(2,4)]        # remove second and fourth column

a1 = array(1:24, c(2, 3, 4))  # multiway array, with dimensions 2x3x4 (a tensor)
a1    # not visible in workspace, but can be printed out on console

a1[1:2, 1:2, 4]    # first two rows and column, of the fourth matrix

which(x=='b')      # which indexes of x are equal to b?

myvec=c(9,5,4,9,11,2,12)
table(myvec)       # returns occurrences of each distinct element in the given object


v1=c(1:8)
v2=c(11:20)

# create matrices from single arrays
m1=cbind(v1, v2); m1    # each array is a column
m2=rbind(v1, v2); m2    # each array is a row


## Dataframes!

mydf = data.frame(id=c(1,2,3), name=c('Mario', 'Giulia', 'Fabio'), sex=c('m', 'f', 'm'), age=c(20, 24, 22))
mydf$age
mydf[-c(2,3)]
mydf[mydf$sex == 'm', ]
mydf[order(mydf$name), ]
colnames(mydf)[2] = 'names'
summary(mydf)
str(mydf)


## Random numbers!

set.seed(1303)    # to reproduce a specific behaviour of our distribution
rnorm(50)         # equivalent to matlab's randr

set.seed(3)
y = rnorm(100)

mean(y)           # distribution mean
var(5)            # variance
sqrt(var(y))      # std. deviation
sd(y)             # std. deviation, same



## Functions!

library(tictoc)

myfun_sum = function(x, y) {
  x+y   # returned values is last evaluation made in the function block
}
myfun_sum(5, 2)

a = 0
tic()
for (i in 1:10^7) a[i]<-sin(2*pi*i/sqrt(2))
toc()

a <- numeric(10^7)
tic()
for (i in 1:10^7) a[i]<-sin(2*pi*i/sqrt(2))
toc()


## Graphs!

x = rnorm(50)
y = x + rnorm(50, mean=50, sd=.1)
cor(x, y)
plot(x, y)
lm(y~x)

abline(lm(y~x), col='blue')
abline(50, 0.99, col='red')


# Create and save plots:
x = rnorm(100)
y = rnorm(100)

dev.new()
plot(x, y)
plot(x, y, xlab='this is the x-axis', ylab="this sis the y-axis", main="Plot of X vs Y")
pdf("Figure.pdf")
plot(x, y, col="green")
dev.off()
png("Figure.png")
plot(x, y, col="red")
dev.off()
dev.print(device=pdf, "FigBlack.pdf")

x = seq(-pi, pi, length=50)
y = x

f = outer(x, y, function(x, y) cos(y)/(1-x^2))
contour(x, y, f)
dev.print(device = pdf, "cont1.pdf")
contour(x, y, fa, n)













