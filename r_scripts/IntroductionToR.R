
# Library / package
library(datasets)

# Calculator 
x=10^2
y=2*x

# Vectors
vec1=c(1,21,50,80,45,0)
sum(vec1)
mean(vec1)
sd(vec1)
summary(vec1)
var(vec1)

# Matrices
ma1=matrix(data=vec1,nrow = 4)

# Data frames
t=data.frame(x=rnorm(100),y=rnorm(100,mean = 5))
summary(t)
x=t$x
y=t$y

# R Datasets
mdata=cars

# Reading variables from a data.frame
with(mdata,mean(speed+dist))

mean(mdata$speed+mdata$dist)

# Structure and Summary
str(mdata)
summary(mdata)
print(mdata)
View(mdata)

# Creating new variables 
mdata$time=mdata$dist/mdata$speed

mdata$speed2=mdata$speed^2

# Rename the variables 
library(reshape)
mdata=rename(mdata,c("speed"="mspeed", "speed2"="s2"))

# Create categories from continuous variable
mdata$newdata= (mdata$mspeed >=5) + (mdata$mspeed >=10) + (mdata$mspeed >=15)  +(mdata$mspeed >=20) 
mdata$ndata= cut(mdata$mspeed, breaks = c(4,5,10), labels = c("cut0","cut1"), right = FALSE)

# Creating factors
x=c(0,0,1,1,2)
x=factor(x,labels = c("Control","Low Dose","High Dose"))

# Drop or keep variables in a dataset
library(datasets)
library(carData)
mdata=Salaries

submdata=subset(mdata,select = c("discipline","salary"))
submdata1=mdata[c(1:100),-c(1,2)]
submdata2=subset(mdata,select = c(-2,-3))

# Keep Unique values
uni=unique(mdata)

# Identify duplicated values
dup=duplicated(mdata)

# stack datasets
newdata=cbind(rnorm(50),rnorm(50))
rb=rbind(rnorm(50),rnorm(50))


# Merge datasets
ds1=cars
ds1$s2=cars$speed**2
ds1$dist=NULL
ds=merge(ds1,cars,by = "speed",all=TRUE)


# Prabability distributions
mdata=rnorm(100,mean = 0,sd=1)
hist(mdata,freq = TRUE)
plot(mdata,
     type="o",
     col="blue",
     xlim = c(-1,100),ylim = c(-3,4),
     xlab = "Number of Random Generation",
     ylab = "Values", 
     main = "100 Obs. from Standard Normal Dist.")

boxplot(mdata,notch = T)
boxplot(cars)

dt=rnorm(1000000,mean=10,sd=3)
var(dt)
quantile(dt,c(0.25,0.5,0.75,0.99,0.9995))


library(moments)
skewness(dt)
kurtosis(dt)


hist(dt)
hist(scale(dt))

# Probability distributions
set.seed(5)
x=rnorm(100,mean=1,sd = 1)
xp=pnorm(1.96)
y= pbinom(2,size = 10,prob = 0.5)
y
q=qnorm(0.975)




set.seed(1)
x=runif(10,min = 0,max = 1)
x2=rnorm(10)

x3=rnorm(10)
rexp(10,rate = 1)


# Mathematical functions
min(rnorm(100))
max(rnorm(100))
abs(rnorm(100))
sqrt(abs(rnorm(100)))
2**9
2^9
exp(1)
log(1)
log10(10)
log2(2)
log(45,base = 45)

factorial(3)
choose(10,5)




# Generate some data
x<-1:10; y1=x*x; y2=2*y1
plot(x, y1, type="b", pch=19, col="red", xlab="x", ylab="y",
     las=1)
# Add a line
lines(x, y2, pch=18, col="blue", type="b", lty=3)
# Add a legend
legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)



