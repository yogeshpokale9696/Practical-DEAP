#Assignment -2

#install.packages("xts",dependency=TRUE)

#(data=women)
# Data Visualization Techniques

# Q.1
library(tidyverse)
library(ggplot)
(data=mpg)


df=data.frame(data);df
#no of rows
print(nrow(df))

#no of Columns
print(ncol(df))

#What does the drv variable describe?
# f= front wheel ,r = rear wheel , 4 = All wheel drive

df1=select(df, c('hwy','cyl'));df1
plot(df1)
#What happens if you make a scatterplot of class vs drv? Why is the plot not useful?
#--->Scatterplot requires continous valus that's why if we plot then plot will be blank insted of we  can use ggpllot to plot on descriptive value
ggplot(df,aes(x=class,y=drv))+geom_point()

# Q.2

dataw=women;dataw
#What do you see?
#---->Height and Weight oof womens
dfw=data.frame(dataw);dfw
#no of rows
print(nrow(dfw))
# no of colums 
print(ncol(dfw))

plot(dfw)

# plot a scatter plot
plot(dfw$height,dfw$Sweight, main='Regression for height and weight of women', xlab='Height',ylab='Weight')
  
# plot a regression line

r1=lm(weight~height,data=dataw)
cr=coef(r1);cr
mcr=matrix(cr);mcr
a=mcr[1,1];a
b=mcr[2,1];b
x1=2;ey=a+b*x1
esty=fitted(r1)
dfw=data.frame(dataw);dfw
d=data.frame(dfw$height,dfw$weight,esty);d
if(b>0){cat("Equation is Y=",a,"+",b,"*X","\n");}else{cat("Equation is Y=",a,"-",b,"*X","\n");}
cat("Estimate of Y on X=",x1,"is=",ey,"\n")



 line <-lm(weight~height,data=dataw)


ggplot(dataw, aes(height, weight)) +geom_point()+ stat_smooth(method =lm)


# Q.3

datai=iris;data
dfi=data.frame(datai);dfi
#no of rows
print(nrow(dfi))
# no of colums 
print(ncol(dfi))

plot(dfi,col="red")

# plot a regression line
r1=lm(Sepal.Length~Sepal.Width,data=datai)
cr=coef(r1)
mcr=matrix(cr);mcr
a=mcr[1,1];a
b=mcr[2,1];b
x1=2;ey=a+b*x1
esty=fitted(r1)
dfi
d=data.frame(dfi$Sepal.Length,dfi$Sepal.Width,esty);d
if(b>0){cat("Equation is Y=",a,"+",b,"*X","\n");}else{cat("Equation is Y=",a,"-",b,"*X","\n");}
cat("Estimate of Y on X=",x1,"is=",ey,"\n")




 line <-lm(Sepal.Length~Sepal.Width,data=datai)


ggplot(datai, aes(Sepal.Length, Sepal.Width)) +geom_point()+ stat_smooth(method =lm)

#

line <-lm(Petal.Length~Petal.Width,data=datai)


ggplot(datai, aes(Petal.Length,Petal.Width)) +geom_point()+ stat_smooth(method =lm)


# Q.4

--->
#1.The plots are a time-series plot and a sparkline.

#2.A noticeable weekly cycle; probably they have to assignments

#A sustained, high level of traffic in the first week February - maybe a midterm test.

# Q.5
print(getwd())
setwd("C:"
setwd("C:/Users/User6/Desktop/MSc(CS) Part-1/Yogesh Pokale/DEAP")
print(getwd())
data <- read.csv("Real_estate.csv")
print(data)
pairs(data,col="orange")

plot(data$X4.number.of.convenience.stores,data$Y.house.price.of.unit.area,col="orange")

# When number.of.convenience.stores increses then house.price.of.unit.area increases respectively

# house.price.of.unit.area Is also depends on langitude and latitude (Place)


#Q.6

data <- read.csv("website-traffic.csv")
print(data)


day.names = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday","Thursday", "Friday" )
days = factor(data$DayOfWeek, level=day.names);
boxplot(data$Visits ~ days)

#2

# A better plot using the xts library
library(xts)
date.order = as.Date(web$MonthDay, format=" %B %d")
web.visits = xts(web$Visits, order.by=date.order)
plot(web.visits, major.format="%b")

# Q.7
data <- read.csv("six-point-board-thickness.csv")
df=data.frame(data,100)
print(df[0:100,])
df1=df[0:100,2:7]
boxplot(df1,xlab="Positions",ylab="Thickness(one thousands of an inch)")



summary(data )

plot(data [1:100,2], type='l')
plot(data [1:100,5], type='l')
first100 <- data [1:100, 2:7]# Ingnoring date time column

boxplot(first100, ylab="Thickness [mils]")

#The thick center line on each boxplot is the median of that variable

# it is not symmetric distribution means data is either positively or negatively
#skewed

# Q.8
families = c("family1","family2","family3");families
  food = c(5000,7000,18000);food
  Rent = c(3000,4000,15000);Rent
  Clothing = c(3000,3000,10000);Clothing
  Education = c(1500,4000,8000);Education
  Misc = c(500,2000,8000);Misc

  colors = c("green","orange","brown","yellow","blue")
  families = c("family1","family2","family3")
  items of expenditure = c("food","Rent","Clothing","Education","Misc")

  Values = matrix(c(food,Rent,Clothing,Education,Misc), nrow = 5 , ncol= 3 , byrow =TRUE);Values 
  barplot(Values , main = "total revenue" , names.arg = families , xlab = "families" , ylab = "Items of expenditure" , col = colors, beside = T)
  legend(locator(1) ,legend=c("food","Rent","Clothing","Education","Misc"), fill = colors)
  



# Q.9
 colors = c("green","orange","brown","yellow","blue")
  Product = c("Product1","Product2","Product3","Product4")
  Year = c("1992","1993","1994","1995","1996")

  Values = matrix(c(1443,2465,1471,4483,2450,2424,3438,1449,4506,3483,432,446,476,622,454,999,3593,2665,1679,2483), nrow = 4 , ncol= 5 , byrow =TRUE)
  barplot(Values , main = "Wholesale Production" , names.arg = Year , xlab = "Year" , ylab = "Product" , col = colors, beside = TRUE)
  legend(locator(1) ,legend=c("Product1","Product2","Product3","Product4"), fill = colors)
  
