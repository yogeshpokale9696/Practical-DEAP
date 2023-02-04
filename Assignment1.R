
#Assingment 1 
 # 1. Create a vector named marks representing following markd obtained by 10 students in a class test..
  marks=c(15,27,30,12,29,9,35,25,28,16);marks
for(i in 1:length(marks)) { if(marks[i]>25){a=c(a,marks[i])}};a
#a=(marks>25)
#a
a=marks[marks>25];a # Values greater than 25.
#  2. creates a vector of numbers 1 to 39.
num=seq(1:39);num

# 3. Create a vector of numbers 5 reapeted 12 times.
repp=rep(5,12);repp

# 4 . Suppose x and y are two vectors containing elements 2,5,3,7,5,8,4,2,7 and 2,4,7 respectively.
x=c(2,5,3,7,5,8,4,2,7);x
y=c(2,4,7);y
# (1) . Create vectors x+y,x/y,x*y

add=x+y;add
div=x/y;div
mult=x*y;mult

# (2). Argument x by adding y to right 
x=c(x,y);x

# (3).Argument y by adding elements 4,3,2,5,8,9,12,15 at the end.
y=c(y,4,3,2,5,8,9,12,15);y

# (4). Create a vector z as addition of x and y
x;y
y=c(y,0)
z=x+y;z


# 5.create a vector x with elements 1,5,2,3,7,8,6 and create vectors y,z,w from x using y=x^2,z=1/x,w=log10x.  
x=c(1,5,2,3,7,8,6);x
y=x^2;y
z=1/x;z
w=log10(x);w

# 6. Suppose age is a vector containing ages of 10 persons as 22,27,31,41,30,25,19,20,23 and 35.
age=c(22,27,31,41,30,25,19,20,23,35);age
# (1).Acess age of fourth person
age[4]
# (2).Create a vector age30 with age of person>30.
age30=age[age>30];age30
# (3).Find the number of elements in vector age.
 len=length(age);len
# (4).Access age of last three person.
age[c((len-2),(len-1),(len))]
tail(age,3)
# (5) .Access ages of person except 5th and 7th.
age[-c(5,7)]
# (6).Create a vector age2 with age of persons between 20 to 25.
age2=age[20<age & age<25];age2

# 7.Following are Height in cms and Weight in Kg of 10 boys.

H=c(140,137,150,147,139,140,150,132,138,140);H
W=c(55,57,59,62,61,60,60,58,59,57);W
#  (1).Prepare a data Frame of Height and Weight.
dataf=data.frame("Height",H,"Weight",W);dataf

# (2).Create a vector of boys with height>145 cms.

a=H[H>145];a
#  (3)Create a vector of boys with Weight>55 Kg.
b=W[W>55];b
subset(dataf,H>140 && W>55)
# (4)Create a data frame of the boys with height >140 and Weight >60.
#d2=data.frame(H[H>140],W[W>55]);d2
d3=data.frame(subset(dataf,H>140 & W>60));d3
subset(dataf,H>140 & W>60)
# 8.Create a data frame of the following two vectors :
#  Add vector/variable named value =price*quantity in the created data frame.  
price=c(10,15,20,42,50,60);price
quantity=c(4,30,15,10,16,8);quantity
d3=data.frame(price,quantity);d3

value=price*quantity;value
#d3=data.frame(d3,value);d3
transform(d3,"Value"=value)

# 9.Draw scatter diagram and final correlation coefficient and write down commwnt 
x=c(56,42,72,36,63,47,55,49,38,42);x
y=c(147,125,160,118,149,128,150,145,115,140);y
cd=data.frame(x,y);cd
r=cor(d)
r1=lm(y~x)
cr=coef(r1)
mcr=matrix(cr);mcr
a=mcr[1,1];a
b=mcr[2,1];b
x1=2;ey=a+b*x1
esty=fitted(r1)
d=data.frame(x,y,esty);d
if(b>0){cat("Equation is Y=",a,"+",b,"*X","\n");}else{cat("Equation is Y=",a,"-",b,"*X","\n");}
cat("Estimate of Y on X=",x1,"is=",ey,"\n")
plot(d)
plot(x,y)

corr=cor(X,Y);corr
 # comment :- Strong Positive

#  10.Represent the following data on the lengths of stay in short term hospitals by 21 randmoly selected patients using box plot

#  4,4,12,18,9,6,12,3,6,15,7,3,55,1,10,13,5,7,1,23,9
stay=c(4,4,12,18,9,6,12,3,6,15,7,3,55,1,10,13,5,7,1,23,9);stay
summary(stay)
dput(summary(stay))
boxplot.stats(stay)
boxplot(stay,ylab="Stay of patients", col = "green",main="Hospital")
f=fivenum(stay);f
text(rep(1.19,5),f,labels=c("Min","Q1","Median","Q3","Max"))
text(2,1,"median")
#x=seq(1,10,2);x