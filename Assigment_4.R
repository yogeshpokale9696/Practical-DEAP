#Data Exploration and Data Preparation

#Assignment 04: Continuous random variate generation
-------------------------------------------------------------------------------------------------------
install.packages("nortest")
library(nortest)

#Q.01

beta.rejection <- function(f, c, g, n) {
naccepts <- 0
result.sample <- rep(NA, n)
while (naccepts < n) {
y <- runif(1, 0, 1)
u <- runif(1, 0,1)
if ( u <= f(y) / (c*g(y)) ) {
naccepts <- naccepts + 1;
result.sample[naccepts] = y
}
}
result.sample
}
f <- function(x) 60*(x^3)*(1-x)^2
g <- function(x) x/x
c <- 2.0736
result <- beta.rejection(f, c, g, 10000)
result;
print(mean(result))
hist(result,freq = FALSE)
points(seq(0,1,0.01),dbeta(seq(0,1,0.01)
,4,3),type = "l")

-------------------------------------------------------------------------------------------------------

#Q.02

n=1000;
theta=1000;
U=c(runif(n,min=0,max=1));U
X=c(-theta*log(1-U));X

sMean = mean(X); sMean
pMean = theta; pMean

fr = table(X)/n ; fr
entropy = sum(-fr*log(fr)/log(2));entropy

-------------------------------------------------------------------------------------------------------

#Q.03

size = 100;
mean=45000;
sd=2200;
var=(sqrt(2200));var

U1=c(runif(size,min=0,max=1));U1
U2=c(runif(size,min=0,max=1));U2

Z1=c((sqrt(-2*log(U1)))*(cos(2*3.14*U2)));Z1
Z2=c((sqrt(-2*log(U1)))*(sin(2*3.14*U2)));Z2

X1=c(mean+Z1*var);X1
X2=c(mean+Z2*var);X2


fr1 = table(X1)/size ; fr1
entropy1 = sum(-fr1*log(fr1)/log(2));entropy1


fr2 = table(X2)/size ; fr2
entropy2 = sum(-fr2*log(fr2)/log(2));entropy2

-------------------------------------------------------------------------------------------------------

#Q.04

n = 300;
mean=350;
sd=30;
var=(sqrt(30));var

U1=c(runif(n,min=0,max=1));U1
U2=c(runif(n,min=0,max=1));U2

V1=c(2*U1-1);V1
V2=c(2*U2-1);V2

w=c((V1^2)+(V2^2));w

for(i in w){
if(i <= 1){y=(sqrt((-2*log(w))/w))}}

print(y)

Z1=V1*y;Z1
Z2=V2*y;Z2

X1=mean+Z1*var;X1
X2=mean+Z2*var;X2

fr1 = table(X1)/n ; fr1
entropy1 = sum(-fr1*log(fr1)/log(2));entropy1

fr2 = table(X2)/n ; fr2
entropy2 = sum(-fr2*log(fr2)/log(2));entropy2

-------------------------------------------------------------------------------------------------------

#Q.05

n=30;
a=8.00;
b=8.30;

U=c(runif(n,min=0,max=1));U

X=a+(b-a)*U;X

sMean = mean(X); sMean
pMean = (a+b)/2; pMean

fr = table(X)/n ; fr
entropy = sum(-fr*log(fr)/log(2));entropy

-------------------------------------------------------------------------------------------------------

#Q.06

#Q1

n=10
for(i in 1:n){
size = 1000;
mean=45000;
sd=2200;
var=(sqrt(2200));var

U1=c(runif(size,min=0,max=1));U1
U2=c(runif(size,min=0,max=1));U2

Z1=c((sqrt(-2*log(U1)))*(cos(2*3.14*U2)));Z1
Z2=c((sqrt(-2*log(U1)))*(sin(2*3.14*U2)));Z2

X1=c(mean+Z1*var);X1
X2=c(mean+Z2*var);X2


fr1 = table(X1)/size ; fr1
entropy1[i] = sum(-fr1*log(fr1)/log(2))



fr2 = table(X2)/size ; fr2
entropy2[i] = sum(-fr2*log(fr2)/log(2))
}

cat("Entropy  X1")
entropy1



cat("Entropy  X2")
entropy2
---------------------------------------------------------

#Q2
fmean=array()
for(i in 1:10){
n=1000
beta.rejection <- function(f, c, g, n) {
naccepts <- 0
result.sample <- rep(NA, n)
while (naccepts < n) {
y <- runif(1, 0, 1)
u <- runif(1, 0,1)
if ( u <= f(y) / (c*g(y)) ) {
naccepts <- naccepts + 1;
result.sample[naccepts] = y
}
}
result.sample
}
f <- function(x) 60*(x^3)*(1-x)^2
g <- function(x) x/x
c <- 2.0736
result = beta.rejection(f, c, g, n)
result;
fmean[i]=mean(result)
}

fmean
---------------------------------------------------------------------------
#Q3


n=10 #Sample size
size=1000
mean=9
sd=4
var=sqrt(4);var

U=c(runif(1000,min=0,max=1));U1
U1=U[0:501];U1
U2=U[500:1000];U2


sn=seq(1,n);sn##Sample number
m1=6;sd=2;no=0;
z=rep(0,n);
x=rep(0,n);x
y=rep(0,n);y
u=rep(0,n);v=rep(0,n);m=rep(0,n);f
1=rep(0,n);g1=rep(0,n);gf=rep(0,n);

u=runif(1,0,1) # To generate sample
from C(0,1)
x=tan(u*pi) # Sample point of
C(0,1)

f1=(1/sqrt(2*pi))*exp(-0.5*x*x);
#value of pdf f1(N(0,1)) at point x

g1=1/(pi*(1+x*x)); # value of pdf
g1 (C(0,1)) #at point x

m= sqrt(exp(1)/(2*pi));m



gf=f1/(m*g1);gf

v=U2;v# To check condition

if (v[1] <= gf) {z[no] = x; no=no+1;}

z; # Vector of sample from N(0,1)

y=m1+z*sd;y #Sample from N(6,4)
y
d4=data.frame("Sample
No."=sn,"Z~N(0,1)"=z,"Y~N(6,4)"=y);d4

---------------------------------------------------------------------
dummy Q.6 .3

n=10
size=1000
mean=9
sd=4
var=sqrt(4);var

U=c(runif(1000,min=0,max=1));U1
U1=U[0:500];U1
U2=U[500:1000];U2
x=tan(U1*pi);x
f1=(1/sqrt(2*pi))*exp(-0.5*x*x);f1
g1=1/(pi*(1+x*x));g1 # value of pdf

m= sqrt(exp(1)/(2*pi));m
gf=f1/(m*g1);gf

for(j in 0:500){
if (U2[j] <= gf) {z[j] = x;}
}
z; # Vector of sample from N(0,1)


-------------------------------------------------------------------------

#Q4

n <- 1000
u <- runif(n)
x.vect <- numeric(n) # inititating vector space in R's memory

for (i in 1:length(u)) {
  if (u[i] < .5) {
    x.vect[i] <- log(2*u[i])
  }
 
  else {
    x.vect[i] <- -1*log(2*(1-u[i]))
  }
}

hist(x.vect, freq = F, ylim = c(0, 0.5), xlab = "x",main= expression(f(x)==frac(1,2)*e^{-abs(x)}))

# curve draws a function of x; if add = T it uses x-values on the previous
# plot's x-axis
curve(.5*exp(-abs(x)), col = "red", add = T)

# lines takes a vector of x & y ('density' provides both vectors)
lines(density(x.vect), lty = 2, lwd = 2)