#Test for random numbers

#Q1

#install.packages("randtests")
library(randtests)
los = 0.05

data = c(0.324,0.485,0.724,0.624,0.135,0.821,0.514,0.635,0.292,0.443,0.356,0.629); data
data1= runs.test(data); data1

pval = data1$p.value ; pval

if(pval < los){cat("Genarated number are not random")} else {cat("Genarated numbers are random")}

------------------------------------------------------------------------------------------

#Q2

#install.packages("randtests")
library(randtests)

los = 0.05
data = c(0.41,0.68,0.89,0.94,0.74,0.91,0.55,0.62,0.36,0.27,0.19,0.72,0.75,0.08,0.54,
0.02,0.01,0.36,0.16,0.28,0.18,0.01,0.95,0.69,0.18,0.47,0.23,0.32,0.82,0.53,0.31,0.42,0.73,0.04,0.83,0.45,0.13,0.57,0.63,0.29);data

data2= runs.test(data); data2

pval = data2$p.value ; pval

if(pval < los){cat("Genarated number are not random")} else {cat("Genarated numbers are random")}

------------------------------------------------------------------------------------------

Q3

N= 1000;N
los = 0.05;los
condition = c(0.72,0.27,0.01); condition
ObsFrq = c(680,289,31); ObsFrq
n = length(ObsFrq); n
Ei = c(N * condition);Ei
Tcal  = (sum((ObsFrq^2)/Ei)-N);Tcal
pval = pchisq(Tcal,df = n-1, lower.tail=F);pval

if(pval < los){cat("Genarated number are not random")} else {cat("Genarated numbers are random")}

-------------------------------------------------------------------------------------------

Q4

a=13;a
c= 3;c
m = 26;m
los = 0.05;

lcg = function(a,c,m,len,seed)
{
  x = rep(0,len)
  x[1] =seed
  for(i in 1 :(len - 1)){

		x[i+1] = (( (a * x[i]) + c)%% m); x
}
 Ri = x/m; Ri
 return(list(x=x,Ri = Ri)) 
}

seq1 = c(lcg(a,c,m,30,1));seq1

R1= runs.test(seq1$Ri);R1

pval = R1$p.value ; pval

if(pval < los){cat("Genarated number are not random")} else {cat("Genarated numbers are random")}


seq2 = c(lcg(a,c,m,30,2));seq2

R1= runs.test(seq2$Ri);R1

pval = R1$p.value ; pval



seq3 = c(lcg(a,c,m,30,2));seq3

R1= runs.test(seq3$Ri);R1

pval = R1$p.value ; pval

if(pval < los){cat("Genarated number are not random")} else {cat("Genarated numbers are random")}


seq4 = c(lcg(a,c,m,30,2));seq4

R1= runs.test(seq4$Ri);R1

pval = R1$p.value ; pval

if(pval < los){cat("Genarated number are not random")} else {cat("Genarated numbers are random")}




--------------------------------------------------------------------------------------------

#Q.05

cat("To test\n H0: Generated numbers are Indpendent Against  H1: Generated numbers are dependent")

los=0.05
n= 1000;n
condition=c(0.504,0.432,0.027,0.036,0.001);condition
ObsFre = c(560,394,32,13,1);ObsFre
ExpFre = c(condition * n);ExpFre
Tcal =sum(((ObsFre-ExpFre)^2)/ExpFre);Tcal
pv = pchisq(Tcal, df = (length(ObsFre)-1), lower.tail = F);pv

if(pv < los){cat("Reject H0 i.e. Generated numbers are dependent")}else{cat("Accept H0 i.e. Generated numbers are independent")}

-------------------------------------------------------------------------------------------------------

#Q.06

cat("To test\n H0: Generated number follows U(0,1) against  H1: Generated numbers do not follows U(0,1)")

a=125;a
m=2^12;m
n=1000;n
c=1;c
los=0.05
lcg = function(a,c,m,len,seed){
x=rep(0,len)
x[1]=seed
for(i in 1:(len-1)){
x[i+1] =  (a * x[i] +c) %% m
}
u = x/m
return(list(x=x,u=u))
}

data=lcg(a,c,m,1000,1);data
x=data$u;x

n1=0;n2=0;n3=0;n4=0;n5=0;n6=0;n7=0;n8=0;n9=0;n10=0;

for(i in 1:n)
{
if(x[i]<0.1){n1=n1+1}
if((x[i]>=0.1)&&(x[i]<0.2)){n2=n2+1}
if((x[i]>=0.2)&&(x[i]<0.3)){n3=n3+1}
if((x[i]>=0.3)&&(x[i]<0.4)){n4=n4+1}
if((x[i]>=0.4)&&(x[i]<0.5)){n5=n5+1}
if((x[i]>=0.5)&&(x[i]<0.6)){n6=n6+1}
if((x[i]>=0.6)&&(x[i]<0.7)){n7=n7+1}
if((x[i]>=0.7)&&(x[i]<0.8)){n8=n8+1}
if((x[i]>=0.8)&&(x[i]<0.9)){n9=n9+1}
if((x[i]>=0.9)&&(x[i]<1)){n10=n10+1}
}

ObsFre=c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10);ObsFre
sumob=sum(ObsFre);sumob
k=length(ObsFre);k
p=1/k;p
ExpFre=rep((sumob*p),k);ExpFre

Tcal =sum(((ObsFre-ExpFre)^2)/ExpFre);Tcal
pv = pchisq(Tcal, df = (length(ObsFre)-1), lower.tail = F);pv

if(pv <los){cat("Reject H0 i.e. Generated numbers dont follows U(0,1)")}else{cat("Accept H0 i.e. Generated numbers fllows U(0,1)")}
-------------------------------------------------------------------------------------------------------

#Q.07


cat("To test\n H0: Generated number follows U(0,1) against  H1: Generated numbers do not follows U(0,1)")

a=1103515245;a
m=2^32;m
c=12345;c
los=0.05

lcg = function(a,c,m,len,seed){
x=rep(0,len)
x[1]=seed
for(i in 1:(len-1)){
x[i+1] =  (a * x[i] +c) %% m
}
u = x/m
return(list(x=x,u=u))
}

data=lcg(a,c,m,30,7);data

ans= ks.test(data$u,"punif",0,1);ans
pv=ans$p.value;pv
tcal=ans$statistic;tcal

if(pv <los){cat("Reject H0 i.e. Generated numbers dont follows U(0,1)")}else{cat("Accept H0 i.e. Generated numbers fllows U(0,1)")}

------------------------------------------------------------------------------------------------------------------------------------

