# 2. Simulate the life time of 1000 electronic component which has exponential distribution
#with mean 1000 hrs. Also find sample mean and compare with population mean.
#Calculate entropy of obtained sample and determines how much real and CPU time
#(in seconds) the currently running R process has already taken.

#Given 
N=1000
mean=1000 #hrs
R=runif(1, min=0, max=1);R
p=1-exp(-1/mean);p

T=-mean*log((1-R), base = exp(1));T

for(i in 1:N)
{
Y=log((1-R), base = exp(1))/log((1-p), base = exp(1))
}
Y