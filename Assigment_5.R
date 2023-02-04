#Assignment-5

#Q1
# Binomial by direct method
n = 100
p = rep(9,0);P=rep(9,0)
for(j in 1:9)
 {
 p[j] = dbinom(j,8,0.6)#p(p(x),size,prob)
 } 
 P = cumsum(p);P
 y = rep(0,n);r = rep(0,n)
 for(i in 1:n)
 {
  P = cumsum(p)
  X = c(0,1,2,3,4,5,6,7,8);
  counter =1;
  r = runif(1,min = 0,max =1);
  while(r>P[counter])
  counter = counter + 1;
  end
  y[i] = X[counter]
 }
y
table(y)
fq = table(y)/n;fq
entropy = sum(-fq*log(fq)/log(2));entropy
#hist(y,main="Random Sample",xlab="X Range",border="purple")
ptm = proc.time();ptm

# Binomial by convulation method

N = 10000
x = rep(0,N)
n = 8;p=0.6
for(i in 1:N)
{
 u = rep(0,n);y = rep(0,n);
 for(j in 1:n)
 {
  u[j] = runif(1,0,1);u[j]
  if(u[j] <= p){y[j] = 1} else{y[j]=0}
 }
 y;x[i]=sum(y)
}
x; 
table(x);
fq = table(x)/N;fq
entropy = sum(-fq*log(fq)/log(2));entropy

# Q.2
# by direct method

n = 1000
p = rep(0,n)
for(i in 1:20)
{
 p[i] = 0.65*(0.35^i);p[i]
}
p = c(0.65,p);p
sum(p)
y = rep(0,n);r=rep(0,n)
for(i in 0:n)
{
 P=cumsum(p);
 X = c(0,1,2,3,4,5,6,7,8,9,10);
 counter = 1;
 r = runif(1,0,1);
 while(r > P[counter])
 counter = counter +1 ;
 end
 y[i] = X[counter]
}
y
table(y);
mean1 = mean(y);mean1
pmean = 0.35/0.65;pmean
 fq = table(y)/n;fq
entropy = sum(-fq*log(fq)/log(2));entropy

#using exponential method

n = 1000
y = rep(0,n);r=rep(0,n)
p = 0.65
for(i in 1:n)
{
 r = runif(1,0,1)
 y[i] = round((log(1-r)/log(1-p)),0)
}
y
mean1 = mean(y);mean1
pmean = 0.35/0.65;pmean
fq = table(y)/n;fq
entropy = sum(-fq*log(fq)/log(2));entropy

# Q.3

 n1 = 25
p = rep(0,n1)
p1=exp(-5);p1
for(i in 1:n1)
{
 p[i]=exp(-5)*5^i/factorial(i);p[i]
}
p =c(p1,p);p;sum(p)
for(j in 1:10)
{
 n = 1000
 y = rep(0,n);r=rep(0,n)
 for(i in 0:n)
{
 P= cumsum(p);
 x = seq(1,n1);x
 X = c(0,x);
 counter = 1;
 r= runif(1,0,1)
 while(r > P[counter])
 counter = counter + 1;
 end
 y[i] = X[counter]
}
y
mean1[j] = mean(y);
fq = table(y)/n;fq
entropy[j]= sum(-fq*log(fq)/log(2));entropy[j]
}
mean1
pmean = 5;pmean
entropy
meanentropy = mean(entropy);meanentropy

