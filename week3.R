### 106022103 劉弘祥 2018/03/07
## Exercise 1
rm(list=ls())
ID <- 106022103
X <- ID%%100+5 # take last two numbers of ID numbers +5 to be x
ExpermientTimes <- c(100,200,500,1000,10000)


theory_unique_rate <- function(x){
# input x for the number of people
# return the rate of theory rate which
# all people have different bitrhday.
      if(x>=365) return (0)
      else return (prod(365:(365-x+1))/(365^x));
}

experiment_unique_rate <- function(x,n=100){
# input the x for the number of people 
# input the n for the times we erpermient
# return the rate of the empermient rate which
# all people have different birthday.
   Sum <- 0
   for (i in 1:n){
      x <- sample(1:365,x,replace=T) # random select date in 365 days
      if(length(x)>length(unique(x))) Sum <- Sum+1 # if there is not exactly all  unique,Sum+1
   }
   return(Sum/n)
}


for (j in 1:length(ExpermientTimes)){
   print(c("Expermient for",ExpermientTimes[j],"times"))
   print(c("Theory",theory_unique_rate(X)))
   # print(theory_unique_rate(X))
   print(c("Expermient",experiment_unique_rate(X,ExpermientTimes[j])))
   # print(experiment_unique_rate(X,ExpermientTimes[i]))
   
}




##Exercise 2
BIRTH=0930
setwd("D:/NTHU/Course/統計資料分析")
DBH <- read.table("DBH.txt",header=T)
DBH <- as.vector(DBH[,1])#convert the table to vector
sample_DBH <- sample(DBH,BIRTH,replace=F)#use BIRTH the be the numbers of sample
mean(sample_DBH)
median(sample_DBH)
pracma::Mode(sample_DBH)# should install pracma firtst
sd(sample_DBH)# Stand Error
var(sample_DBH)# variance of sample
min(sample_DBH)
max(sample_DBH)
sum(sample_DBH)
length(sample_DBH)
range(sample_DBH)
moments::skewness(sample_DBH) #樣本的偏態
moments::kurtosis(sample_DBH) #樣本的峰態
exp(mean(log(sample_DBH))) # Geometric Average
PercentPoint <- quantile(sample_DBH,c(0,0.1,0.25,0.5,0.75,0.9,1))#10 25 50 75 90 %
table(cut(sample_DBH,breaks = PercentPoint))#count the total numbers of each group
hist(sample_DBH,breaks = PercentPoint)


