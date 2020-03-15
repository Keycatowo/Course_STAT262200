Toss.CLT <- function(n){
   sd <- sqrt(n/4)
   par(mfrow=c(1,2))
   p <- dbinom(seq(0,n),n,0.5)
   barplot(p,col=3,width = 1,space = 0,ylim = c(0 ,0.5),names.arg = seq(0,n),
           xlab = 'Number of Heads',ylab = 'PCT per Head',main=paste(n,'','TOSSES'))
   index <- matrix(0,(n/sd),sd+1)
   for(i in 1:(n/sd)){
      index[i,] <- seq(sd*(i-1),sd*i)
   }
   index <- index + 1
   res <- apply(index,1,function(x){
      y <- rep(1,length(x))
      y[1] <- y[length(x)] <- 0.5
      sum(p[x]*y)
   })
   plot(seq(-4,4,0.1),dnorm(seq(-4,4,0.1)),type = "l",lwd=2,
        xlab = 'Number of SD',ylab = 'PCT per SD',ylim=c(0,0.5),main = paste0(n,'','TOSSES'))
   num <- n/sd/2
   x <- c(sort(-seq(0.5,(num-1)+0.5,1)),seq(0.5,(num-1)+0.5,1))
   for(i in 1:length(x)){
      rect(x[i]-(0.5-0.05), 0, x[i]+(0.5-0.05),res[i],border = 3, col = 3)
   }
   lines(seq(-4,4,0.1),dnorm(seq(-4,4,0.1)),lwd = 2)
}


Toss.CLT(1)
Toss.CLT(2)
Toss.CLT(4)
Toss.CLT(8)
Toss.CLT(16)
Toss.CLT(32)
Toss.CLT(64)
Toss.CLT(128)
Toss.CLT(256)


observed <- c(315,108,101,32)
ect <- sum(observed)*c(9/16,3/16,3/16,1/16)

qchisq(0.95,8)

dpois(lambda = 0.7)



## 3
observed <- c(1968,1919,2042,1932,2003,2136)
sum(observed)
qchisq(0.95,5)



## 4

observed <- c(7,9,73,51,80,15,8)
cum <- pnorm(seq(15.25,17.75,0.5),mean = 16.5,sd = 1)
p <- c(pnorm(15.25,mean = 16.5,sd = 1),
       diff(cum),1-pnorm(17.75,mean = 16.5,sd = 1))
expectation <- sum(observed)*p

