### Week4 106022103
##Excecise 1
# Poission Distribution
# landa*n=1.29, k=0,1,2,3,4++
Sum <- 0
for (i in 0:3){
   p <- dpois(i,1.29)#Calculate the pdf for Poison(landa=1.29) at x=i
   print(c(i,p,p*100))
   Sum <- Sum+p
}
   print(c("4+",1-Sum,(1-Sum)*100))
   
##Exercise 2
ExperimentNumbers <- c(103,143,98,42,8,4,2)
Sum <- 0
for (i in 0:2){
   p <- dpois(i,1.3225)#Calculate the pdf for Poison(landa=1.3225) at x=i
   print(c(i,p,p*400))
   Sum <- Sum+p
}
print(c("3+",1-Sum,(1-Sum)*400))
for (i in 3:6){
   p <- dpois(i,1.3225)#Calculate the pdf for Poison(landa=1.3225) at x=i
   print(c(i,p,p*400))
}

##Exercise 3
for (i in 0:18){
   p <- dpois(i,35.31)#Calculate the pdf for Poison(landa=35.31) at x=i
   Sum <- Sum+p
}
print(c("<=18",Sum))


##Exercise 4
ManProperbility <- function(n){
   if(n<4000) return(choose(1068,n)*((104/200)^(n))*((96/200)^(1068-n)))
      else return (exp(lchoose(1068,n)+((104/200)*(n))+((96/200)*(1068-n))))
}
#a
Sum <- 0
for (i in 0:533){
   Sum <- Sum+ManProperbility(i)
}
   print(Sum)
#d
ManProperbility(534)
##Exercise 5
#a
dbinom(1,2,0.5)*100
#b
dbinom(2,4,0.5)*100
#c
dbinom(5,10,0.5)*100
#d
dbinom(50,50,0.5)*100
#f
CoinExact <- function(n){
   return(dbinom(n,2*n,0.5))
}
CoinStitlingFormula <- function(n){
   return ((pi*n)^-0.5)
}
N <- c(1,2,5,50,100,500)
for (i in 1:6){
   print(N[i])
   print(CoinExact(N[i]))
   print(CoinStitlingFormula(N[i]))
}

print( c( CoinExact(N[i]),CoinStitlingFormula(N[i]) ) )

