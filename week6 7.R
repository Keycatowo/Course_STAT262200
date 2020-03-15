###hw4 week6 7 106022103 劉弘祥

##1

to_east <- c(8,18,28,38,48,58)
to_west <- c(0,10,20,30,40,50,60)
P_to_east <- 0
P_to_west <- 0

for (i in 1:6){
   P_to_east <- P_to_east+(to_east[i]-to_west[i])
   P_to_west <- P_to_west+(to_west[i+1]-to_east[i])
}
print(c("The Probability to east is ",P_to_east/60))
print(c("The Probability to west is ",P_to_west/60))
rm(list=ls())

##2
# (a)
qt(0.95,1)
qt(0.95,5)
qt(0.95,10)
qt(0.95,100)
print("當自由度越大，95%分位點越小")
qt(0.95,Inf)
qnorm(0.95)
print("當自由度趨近很大時，95%分位點趨近常態分佈至95%分位點")

# (b)
DF <- c(1,2,5,10)
for (i in 1:4){
   a <- qchisq(0.05,DF[i])
   b <- qchisq(0.95,DF[i])
   X <- sprintf("當自由度為%i時,%%5的分位點為%f，%%95分位點為%f",i,a,b)
   print(X)
}
print("當卡方分佈自由度越大的時候，5%和95%分位點都越大")
rm(list=ls())

##3
# (a)
1-pexp(40,rate=0.02) # 分佈表壽命，故以1減之表示存活
# (b)
(1-pexp(10+40,0.02))/(1-pexp(10,0.02))
# (c)
(1-pexp(40+40,0.02))/(1-pexp(40,0.02))
# (d)
print("abc結果無異，可見指數分佈的無記憶性")
# (e)
qexp(0.5,0.02)

##4
M <- 140
S <- 15
# (a)
X <- 1-(round(pnorm(155,mean=M,sd=S),3)-round(pnorm(125,mean=M,sd=S),3)) # 全部減去155~125的合格及表示不合格比率
print(round(X,3))
# (b)
X <- 1-round(pnorm(170,mean=M,sd=S),3)
print(X)
# (c)
round(qnorm(0.933,mean=M,sd=S))
rm(list=ls())

##5
par(mfrow=c(2,3))
#常態
cut <- seq(-10,10,length.out=1000)
graph.1 <- plot(cut,dnorm(cut,mean=5,sd=2),type="l",xlab=NA,ylab=NA)
title("常態-機率分佈圖")
N.1 <- pnorm(cut,mean=5,sd=2)
graph.2 <- plot(cut,N.1,type="l",xlab=NA,ylab=NA)
title("常態-累進分佈圖")
graph.3 <- boxplot(N.1)
title("常態-盒狀百分位圖")
#卡方
cut <- seq(0,25,length.out = 1000)
graph.1 <- plot(cut,dchisq(cut,df=5),type="l",xlab=NA,ylab=NA)
title("卡方-機率分佈圖")
F.1 <- pchisq(cut,df=5)
graph.4 <- plot(cut,F.1,type="l",xlab=NA,ylab=NA)
title("卡方-累進分佈圖")
graph.5 <- boxplot(F.1)
title("卡方-盒狀百分位圖")

