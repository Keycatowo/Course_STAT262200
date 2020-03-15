#### HW week5 106022103

##下一代的變化
NextGeneration <- function(AA,Aa,aa){
   Distrubution <-rep(0,3)
   Distrubution[1] <- AA^2 + Aa^2*(1/4) + 2*Aa*AA*(1/2) 
   Distrubution[2] <- Aa^2*(1/2) + 2*AA*aa + 2*AA*Aa*(1/2) + 2*aa*Aa*(1/2)
   Distrubution[3] <- aa^2 + Aa^2*(1/4) + 2*aa*Aa*(1/2)
   return(Distrubution)
}
#印出
PrintDis <- function(Dis){
   Dis[1] <- round(Dis[1],digits=2)
   Dis[2] <- round(Dis[2],digits=2)
   Dis[3] <- round(Dis[3],digits=2)
   print(c(Dis[1],Dis[2],Dis[3]))
   
}

#產生隨機值
AA <- runif(1,0,1)
Aa <- runif(1,0,1)
aa <- runif(1,0,1)
#調整為比例
S <- AA+Aa+aa
AA <- round(AA/S,digits=2)
Aa <- round(Aa/S,digits=2)
aa <- round(aa/S,digits=2)
rm(S)
#初代
Dis <- c(AA,Aa,aa)
PrintDis(Dis)
#二代
Dis <- NextGeneration(Dis[1],Dis[2],Dis[3])
PrintDis(Dis)
#三代
Dis <- NextGeneration(Dis[1],Dis[2],Dis[3])
PrintDis(Dis)
#四代
Dis <- NextGeneration(Dis[1],Dis[2],Dis[3])
PrintDis(Dis)
#五代
Dis <- NextGeneration(Dis[1],Dis[2],Dis[3])
PrintDis(Dis)
