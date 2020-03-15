##Code by 106022103 ¼B¥°²»
## 2019

#A
a <- c(0,1,2,3,4,5,6,7,8) # c() for combination
a
a <- 0:8
length(a)
b <- matrix(a,3,3) #matrix(data,row,column)
b
c <- matrix(a,3,3,byrow=TRUE) #set data in matrix by rows
c
range(b)
head(b,2) #first two "rows" of matrix
a[2]
c[1,2]

#B
sum(a);prod(a)
min(a);max(a)
x=2;x^a;a**x # ^ or ** are both for using
log(a,base=x)
sin(x);cos(x);tan(x)

#C
polyroot(c(-1,0,1)) # ¤É¾­±Æ¦C

#D
   #Method1
x <- -10:10
y <- x^3-8*x^2+50*x+5
plot(x,y,xlab='x',ylab='y',type='l') # " and ' are both for using, l for lines
   #Method2
curve(x^3-8*x^2+50*x+5,from=-10,to=10,xlab="x",ylab="y")

#E
cbind(x,y) #cobine to matrix

