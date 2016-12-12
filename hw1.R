# log factorial function
logFac1 = function(n = 5){
  a = 0 
  if (n == 0 || n == 1){
    return(a);
  }
  else {
    for(num in seq(1,n, by=1)){
      a = a + log(num)
    }
  }
  
  return(a);
}


# log factorial using recursion

logFac2 = function(n=5){
  a = 0 
  if ( n==0 || n ==1){
    return(0)
  }
  else{
    a = logFac2(n-1) + log(n)
  }
  return(a)
}

x = seq(100, 3100, by=100)
options(expressions = 50000)

mylist1 = vector(length=31,mode="numeric")
mylist2 = vector(length=31,mode="numeric")
mylist3 = vector(length=31,mode="numeric")

for(num in seq(1,31,by=1)){
  cat("NUM: ", num,"\n")
  temp = seq(1,x[num], by=1)
  mylist1[num]=system.time(sum(sapply(temp,logFac1)))[3]
  mylist2[num]=system.time(sum(sapply(temp,logFac2)))[3]
  mylist3[num]=system.time(sum(sapply(temp,lfactorial)))[3]
}
g_range =range(0, mylist1, mylist2,mylist3)
plot(x,mylist1,type="l",col="red",ylim=g_range,xlab = "N",ylab = "Time")
lines(x,mylist2,col="green")
lines(x,mylist3,col="blue")
legend('topleft', legend = c("NonRecursive","Recursive","lfactorial") , 
       lty=1, col=c('red', 'green', 'blue'), bty='n', cex=.75)






