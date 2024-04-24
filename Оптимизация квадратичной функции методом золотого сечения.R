#золотое сечение

mm <- function(a,b,eps){
  x <- seq(-10,15,by=1)
  f <- function(x){
    (x-1.73)^2
  }
  y <- f(x)
  plot(x,y,type='l',col='pink',main='оптимизация квадратичной функции')
  abline(h=0)
  abline(v=0)
  tt <- c() #вектор посещаемых нами точек
  fib <- function(n){
    if(n==1){
      return(0)
    }
    if(n==2){
      return(1)
    } else{
      return(fib(n-1)+fib(n-2))
    }
  }
  phi <- fib(25)/fib(24)
  print(phi)
  xl <- b-(b-a)/phi
  xr <- a+(b-a)/phi
  i <- 0
  while((b-a)>=eps){
    if(f(xl)>=f(xr)){
      a <- xl
      xl <- xr
      xr <- a+(b-a)/phi
      i <- i+1
      tt <- c(tt,xl)
    }
    else {
      b <- xr
      xr <- xl
      xl <- b-(b-a)/phi
      i <- i+1
      tt <- c(tt,xr)}
    
  }
  print(i)
  print(tt[1:5])
  points((xr+xl)/2,f((xr+xl)/2),lwd=4,col='violet')
  points(tt[1:5],f(tt[1:5]),col='blue')
  points(c(tt[i],tt[i-1],tt[i-2],tt[i-3]),c(f(tt[i]),f(tt[i-1]),f(tt[i-2]),f(tt[i-3])),col='red')
  print(tt)
  return((xr+xl)/2)
}
otv <- mm(-100,100,0.00001)