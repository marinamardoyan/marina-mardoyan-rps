# 17 личная задача
#Даны целые M и N и вектор действительных чисел X[1..N]. Найти целое число i
#(1<=i<=N-M), для которого сумма x[i]+...+x[i+M] ближе всего к нулю.


f <- function (M,x) {
  N <- length(x)
  if (M >= N|M < 0) {
    stop("Wrong M")
  }
  Smin <- Inf
  for (j in 1:(N - M)) {
    S <- 0
    for (k in j:(j + M)){
      S <- S + x[k]
    }
    print(S)
    if (abs(S) < Smin) {
      Smin <- abs(S)
      i <- j 
    }
  }
  return(i)
}
f(3,c(2,5,6,-12,56,7,8,90)) 