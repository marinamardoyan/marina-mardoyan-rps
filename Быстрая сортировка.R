# быстрая сортировка

quick_sort <- function(x){
  left <- c()
  right <- c() 
  mid<-x[1]
  posmid <- 1
  x<-x[-posmid]
  left <- x[which(x<=mid)]
  right <-x[which(x>mid)]
  if(length(left)>1){
    left <- quick_sort(left)
  }  
  if(length(right)>1){
    right <- quick_sort(right)
  }
  return(c(left,mid,right))
} 
x <- c(2,6,7,8,32,5)
quick_sort(x)