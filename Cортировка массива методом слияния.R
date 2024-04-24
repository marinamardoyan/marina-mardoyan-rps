#сортировка слиянием

funcmerge<- function(left,right){
  r<- c()
  if (left[length(left)]<= right[1]){
    r<-c(left,right)
  } 
  else if (right[length(right)]<= left[1]){
    r<-c(right,left)
  } 
  else{
    while(length(left)>0 & length(right)>0){
      if (left[1]<=right[1]){
        r<-c(r,left[1])
        left<-left[-1]
      } else{
        r<-c(r,right[1])
        right<-right[-1]
      }
    }
    while(length(left)>0 & length(right)==0) {
      r<-c(r,left[1])
      left<-left[-1]
    }
    while(length(right)>0 & length(left)==0) {
      r<-c(r,right[1])
      right<-right[-1]
    }
  }
  return(r)
}

funcrec<-function(x){
  if (length(x)<2){
    return(x)
  }
  else{
    z<-ceiling(length(x)/2)
    left<-funcrec(x[1:z])
    right<-funcrec(x[(z+1):length(x)])
    res<-funcmerge(left,right)
    return(res)
  }
}
x<-sample(-5:5,7 ,replace=TRUE)
print(x)
funcrec(x)