#сортировка выбором

selection<-function(a){
  minimum<-function(m){
    min<-m[1]
    pos<-1
    for (i in 1:length(m)){
      if (m[i]<min){
        min<-m[i]
        pos<-i
      }
    }
    return(pos)
  }
  for(i in 1:(length(a)-1)){     #столько раз проходимся по массиву
    a1<-minimum(a[i:length(a)])+i-1 #номер минимума в оставшемся массиве
    carman<-a[i]
    a[i]<-a[a1]
    a[a1]<-carman
  }
  return(a)
}
a<-selection(sample(-5:5,9,replace=T))
print(a)