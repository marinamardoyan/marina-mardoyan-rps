#сортировка массива пузырьком 

puz<-function(x){
  perestanovka<-0
  srav<-0
  for (i in 1:(length(x)-1)){
    for (j in 1:(length(x)-i)){
      srav<-srav+1
      if (x[j]>x[j+1]){
        carman<-x[j]
        x[j]<-x[j+1]
        x[j+1]<-carman
        perestanovka<- perestanovka+1
      }
    }
  }
  return(list(x,srav,perestanovka))
}
x<-puz(sample(0:7,10,replace=T))
puz(c(15,77,3,4))
