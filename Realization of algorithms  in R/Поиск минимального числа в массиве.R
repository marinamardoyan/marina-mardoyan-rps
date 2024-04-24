#Поиск минимального числа в массива
ncr<-function(massiv){
  n<-length(massiv)
  curmin<- massiv[1]
  pos<-1
  for (i in 1:n){
    if(massiv[i] <= curmin){
      curmin<-massiv[i]
      pos<- i
    }
  }
  return(c(curmin , pos))
}
otvet<-ncr(c(-1,1,1,1,1,1))
print(otvet)
