#Квадратное ур-ние
aa=1
bb=-5
cc=6
ure<-function(aa,bb,cc){
  disc<- bb^2-4*aa*cc
  if (aa!=0&disc>0) {
    x1<- (-bb+sqrt(disc))/2/aa
    x2<- (-bb-sqrt(disc))/2/aa
    return(c(x1,x2))
  } else if (disc<0) {
    print("нет решений")  
  } else if (aa!=0&disc==0) {
    x1<- (-bb+sqrt(disc))/2/aa
    x2<- (-bb-sqrt(disc))/2/aa
  } else if (aa==0&bb==0){
    print("нет решений")
  } else if (aa==0&bb!=0){
    x1<-(-cc/bb)
    x2<-(-cc/bb)
    return(c(x1,x2))
  } else if (aa!=0&bb==0){
    x1<- (-sqrt(-cc/aa))
    x2<- (sqrt(-cc/aa))
    return(c(x1,x2))
  } else if(aa!=0&bb!=0&cc==0){
    x1<-(-bb/aa)
    x2<-(-bb/aa)
    return(c(x1,x2))
  }
}
ure(4,3,7)
otvet<-ure(4,3,7)