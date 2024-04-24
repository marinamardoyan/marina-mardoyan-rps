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


#выборочная сортировка массива

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

#фибоначи

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
fib(77)    
    
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

#поиск определителя матрицы
z <- c(-2,12,11,5,5,6,23,9,7,66,7,8,5,6,7,9)
mm <- matrix(z,nrow=4, byrow=T)
mm
opred <- function(mm){
  s <- 0
  if(dim(mm)[1]==1){
    return(mm)
  }
  else{
    for( i in 1:ncol(mm)){
      s <- s+(-1)^(i+1)*mm[1,i]*opred(mm[-1,-i])
    }
    return(s)
  }
}
opred(mm)
det(mm)

#алгоритм хука- дживса по нахождению локальной точки минимума
kl <- function(x,y){
  return(x^2+4*y^2+3*x*y+2*x+16*y)
}
x <- seq(-10,10,0.1)
y<- seq(-10,10,0.1)
z <- outer(x,y,kl)
contour(x,y,z)
huka_jeeves <- function(x0,y0,h,eps,nmax){
  nshagov <- 0
  while(h>eps&nshagov<nmax){
    xh <- c(0,-h,+h)[which.min(c(kl(x0,y0),kl(x0-h,y0),kl(x0+h,y0)))]
    yh <-  c(0,-h,+h)[which.min(c(kl(x0+xh,y0),kl(x0+xh,y0-h),kl(x0+xh,y0+h)))]
    x0 <- x0+xh
    y0 <- y0+yh
    h <-  h/2}
  return(c(x0,y0,nshagov))
}

huka_jeeves(-13,42,2,0.01,100)



#bfs

edg <- c(1,2,
         1,4,
         1,3,
         3,6,
         4,5,
         5,7,
         8,8)
g<- graph(edg,directed = T)
plot(g,edge.arrow.size=0.5,vertex.size=25,edge.width=1,edge.length=3,layout=layout_as_tree)
bfs <- function(g,s){
  ochered <- c(s)
  visited <- c()
  while(length(ochered)>0){
    visited <- c(visited,ochered[1]) #помещаем в посещенные первый элемент очереди
    ochered <- c(ochered[-1],neighbors(g,ochered[1])) #из очереди убираем первый элемент и добывляем его соседей
  }
  if(length(visited)<length(g)){
    print('граф несвязный')
  }
  return(visited)
}
bfs(g,1)

#dfs
edg <- c(1,2,
         1,4,
         1,3,
         3,6,
         4,5,
         5,7,
         8,8)
nn<- graph(edg,directed = T)
plot(nn,edge.arrow.size=0.5,vertex.size=25,edge.width=1,edge.length=3,layout=layout_as_tree)
g <- as_adjacency_matrix(nn)
dfs <- function(g, s) {
  n <- nrow(g)
  visited <- rep(FALSE, n)
  ochered <- c(s)
  visited[s] <- TRUE
  while (length(ochered) > 0) {
    node <- ochered[length(ochered)] # берем последний элемент из очереди
    cat(node,'') #принтим его 
    ochered <- ochered[-length(ochered)] # убираем его из очереди
    for (i in 1:n) {
      if (g[node, i] == 1 && !visited[i]) { #ищем его соседей из матрицы смежности
        visited[i] <- TRUE 
        ochered <- c(ochered, i) #соседа в очередь
      }
    }
  }
}
dfs(g,1)




#алгоритм крускала
M <- matrix(0,ncol=7,nrow=7,byrow=T)
M[1,] <- c(0,0,3,0,0,4,6)
M[2,] <- c(0,0,1,8,3,0,5)
M[3,] <- c(3,1,0,0,0,0,4)
M[4,] <- c(0,8,0,0,5,4,0)
M[5,] <- c(0,3,0,5,0,0,1)
M[6,] <- c(4,0,0,4,0,0,3)
M[7,] <- c(6,5,4,0,1,3,0)
g <- M
a <- graph.adjacency(g,mode='undirected',weighted = T)
plot(a,layout=layout_nicely,edge.label=g[lower.tri(g)][g[lower.tri(g)]!=0])
set.seed(1)

R <- list(c(3,1,3),c(4,1,6),c(6,1,7),c(1,2,3),c(8,2,4),c(3,2,5),c(5,2,7),c(4,3,7),c(5,4,5),c(4,4,6),c(1,5,7),c(3,6,7)) #задаем списком вес ребра(1 элемент) и вершины(2,3 эл)
sorted_edges <- function(){
  for( i in 1:(length(R)-1)){ #cортировка пузырьком ребер по их весу
    for(j in 1:(length(R)-i)){
      if(R[[j]][1]>=R[[j+1]][[1]]){
        carman <- R[j]
        R[j] <- R[j+1]
        R[j+1] <- carman
      }
    }
  }
  return(R)
}
se <- sorted_edges()
min_ostov <- function(a){
  x <- c() #массив неподходящих вершин
  U <- c() #массив соединенных вершин
  for (i in 1:length(se)){
    if(!(se[[i]][2] %in% U)|(!(se[[i]][3] %in% U))){ #если какой то вершины нет в U то
      U <- c(U,se[[i]][2],se[[i]][3])
      }
    else{
      x <- c(x,se[i]) #иначе отправляем ребро в массив х
    }
  }
  uu <- graph(U,directed=F) 
  if(is_tree(uu)==F){ #проверяем является ли деревом получившийся массив U 
    U <- c(U,x[[1]][2],x[[1]][3]) #если нет то в U засовываем первый элемент из х(тк веса отсортированы то первый элемент массива будет иметь наименьший вес);соединяя два дерева одним ребром невозможно получить цикл
    x <- x[-1]
  }
  return(U)
}
U <- min_ostov()
cc <- c() #дальше мы должны получить массив из номеров ребер в Е(а) которых нет в U
for (i in 1:max(get.edge.ids(a,U))){ #get.edge.ids(a,U)) - это номера ребер U в E(a)
  if(!(i %in% get.edge.ids(a,U))){ #если i-того элемента из ребер в графе а нет в  U 
    cc <- c(cc,i)
  }
}
E(a)[get.edge.ids(a,U)]$color='pink'
E(a)[get.edge.ids(a,U)]$width=2
E(a)[cc]$color='grey'
E(a)[cc]$width=1

plot(a,edge.label=g[lower.tri(g)][g[lower.tri(g)]!=0])
set.seed(1)

#8 личная задача
##Имеется N прямоугольных конвертов и N прямоугольных открыток различных размеров. 
#Можно ли разложить все открытки по конвертам, 
#чтобы в каждом конверте было по одной открытке.
#Замечание. Открытки нельзя складывать, сгибать и т.п., но можно помещать в конверт под углом. 
#Например, открытка с размерами сторон 5:1 помещается в конверты с размерами 5:1, 6:3, 4.3:4.3, но не входит в конверты с размерами 4:1, 10:0.5, 4.2:4.2.
check <- function(e,e1,p,p1,st){ #е-меньшая длина конверта, 
 # е1-большая,р- меньшая длина открытки,р1-большая,st-угол  
  f <-  FALSE
  if(e < p && e1 < p1){
    f <- FALSE
  }else{
    if(e >= p && e1 >= p1){
      f <- TRUE
    }else{  #кладем открытку под неким углом phi изначально он равен 0
      phi <-  0
      while(phi < pi/2){  #почему? Хороший вопрос, pi/2 = 90, то иесть мы идем в это условие 
        #только, если одна сторона открытки больше другой *(тк это прямоугольник), если повернуть на 90 градусов открытку,
        #то она точно не поместится, так как большая сторона будет торчать
        if(sin(phi)*p1 + cos(phi)*p <= e1 &&
           cos(phi)*p1 + p*sin(phi) <= e){
          f <- TRUE
          break
        }
        phi <-  phi+st
        #print(phi) Показывает как менялся угол и как он в конце концов нашелся 
      }
    }
  }
  return(f)
}
#Если открытка входит, то нам пишут TRUE
zadacha <- function(otk,konv){
  n <- length(otk)
  print (n)
  vec <- rep(0,2*n)
  m <- matrix(vec,2*n,2*n)
  #нулевая матрица квадратная (2*n x 2*n)
  for(i in 1:n){    #для всех открыток 
    vector <- c()   #     вектор в будущем он будет логичесим, то есть мы будем в него писакть T или F
    for(j in (n+1):(2*n)) { #смотрим для конвертов
      if(sum(m[j,]==1)==0){ #если во всей j-той строчке нули,то
        temp_o <- otk[[i]]#запихиваем в переменную длины i-той открытки
        temp_k <- konv[[j-n]] #запихиваем в переменную длины j-n-ного конверта
        #для каждой открытки прогоняем конверт
        e <- min(temp_k) #меньшая сторона конверта
        e1 <- max(temp_k) #большая сторона конверта
        p <- min(temp_o) #меньшая сторона открытки
        p1 <- max(temp_o) #большая сторона открытки
        temp <- check(e,e1,p,p1,pi/100) #проверяем входит ли открытка в конверт
        vector <- c(vector,temp)
      }else{
        vector <- c(vector,FALSE)
      }
    }
    if(sum(vector!=FALSE)==0){ #Вектор состоит из FALSE нет подходящего конверта 
      stop('нет подходящего конверта')
    }
    #дальше распределяем открытки в конверты исходя из площади
    minimal <- +Inf
    for(b in which(vector==TRUE)){ #проверяем те конверты,в которых входят октрытки
      s <- (k[[b]][1])*(k[[b]][2]) #находим площадь конверта
      if(s <= minimal){
        minimal <- s
        n_min <- b
      }
    }
    m[i,n+n_min] <- 1 #1 означает что мы прошли открытку и конверт
    m[n+n_min,i] <- 1  #тк квадратная матрица
  }
  a <- graph.adjacency(m,mode ='undirected')
  plot.igraph(a,vertex.size=10)
  for(i in 1:n){
    g <-  which(m[,i]==1)
    print(paste('открытка',i,'в конверте', g-n))
  }
}
#открытки
a1 <- c(1,5)
a2 <- c(0.5,0.5)
a3 <- c(511,1)
a4 <- c(100,1)
#конверты
b1 <- c(100,2)
b2 <- c(0.5,0.5)
b3 <- c(511,4.3)
b4 <- c(4.3,4.3)
o <- list(a1,a2,a3,a4)
k <- list(b1,b2,b3,b4)
zadacha(o,k)










