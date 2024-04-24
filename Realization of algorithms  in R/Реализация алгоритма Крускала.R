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
