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

