#install.packages("igraph")
library(igraph)

setwd("C:/Users/vitor/Downloads")
A=read.csv2("class-graph.csv",header = F,sep=",",stringsAsFactors=FALSE)
A[1,1]=0
A$V1=as.integer(A$V1)
A=as.matrix(A)

A_r2=t(A)%*%A
#diag(A)
#a
sum(distances(graph_from_adjacency_matrix(A))<=2)-20
#b
sum(degree(graph_from_adjacency_matrix(A)))/2

#c
vetor_k=vector()
componentes=vector()

for(p in 1:1000){
  minha_matriz=matrix(0,ncol = 20,nrow=20)
  for (i in 1:18){
      minha_matriz[(i+1):20,i]=rbinom(20-i,1,0.133)
      minha_matriz[i,(i+1):20]=minha_matriz[(i+1):20,i]
  }
  k=0
  for (u in 1:20){
    for (w in 1:20){
      if(minha_matriz[u,w]>0) k=k+1
    }
  }
  vetor_k[p]=k
  componentes[p]=count_components(graph_from_adjacency_matrix(minha_matriz))
}
mean(vetor_k)/19
mean(componentes)
sd(componentes)

