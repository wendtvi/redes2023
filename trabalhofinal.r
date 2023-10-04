library(igraph)

N=700
resultados=matrix(NA,ncol = 4,nrow = N)

for (k in 1:N){
n=100
t=2
mu_i11=rnorm(n, 0,1)
mu_t11=rnorm(n, 0,1)
mu_i00=rnorm(n, 0,1)
mu_t00=rnorm(n, 0,1)
mu_i10=rnorm(n, 0,1)
mu_t10=rnorm(n, 0,1)
mu_i01=rnorm(n, 0,1)
mu_j=rnorm(n, 0,1)
mu_t01=rnorm(n, 0,1)
b1=0.5
b2=0.3
delta1=0.1
delta2=0.05
delta3=0.1
delta4=0.7

W_star_00=matrix(0,ncol = n,nrow = n)
W_star_11=matrix(0,ncol = n,nrow = n)
W_00=matrix(0,ncol = n,nrow = n)
W_11=matrix(0,ncol = n,nrow = n)
W_star_01=matrix(0,ncol = n,nrow = n)
W_star_10=matrix(0,ncol = n,nrow = n)
W_01=matrix(0,ncol = n,nrow = n)
W_10=matrix(0,ncol = n,nrow = n)
for (i in 1:n){
  for (j in 1:n){
    if (i!=j){
      if(j>i){
      W_star_00[i,j]=mu_i00[i]+mu_j[j]
      W_star_11[i,j]=mu_i11[i]+mu_j[j]+delta1+delta2
      W_00[i,j]=(W_star_00[i,j]+rnorm(1,W_star_00[i,j],1))
      W_11[i,j]=(W_star_11[i,j]+rnorm(1,W_star_11[i,j],1))
      W_star_01[i,j]=mu_i01[i]+mu_j[j]+delta2
      W_star_10[i,j]=mu_i10[i]+mu_j[j]
      W_01[i,j]=(W_star_01[i,j]+rnorm(1,W_star_01[i,j],1))
      W_10[i,j]=(W_star_10[i,j]+rnorm(1,W_star_10[i,j],1))
      }
      if(j<i){
        W_star_00[i,j]=W_star_00[j,i]
        W_star_11[i,j]=W_star_11[j,i]
        W_00[i,j]=W_00[j,i]
        W_11[i,j]=W_11[j,i]
        W_star_01[i,j]=W_star_01[j,i]
        W_star_10[i,j]=W_star_10[j,i]
        W_01[i,j]=W_01[j,i]
        W_10[i,j]=W_10[j,i]
      }
    }
  }
  W_00[i,]=W_00[i,]/sum(W_00[i,])
  W_11[i,]=W_11[i,]/sum(W_11[i,])
  W_01[i,]=W_01[i,]/sum(W_01[i,])
  W_10[i,]=W_10[i,]/sum(W_10[i,])
}

Adj_00=matrix(as.numeric(W_00>.003),ncol = n)
Adj_11=matrix(as.numeric(W_11>.003),ncol = n)
Adj_01=matrix(as.numeric(W_01>.003),ncol = n)
Adj_10=matrix(as.numeric(W_10>.003),ncol = n)


erro_00=rnorm(n,0,1)
erro_11=rnorm(n,0,1)
erro_01=rnorm(n,0,1)
erro_10=rnorm(n,0,1)


Y00=vector()
Y01=vector()
Y10=vector()
Y11=vector()
Y10_cic=vector()
F_inver_F_Y10=vector()
F_Y10=vector()
lambda=vector()
for (l in 1:n){
  #Estimando efeito da Rede
  Y10_cic[l]=sum(Adj_10[l,])
  
  F_Y10[l]=pbinom((Y10_cic), size=n-1, prob=(sum(Adj_00[l,])/((n-1))))
  
  F_inver_F_Y10[l]=qbinom(F_Y10,size=(n-1), prob=(sum(Adj_01[l,])/((n-1))))
  
  #ATT de rede
  lambda[l]=(sum(Adj_11[l,])/((n-1)))-F_inver_F_Y10/((n-1))
  
  Y00[l]=(mu_i00[l])+erro_00[l]
  Y01[l]=(mu_i01[l])+erro_01[l]+(sum((W_star_01[l,]))/(n-1))
  Y10[l]=(mu_i10[l])+erro_10[l]
  Y11[l]=(mu_i11[l])+delta3+(sum((W_star_11[l,]))/(n-1))+erro_11[l]
}
#Y00=(mu_i00)+erro_00
#Y01=(mu_i01)+(mu_t01)+erro_01
#Y10=(mu_i10)+erro_10
#Y11=(mu_i11)+(mu_t11)+delta3+erro_11

ATT=mean(Y11)-(mean(Y01)-mean(Y00)+mean(Y10))
resultados[k,2]=ATT
resultados[k,3]=mean(lambda)
resultados[k,1]=delta3
resultados[k,4]=ATT-(mean(lambda))
}

mean(resultados[k,2])
sd(resultados[k,4])
mean(resultados[k,3])
delta3
mean(resultados[k,4])



plot(graph_from_adjacency_matrix(
  Adj_00,
  mode = c("undirected"),
  weighted = TRUE,
  diag = FALSE,
  add.colnames = NULL,
  add.rownames = NA
),       edge.width = .3,
edge.color = "gray",
edge.arrow.size=0.1,
edge.arrow.width=0.1,
edge.curved=0.01,
edge.label.font =.5, 
edge.label.cex = .5,

vertex.color="gray90", 
vertex.size=2, 
vertex.frame.color="black",


vertex.label.color= "black", 
vertex.label.cex=1, 
vertex.label.font=1,
vertex.label.dist=0,
vertex.shape= "circle"
) 
