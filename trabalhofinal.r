n=20
t=2
p=0.2
vetor_trat=rbinom(n,1,p)
mu_i=rnorm(n, 0,1)
mu_j=rnorm(n, 0,1)
mu_t=rnorm(n, 0,1)
b1=0.5
b2=0.3
delta1=0.6
delta2=0.5
delta3=0.1
delta4=0.7

W_star_0=matrix(0,ncol = n,nrow = n)
W_star_1=matrix(0,ncol = n,nrow = n)
W_0=matrix(0,ncol = n,nrow = n)
W_1=matrix(0,ncol = n,nrow = n)
for (i in 1:n){
  for (j in 1:n){
    if (i!=j){
      W_star_0[i,j]=10+mu_i[i]+mu_j[j]
      W_star_1[i,j]=10+mu_i[i]+mu_j[j]+delta1+delta2
      W_0[i,j]=exp(W_star_0[i,j])*rpois(1,W_star_0[i,j])
      W_1[i,j]=exp(W_star_1[i,j])*rpois(1,W_star_1[i,j])
    }
  }
  W_0[i,]=W_0[i,]/sum(W_0[i,])
  W_1[i,]=W_1[i,]/sum(W_1[i,])
}

erro_0=rnorm(n,0,1)
erro_1=rnorm(n,0,1)

Yg0=mu_i+erro_0
Yg1=mu_i+mu_t+delta3*vetor_trat+delta4*+erro_1
