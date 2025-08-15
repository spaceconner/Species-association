env <- read.csv("env_high.csv",header = T, row.names = 1)
library(vegan)
library(ggplot2)
library(ggrepel)


Algae <- read.csv('algae_TX_high.csv',header = T,row.names = 1)
Cyanobacteria <- read.csv('cyanobacteria_TX_high.csv',header = T,row.names = 1)
Rotifer <- read.csv('rotifer_TX_high.csv',header = T,row.names = 1)
Cladocera <- read.csv('cladocera_TX_high.csv',header = T,row.names = 1)
Copepod <- read.csv('copepod_TX_high.csv',header = T,row.names = 1)

library('glasso')
library(igraph)
library(vegan)
library('ade4')
library('plyr')
library(ggplot2)
library('reshape2')
library(qgraph)
library(foreign)
library(gridExtra)
library(betapart)
library(betalink)
library(factoextra)

Algae_t <- t(Algae)
Cyanobacteria_t <- t(Cyanobacteria)
Rotifer_t <- t(Rotifer)
Cladocera_t <- t(Cladocera)
Copepod_t <- t(Copepod)

write.csv(Algae_t,'Algae_t.csv')
write.csv(Cyanobacteria_t,'Cyanobacteria_t.csv')
write.csv(Rotifer_t,'Rotifer_t.csv')
write.csv(Cladocera_t,'Cladocera_t.csv')
write.csv(Copepod_t,'Copepod_t.csv')

TX_high_class <- read.csv('class_TX_high.csv',header = T,row.names = 1)
TX_high.table <- TX_high_class

Mxp.plot <- rbind(t(Algae),t(Cyanobacteria),t(Rotifer),t(Cladocera),t(Copepod))
Mxp.plot[Mxp.plot>0] <- 1
sum(apply(Mxp.plot,2,sum)==0)
Mxp.plot = as.matrix(Mxp.plot)[,apply(Mxp.plot,2,sum) > 5]
#group names
groupes.names=c("TN","TP",'Algae','Cyanobacteria','Rotifer','Cladocera','Copepod')
#build the similarity matrix (Jaccard index and true turnover componenet of the Jaccard index)
n_plot=length(row.names(Algae))

mat.beta.jac=matrix(nrow = n_plot*(n_plot-1)/2,ncol=length(groupes.names))
mat.beta.jtu=matrix(nrow = n_plot*(n_plot-1)/2,ncol=length(groupes.names))
mat.beta.jne=matrix(nrow = n_plot*(n_plot-1)/2,ncol=length(groupes.names))

colnames(mat.beta.jac)=groupes.names
colnames(mat.beta.jtu)=groupes.names
colnames(mat.beta.jne)=groupes.names
#？
Mxp.plot.loc.list=c()
for(group in groupes.names){
  print(group)
  Mxp.plot.loc=Mxp.plot[TX_high.table[which(TX_high.table[,2]==group),1],]
  
  Mxp.plot.loc[which(Mxp.plot.loc>0)]=1
  res=beta.pair(t(Mxp.plot.loc), index.family = "jaccard")
  
  mat.beta.jac[,group]=as.vector(res$beta.jac)  #jaccard index
  mat.beta.jtu[,group]=as.vector(res$beta.jtu)  #true turnover
  mat.beta.jne[,group]=as.vector(res$beta.jne)  #nested turnover 
  
  Mxp.plot.loc.list=c(Mxp.plot.loc.list,list(Mxp.plot.loc))
}

names(Mxp.plot.loc.list)=groupes.names

#environmental euclidian distances
mat.beta.jac[,1]=as.vector(dist(env$TN))
mat.beta.jac[,2]=as.vector(dist(env$TP))

mat.beta.jtu[,1]=as.vector(dist(env$TN))
mat.beta.jtu[,2]=as.vector(dist(env$TP))

mat.beta.jne[,1]=as.vector(dist(env$TN))
mat.beta.jne[,2]=as.vector(dist(env$TP))

graphics.off()
for(i in 3:ncol(mat.beta.jac)){
  plot(mat.beta.jtu[,i]/mat.beta.jac[,i],ylim=c(0,1),main=colnames(mat.beta.jac)[i])
  points(mat.beta.jne[,i]/mat.beta.jac[,i],col='red')
}

graphics.off()
for(i in 1:ncol(mat.beta.jac)){
  par(mfrow=c(1,2))
  hist(mat.beta.jac[,i],main=paste(colnames(mat.beta.jac)[i],"jac"),breaks = 15)
  hist(mat.beta.jtu[,i],main=paste(colnames(mat.beta.jac)[i],"jtu"),breaks = 15)
}

#build the empirical variance-covariance matrix
write.csv(mat.beta.jac,"mat.beta.jac.csv")
write.csv(mat.beta.jne,"mat.beta.jne.csv")
write.csv(mat.beta.jtu,"mat.beta.jtu.csv")
mat.beta.jac <- read.csv(file.choose(),header = T,row.names = 1)
mat.beta.jne <- read.csv(file.choose(),header = T,row.names = 1)
mat.beta.jtu <- read.csv(file.choose(),header = T,row.names = 1)

s=cov(mat.beta.jac)
s_cor=cor(mat.beta.jac)

s.jtu=cov(mat.beta.jtu) #true turnover
s_cor.jtu=cor(mat.beta.jtu)

s.jne=cov(mat.beta.jne) #nested turnover
s_cor.jne=cor(mat.beta.jne)

##############network inference#################################################
#infer the precision matrix, select the optimal lambda value using an infromation
BICgraph <- EBICglasso(s, nrow(mat.beta.jac),gamma =0.5,lambda.min.ratio = 0.00000001)
sum(BICgraph!=0)

BICgraph.jtu <- EBICglasso(s.jtu, nrow(mat.beta.jac),gamma =1.5,lambda.min.ratio = 0.00000001)
sum(BICgraph.jtu!=0)

BICgraph.jne <- EBICglasso(s.jne, nrow(mat.beta.jac),gamma =2.2,lambda.min.ratio = 0.00000001)

#here, we select the highest value of gamma (prior on the number of edges) that give an non empty edge set 
#using an information criterion (EBIC, see qgraph doc)

med.pcor=median(BICgraph[which(BICgraph>0)][order(BICgraph[which(BICgraph>0)])]) #keep only coefficients above the median
med.pcor.jtu=median(BICgraph.jtu[which(BICgraph.jtu>0)][order(BICgraph.jtu[which(BICgraph.jtu>0)])])
med.pcor.jne=median(BICgraph.jne[which(BICgraph.jne>0)][order(BICgraph.jne[which(BICgraph.jne>0)])])

BICgraph.med=BICgraph
BICgraph.med[which(BICgraph<med.pcor)]=0

BICgraph.jtu.med=BICgraph.jtu
BICgraph.jtu.med[which(BICgraph.jtu<med.pcor.jtu)]=0

BICgraph.jne.med=BICgraph.jne
BICgraph.jne.med[which(BICgraph.jne<med.pcor.jne)]=0

g=graph_from_adjacency_matrix(BICgraph,mode = "undirected",weighted = T)
g.med=graph_from_adjacency_matrix(BICgraph.med,mode = "undirected",weighted = T)

g.jtu=graph_from_adjacency_matrix(BICgraph.jtu,mode = "undirected",weighted = T)
g.jtu.med=graph_from_adjacency_matrix(BICgraph.jtu.med,mode = "undirected",weighted = T)

g.jne=graph_from_adjacency_matrix(BICgraph.jne,mode = "undirected",weighted = T)
g.jne.med=graph_from_adjacency_matrix(BICgraph.jne.med,mode = "undirected",weighted = T)

graphics.off()
par(mfrow=c(1,3))
plot(g.med)
plot(g.jtu.med)
plot(g.jne.med)

write.csv(BICgraph.med,'TX_high_result.csv')

