#We constructed the species association relationships by the same method.
#We only showed an example here.
library(ecoCopula)
phyto_zoo <- read.csv("02 spe_select station3.csv",header = T, row.names = 1)
env <- read.csv("01 env_select station3.csv",header = T, row.names = 1)

phyto_zoo_select <- phyto_zoo[,-1]
env_process <- env[,-c(1,2,5,7,11,12,14:17)]

library(Hmisc)
env_varclus <- varclus(as.matrix(env_process),similarity = "spearman",method = 'complete')
plot(env_varclus)
env_select <-  env[,-c(1,2,3,4,5,6,7,11,12,14:17)]
env_select_varclus <- varclus(as.matrix(env_select),similarity = "spearman",method = 'complete' )
plot(env_select_varclus)

X <- log10(env_select+1)

phyto_zoo_select <- as.matrix(phyto_zoo_select)
X <- as.matrix(X)

Station3_nb <- manyglm(phyto_zoo_select~X, family = "negative.binomial")
Station3_gr=cgr(Station3_nb,lambda = 0)

options(max.print=10000000,width = 10000)
sink("species association.txt")
summary.cgr(Station3_gr)
sink()
