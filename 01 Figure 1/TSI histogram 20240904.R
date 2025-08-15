TX_env <- read.csv('TX_env.csv',header = T,row.names = 1)
ADK_env <- read.csv('ADK lakes env.csv',header = T,row.names = 1)
DN_env <- read.csv('Subtropical env 20230410.csv',header = T,row.names = 1)
Station3_env <- read.csv('03 env_sta3_final.csv',header = T,row.names = 1)
Station9_env <- read.csv('03 env_sta9_final.csv',header = T,row.names = 1)

Japan_env <- rbind(Station3_env,Station9_env)

par(mfrow=c(3,4))
p1 <- hist(x=ADK_env$TSIc,freq = T,xlim = c(0,100),breaks = c(0,10,20,30,40,50,60,70,80,90,100))
p2 <- hist(x=DN_env$TSIc,freq = T,xlim = c(0,100),breaks = c(0,10,20,30,40,50,60,70,80,90,100))
p3 <- hist(x=TX_env$TSIc,freq = T,xlim = c(0,100),ylim = c(0,100),breaks = c(0,10,20,30,40,50,60,70,80,90,100))
p4 <- hist(x=Japan_env$TSIc,freq = T,xlim = c(0,100),ylim = c(0,400),breaks = c(0,10,20,30,40,50,60,70,80,90,100))
