library(openxlsx)
library(ggpubr)

association_algae_cya <- read.xlsx('Species association algae cya 20240822.xlsx')
association_algae_cya <- association_algae_cya[1:8]
TSIc_mean <- read.csv('TSIc all mean.csv',header = T)

merge_data <- merge(association_algae_cya,TSIc_mean,by='NewName')

merge_data[is.na(merge_data)]=0

colnames(merge_data)=c('Lake',
                       'Algae_Algae','Algae_Cya',
                       'Algae_Zoo','Cya_Cya',
                       'Cya_Zoo','Zoo_Zoo',
                       'Association','TSIc')
#for Cya_Cya some lakes had no value, only 25 lakes got values
merge_data_cya <- subset(merge_data,Cya_Cya!=0)

library(ggpubr)
p1 <- ggscatter(merge_data,x='TSIc',y='Association',color = 'black',
                size = 3,add = "reg.line", conf.int = TRUE,
                ylim=c(0,0.3),xlim=c(0,100),ggtheme = theme_bw(),legend='bottom')+
  stat_cor(method = 'spearman', label.x = 40)+
  scale_y_continuous(expand = c(0.05, 0))+
  scale_x_continuous(expand=c(0.05,0),breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  rremove('grid')
p1

pp8 <- ggscatter(merge_data,x='TSIc',y='Algae_Algae',color = 'black',
                 size = 3,add = "reg.line", conf.int = TRUE,
                 ylim=c(0,0.3),xlim=c(0,100),ggtheme = theme_bw(),legend='bottom')+
  stat_cor(method = 'spearman', label.x = 40)+
  scale_y_continuous(expand = c(0.05, 0))+
  scale_x_continuous(expand=c(0.05,0),breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  theme(axis.text.x = element_text(size = 10,color = 'black'),
        axis.text.y = element_text(size = 10,color = 'black'),
        axis.title = element_text(size = 12))+
  rremove('grid')
pp8

pp9 <- ggscatter(merge_data,x='TSIc',y='Algae_Cya',color = 'black',
                 size = 3,add = "reg.line", conf.int = TRUE,
                 ylim=c(0,0.3),xlim=c(0,100),ggtheme = theme_bw(),legend='bottom')+
  stat_cor(method = 'spearman', label.x = 40)+
  scale_y_continuous(expand = c(0.05, 0))+
  scale_x_continuous(expand=c(0.05,0),breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  theme(axis.text.x = element_text(size = 10,color = 'black'),
        axis.text.y = element_text(size = 10,color = 'black'),
        axis.title = element_text(size = 12))+
  rremove('grid')
pp9

pp10 <- ggscatter(merge_data,x='TSIc',y='Algae_Zoo',color = 'black',
                 size = 3,add = "reg.line", conf.int = TRUE,
                 ylim=c(0,0.3),xlim=c(0,100),ggtheme = theme_bw(),legend='bottom')+
  stat_cor(method = 'spearman', label.x = 40)+
  scale_y_continuous(expand = c(0.05, 0))+
  scale_x_continuous(expand=c(0.05,0),breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  theme(axis.text.x = element_text(size = 10,color = 'black'),
        axis.text.y = element_text(size = 10,color = 'black'),
        axis.title = element_text(size = 12))+
  rremove('grid')
pp10

pp11 <- ggscatter(merge_data_cya,x='TSIc',y='Cya_Cya',color = 'black',
                 size = 3,add = "reg.line", conf.int = TRUE,
                 ylim=c(0,0.5),xlim=c(0,100),ggtheme = theme_bw(),legend='bottom')+
  stat_cor(method = 'spearman', label.x = 40,label.y = 0.2)+
  scale_y_continuous(expand = c(0.05, 0))+
  scale_x_continuous(expand=c(0.05,0),breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  theme(axis.text.x = element_text(size = 10,color = 'black'),
        axis.text.y = element_text(size = 10,color = 'black'),
        axis.title = element_text(size = 12))+
  rremove('grid')
pp11

pp12 <- ggscatter(merge_data,x='TSIc',y='Cya_Zoo',color = 'black',
                 size = 3,add = "reg.line", conf.int = TRUE,
                 ylim=c(0,0.3),xlim=c(0,100),ggtheme = theme_bw(),legend='bottom')+
  stat_cor(method = 'spearman', label.x = 40)+
  scale_y_continuous(expand = c(0.05, 0))+
  scale_x_continuous(expand=c(0.05,0),breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  theme(axis.text.x = element_text(size = 10,color = 'black'),
        axis.text.y = element_text(size = 10,color = 'black'),
        axis.title = element_text(size = 12))+
  rremove('grid')
pp12

pp13 <- ggscatter(merge_data,x='TSIc',y='Zoo_Zoo',color = 'black',
                 size = 3,add = "reg.line", conf.int = TRUE,
                 ylim=c(0,0.5),xlim=c(0,100),ggtheme = theme_bw(),legend='bottom')+
  stat_cor(method = 'spearman', label.x = 40)+
  scale_y_continuous(expand = c(0.05, 0))+
  scale_x_continuous(expand=c(0.05,0),breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  theme(axis.text.x = element_text(size = 10,color = 'black'),
        axis.text.y = element_text(size = 10,color = 'black'),
        axis.title = element_text(size = 12))+
  rremove('grid')
pp13


ppp3 <- ggarrange(  pp8,
                   pp9,
                   pp10,
                   pp11,
                   pp12,
                   pp13,
                   ncol = 3,
                   nrow = 2,
                   align = c('hv'),
                   widths =c(8,8),##left right figue width
                   heights = c(6,6),
                   font.label = list(size = 14,
                                     color = "black",
                                     face = "bold", 
                                     family = NULL),
                   legend = NULL)
ppp3


