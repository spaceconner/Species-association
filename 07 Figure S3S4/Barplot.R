library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(tidyr)

Sub_data <- read.xlsx('Temperate and subtropical merge 20240829.xlsx',sheet = 'Subtropical')
Tem_data <- read.xlsx('Temperate and subtropical merge 20240829.xlsx',sheet = 'Temperate')

Sub_algae <- subset(Sub_data,new_group=='Algae & Algae')
Sub_algae_cya <- subset(Sub_data,new_group=='Algae & Cyanobacteria')
Sub_algae_zoo <- subset(Sub_data,new_group=='Algae & Cladocera'|new_group=='Algae & Copepod'|new_group=='Algae & Rotifer')
Sub_cya <- subset(Sub_data,new_group=='Cyanobacteria & Cyanobacteria')
Sub_cya_zoo <- subset(Sub_data,new_group=='Cyanobacteria & Cladocera'|new_group=='Cyanobacteria & Copepod'|new_group=='Cyanobacteria & Rotifer')
Sub_zoo <- subset(Sub_data,new_group=='Copepod & Rotifer'|
                           new_group=='Rotifer & Rotifer'|
                           new_group=='Cladocera & Rotifer'|
                           new_group=='Cladocera & Cladocera'|
                           new_group=='Cladocera & Copepod'|
                           new_group=='Copepod & Copepod')

Tem_algae <- subset(Tem_data,new_group=='Algae & Algae')
Tem_algae_cya <- subset(Tem_data,new_group=='Algae & Cyanobacteria')
Tem_algae_zoo <- subset(Tem_data,new_group=='Algae & Cladocera'|new_group=='Algae & Copepod'|new_group=='Algae & Rotifer')
Tem_cya <- subset(Tem_data,new_group=='Cyanobacteria & Cyanobacteria')
Tem_cya_zoo <- subset(Tem_data,new_group=='Cyanobacteria & Cladocera'|new_group=='Cyanobacteria & Copepod'|new_group=='Cyanobacteria & Rotifer')
Tem_zoo <- subset(Tem_data,new_group=='Copepod & Rotifer'|
                    new_group=='Rotifer & Rotifer'|
                    new_group=='Cladocera & Rotifer'|
                    new_group=='Cladocera & Cladocera'|
                    new_group=='Copepod & Cladocera'|
                    new_group=='Cladocera & Copepod'|
                    new_group=='Copepod & Copepod')

df1 <- Sub_algae
df1$Low_High <- factor(df1$Low_High, levels = c("Low", "High"))
plot1 <- ggbarplot(df1, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot1


df2 <- Sub_algae_cya
df2$Low_High <- factor(df2$Low_High, levels = c("Low", "High"))
plot2 <- ggbarplot(df2, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot2


df3 <- Sub_algae_zoo
df3$Low_High <- factor(df3$Low_High, levels = c("Low", "High"))
plot3 <- ggbarplot(df3, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot3


df4 <- Sub_cya
df4$Low_High <- factor(df4$Low_High, levels = c("Low", "High"))
plot4 <- ggbarplot(df4, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot4


df5 <- Sub_cya_zoo
df5$Low_High <- factor(df5$Low_High, levels = c("Low", "High"))
plot5 <- ggbarplot(df5, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot5


df6 <- Sub_zoo
df6$Low_High <- factor(df6$Low_High, levels = c("Low", "High"))
plot6 <- ggbarplot(df6, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot6

ggarrange(plot1,  plot2,  plot3,
          plot4,  plot5,  plot6,
          ncol = 3, nrow = 2, widths = c(1, 1, 1))



df7 <- Tem_algae
df7$Low_High <- factor(df7$Low_High, levels = c("Low", "High"))
plot7 <- ggbarplot(df7, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot7


df8 <- Tem_algae_cya
df8$Low_High <- factor(df8$Low_High, levels = c("Low", "High"))
plot8 <- ggbarplot(df8, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot8


df9 <- Tem_algae_zoo
df9$Low_High <- factor(df9$Low_High, levels = c("Low", "High"))
plot9 <- ggbarplot(df9, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot9


df10 <- Tem_cya
df10$Low_High <- factor(df10$Low_High, levels = c("Low", "High"))
plot10 <- ggbarplot(df10, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot10


df11 <- Tem_cya_zoo
df11$Low_High <- factor(df11$Low_High, levels = c("Low", "High"))
plot11 <- ggbarplot(df11, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot11


df12 <- Tem_zoo
df12$Low_High <- factor(df12$Low_High, levels = c("Low", "High"))
plot12 <- ggbarplot(df12, x = "Low_High", y = "abs",
                   add = "mean_se",
                   fill = "Low_High",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sub_algae") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot12

ggarrange(plot7,  plot8,  plot9,
          plot10,  plot11,  plot12,
          ncol = 3, nrow = 2, widths = c(1, 1, 1))
