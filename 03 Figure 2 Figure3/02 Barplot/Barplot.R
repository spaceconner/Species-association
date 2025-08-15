library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(tidyr)

All_data <- read.xlsx('Species association.xlsx',sheet = 'All')
Sub_data <- read.xlsx('Species association.xlsx',sheet = 'Subtropical')
Tem_data <- read.xlsx('Species association.xlsx',sheet = 'Temperate')


df1 <- pivot_longer(All_data , cols = c("Low", "High"), names_to = "Group", values_to = "Value")
df1$Group <- factor(df1$Group, levels = c("Low", "High"))
plot1 <- ggbarplot(df1, x = "Group", y = "Value",
                   add = "mean_se",
                   fill = "Group",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sheet 1") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot1

df2 <- pivot_longer(Tem_data, cols = c("Low", "High"), names_to = "Group", values_to = "Value")
df2$Group <- factor(df2$Group, levels = c("Low", "High"))

plot2 <- ggbarplot(df2, x = "Group", y = "Value",
                   add = "mean_se",
                   fill = "Group",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sheet 2") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot2

df3 <- pivot_longer(Sub_data, cols = c("Low", "High"), names_to = "Group", values_to = "Value")
df3$Group <- factor(df3$Group, levels = c("Low", "High"))

plot3 <- ggbarplot(df3, x = "Group", y = "Value",
                   add = "mean_se",
                   fill = "Group",
                   palette = c("Low" = "#F1B171", "High" = "#719DDE"),
                   ylab = "Mean ± SE", xlab = NULL,
                   title = "Sheet 3") +
  stat_compare_means(method = "wilcox.test", label.y = 0.33) +
  theme_pubr()
plot3
ggarrange(plot1, NULL, NULL,NULL,
          plot2, NULL, NULL,NULL,
          plot3, NULL, NULL,NULL,
          ncol = 4, nrow = 3, widths = c(1, 0.1, 0.1))
