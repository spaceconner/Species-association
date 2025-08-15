library(openxlsx)
library(ggpubr)
ADK <- read.xlsx('Degree.xlsx',sheet = 'ADK')
Subtropical_Low <- read.xlsx('Degree.xlsx',sheet = 'Subtropical_Low')
Tingxi_Low <- read.xlsx('Degree.xlsx',sheet = 'Tingxi_Low')
Kasumigaura <- read.xlsx('Degree.xlsx',sheet = 'Lake Kasumigaura')
Subtropical_High <- read.xlsx('Degree.xlsx',sheet = 'Subtropical_High')
Tingxi_High <- read.xlsx('Degree.xlsx',sheet = 'Tingxi_High')

df1 <- ADK
plot1 <- ggbarplot(df1, x = "group", y = "value",
                   fill = "grey",
                   ylab = "Degree", xlab = NULL,
                   title = "ADK") +
  theme_pubr()
plot1

df2 <- Subtropical_Low
plot2 <- ggbarplot(df2, x = "group", y = "value",
                   fill = "grey",
                   ylab = "Degree", xlab = NULL,
                   title = "Subtropical_Low") +
  theme_pubr()
plot2

df3 <- Tingxi_Low
plot3 <- ggbarplot(df3, x = "group", y = "value",
                   fill = "grey",
                   ylab = "Degree", xlab = NULL,
                   title = "Tingxi_Low") +
  theme_pubr()
plot3

df4 <- Kasumigaura
plot4 <- ggbarplot(df4, x = "group", y = "value",
                   fill = "grey",
                   ylab = "Degree", xlab = NULL,
                   title = "Lake Kasumigaura") +
  theme_pubr()
plot4

df5 <- Subtropical_High
plot5 <- ggbarplot(df5, x = "group", y = "value",
                   fill = "grey",
                   ylab = "Degree", xlab = NULL,
                   title = "Subtropical_High") +
  theme_pubr()
plot5

df6 <- Tingxi_High
plot6 <- ggbarplot(df6, x = "group", y = "value",
                   fill = "grey",
                   ylab = "Degree", xlab = NULL,
                   title = "Tingxi_High") +
  theme_pubr()
plot6
ggarrange(plot1,  plot2,  plot3,
          plot4,  plot5,  plot6,
          ncol = 3, nrow = 2, widths = c(1, 1, 1))
