library(readxl)
library(ggpubr)

merge_data <- read_excel('Richness merge 20241105.xlsx')
merge_data$richness_phyto <- merge_data$richness_algae+merge_data$richness_cya

library(mgcv)
library(ggplot2)
library(ggsci)
gam_Algae <- gam(merge_data$richness_algae ~ s(merge_data$TSIc,k=10), family=gaussian(link='identity'))
summary(gam_Algae)
plot(gam_Algae)
gam_Cyanobacteria <- gam(merge_data$richness_cya ~ s(merge_data$TSIc,k=8), family=gaussian(link='identity'))
summary(gam_Cyanobacteria)
plot(gam_Cyanobacteria)
gam_Zooplankton <- gam(merge_data$richness_zoo ~ s(merge_data$TSIc,k=10), family=gaussian(link='identity'))
summary(gam_Zooplankton)
plot(gam_Zooplankton)


p1 <- ggplot(merge_data, aes(x = TSIc, y = richness_algae, color=TSIc_group)) +
        geom_point(size=3) +
        ylim(0, 60)+
        scale_x_continuous(limits=c(0,100),breaks = seq(0, 100, 20))+
        stat_smooth(method = "gam", formula = y ~ s(x,k=10), color = "black", se = TRUE) +
        #labs(title = "Algae")+
        scale_color_manual(values = c("Low" = "#F1B171", "High" = "#719DDE")) +
        theme(
          panel.background = element_blank(),      
          panel.grid.major = element_blank(),     
          panel.grid.minor = element_blank(),     
          panel.border = element_rect(colour = "black", fill = NA),  
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 10, color = "black"),
          legend.position = "none"
        )
p1

p2 <- ggplot(merge_data, aes(x = TSIc, y = richness_cya, color=TSIc_group)) +
        geom_point(size=3) +
        scale_x_continuous(limits=c(0,100),breaks = seq(0, 100, 20))+
        scale_y_continuous(limits=c(0,20),breaks = seq(0, 20, 5))+
        #ylim(0, 125)+
        stat_smooth(method = "gam", formula = y ~ s(x,k=8), color = "black", se = TRUE) +
        #labs(title = "Cyanobacteria")+
        scale_color_manual(values = c("Low" = "#F1B171", "High" = "#719DDE")) +
        theme(
          panel.background = element_blank(),      
          panel.grid.major = element_blank(),     
          panel.grid.minor = element_blank(),     
          panel.border = element_rect(colour = "black", fill = NA),  
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 10, color = "black"),
          legend.position = "none"
        )
p2

p3 <- ggplot(merge_data, aes(x = TSIc, y = richness_zoo, color=TSIc_group)) +
        geom_point(size=3) +
        ylim(0, 30)+
        scale_x_continuous(limits=c(0,100),breaks = seq(0, 100, 20))+
        stat_smooth(method = "gam", formula = y ~ s(x,k=10), color = "black", se = TRUE) +
        #labs(title = "Zooplankton")+
        scale_color_manual(values = c("Low" = "#F1B171", "High" = "#719DDE")) +
        theme(
          panel.background = element_blank(),      
          panel.grid.major = element_blank(),     
          panel.grid.minor = element_blank(),     
          panel.border = element_rect(colour = "black", fill = NA),  
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 10, color = "black"),
          legend.position = "none"
        )
p3

library(cowplot)
combined_plot <- plot_grid(p1, p2, p3,nrow = 2, ncol = 3, align = 'hv')
combined_plot
ggsave("richness_each_plots.pdf", combined_plot, width = 18, height = 12, units = "cm")

#******************************************************************************

