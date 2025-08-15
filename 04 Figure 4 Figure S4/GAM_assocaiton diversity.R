association_diversity <- read.csv('interaction_div_all.csv', header=TRUE, row.names = 1)
library(mgcv)
library(ggplot2)
library(ggsci)
gam_all <- gam(association_diversity$All ~ s(association_diversity$TSIc,k=10), family=gaussian(link='identity'))
summary(gam_all)
plot(gam_all)
gam_Algae <- gam(association_diversity$Algae ~ s(association_diversity$TSIc,k=10), family=gaussian(link='identity'))
summary(gam_Algae)
plot(gam_Algae)
gam_Cyanobacteria <- gam(association_diversity$Cyanobacteria ~ s(association_diversity$TSIc,k=10), family=gaussian(link='identity'))
summary(gam_Cyanobacteria)
plot(gam_Cyanobacteria)
gam_Zooplankton <- gam(association_diversity$Zooplankton ~ s(association_diversity$TSIc,k=10), family=gaussian(link='identity'))
summary(gam_Zooplankton)
plot(gam_Zooplankton)


p1 <- ggplot(association_diversity, aes(x = TSIc, y = All, color=TSI_group)) +
        geom_point(size=3) +
        ylim(0, 600)+
        scale_x_continuous(limits=c(0,100),breaks = seq(0, 100, 20))+
        stat_smooth(method = "gam", formula = y ~ s(x,k=10), color = "black", se = TRUE) +
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
ggsave("div_all_plots.pdf", p1, width = 10, height = 8, units = "cm")

p2 <- ggplot(association_diversity, aes(x = TSIc, y = Algae, color=TSI_group)) +
        geom_point(size=3) +
        ylim(0, 700)+
        scale_x_continuous(limits=c(0,100),breaks = seq(0, 100, 20))+
        stat_smooth(method = "gam", formula = y ~ s(x,k=10), color = "black", se = TRUE) +
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

p3 <- ggplot(association_diversity, aes(x = TSIc, y = Cyanobacteria, color=TSI_group)) +
        geom_point(size=3) +
        scale_x_continuous(limits=c(0,100),breaks = seq(0, 100, 20))+
        scale_y_continuous(limits=c(0,125),breaks = seq(0, 125, 25))+
        #ylim(0, 125)+
        stat_smooth(method = "gam", formula = y ~ s(x,k=10), color = "black", se = TRUE) +
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

p4 <- ggplot(association_diversity, aes(x = TSIc, y = Zooplankton, color=TSI_group)) +
        geom_point(size=3) +
        ylim(0, 400)+
        scale_x_continuous(limits=c(0,100),breaks = seq(0, 100, 20))+
        stat_smooth(method = "gam", formula = y ~ s(x,k=10), color = "black", se = TRUE) +
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
p4

library(cowplot)
combined_plot <- plot_grid(p2, p3, p4,nrow = 2, ncol = 3, align = 'hv')
combined_plot
ggsave("div_each_plots.pdf", combined_plot, width = 18, height = 12, units = "cm")

#******************************************************************************

