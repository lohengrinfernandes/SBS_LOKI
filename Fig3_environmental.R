#################################################################################################################
####################### --- Loading packages and seting basic graphic definitions --- ###########################
#################################################################################################################

lapply(c("dplyr","scatterplot3d","ggplot2","gridExtra","viridis","sf","rnaturalearth","ggspatial","MASS","grid",
       "geobr"), require, character.only = TRUE)

source("addgrids3d.R")

basic_graphical_theme <- function() {
        theme(axis.text.y = element_text(colour = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
             axis.text.x = element_text(colour = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.0, face = "plain"),
             #       
             panel.grid.major = element_line(linewidth = 0.5, colour = "grey95", lineend = "butt"), 
             panel.background = element_blank(),
             panel.border = element_rect(colour = "black", linewidth = 1, fill = NA),
             #      
             strip.background = element_blank(),
             strip.text = element_blank(),
             #
             axis.line.x.bottom = element_line(linewidth = 0, colour = "black", lineend = "butt"),
             axis.line.x.top    = element_line(linewidth = 0, colour = "black", lineend = "butt"),
             axis.line.y.left   = element_line(linewidth = 0, colour = "black", lineend = "butt"),
             axis.line.y.right  = element_line(linewidth = 0, colour = "black", lineend = "butt"),
             #
             axis.ticks.y = element_blank(),
             axis.ticks.x = element_blank(),
             #
             legend.position = "none",
             #
             axis.title.x = element_text(hjust = 0.5, vjust = -2, colour = "black", size = 12, face = "bold"),
             axis.title.y = element_text(hjust = 0.5, vjust = 3, colour = "black", size = 12, face = "bold"))}



#################################################################################################################
############################################# --- Environmental --- #############################################
#################################################################################################################



tri_data_rad <- 
  radiolaria_final %>%
    filter(data == "adjusted") %>%
      as.data.frame() 

radiolaria2d <-
  ggplot(tri_data_rad) +
    geom_point(aes(x = sst, y = -depth, color = flu), shape = 19, alpha = 0.6, size = tri_data_rad$adj_rad * 200) +
    scale_color_viridis(option = "C", direction = 1) +
    scale_y_continuous(limits = c(-140, 0), breaks = seq(-140, 0, by = 20)) +
    scale_x_continuous(limits = c(12, 24), breaks = seq(12, 24, by = 2)) +
    labs(x = NULL, y = "Depth (m)", color = "Fluorescence \n(mg / m³)") +
    basic_graphical_theme() +
    theme(axis.text.x = element_blank(),
          legend.position = "none")+ 
    #
    annotate("point", x = 12.5, y = -5,  colour = "black", size = max(tri_data_rad$adj_rad) * 200, shape = 21, stroke = 2) +
    annotate("point", x = 12.5, y = -20, colour = "black", size = mean(tri_data_rad$adj_rad) * 200, shape = 21, stroke = 2) +
    annotate("point", x = 12.5, y = -25, colour = "black", size = min(tri_data_rad$adj_rad) * 200, shape = 21, stroke = 2) +
    #
    annotate("text", x = 14, y = -5,  label = paste(round(max(tri_data_rad$adj_rad), 3)), colour = "black", size = 4) +
    annotate("text", x = 14, y = -15, label = paste(round(mean(tri_data_rad$adj_rad), 3)), colour = "black", size = 4) +
    annotate("text", x = 14, y = -25, label = "<0.001", colour = "black", size = 4) 



tri_data_diat <- 
  diatoms_final %>%
    filter(data == "adjusted") %>%
      as.data.frame() 

diatoms2d <-
  ggplot(tri_data_diat) +
    geom_point(aes(x = sst, y = -depth, color = flu), shape = 19, alpha = 0.6, size = tri_data_diat$adj_diat * 200) +
    scale_color_viridis(option = "C", direction = 1) +
    scale_y_continuous(limits = c(-140, 0), breaks = seq(-140, 0, by = 20)) +
    scale_x_continuous(limits = c(12, 24), breaks = seq(12, 24, by = 2)) +
    labs(x = NULL, y = NULL, color = "Fluorescence \n(mg / m³)") +
    basic_graphical_theme() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")+ 
    #
    annotate("point", x = 12.5, y = -5,  colour = "black", size = max(tri_data_diat$adj_diat) * 200, shape = 21, stroke = 2) +
    annotate("point", x = 12.5, y = -20, colour = "black", size = mean(tri_data_diat$adj_diat) * 200, shape = 21, stroke = 2) +
    annotate("point", x = 12.5, y = -25, colour = "black", size = min(tri_data_diat$adj_diat) * 200, shape = 21, stroke = 2) +
    #
    annotate("text", x = 14, y = -5,  label = paste(round(max(tri_data_diat$adj_diat), 3)), colour = "black", size = 4) +
    annotate("text", x = 14, y = -15, label = paste(round(mean(tri_data_diat$adj_diat), 3)), colour = "black", size = 4) +
    annotate("text", x = 14, y = -25, label = "<0.001", colour = "black", size = 4) 



tri_data_cop <- 
  copepoda_final %>%
    filter(data == "adjusted") %>%
      as.data.frame() 

copepoda2d <-
  ggplot(tri_data_cop) +
    geom_point(aes(x = sst, y = -depth, color = flu), shape = 19, alpha = 0.6, size = tri_data_cop$adj_cop * 200) +
    scale_color_viridis(option = "C", direction = 1) +
    scale_y_continuous(limits = c(-140, 0), breaks = seq(-140, 0, by = 20)) +
    scale_x_continuous(limits = c(12, 24), breaks = seq(12, 24, by = 2)) +
    labs(x = "SST (°C)", y = "Depth (m)", color = "Fluorescence \n(mg / m³)") +
    basic_graphical_theme() +
    theme(legend.position = "none")+ 
    #
    annotate("point", x = 12, y = 0,   colour = "black", size = max(tri_data_cop$adj_cop) * 200, shape = 21, stroke = 2) +
    annotate("point", x = 12, y = -10, colour = "black", size = mean(tri_data_cop$adj_cop) * 200, shape = 21, stroke = 2) +
    annotate("point", x = 12, y = -15, colour = "black", size = min(tri_data_cop$adj_cop) * 200, shape = 21, stroke = 2) +
    #
    annotate("text", x = 13.2, y = 0, label = paste(round(max(tri_data_cop$adj_cop), 3)), colour = "black", size = 4) +
    annotate("text", x = 13.2, y = -8, label = paste(round(mean(tri_data_cop$adj_cop), 3)), colour = "black", size = 4) +
    annotate("text", x = 13.2, y = -15, label = paste(round(min(tri_data_cop$adj_cop), 3)), colour = "black", size = 4) 





tri_data_gel <- 
  gelatinous_final %>%
    filter(data == "adjusted") %>%
      as.data.frame() 

gelatinous2d <-
  ggplot(tri_data_gel) +
    geom_point(aes(x = sst, y = -depth, color = flu), shape = 19, alpha = 0.6, size = tri_data_gel$adj_gel * 80) +
    scale_color_viridis(option = "C", direction = 1) +
    scale_y_continuous(limits = c(-140, 0), breaks = seq(-140, 0, by = 20)) +
    scale_x_continuous(limits = c(12, 24), breaks = seq(12, 24, by = 2)) +
    labs(x = "SST (°C)", y = NULL, color = "Fluorescence \n(mg / m³)") +
    basic_graphical_theme() +
    theme(axis.text.y = element_blank(),
          legend.position = c(0.86, 0.2),
          legend.key.size = unit(0.7, "cm"), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10))+ 
    #
    annotate("point", x = 12.5, y = -5,  colour = "black", size = max(tri_data_gel$adj_gel) * 80, shape = 21, stroke = 2) +
    annotate("point", x = 12.5, y = -20, colour = "black", size = mean(tri_data_gel$adj_gel) * 80, shape = 21, stroke = 2) +
    annotate("point", x = 12.5, y = -25, colour = "black", size = min(tri_data_gel$adj_gel) * 80, shape = 21, stroke = 2) +
    #
    annotate("text", x = 14, y = -5,  label = paste(round(max(tri_data_gel$adj_gel), 3)), colour = "black", size = 4) +
    annotate("text", x = 14, y = -15, label = paste(round(mean(tri_data_gel$adj_gel), 3)), colour = "black", size = 4) +
    annotate("text", x = 14, y = -25, label = "<0.001", colour = "black", size = 4) 





layout <- rbind(c(1, 2), c(3, 4))

taxons2d <- grid.arrange( radiolaria2d,  diatoms2d, copepoda2d, gelatinous2d, layout_matrix = layout)

ggsave(taxons2d, width = 30, height = 30, units = "cm", dpi=600, path="./figures", 
       filename="Fig. 3 - Environmental.pdf", onefile = FALSE, useDingbats = FALSE) 

ggsave(taxons2d, width = 30, height = 30, units = "cm", path="./figures", 
       filename="Fig. 3 - Environmental.jpeg") 





