#################################################################################################################
####################### --- Loading packages and seting basic graphic definitions --- ###########################
#################################################################################################################

lapply(c("dplyr","scatterplot3d","ggplot2","gridExtra","viridis","sf","rnaturalearth","ggspatial","MASS","grid",
         "geobr"), require, character.only = TRUE)


map_theme <- function() {
  list(scale_x_continuous(limits = c(-50, -39), 
                          breaks = seq(-50, -39, by = 2),
                          labels = c("50°W", "48°W", "46°W", "44°W", "42°W", "40°W")),
       scale_y_continuous(limits = c(-27, -21), 
                          breaks = seq(-27, -21, by = 2),
                          labels = c("27°S", "25°S", "23°S", "21°S")),
       theme(axis.text.y = element_text(colour = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
             axis.text.x = element_text(colour = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0.0, face = "plain"),
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
             legend.position = "right",
             legend.key.size = unit(0.5, "cm"),         
             legend.text = element_text(size = 8),       
             legend.title = element_text(size = 8),
             #
             axis.title.x = element_blank(),
             axis.title.y = element_blank()))}




#################################################################################################################
######################################### --- INTERPOLATION GRAPHICS --- ########################################
################################################################################################################# 


facet_labels <- 
  data.frame(layer = factor(c("upper", "middle", "below"), 
                            levels = c("upper", "middle", "below")),
             lon = c(-50, -50, -50),
             lat = c(-22, -22, -22),  
             label = c("MLD", "Thermocline", "Deep Layer"))


copepoda_inter_graph <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  ggplot() +
  facet_grid(factor(layer, levels = c("upper", "middle", "below")) ~ .) +
  geom_tile(data = copepoda_layers, aes(x = lon, y = lat, fill = copepoda)) +
  scale_fill_viridis(option = "H", discrete = FALSE) +
  geom_sf(fill = "grey70", color = "black") +
  geom_text(data = facet_labels, aes(x = lon, y = lat, label = label), color = "black", 
            hjust = 0, vjust = -1, fontface = "bold", size = 4) +
  labs(fill = "Copepoda") +
  map_theme()+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.7, "cm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.title.position = "top")



radiolaria_inter_graph <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  ggplot() +
  facet_grid(factor(layer, levels = c("upper", "middle", "below")) ~ .) +
  geom_tile(data = radiolaria_layers, aes(x = lon, y = lat, fill = radiolaria)) +
  scale_fill_viridis(option = "H", discrete = FALSE) +
  geom_sf(fill = "grey70", color = "black") +
  geom_text(data = facet_labels, aes(x = lon, y = lat, label = label), color = "black", 
            hjust = 0, vjust = -1, fontface = "bold", size = 4) +
  labs(fill = "Radiolaria") +
  map_theme()+
  theme(axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.7, "cm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.title.position = "top")


gelatinous_inter_graph <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  ggplot() +
  facet_grid(factor(layer, levels = c("upper", "middle", "below")) ~ .) +
  geom_tile(data = gelatinous_layers, aes(x = lon, y = lat, fill = gelatinous)) +
  scale_fill_viridis(option = "H", discrete = FALSE) +
  geom_sf(fill = "grey70", color = "black") +
  geom_text(data = facet_labels, aes(x = lon, y = lat, label = label), color = "black", 
            hjust = 0, vjust = -1, fontface = "bold", size = 4) +
  labs(fill = "Gelatinous") +
  map_theme()+
  theme(axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.7, "cm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.title.position = "top")


diatoms_inter_graph <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  ggplot() +
  facet_grid(factor(layer, levels = c("upper", "middle", "below")) ~ .) +
  geom_tile(data = diatoms_layers, aes(x = lon, y = lat, fill = diatoms)) +
  scale_fill_viridis(option = "H", discrete = FALSE) +
  geom_sf(fill = "grey70", color = "black") +
  geom_text(data = facet_labels, aes(x = lon, y = lat, label = label), color = "black", 
            hjust = 0, vjust = -1, fontface = "bold", size = 4) +
  labs(fill = "Diatoms") +
  map_theme()+
  theme(axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.7, "cm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.title.position = "top")


layout <- rbind(c(1, 2, 3, 4))

inter_taxons <- grid.arrange(copepoda_inter_graph,
                               radiolaria_inter_graph, 
                               gelatinous_inter_graph, 
                               diatoms_inter_graph, layout_matrix = layout)


ggsave(inter_taxons, width = 42, height = 20, units = "cm", dpi=600, path="./figures", 
       filename="Fig. 2 - Interpolation.pdf", onefile = FALSE, useDingbats = FALSE) 
