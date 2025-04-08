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
########################################### --- Study area map --- ##############################################
#################################################################################################################


brazil_map <- 
  ne_countries(scale = "medium", returnclass = "sf")%>%
    ggplot() +
      geom_sf(color="black", fill="grey70")+
      labs(x = "Longitude", y = "Latitude")+ 
      scale_x_continuous(limits = c(-52, -32), 
                         breaks = seq(-52, -32, by = 5),
                         labels = c("52°W", "47°W", "42°W", "37°W", "32°W"))+
      scale_y_continuous(limits = c(-35, 0), 
                         breaks = seq(-35, -0, by = 5),
                         labels = c("35°S", "30°S", "25°S", "20°S", "15°S", "10°S", "5°S", "0°"))+
      annotate(geom="text", x=-47, y=-12, label="Brazil", fontface="bold", color="black", size=6) +
      annotation_scale(location="bl", width_hint=0.5, bar_cols=c("black","white"), line_width=0.5,
                      height=unit(0.2, "cm"), pad_x=unit(1, "cm"), pad_y=unit(0.5, "cm"), text_cex=0.8) +
      annotate("rect", xmin = -47, xmax = -41, ymin = -25, ymax = -22, colour = alpha("tomato", 0.9), fill = NA, linewidth = 2)+
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
            legend.position = element_blank())


stations <- copepoda_final %>% distinct(station, lat, lon)

sampa <- read_state(code_state = "SP",  year=2020, showProgress = FALSE)
rioja <- read_state(code_state = "RJ",  year=2020, showProgress = FALSE)
espir <- read_state(code_state = "ES",  year=2020, showProgress = FALSE)
paran <- read_state(code_state = "PR",  year=2020, showProgress = FALSE)
world <-  ne_countries(scale = "medium", returnclass = "sf") 

region <-
  ggplot() +
    geom_sf(data = world, fill = "grey70", color = "black") +
    geom_sf(data = sampa, fill = "grey70", color = "black", lwd =0.1)+
    geom_sf(data = rioja, fill = "grey70", color = "black", lwd =0.1)+
    geom_sf(data = espir, fill = "grey70", color = "black", lwd =0.1)+
    geom_sf(data = paran, fill = "grey70", color = "black", lwd =0.1)+
    geom_point(data = stations, aes(x = lon, y = lat), shape = 19, color = "tomato", size = 1)+
    #geom_text(data = stations, aes(x = lon, y = lat, label = station), size = 2.5)+
    map_theme()


layout_map <- rbind(c(1,2))

map_plot <- grid.arrange(brazil_map, region, layout_matrix = layout_map)

ggsave(map_plot, width = 20, height = 20, units = "cm", dpi=600, path="./figures", filename="Fig. 1 - Map.pdf", onefile = FALSE, useDingbats = FALSE) 



