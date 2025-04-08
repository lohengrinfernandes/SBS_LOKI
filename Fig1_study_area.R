#################################################################################################################
####################### --- Loading packages and seting basic graphic definitions --- ###########################
#################################################################################################################

lapply(c("dplyr","scatterplot3d","ggplot2","gridExtra","viridis","sf","rnaturalearth","ggspatial","MASS","grid",
         "geobr"), require, character.only = TRUE)


map_theme <- function() {
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
            legend.position = element_blank())}



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
      annotate("rect", xmin = -47, xmax = -41, ymin = -25, ymax = -22, 
               colour = alpha("orange", 1), fill = NA, linetype = "dashed", linewidth = 1.5)+
      map_theme()+
      theme(panel.border = element_blank(),
            axis.line = element_line(color = "black"))
     



stations <- copepoda_final %>% distinct(station, lat, lon)
sampa <- read_state(code_state = "SP",  year=2020, showProgress = FALSE)
rioja <- read_state(code_state = "RJ",  year=2020, showProgress = FALSE)
espir <- read_state(code_state = "ES",  year=2020, showProgress = FALSE)
paran <- read_state(code_state = "PR",  year=2020, showProgress = FALSE)
santa <- read_state(code_state = "SC",  year=2020, showProgress = FALSE)
minas <- read_state(code_state = "MG",  year=2020, showProgress = FALSE)
world <-  ne_countries(scale = "medium", returnclass = "sf") 

sstat <- stations %>% filter(!station %in% c(218,219,224,240,241,242))

region <-
  ggplot() +
    geom_sf(data = sampa, fill = "grey70", color = "black", lwd =0.05)+
    geom_sf(data = rioja, fill = "grey70", color = "black", lwd =0.05)+
    geom_sf(data = espir, fill = "grey70", color = "black", lwd =0.05)+
    geom_sf(data = paran, fill = "grey70", color = "black", lwd =0.05)+
    geom_sf(data = santa, fill = "grey70", color = "black", lwd =0.05)+
    geom_sf(data = minas, fill = "grey70", color = "black", lwd =0.05)+
    annotate("rect", xmin = -42.5, xmax = -41, ymin = -23.5, ymax = -22.5, 
             colour = alpha("orange", 1), fill = NA, linewidth = 1.2)+
    #geom_point(data = sstat, aes(x = lon, y = lat), color = "tomato", 
    #          fontface = "bold", size = 1.5)+
    geom_text(data = sstat, aes(x = lon, y = lat, label = station ), color = "tomato", 
              fontface = "bold", size = 2)+
    scale_x_continuous(limits = c(-50, -39), 
                       breaks = seq(-50, -39, by = 2),
                       labels = c("50°W", "48°W", "46°W", "44°W", "42°W", "40°W"))+
    scale_y_continuous(limits = c(-27, -21), 
                       breaks = seq(-27, -21, by = 2),
                       labels = c("27°S", "25°S", "23°S", "21°S"))+
    labs(x = NULL, y = NULL)+
    map_theme()



pstat <- stations %>% filter(station %in% c(218,219,224,240,241,242))

bbox_corte <- st_bbox(c(xmin = -42.5, xmax = -41, ymin = -23.6, ymax = -22.5), crs = st_crs(rioja))

rioja_crop <- st_crop(rioja, bbox_corte)

CFUS <-
  ggplot() +
    geom_sf(data = rioja_crop, fill = "grey70", color = "black", lwd = 0.1)+
    geom_point(data = pstat, aes(x = lon, y = lat), shape = 19,
              color = alpha("purple", 1), size = 4)+
    geom_text(data = pstat, aes(x = lon+0.06, y = lat+0.03, label = station), 
              color = "purple", fontface = "bold", size = 3)+
    scale_x_continuous(limits = c(-42.5, -41), 
                       breaks = as.numeric(c(-42.5, -42, -41.5, -41)),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(-23.6, -22.5), 
                       breaks = as.numeric(c(-23.5, -23.0, -22.5)),
                       expand = c(0, 0))+
    labs(x = NULL, y = NULL)+
    map_theme()


ggsave(CFUS, width = 15, height = 15, units = "cm", dpi=600, path="./figures", 
       filename="CFUS.pdf", device = cairo_pdf, onefile = FALSE)

ggsave(region, width = 15, height = 15, units = "cm", dpi=600, path="./figures", 
       filename="region.pdf", onefile = FALSE, useDingbats = FALSE) 

ggsave(brazil_map, width = 15, height = 20, units = "cm", dpi=600, path="./figures", 
       filename="brazil_map.pdf", onefile = FALSE, useDingbats = FALSE)
 





 

