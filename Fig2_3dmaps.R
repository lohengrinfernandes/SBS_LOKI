#################################################################################################################
####################### --- Loading packages and seting basic graphic definitions --- ###########################
#################################################################################################################

lapply(c("dplyr","scatterplot3d","ggplot2","gridExtra","viridis","sf","rnaturalearth","ggspatial","MASS","grid",
       "geobr"), require, character.only = TRUE)

source("addgrids3d.R")



#################################################################################################################
########################################## --- Copepoda 3D mapping  --- #########################################
#################################################################################################################


tri_data_cop <- copepoda_final %>%
  filter(data == "adjusted") %>%
  as.data.frame()


pdf("./figures/copepoda_3d.pdf", width = 8, height = 8)

colors <- 
  case_when(tri_data_cop$strat == "below"  ~ alpha("#329243", 0.6),
            tri_data_cop$strat == "middle" ~ alpha("#FD8E3F", 0.6),
            tri_data_cop$strat == "upper"  ~ alpha("#D73529", 0.6), TRUE ~ NA_character_ )

s3d <- scatterplot3d(  
  x = tri_data_cop$lon,
  y = tri_data_cop$lat,
  z = -tri_data_cop$depth,
  angle = 50,
  xlab = "Longitude",
  ylab = "Latitude",
  zlab = "Depth (m)",
  main = "Copepodda",
  pch = "",
  xlim = c(-42.4, -41.0),
  ylim = c(-23.8, -22.8),
  zlim = c(0, -140), 
  grid=FALSE, 
  col.axis = "black",
  lty.axis = 1, 
  box=FALSE)

addgrids3d(  
  x = tri_data_cop$lon,
  y = tri_data_cop$lat,
  z = -tri_data_cop$depth,
  angle = 50,
  xlim = c(-42.4, -41.0),
  ylim = c(-23.8, -22.8),
  zlim = c(0, -140), 
  grid = c("xy", "xz", "yz"),
  col.grid = "grey90")

s3d$points3d(  
  x = tri_data_cop$lon,
  y = tri_data_cop$lat,
  z = -tri_data_cop$depth,
  cex = tri_data_cop$adj_cop * 100,
  col = alpha(colors,0.6),
  type = "p",
  pch = 19)


max_size_cop <- max(tri_data_cop$adj_cop * 100)
median_size_cop <- mean(tri_data_cop$adj_cop * 100)
min_size_cop <- min(tri_data_cop$adj_cop * 100)

legend("topleft",
       legend = c(paste(round(max_size_cop / 100, 3)),
                  paste(round(median_size_cop / 100, 3)),
                  paste(round(min_size_cop / 100, 3))),
       pch = 21,
       pt.cex = c(max_size_cop, median_size_cop, min_size_cop),
       col = "black",
       pt.bg = NA,
       bty = "n", 
       title = NULL)

dev.off()



#################################################################################################################
######################################## --- Radiolaria 3D mapping  --- #########################################
#################################################################################################################

tri_data_rad <- radiolaria_final %>%
  filter(data == "adjusted") %>%
  as.data.frame()


pdf("./figures/radiolaria_3d.pdf", width = 8, height = 8)

colors <- 
  case_when(tri_data_rad$strat == "below"  ~ alpha("#329243", 0.6),
            tri_data_rad$strat == "middle" ~ alpha("#FD8E3F", 0.6),
            tri_data_rad$strat == "upper"  ~ alpha("#D73529", 0.6), TRUE ~ NA_character_ )

s3d <- scatterplot3d(  
  x = tri_data_rad$lon,
  y = tri_data_rad$lat,
  z = -tri_data_rad$depth,
  angle = 50,
  xlab = "Longitude",
  ylab = "Latitude",
  zlab = "Depth (m)",
  main = NULL,
  pch = "",
  xlim = c(-42.4, -41.0),
  ylim = c(-23.8, -22.8),
  zlim = c(0, -140), 
  grid=FALSE, 
  col.axis = "black",
  lty.axis = 1, 
  box=FALSE)

addgrids3d(  
  x = tri_data_rad$lon,
  y = tri_data_rad$lat,
  z = -tri_data_rad$depth,
  angle = 50,
  xlim = c(-42.4, -41.0),
  ylim = c(-23.8, -22.8),
  zlim = c(0, -140), 
  grid = c("xy", "xz", "yz"),
  col.grid = "grey90")

s3d$points3d(  
  x = tri_data_rad$lon,
  y = tri_data_rad$lat,
  z = -tri_data_rad$depth,
  cex = tri_data_rad$adj_rad * 100,
  col = alpha(colors,0.6),
  type = "p",
  pch = 19)


max_size_rad <- max(tri_data_rad$adj_rad * 100)
median_size_rad <- mean(tri_data_rad$adj_rad * 100)
min_size_rad <- min(tri_data_rad$adj_rad * 100)

legend("topleft",
       legend = c(paste(round(max_size_rad / 100, 3)),
                  paste(round(median_size_rad / 100, 3)),
                  paste("<0.001")),
       pch = 21,
       pt.cex = c(max_size_rad, median_size_rad, min_size_rad),
       col = "black",
       pt.bg = NA,
       bty = "n", 
       title = NULL)

dev.off()



#################################################################################################################
######################################## --- Gelatinous 3D mapping  --- #########################################
#################################################################################################################

tri_data_gel <- gelatinous_final %>%
  filter(data == "adjusted") %>%
  as.data.frame()


pdf("./figures/gelatinous_3d.pdf", width = 8, height = 8)

colors <- 
  case_when(tri_data_gel$strat == "below"  ~ alpha("#329243", 0.6),
            tri_data_gel$strat == "middle" ~ alpha("#FD8E3F", 0.6),
            tri_data_gel$strat == "upper"  ~ alpha("#D73529", 0.6), TRUE ~ NA_character_ )

s3d <- scatterplot3d(  
  x = tri_data_gel$lon,
  y = tri_data_gel$lat,
  z = -tri_data_gel$depth,
  angle = 50,
  xlab = "Longitude",
  ylab = "Latitude",
  zlab = "Depth (m)",
  main = NULL,
  pch = "",
  xlim = c(-42.4, -41.0),
  ylim = c(-23.8, -22.8),
  zlim = c(0, -140), 
  grid=FALSE, 
  col.axis = "black",
  lty.axis = 1, 
  box=FALSE)

addgrids3d(  
  x = tri_data_gel$lon,
  y = tri_data_gel$lat,
  z = -tri_data_gel$depth,
  angle = 50,
  xlim = c(-42.4, -41.0),
  ylim = c(-23.8, -22.8),
  zlim = c(0, -140), 
  grid = c("xy", "xz", "yz"),
  col.grid = "grey90")

s3d$points3d(  
  x = tri_data_gel$lon,
  y = tri_data_gel$lat,
  z = -tri_data_gel$depth,
  cex = tri_data_gel$adj_gel * 30,
  col = alpha(colors,0.6),
  type = "p",
  pch = 19)

max_size_gel <- max(tri_data_gel$adj_gel * 30)
median_size_gel <- mean(tri_data_gel$adj_gel * 30)
min_size_gel <- min(tri_data_gel$adj_gel * 30)

legend("topleft",
       legend = c(paste(round(max_size_gel / 30, 3)),
                  paste(round(median_size_gel/ 30, 3)),
                  paste("<0.001")),
       pch = 21,
       pt.cex = c(max_size_gel, median_size_gel, min_size_gel),
       col = "black",
       pt.bg = NA,
       bty = "n", 
       title = NULL)

dev.off()



#################################################################################################################
########################################### --- Diatoms 3D mapping  --- #########################################
#################################################################################################################

tri_data_diat <- diatoms_final %>%
  filter(data == "adjusted") %>%
  as.data.frame()


pdf("./figures/diatoms_3d.pdf", width = 8, height = 8)

colors <- 
  case_when(tri_data_diat$strat == "below"  ~ alpha("#329243", 0.6),
            tri_data_diat$strat == "middle" ~ alpha("#FD8E3F", 0.6),
            tri_data_diat$strat == "upper"  ~ alpha("#D73529", 0.6), TRUE ~ NA_character_ )

s3d <- scatterplot3d(  
  x = tri_data_diat$lon,
  y = tri_data_diat$lat,
  z = -tri_data_diat$depth,
  angle = 50,
  xlab = "Longitude",
  ylab = "Latitude",
  zlab = "Depth (m)",
  main = NULL,
  pch = "",
  xlim = c(-42.4, -41.0),
  ylim = c(-23.8, -22.8),
  zlim = c(0, -140), 
  grid=FALSE, 
  col.axis = "black",
  lty.axis = 1, 
  box=FALSE)

addgrids3d(  
  x = tri_data_diat$lon,
  y = tri_data_diat$lat,
  z = -tri_data_diat$depth,
  angle = 50,
  xlim = c(-42.4, -41.0),
  ylim = c(-23.8, -22.8),
  zlim = c(0, -140), 
  grid = c("xy", "xz", "yz"),
  col.grid = "grey90")

s3d$points3d(  
  x = tri_data_diat$lon,
  y = tri_data_diat$lat,
  z = -tri_data_diat$depth,
  cex = tri_data_diat$adj_diat * 70,
  col = alpha(colors,0.6),
  type = "p",
  pch = 19)

max_size_diat <- max(tri_data_diat$adj_diat * 70)
median_size_diat <- mean(tri_data_diat$adj_diat * 70)
min_size_diat <- min(tri_data_diat$adj_diat * 70)

legend("topleft",
       legend = c(paste(round(max_size_diat / 70, 3)),
                  paste(round(median_size_diat / 70, 3)),
                  paste("<0.001")),
       pch = 21,
       pt.cex = c(max_size_diat, median_size_diat, min_size_diat),
       col = "black",
       pt.bg = NA,
       bty = "n", 
       title = NULL)

dev.off()

