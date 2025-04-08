#################################################################################################################
####################### --- Loading packages and seting basic graphic definitions --- ###########################
#################################################################################################################


lapply(c("dplyr","tidyverse","randomForest","MASS"), require, character.only = TRUE)


source("sup_boot_rf.R")
options(scipen = 999) 

#################################################################################################################
####################### --- STEP 1: Handling dataframe and preparing for analysis --- ###########################
#################################################################################################################


df_original <- read.csv("df_original.csv", head = T)
spatial_data <- read.csv("spatial_data.csv", head = T, sep = ";" , dec = ",")

df_termoclina <- 
  df_original %>%
    dplyr::select(station,lat,lon,depth,temperature)%>%
      bind_rows(., spatial_data %>% dplyr::select(-salinity, -fluorescence))%>%
        distinct(station,lat,lon,depth,temperature)%>%
          group_by(station)%>%
            arrange(station, depth) %>%
              fill(temperature, .direction = "down")%>%
                fill(temperature, .direction = "up")%>%
                  mutate(temp_diff_values = abs(c(NA, diff(temperature)))) %>%
                    fill(temp_diff_values, .direction = "up")%>%    
                      mutate(thermocline_indices = list(if (any(temp_diff_values >= 0.2)) which(temp_diff_values >= 0.2) else NA_integer_)) %>%
                        mutate(termoclina_in = if_else(length(thermocline_indices[[1]]) > 0, depth[min(thermocline_indices[[1]])], NA_real_))%>%
                          mutate(termoclina_fin = if_else(length(thermocline_indices[[1]]) > 0, depth[min(max(thermocline_indices[[1]]) + 1, length(depth))],NA_real_ )) %>%
                            as.data.frame()%>%
                              mutate(strat = case_when(
                                     thermocline_indices == "NA" ~ "upper",
                                     depth < termoclina_in ~ "upper",
                                     depth > termoclina_fin ~ "below",
                                     depth >= termoclina_in & depth <= termoclina_fin ~ "middle", TRUE ~ NA_character_))%>%
                                dplyr::select(station,lat,lon,depth,temperature,strat)


  
thermo_profiles <-
  ggplot(df_termoclina) +
  geom_line(aes(x = temperature, y = -depth, color = strat), linewidth = 1) +
  labs(x = "SST°C", y = "Depth (m)")+
  scale_y_continuous()+
  facet_wrap(~station, nrow = 4)


ggsave(thermo_profiles, width = 40, height = 20, units = "cm", dpi=600, path="./figures", filename="Fig. S1 - Thermo_profiles.jpeg")
ggsave(thermo_profiles, width = 40, height = 20, units = "cm", dpi=600, path="./figures", filename="Fig. S1 - Thermo_profiles.pdf", onefile = FALSE, useDingbats = FALSE) 



df_env <- 
  df_original %>%
    dplyr::select("station","depth","temperature","fluorescence","salinity","oxygen") %>%
      mutate(salinity = ifelse(salinity < 30, NA, salinity),
         fluorescence = ifelse(fluorescence <= 0, NA, fluorescence),
               oxygen = ifelse(oxygen <= 0, NA, oxygen)) %>%
        group_by(station,depth)%>%
          summarise(temperature = mean(temperature, na.rm = TRUE), 
                   fluorescence = mean(fluorescence, na.rm = TRUE),
                       salinity = mean(salinity, na.rm = TRUE), 
                         oxygen = mean(oxygen, na.rm = TRUE))%>%
            mutate(d_n = ifelse(station == "240" | station == "241", "night", "day")) %>%
              mutate_at(vars(temperature, fluorescence, salinity, oxygen), ~ na_if(., NaN)) %>%
                as.data.frame()

df_zoo <-
  df_original %>%
    dplyr::select("numberid","station","depth","category") %>%
        mutate(abund = 1) %>%
          group_by(numberid, station, depth) %>%
            spread(category, abund)%>%
              replace(is.na(.), 0)%>%
                as.data.frame() %>%
                  group_by(station, depth) %>%
                    summarise(across(Copepoda:Radiolaria, ~ sum(.)))%>%
                     group_by(station)%>%
                       mutate(across(Copepoda:Radiolaria, ~ . / sum(.)))%>%
                         ungroup() %>%
                           mutate(across(Copepoda:Radiolaria, ~ replace(., is.nan(.), 0))) %>%
                             as.data.frame()
                

df_coord <- df_original %>% dplyr::select(station, lat, lon) %>% distinct()


df_join <- 
  left_join(df_zoo, df_env, by = c("station", "depth")) %>%
  left_join(., df_coord, by = "station") %>%
  as.data.frame()



#################################################################################################################
############################# --- STEP 2: Machine learning model for Copepoda --- ###############################
#################################################################################################################

z_value <- qnorm(1 - (1 - 0.99) / 2)

df_copepoda <- 
  df_join %>%
  dplyr::select(lat, lon, station, depth, Copepoda, temperature, fluorescence, salinity) %>%
  mutate(Copepoda = ifelse(Copepoda < mean(Copepoda) - z_value * sd(Copepoda) |  
                           Copepoda > mean(Copepoda) + z_value * sd(Copepoda), NA, Copepoda)) %>%
  mutate(Copepoda = ifelse(is.na(Copepoda), mean(Copepoda, na.rm = TRUE), Copepoda)) %>%
  mutate(Copepoda = log(Copepoda +1))%>%
  as.data.frame()


df_final_copepoda <- 
  rfImpute(Copepoda ~ temperature + fluorescence + salinity, data = df_copepoda, iter = 6, ntree = 999) %>%           
  as.data.frame() %>%
  bind_cols("station" = df_copepoda$station, 
              "depth" = df_copepoda$depth,  
                "lat" = df_copepoda$lat,   
                "lon" = df_copepoda$lon) %>%
  as.data.frame()%>%
  dplyr::select(station, depth, lat, lon, Copepoda, temperature, fluorescence, salinity)


model_copepoda <- sup_boot_rf(dataframe = df_final_copepoda, 
                              response_variable = "Copepoda",
                              predictores = c("depth","temperature","fluorescence","salinity"), 
                              boots = 1000,
                              train_size = 0.8, 
                              mtry = 4, 
                              ntree = 999, 
                              projection = spatial_data)


range(df_final_copepoda$Copepoda)
huber(model_copepoda$metrics$VarExpl)    
huber(model_copepoda$metrics$MSR)        
huber(model_copepoda$validation$RMSE)     
huber(model_copepoda$validation$MAE)


dffinal_copepoda_adj <- 
  data.frame(adj_cop = rowMeans(model_copepoda$adjusted)) %>%
    bind_cols(station = df_final_copepoda$station,
                depth = df_final_copepoda$depth,
                  lat = df_final_copepoda$lat, 
                  lon = df_final_copepoda$lon,
                  sst = df_final_copepoda$temperature,
                  flu = df_final_copepoda$fluorescence,
                  sal = df_final_copepoda$salinity) %>%
      mutate(data = "adjusted") %>%
        as.data.frame()


dffinal_copepoda_pred <- 
  data.frame(adj_cop = rowMeans(model_copepoda$future_predictions)) %>%
    bind_cols(station = spatial_data$station,
                depth = spatial_data$depth,
                  lat = spatial_data$lat, 
                  lon = spatial_data$lon,
                  sst = spatial_data$temperature,
                  flu = spatial_data$fluorescence,
                  sal = spatial_data$salinity) %>%
      mutate(data = "predicted") %>%
        as.data.frame()



copepoda_final <- 
  bind_rows(dffinal_copepoda_adj, dffinal_copepoda_pred) %>%
  left_join(., df_termoclina %>% dplyr::select(-temperature), by=c("station","depth","lat","lon"))%>%
  mutate(adj_cop = exp(adj_cop) - 1)%>%
  as.data.frame()



################################
#### --- INTERPOLATION --- #####
################################

lon_seq <- seq(min(copepoda_final$lon), max(copepoda_final$lon), length.out = 20)  
lat_seq <- seq(min(copepoda_final$lat), max(copepoda_final$lat), length.out = 20) 
grid_ex <- expand.grid(lon = lon_seq, lat = lat_seq) 


copepoda_upper <- 
  copepoda_final %>%
    filter(strat == "upper")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_cop = mean(adj_cop))%>%
           as.data.frame()%>%
             randomForest(adj_cop ~ lon + lat, data = ., importance = T, ntree = 999) %>%
               predict(., grid_ex)%>%
                 as.data.frame()%>%
                   bind_cols(grid_ex)%>%
                     setNames(c("copepoda", "lon", "lat"))%>%
                       mutate(layer = "upper")

copepoda_middle <- 
  copepoda_final %>%
    filter(strat == "middle")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_cop = mean(adj_cop))%>%
           as.data.frame()%>%
             randomForest(adj_cop ~ lon + lat, data = ., importance = T, ntree = 999) %>%
               predict(., grid_ex)%>%
                 as.data.frame()%>%
                   bind_cols(grid_ex)%>%
                     setNames(c("copepoda", "lon", "lat"))%>%
                       mutate(layer = "middle")

copepoda_below <- 
  copepoda_final %>%
    filter(strat == "below")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_cop = mean(adj_cop))%>%
           as.data.frame()%>%
             randomForest(adj_cop ~ lon + lat, data = ., importance = T, ntree = 999) %>%
               predict(., grid_ex)%>%
                 as.data.frame()%>%
                   bind_cols(grid_ex)%>%
                     setNames(c("copepoda", "lon", "lat"))%>%
                       mutate(layer = "below")


copepoda_layers <- bind_rows(copepoda_upper,copepoda_middle,copepoda_below)



#################################################################################################################
############################## --- Machine learning model for Diatomacea --- ####################################
#################################################################################################################

z_value <- qnorm(1 - (1 - 0.99) / 2)

df_diatoms <- 
  df_join %>%
  dplyr::select(lat, lon, station, depth, Diatoms, temperature, fluorescence, salinity) %>%
  mutate(Diatoms = ifelse(Diatoms < mean(Diatoms) - z_value * sd(Diatoms) |  
                             Diatoms > mean(Diatoms) + z_value * sd(Diatoms), NA, Diatoms)) %>%
  mutate(Diatoms = ifelse(is.na(Diatoms), mean(Diatoms, na.rm = TRUE), Diatoms)) %>%
  mutate(Diatoms = log(Diatoms +1))%>%
  as.data.frame()


df_final_diatoms <- 
  rfImpute(Diatoms ~ temperature + fluorescence + salinity, data = df_diatoms, iter = 6, ntree = 999) %>%           
  as.data.frame() %>%
  bind_cols("station" = df_diatoms$station, 
            "depth" = df_diatoms$depth,  
            "lat" = df_diatoms$lat,   
            "lon" = df_diatoms$lon) %>%
  as.data.frame()%>%
  dplyr::select(station, depth, lat, lon, Diatoms, temperature, fluorescence, salinity)


model_diatoms <- sup_boot_rf(dataframe = df_final_diatoms, 
                              response_variable = "Diatoms",
                              predictores = c("depth","temperature","fluorescence","salinity"), 
                              boots = 1000,
                              train_size = 0.8, 
                              mtry = 4, 
                              ntree = 999, 
                              projection = spatial_data)

range(df_final_diatoms$Diatoms)
huber(model_diatoms$metrics$VarExpl)    
huber(model_diatoms$metrics$MSR)        
huber(model_diatoms$validation$RMSE)     
huber(model_diatoms$validation$MAE)


dffinal_diatoms_adj <- 
  data.frame(adj_diat = rowMeans(model_diatoms$adjusted)) %>%
  bind_cols(station = df_final_diatoms$station,
            depth = df_final_diatoms$depth,
            lat = df_final_diatoms$lat, 
            lon = df_final_diatoms$lon,
            sst = df_final_diatoms$temperature,
            flu = df_final_diatoms$fluorescence,
            sal = df_final_diatoms$salinity) %>%
  mutate(data = "adjusted") %>%
  as.data.frame()


dffinal_diatoms_pred <- 
  data.frame(adj_diat = rowMeans(model_diatoms$future_predictions)) %>%
  bind_cols(station = spatial_data$station,
            depth = spatial_data$depth,
            lat = spatial_data$lat, 
            lon = spatial_data$lon,
            sst = spatial_data$temperature,
            flu = spatial_data$fluorescence,
            sal = spatial_data$salinity) %>%
  mutate(data = "predicted") %>%
  as.data.frame()



diatoms_final <- 
  bind_rows(dffinal_diatoms_adj, dffinal_diatoms_pred) %>%
  left_join(., df_termoclina %>% dplyr::select(-temperature), by=c("station","depth","lat","lon"))%>%
  mutate(adj_diat = exp(adj_diat) - 1)%>%
  as.data.frame()



################################
#### --- INTERPOLATION --- #####
################################

lon_seq <- seq(min(diatoms_final$lon), max(diatoms_final$lon), length.out = 20)  
lat_seq <- seq(min(diatoms_final$lat), max(diatoms_final$lat), length.out = 20) 
grid_ex <- expand.grid(lon = lon_seq, lat = lat_seq) 


diatoms_upper <- 
  diatoms_final %>%
    filter(strat == "upper")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_diat = mean(adj_diat))%>%
          as.data.frame()%>%
            randomForest(adj_diat ~ lon + lat, data = ., importance = T, ntree = 999) %>%
              predict(., grid_ex)%>%
                as.data.frame()%>%
                  bind_cols(grid_ex)%>%
                    setNames(c("diatoms", "lon", "lat"))%>%
                      mutate(layer = "upper")

diatoms_middle <- 
  diatoms_final %>%
    filter(strat == "middle")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_diat = mean(adj_diat))%>%
          as.data.frame()%>%
            randomForest(adj_diat ~ lon + lat, data = ., importance = T, ntree = 999) %>%
              predict(., grid_ex)%>%
                as.data.frame()%>%
                  bind_cols(grid_ex)%>%
                    setNames(c("diatoms", "lon", "lat"))%>%
                      mutate(layer = "middle")

diatoms_below <- 
  diatoms_final %>%
    filter(strat == "below")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_diat = mean(adj_diat))%>%
          as.data.frame()%>%
            randomForest(adj_diat ~ lon + lat, data = ., importance = T, ntree = 999) %>%
              predict(., grid_ex)%>%
                as.data.frame()%>%
                  bind_cols(grid_ex)%>%
                    setNames(c("diatoms", "lon", "lat"))%>%
                      mutate(layer = "below")


diatoms_layers <- bind_rows(diatoms_upper,diatoms_middle,diatoms_below)




#################################################################################################################
############################## --- Machine learning model for Radiolaria --- ####################################
#################################################################################################################

z_value <- qnorm(1 - (1 - 0.99) / 2)

df_radiolaria <- 
  df_join %>%
  dplyr::select(lat, lon, station, depth, Radiolaria, temperature, fluorescence, salinity) %>%
  mutate(Radiolaria = ifelse(Radiolaria < mean(Radiolaria) - z_value * sd(Radiolaria) |  
                             Radiolaria > mean(Radiolaria) + z_value * sd(Radiolaria), NA, Radiolaria)) %>%
  mutate(Radiolaria = ifelse(is.na(Radiolaria), mean(Radiolaria, na.rm = TRUE), Radiolaria)) %>%
  mutate(Radiolaria = log(Radiolaria +1))%>%
  as.data.frame()


df_final_radiolaria <- 
  rfImpute(Radiolaria ~ temperature + fluorescence + salinity, data = df_radiolaria, iter = 6, ntree = 999) %>%           
  as.data.frame() %>%
  bind_cols("station" = df_radiolaria$station, 
              "depth" = df_radiolaria$depth,  
                "lat" = df_radiolaria$lat,   
                "lon" = df_radiolaria$lon) %>%
  as.data.frame()%>%
  dplyr::select(station, depth, lat, lon, Radiolaria, temperature, fluorescence, salinity)


model_radiolaria <- sup_boot_rf(dataframe = df_final_radiolaria, 
                             response_variable = "Radiolaria",
                             predictores = c("depth","temperature","fluorescence","salinity"), 
                             boots = 1000,
                             train_size = 0.8, 
                             mtry = 4, 
                             ntree = 999, 
                             projection = spatial_data)

range(df_final_radiolaria$Radiolaria)
huber(model_radiolaria$metrics$VarExpl)    
huber(model_radiolaria$metrics$MSR)        
huber(model_radiolaria$validation$RMSE)     
huber(model_radiolaria$validation$MAE)


dffinal_radiolaria_adj <- 
  data.frame(adj_rad = rowMeans(model_radiolaria$adjusted)) %>%
  bind_cols(station = df_final_radiolaria$station,
            depth = df_final_radiolaria$depth,
            lat = df_final_radiolaria$lat, 
            lon = df_final_radiolaria$lon,
            sst = df_final_radiolaria$temperature,
            flu = df_final_radiolaria$fluorescence,
            sal = df_final_radiolaria$salinity) %>%
  mutate(data = "adjusted") %>%
  as.data.frame()


dffinal_radiolaria_pred <- 
  data.frame(adj_rad = rowMeans(model_radiolaria$future_predictions)) %>%
  bind_cols(station = spatial_data$station,
            depth = spatial_data$depth,
            lat = spatial_data$lat, 
            lon = spatial_data$lon,
            sst = spatial_data$temperature,
            flu = spatial_data$fluorescence,
            sal = spatial_data$salinity) %>%
  mutate(data = "predicted") %>%
  as.data.frame()



radiolaria_final <- 
  bind_rows(dffinal_radiolaria_adj, dffinal_radiolaria_pred) %>%
  left_join(., df_termoclina %>% dplyr::select(-temperature), by=c("station","depth","lat","lon"))%>%
  mutate(adj_rad = exp(adj_rad) - 1)%>%
  as.data.frame()



################################
#### --- INTERPOLATION --- #####
################################

lon_seq <- seq(min(radiolaria_final$lon), max(radiolaria_final$lon), length.out = 20)  
lat_seq <- seq(min(radiolaria_final$lat), max(radiolaria_final$lat), length.out = 20) 
grid_ex <- expand.grid(lon = lon_seq, lat = lat_seq) 


radiolaria_upper <- 
  radiolaria_final %>%
    filter(strat == "upper")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_rad = mean(adj_rad))%>%
          as.data.frame()%>%
            randomForest(adj_rad ~ lon + lat, data = ., importance = T, ntree = 999) %>%
              predict(., grid_ex)%>%
                as.data.frame()%>%
                  bind_cols(grid_ex)%>%
                    setNames(c("radiolaria", "lon", "lat"))%>%
                       mutate(layer = "upper")

radiolaria_middle <- 
  radiolaria_final %>%
    filter(strat == "middle")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_rad = mean(adj_rad))%>%
          as.data.frame()%>%
            randomForest(adj_rad ~ lon + lat, data = ., importance = T, ntree = 999) %>%
              predict(., grid_ex)%>%
                as.data.frame()%>%
                  bind_cols(grid_ex)%>%
                    setNames(c("radiolaria", "lon", "lat"))%>%
                       mutate(layer = "middle")

radiolaria_below <- 
  radiolaria_final %>%
    filter(strat == "below")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_rad = mean(adj_rad))%>%
          as.data.frame()%>%
            randomForest(adj_rad ~ lon + lat, data = ., importance = T, ntree = 999) %>%
              predict(., grid_ex)%>%
                as.data.frame()%>%
                  bind_cols(grid_ex)%>%
                    setNames(c("radiolaria", "lon", "lat"))%>%
                      mutate(layer = "below")


radiolaria_layers <- bind_rows(radiolaria_upper,radiolaria_middle,radiolaria_below)




#################################################################################################################
############################## --- Machine learning model for Gelatinosos --- ###################################
#################################################################################################################

z_value <- qnorm(1 - (1 - 0.99) / 2)

df_gelatinous <- 
  df_join %>%
  dplyr::select(lat, lon, station, depth, Gelatinous, temperature, fluorescence, salinity) %>%
  #mutate(Gelatinous = ifelse(Gelatinous < mean(Gelatinous) - z_value * sd(Gelatinous) |  
  #                           Gelatinous > mean(Gelatinous) + z_value * sd(Gelatinous), NA, Gelatinous)) %>%
  #mutate(Gelatinous = ifelse(is.na(Gelatinous), mean(Gelatinous, na.rm = TRUE), Gelatinous)) %>%
  mutate(Gelatinous = log(Gelatinous +1))%>%
  as.data.frame()


df_final_gelatinous <- 
  rfImpute(Gelatinous ~ temperature + fluorescence + salinity, data = df_gelatinous, iter = 6, ntree = 999) %>%           
  as.data.frame() %>%
  bind_cols("station" = df_gelatinous$station, 
              "depth" = df_gelatinous$depth,  
                "lat" = df_gelatinous$lat,   
                "lon" = df_gelatinous$lon) %>%
  as.data.frame()%>%
  dplyr::select(station, depth, lat, lon, Gelatinous, temperature, fluorescence, salinity)


model_gelatinous <- sup_boot_rf(dataframe = df_final_gelatinous, 
                                response_variable = "Gelatinous",
                                predictores = c("depth","temperature","fluorescence","salinity"), 
                                boots = 1000,
                                train_size = 0.8, 
                                mtry = 4, 
                                ntree = 999, 
                                projection = spatial_data)


range(df_final_gelatinous$Gelatinous)
huber(model_gelatinous$metrics$VarExpl)    
huber(model_gelatinous$metrics$MSR)        
huber(model_gelatinous$validation$RMSE)     
huber(model_gelatinous$validation$MAE)


dffinal_gelatinous_adj <- 
  data.frame(adj_gel = rowMeans(model_gelatinous$adjusted)) %>%
  bind_cols(station = df_final_gelatinous$station,
            depth = df_final_gelatinous$depth,
            lat = df_final_gelatinous$lat, 
            lon = df_final_gelatinous$lon,
            sst = df_final_gelatinous$temperature,
            flu = df_final_gelatinous$fluorescence,
            sal = df_final_gelatinous$salinity) %>%
  mutate(data = "adjusted") %>%
  as.data.frame()


dffinal_gelatinous_pred <- 
  data.frame(adj_gel = rowMeans(model_gelatinous$future_predictions)) %>%
  bind_cols(station = spatial_data$station,
            depth = spatial_data$depth,
            lat = spatial_data$lat, 
            lon = spatial_data$lon,
            sst = spatial_data$temperature,
            flu = spatial_data$fluorescence,
            sal = spatial_data$salinity) %>%
  mutate(data = "predicted") %>%
  as.data.frame()



gelatinous_final <- 
  bind_rows(dffinal_gelatinous_adj, dffinal_gelatinous_pred) %>%
  left_join(., df_termoclina %>% dplyr::select(-temperature), by=c("station","depth","lat","lon"))%>%
  mutate(adj_gel = exp(adj_gel) - 1)%>%
  as.data.frame()


################################
#### --- INTERPOLATION --- #####
################################

lon_seq <- seq(min(gelatinous_final$lon), max(gelatinous_final$lon), length.out = 20)  
lat_seq <- seq(min(gelatinous_final$lat), max(gelatinous_final$lat), length.out = 20) 
grid_ex <- expand.grid(lon = lon_seq, lat = lat_seq) 


gelatinous_upper <- 
  gelatinous_final %>%
    filter(strat == "upper")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_gel = mean(adj_gel))%>%
          as.data.frame()%>%
            randomForest(adj_gel ~ lon + lat, data = ., importance = T, ntree = 999) %>%
              predict(., grid_ex)%>%
                as.data.frame()%>%
                  bind_cols(grid_ex)%>%
                    setNames(c("gelatinous", "lon", "lat"))%>%
                      mutate(layer = "upper")

gelatinous_middle <- 
  gelatinous_final %>%
    filter(strat == "middle")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_gel = mean(adj_gel))%>%
          as.data.frame()%>%
            randomForest(adj_gel ~ lon + lat, data = ., importance = T, ntree = 999) %>%
              predict(., grid_ex)%>%
                as.data.frame()%>%
                  bind_cols(grid_ex)%>%
                    setNames(c("gelatinous", "lon", "lat"))%>%
                      mutate(layer = "middle")

gelatinous_below <- 
  gelatinous_final %>%
    filter(strat == "below")%>%
      group_by(lat,lon,station)%>%
        summarise(adj_gel = mean(adj_gel))%>%
          as.data.frame()%>%
            randomForest(adj_gel ~ lon + lat, data = ., importance = T, ntree = 999) %>%
              predict(., grid_ex)%>%
                as.data.frame()%>%
                  bind_cols(grid_ex)%>%
                    setNames(c("gelatinous", "lon", "lat"))%>%
                      mutate(layer = "below")


gelatinous_layers <- bind_rows(gelatinous_upper,gelatinous_middle,gelatinous_below)

