#################################################################################################################
####################### --- Loading packages and seting basic graphic definitions --- ###########################
#################################################################################################################

lapply(c("dplyr","scatterplot3d","ggplot2","gridExtra","viridis","sf","rnaturalearth","ggspatial","MASS","grid",
       "geobr"), require, character.only = TRUE)

basic_graphical_theme <- function() {
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
             legend.position = "none",
             #
             axis.title.x = element_text(hjust = 0.5, vjust = -2, colour = "black", size = 10, face = "bold"),
             axis.title.y = element_text(hjust = 0.5, vjust = 3, colour = "black", size = 10, face = "bold"))}


options(scipen = 999) 


#################################################################################################################
###################################### --- All graphics for Copepoda --- ########################################
#################################################################################################################


copepoda_var_imp <- 
  model_copepoda$importance %>% 
    as.data.frame() %>%
      ggplot() +
        geom_boxplot(aes(x = Variable, y = IncMSE), notch = T, outlier.shape = NA, 
                     fill = NA, color = "black", size = 0.5) +
        geom_point(aes(x = Variable, y = IncMSE, size = IncNodePurity),
                   fill = alpha("mediumseagreen",0.1), 
                  color = alpha("mediumseagreen",0.2), shape = 21)+
        scale_x_discrete(limits = c("depth",  "temperature","fluorescence", "salinity"), 
                     labels = c("Depth", "SST°C", "Fluorescence","Salinity")) +  
        scale_y_continuous(limits = c(0, 60), breaks = seq(0,60, by = 10)) +
        labs(x = NULL, y = "%IncMSE") +
        basic_graphical_theme()+
        theme(legend.position = c(0.8,0.8))


###########

copepoda_rmse_validation <- huber(model_copepoda$validation$RMSE)

copepoda_rmse_plot <- 
  model_copepoda$validation %>% 
    as.data.frame() %>% 
      mutate(iteration = seq(1, nrow(.))) %>%
        dplyr::select(RMSE, iteration)%>%
          ggplot() + 
            geom_point(aes(x = iteration, y = RMSE), fill = "mediumseagreen", color = "mediumseagreen", shape = 21, alpha = 0.8, size = 1) +
            scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
            scale_y_continuous(limits = c(0.008,0.020)) +
            annotate("text", x = 500, y = 0.020, 
                     label = paste0("Range ln Copepoda: [", round(min(df_final_copepoda$Copepoda), 2), ", ", 
                                                            round(max(df_final_copepoda$Copepoda), 2), "]"), 
                     size = 4, color = "mediumseagreen", fontface = "bold") +
            geom_hline(yintercept = copepoda_rmse_validation$mu, linetype = "solid", color = "grey60", size = 1) +  
            geom_hline(yintercept = copepoda_rmse_validation$mu + copepoda_rmse_validation$s, linetype = "dashed", color = "grey60", size = 1) +
            geom_hline(yintercept = copepoda_rmse_validation$mu - copepoda_rmse_validation$s, linetype = "dashed", color = "grey60", size = 1) +  
            labs(x = NULL, y = "RMSE") +
            basic_graphical_theme()

###########

copepoda_mae_validation <- huber(model_copepoda$validation$MAE)

copepoda_mae_plot <- 
  model_copepoda$validation %>% 
    as.data.frame() %>% 
      mutate(iteration = seq(1, nrow(.))) %>%
        dplyr::select(MAE, iteration)%>%
            ggplot() + 
            geom_point(aes(x = iteration, y = MAE), fill = "mediumseagreen", color = "mediumseagreen", shape = 21, alpha = 0.8, size = 1) +
            scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
            scale_y_continuous(limits = c(0.006,0.013)) +
            annotate("text", x = 500, y = 0.013, 
                     label = paste0("Range ln Copepoda: [", round(min(df_final_copepoda$Copepoda), 2), ", ", 
                                                            round(max(df_final_copepoda$Copepoda), 2), "]"), 
                     size = 4, color = "mediumseagreen", fontface = "bold") +
            geom_hline(yintercept = copepoda_mae_validation$mu, linetype = "solid", color = "grey60", size = 1) +  
            geom_hline(yintercept = copepoda_mae_validation$mu + copepoda_mae_validation$s, linetype = "dashed", color = "grey60", size = 1) +
            geom_hline(yintercept = copepoda_mae_validation$mu - copepoda_mae_validation$s, linetype = "dashed", color = "grey60", size = 1) +  
            labs(x = NULL, y = "MAE") +
            basic_graphical_theme()


###########

copepoda_stats_msr <- huber(model_copepoda$metrics$MSR)

copepoda_msr_plot <- 
  model_copepoda$metrics %>% 
    as.data.frame() %>% 
      dplyr::select(MSR)%>%
        mutate(variable = "MSR") %>% 
          ggplot()+
            geom_density(aes(x = MSR, fill = variable, color = variable), color = "mediumseagreen", fill = "mediumseagreen", alpha = 0.8, size = 0.5) +
            scale_y_continuous(limits = c(0,25000), breaks = seq(0,25000, by = 5000))+
            scale_x_continuous(limits = c(0.00012,0.00025))+
            geom_vline(xintercept = copepoda_stats_msr$mu, linetype = "dashed", color = "black", size = 0.8) + 
            annotate("text", x = 0.00019, y = 25000, label = paste0(round(copepoda_stats_msr$mu, 5), " ± ", round(copepoda_stats_msr$s, 5)), 
                  color = "black", hjust = -0.1, size = 4, fontface = "bold") +
         labs(x = "MSR", y = NULL, fill = NULL, color = NULL) +
         basic_graphical_theme()


###########

copepoda_stats_varexpl <- huber(model_copepoda$metrics$VarExpl)

copepoda_varexpl_plot <- 
  model_copepoda$metrics %>% 
    as.data.frame() %>% 
      dplyr::select(VarExpl)%>%
        mutate(variable = "VarExpl") %>% 
          ggplot()+
            geom_density(aes(x = VarExpl, fill = variable, color = variable), color = "mediumseagreen", fill = "mediumseagreen", alpha = 0.8, size = 0.5) +
            scale_y_continuous(limits = c(0,0.08), breaks = seq(0,0.1, by = 0.02))+
            scale_x_continuous(limits = c(0,50))+
            geom_vline(xintercept = copepoda_stats_varexpl$mu, linetype = "dashed", color = "black", size = 0.8) + 
            annotate("text", x = 25, y = 0.08, label = paste0(round(copepoda_stats_varexpl$mu, 2), " ± ", round(copepoda_stats_varexpl$s, 2)), 
                  color = "black", hjust = -0.1, size = 4, fontface = "bold") +
         labs(x = "Variance Explained", y = NULL, fill = NULL, color = NULL) +
         basic_graphical_theme()


###########

layout_supplementary <- rbind(c(1, 2), c(3, 4), c(5, NA))

copepoda_plot <- 
  grid.arrange(copepoda_varexpl_plot, 
               copepoda_msr_plot,
               copepoda_mae_plot,
               copepoda_rmse_plot,
               copepoda_var_imp,
               layout_matrix = layout_supplementary)

ggsave(copepoda_plot, width = 20, height = 30, units = "cm", dpi=600, path="./figures", filename="Fig. S2 - copepoda.jpeg")
ggsave(copepoda_plot, width = 20, height = 30, units = "cm", dpi=600, path="./figures", filename="Fig. S2 - copepoda.pdf", onefile = FALSE, useDingbats = FALSE) 




#################################################################################################################
###################################### --- All graphics for Radiolaria --- ######################################
#################################################################################################################


radiolaria_var_imp <- 
  model_radiolaria$importance %>% 
    as.data.frame() %>%
      ggplot() +
        geom_boxplot(aes(x = Variable, y = IncMSE), notch = T, outlier.shape = NA, 
                     fill = NA, color = "black", size = 0.5) +
        geom_point(aes(x = Variable, y = IncMSE, size = IncNodePurity),
                   fill = alpha("mediumseagreen",0.1), 
                  color = alpha("mediumseagreen",0.2), shape = 21)+
        scale_x_discrete(limits = c("depth",  "temperature","fluorescence", "salinity"), 
                     labels = c("Depth", "SST°C", "Fluorescence","Salinity")) +  
        scale_y_continuous(limits = c(0, 70), breaks = seq(0,70, by = 10)) +
        labs(x = NULL, y = "%IncMSE") +
        basic_graphical_theme()+
        theme(legend.position = c(0.8,0.8))


###########

radiolaria_rmse_validation <- huber(model_radiolaria$validation$RMSE)

radiolaria_rmse_plot <- 
  model_radiolaria$validation %>% 
    as.data.frame() %>% 
      mutate(iteration = seq(1, nrow(.))) %>%
        dplyr::select(RMSE, iteration)%>%
          ggplot() + 
            geom_point(aes(x = iteration, y = RMSE), fill = "mediumseagreen", color = "mediumseagreen", shape = 21, alpha = 0.8, size = 1) +
            scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
            scale_y_continuous(limits = c(0.010,0.035)) +
            annotate("text", x = 500, y = 0.035, 
                     label = paste0("Range ln Radiolaria: [", round(min(df_final_radiolaria$Radiolaria), 2), ", ", 
                                                              round(max(df_final_radiolaria$Radiolaria), 2), "]"), 
                     size = 4, color = "mediumseagreen", fontface = "bold") +
            geom_hline(yintercept = radiolaria_rmse_validation$mu, linetype = "solid", color = "grey60", size = 1) +  
            geom_hline(yintercept = radiolaria_rmse_validation$mu + radiolaria_rmse_validation$s, linetype = "dashed", color = "grey60", size = 1) +
            geom_hline(yintercept = radiolaria_rmse_validation$mu - radiolaria_rmse_validation$s, linetype = "dashed", color = "grey60", size = 1) +  
            labs(x = NULL, y = "RMSE") +
            basic_graphical_theme()

###########

radiolaria_mae_validation <- huber(model_radiolaria$validation$MAE)

radiolaria_mae_plot <- 
  model_radiolaria$validation %>% 
    as.data.frame() %>% 
      mutate(iteration = seq(1, nrow(.))) %>%
        dplyr::select(MAE, iteration)%>%
            ggplot() + 
            geom_point(aes(x = iteration, y = MAE), fill = "mediumseagreen", color = "mediumseagreen", shape = 21, alpha = 0.8, size = 1) +
            scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
            scale_y_continuous(limits = c(0.005,0.020)) +
            annotate("text", x = 500, y = 0.020, 
                     label = paste0("Range ln Radiolaria: [", round(min(df_final_radiolaria$Radiolaria), 2), ", ", 
                                                              round(max(df_final_radiolaria$Radiolaria), 2), "]"), 
                     size = 4, color = "mediumseagreen", fontface = "bold") +
            geom_hline(yintercept = radiolaria_mae_validation$mu, linetype = "solid", color = "grey60", size = 1) +  
            geom_hline(yintercept = radiolaria_mae_validation$mu + radiolaria_mae_validation$s, linetype = "dashed", color = "grey60", size = 1) +
            geom_hline(yintercept = radiolaria_mae_validation$mu - radiolaria_mae_validation$s, linetype = "dashed", color = "grey60", size = 1) +  
            labs(x = NULL, y = "MAE") +
            basic_graphical_theme()


###########

radiolaria_stats_msr <- huber(model_radiolaria$metrics$MSR)

radiolaria_msr_plot <- 
  model_radiolaria$metrics %>% 
    as.data.frame() %>% 
      dplyr::select(MSR)%>%
        mutate(variable = "MSR") %>% 
          ggplot()+
            geom_density(aes(x = MSR, fill = variable, color = variable), color = "mediumseagreen", fill = "mediumseagreen", alpha = 0.8, size = 0.5) +
            scale_y_continuous(limits = c(0,8000), breaks = seq(0,8000, by = 1000))+
            scale_x_continuous(limits = c(0.00035,0.00065))+
            geom_vline(xintercept = radiolaria_stats_msr$mu, linetype = "dashed", color = "black", size = 0.8) + 
            annotate("text", x = 0.00050, y = 8000, label = paste0(round(radiolaria_stats_msr$mu, 5), " ± ", round(radiolaria_stats_msr$s, 5)), 
                  color = "black", hjust = 1, size = 4, fontface = "bold") +
         labs(x = "MSR", y = NULL, fill = NULL, color = NULL) +
         basic_graphical_theme()


###########

radiolaria_stats_varexpl <- huber(model_radiolaria$metrics$VarExpl)

radiolaria_varexpl_plot <- 
  model_radiolaria$metrics %>% 
    as.data.frame() %>% 
      dplyr::select(VarExpl)%>%
        mutate(variable = "VarExpl") %>% 
          ggplot()+
            geom_density(aes(x = VarExpl, fill = variable, color = variable), color = "mediumseagreen", fill = "mediumseagreen", alpha = 0.8, size = 0.5) +
            scale_y_continuous(limits = c(0,0.07), breaks = seq(0,0.1, by = 0.02))+
            scale_x_continuous(limits = c(0,50))+
            geom_vline(xintercept = radiolaria_stats_varexpl$mu, linetype = "dashed", color = "black", size = 0.8) + 
            annotate("text", x = 25, y = 0.07, label = paste0(round(radiolaria_stats_varexpl$mu, 2), " ± ", round(radiolaria_stats_varexpl$s, 2)), 
                  color = "black", hjust = 1, size = 4, fontface = "bold") +
         labs(x = "Variance Explained", y = NULL, fill = NULL, color = NULL) +
         basic_graphical_theme()


###########

layout_supplementary <- rbind(c(1, 2), c(3, 4), c(5, NA))

radiolaria_plot <- 
  grid.arrange(radiolaria_varexpl_plot, 
               radiolaria_msr_plot,
               radiolaria_mae_plot,
               radiolaria_rmse_plot,
               radiolaria_var_imp,
               layout_matrix = layout_supplementary)

ggsave(radiolaria_plot, width = 20, height = 30, units = "cm", dpi=600, path="./figures", filename="Fig. S3 - radiolaria.jpeg")
ggsave(radiolaria_plot, width = 20, height = 30, units = "cm", dpi=600, path="./figures", filename="Fig. S3 - radiolaria.pdf", onefile = FALSE, useDingbats = FALSE) 



#################################################################################################################
###################################### --- All graphics for Gelatinous --- ######################################
#################################################################################################################


gelatinous_var_imp <- 
  model_gelatinous$importance %>% 
    as.data.frame() %>%
      ggplot() +
        geom_boxplot(aes(x = Variable, y = IncMSE), notch = T, outlier.shape = NA, 
                     fill = NA, color = "black", size = 0.5) +
        geom_point(aes(x = Variable, y = IncMSE, size = IncNodePurity),
                   fill = alpha("mediumseagreen",0.1), 
                  color = alpha("mediumseagreen",0.2), shape = 21)+
        scale_x_discrete(limits = c("depth",  "temperature","fluorescence", "salinity"), 
                     labels = c("Depth", "SST°C", "Fluorescence","Salinity")) +  
        scale_y_continuous(limits = c(0, 60), breaks = seq(0,60, by = 10)) +
        labs(x = NULL, y = "%IncMSE") +
        basic_graphical_theme()+
        theme(legend.position = c(0.8,0.8))


###########

gelatinous_rmse_validation <- huber(model_gelatinous$validation$RMSE)

gelatinous_rmse_plot <- 
  model_gelatinous$validation %>% 
    as.data.frame() %>% 
      mutate(iteration = seq(1, nrow(.))) %>%
        dplyr::select(RMSE, iteration)%>%
          ggplot() + 
            geom_point(aes(x = iteration, y = RMSE), fill = "mediumseagreen", color = "mediumseagreen", shape = 21, alpha = 0.8, size = 1) +
            scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
            scale_y_continuous(limits = c(0.01,0.07)) +
            annotate("text", x = 500, y = 0.07, 
                     label = paste0("Range ln Gelatinous: [", round(min(df_final_gelatinous$Gelatinous), 2), ", ", 
                                                              round(max(df_final_gelatinous$Gelatinous), 2), "]"), 
                     size = 4, color = "mediumseagreen", fontface = "bold") +
            geom_hline(yintercept = gelatinous_rmse_validation$mu, linetype = "solid", color = "grey60", size = 1) +  
            geom_hline(yintercept = gelatinous_rmse_validation$mu + gelatinous_rmse_validation$s, linetype = "dashed", color = "grey60", size = 1) +
            geom_hline(yintercept = gelatinous_rmse_validation$mu - gelatinous_rmse_validation$s, linetype = "dashed", color = "grey60", size = 1) +  
            labs(x = NULL, y = "RMSE") +
            basic_graphical_theme()

###########

gelatinous_mae_validation <- huber(model_gelatinous$validation$MAE)

gelatinous_mae_plot <- 
  model_gelatinous$validation %>% 
    as.data.frame() %>% 
      mutate(iteration = seq(1, nrow(.))) %>%
        dplyr::select(MAE, iteration)%>%
            ggplot() + 
            geom_point(aes(x = iteration, y = MAE), fill = "mediumseagreen", color = "mediumseagreen", shape = 21, alpha = 0.8, size = 1) +
            scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
            scale_y_continuous(limits = c(0.005,0.030)) +
            annotate("text", x = 500, y = 0.030, 
                     label = paste0("Range ln Gelatinous: [", round(min(df_final_gelatinous$Gelatinous), 2), ", ", 
                                                              round(max(df_final_gelatinous$Gelatinous), 2), "]"), 
                     size = 4, color = "mediumseagreen", fontface = "bold") +
            geom_hline(yintercept = gelatinous_mae_validation$mu, linetype = "solid", color = "grey60", size = 1) +  
            geom_hline(yintercept = gelatinous_mae_validation$mu + gelatinous_mae_validation$s, linetype = "dashed", color = "grey60", size = 1) +
            geom_hline(yintercept = gelatinous_mae_validation$mu - gelatinous_mae_validation$s, linetype = "dashed", color = "grey60", size = 1) +  
            labs(x = NULL, y = "MAE") +
            basic_graphical_theme()


###########

gelatinous_stats_msr <- huber(model_gelatinous$metrics$MSR)

gelatinous_msr_plot <- 
  model_gelatinous$metrics %>% 
    as.data.frame() %>% 
      dplyr::select(MSR)%>%
        mutate(variable = "MSR") %>% 
          ggplot()+
            geom_density(aes(x = MSR, fill = variable, color = variable), color = "mediumseagreen", fill = "mediumseagreen", alpha = 0.8, size = 0.5) +
            scale_y_continuous(limits = c(0,3500), breaks = seq(0,3500, by = 1000))+
            scale_x_continuous(limits = c(0.0005,0.0015))+
            geom_vline(xintercept = gelatinous_stats_msr$mu, linetype = "dashed", color = "black", size = 0.8) + 
            annotate("text", x = 0.001, y = 3500, label = paste0(round(gelatinous_stats_msr$mu, 5), " ± ", round(gelatinous_stats_msr$s, 5)), 
                  color = "black", hjust = 1, size = 4, fontface = "bold") +
         labs(x = "MSR", y = NULL, fill = NULL, color = NULL) +
         basic_graphical_theme()


###########

gelatinous_stats_varexpl <- huber(model_gelatinous$metrics$VarExpl)

gelatinous_varexpl_plot <- 
  model_gelatinous$metrics %>% 
    as.data.frame() %>% 
      dplyr::select(VarExpl)%>%
        mutate(variable = "VarExpl") %>% 
          ggplot()+
            geom_density(aes(x = VarExpl, fill = variable, color = variable), color = "mediumseagreen", fill = "mediumseagreen", alpha = 0.8, size = 0.5) +
            scale_y_continuous(limits = c(0,0.04), breaks = seq(0,0.04, by = 0.02))+
            scale_x_continuous(limits = c(-20,60))+
            geom_vline(xintercept = gelatinous_stats_varexpl$mu, linetype = "dashed", color = "black", size = 0.8) + 
            annotate("text", x = 25, y = 0.04, label = paste0(round(gelatinous_stats_varexpl$mu, 2), " ± ", round(gelatinous_stats_varexpl$s, 2)), 
                  color = "black", hjust = 1, size = 4, fontface = "bold") +
         labs(x = "Variance Explained", y = NULL, fill = NULL, color = NULL) +
         basic_graphical_theme()


###########

layout_supplementary <- rbind(c(1, 2), c(3, 4), c(5, NA))

gelatinous_plot <- 
  grid.arrange(gelatinous_varexpl_plot, 
               gelatinous_msr_plot,
               gelatinous_mae_plot,
               gelatinous_rmse_plot,
               gelatinous_var_imp,
               layout_matrix = layout_supplementary)

ggsave(gelatinous_plot, width = 20, height = 30, units = "cm", dpi=600, path="./figures", filename="Fig. S4 - gelatinous.jpeg")
ggsave(gelatinous_plot, width = 20, height = 30, units = "cm", dpi=600, path="./figures", filename="Fig. S4 - gelatinous.pdf", onefile = FALSE, useDingbats = FALSE) 



#################################################################################################################
###################################### --- All graphics for Diatoms  --- ########################################
#################################################################################################################


diatoms_var_imp <- 
  model_diatoms$importance %>% 
    as.data.frame() %>%
      ggplot() +
        geom_boxplot(aes(x = Variable, y = IncMSE), notch = T, outlier.shape = NA, 
                     fill = NA, color = "black", size = 0.5) +
        geom_point(aes(x = Variable, y = IncMSE, size = IncNodePurity),
                   fill = alpha("mediumseagreen",0.1), 
                  color = alpha("mediumseagreen",0.2), shape = 21)+
        scale_x_discrete(limits = c("depth",  "temperature","fluorescence", "salinity"), 
                     labels = c("Depth", "SST°C", "Fluorescence","Salinity")) +  
        scale_y_continuous(limits = c(0, 60), breaks = seq(0,60, by = 10)) +
        labs(x = NULL, y = "%IncMSE") +
        basic_graphical_theme()+
        theme(legend.position = c(0.8,0.8))


###########

diatoms_rmse_validation <- huber(model_diatoms$validation$RMSE)

diatoms_rmse_plot <- 
  model_diatoms$validation %>% 
    as.data.frame() %>% 
      mutate(iteration = seq(1, nrow(.))) %>%
        dplyr::select(RMSE, iteration)%>%
          ggplot() + 
            geom_point(aes(x = iteration, y = RMSE), fill = "mediumseagreen", color = "mediumseagreen", shape = 21, alpha = 0.8, size = 1) +
            scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
            scale_y_continuous(limits = c(0.01,0.04)) +
            annotate("text", x = 500, y = 0.04, 
                     label = paste0("Range ln Diatoms: [", round(min(df_final_diatoms$Diatoms), 2), ", ", 
                                                           round(max(df_final_diatoms$Diatoms), 2), "]"), 
                     size = 4, color = "mediumseagreen", fontface = "bold") +
            geom_hline(yintercept = diatoms_rmse_validation$mu, linetype = "solid", color = "grey60", size = 1) +  
            geom_hline(yintercept = diatoms_rmse_validation$mu + diatoms_rmse_validation$s, linetype = "dashed", color = "grey60", size = 1) +
            geom_hline(yintercept = diatoms_rmse_validation$mu - diatoms_rmse_validation$s, linetype = "dashed", color = "grey60", size = 1) +  
            labs(x = NULL, y = "RMSE") +
            basic_graphical_theme()

###########

diatoms_mae_validation <- huber(model_diatoms$validation$MAE)

diatoms_mae_plot <- 
  model_diatoms$validation %>% 
    as.data.frame() %>% 
      mutate(iteration = seq(1, nrow(.))) %>%
        dplyr::select(MAE, iteration)%>%
            ggplot() + 
            geom_point(aes(x = iteration, y = MAE), fill = "mediumseagreen", color = "mediumseagreen", shape = 21, alpha = 0.8, size = 1) +
            scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
            scale_y_continuous(limits = c(0.0055,0.02)) +
            annotate("text", x = 500, y = 0.02, 
                     label = paste0("Range ln Diatoms: [", round(min(df_final_diatoms$Diatoms), 2), ", ", 
                                                              round(max(df_final_diatoms$Diatoms), 2), "]"), 
                     size = 4, color = "mediumseagreen", fontface = "bold") +
            geom_hline(yintercept = diatoms_mae_validation$mu, linetype = "solid", color = "grey60", size = 1) +  
            geom_hline(yintercept = diatoms_mae_validation$mu + diatoms_mae_validation$s, linetype = "dashed", color = "grey60", size = 1) +
            geom_hline(yintercept = diatoms_mae_validation$mu - diatoms_mae_validation$s, linetype = "dashed", color = "grey60", size = 1) +  
            labs(x = NULL, y = "MAE") +
            basic_graphical_theme()


###########

diatoms_stats_msr <- huber(model_diatoms$metrics$MSR)

diatoms_msr_plot <- 
  model_diatoms$metrics %>% 
    as.data.frame() %>% 
      dplyr::select(MSR)%>%
        mutate(variable = "MSR") %>% 
          ggplot()+
            geom_density(aes(x = MSR, fill = variable, color = variable), color = "mediumseagreen", fill = "mediumseagreen", alpha = 0.8, size = 0.5) +
            scale_y_continuous(limits = c(0,6500), breaks = seq(0,6500, by = 1000))+
            scale_x_continuous(limits = c(0.0003,0.0007))+
            geom_vline(xintercept = diatoms_stats_msr$mu, linetype = "dashed", color = "black", size = 0.8) + 
            annotate("text", x = 0.0005, y = 6500, label = paste0(round(diatoms_stats_msr$mu, 5), " ± ", round(diatoms_stats_msr$s, 5)), 
                  color = "black", hjust = 1, size = 4, fontface = "bold") +
         labs(x = "MSR", y = NULL, fill = NULL, color = NULL) +
         basic_graphical_theme()


###########

diatoms_stats_varexpl <- huber(model_diatoms$metrics$VarExpl)

diatoms_varexpl_plot <- 
  model_diatoms$metrics %>% 
    as.data.frame() %>% 
      dplyr::select(VarExpl)%>%
        mutate(variable = "VarExpl") %>% 
          ggplot()+
            geom_density(aes(x = VarExpl, fill = variable, color = variable), color = "mediumseagreen", fill = "mediumseagreen", alpha = 0.8, size = 0.5) +
            scale_y_continuous(limits = c(0,0.09), breaks = seq(0,0.09, by = 0.02))+
            scale_x_continuous(limits = c(-10,30))+
            geom_vline(xintercept = diatoms_stats_varexpl$mu, linetype = "dashed", color = "black", size = 0.8) + 
            annotate("text", x = 9, y = 0.09, label = paste0(round(diatoms_stats_varexpl$mu, 2), " ± ", round(diatoms_stats_varexpl$s, 2)), 
                  color = "black", hjust = 1, size = 4, fontface = "bold") +
         labs(x = "Variance Explained", y = NULL, fill = NULL, color = NULL) +
         basic_graphical_theme()


###########

layout_supplementary <- rbind(c(1, 2), c(3, 4), c(5, NA))

diatoms_plot <- 
  grid.arrange(diatoms_varexpl_plot, 
               diatoms_msr_plot,
               diatoms_mae_plot,
               diatoms_rmse_plot,
               diatoms_var_imp,
               layout_matrix = layout_supplementary)

ggsave(diatoms_plot, width = 20, height = 30, units = "cm", dpi=600, path="./figures", filename="Fig. S5 - diatoms.jpeg")
ggsave(diatoms_plot, width = 20, height = 30, units = "cm", dpi=600, path="./figures", filename="Fig. S5 - diatoms.pdf", onefile = FALSE, useDingbats = FALSE) 

