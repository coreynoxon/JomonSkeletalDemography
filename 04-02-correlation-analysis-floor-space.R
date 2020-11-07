###04-2 - correlation-analysis- floor space correlations

#These create correlation matrices for posthole measurements

sampleclean <- readRDS(here::here("data/derived-data/sampleclean.rds"))

###New Floor Correlations

SpNewFloorMeanDepth <- sampleclean_trim %>%
  select(NewFloor, mean_depth) %>%
  na.omit(as.numeric(NewFloor, mean_depth))
SpNewFloorMeanDepthResult <- Hmisc::rcorr(as.matrix(SpNewFloorMeanDepth, type = "spearman"))

###New Floor Correlation Plots

#Floor Space and Mean Post Depth
SpearmanNewFloorMeanDepth <- ggscatter(sampleclean, x = "NewFloor", y = "mean_depth", 
                                       add = "reg.line", conf.int = TRUE,
                                       title = "Combined Types",
                                       xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Mean Depth",
                                       shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 1) +
  coord_cartesian(ylim = c(0,1.1), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Mean Post Depth - Main Type
SpearmanNewFloorMeanDepthMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "mean_depth", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Mean Depth",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 35, label.y = 1) +
  coord_cartesian(ylim = c(0,1.1), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Mean Post Depth - MainWall Type
SpearmanNewFloorMeanDepthMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "mean_depth", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Mean Depth",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 1) +
  coord_cartesian(ylim = c(0,1.1), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Post Depth SD
SpearmanNewFloorDepthSD <- ggscatter(sampleclean, x = "NewFloor", y = "sd_depth", 
                                     add = "reg.line", conf.int = TRUE,
                                     title = "Combined Types",
                                     xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Depth SD",
                                     shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.48) +
  coord_cartesian(ylim = c(0,0.5), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Post Depth SD - Main Type
SpearmanNewFloorDepthSDMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "sd_depth", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Depth SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.48) +
  coord_cartesian(ylim = c(0,0.5), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Post Depth SD - MainWall Type
SpearmanNewFloorDepthSDMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "sd_depth", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Depth SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.48) +
  coord_cartesian(ylim = c(0,0.5), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Mean Post Diameter
SpearmanNewFloorMeanDiameter <- ggscatter(sampleclean, x = "NewFloor", y = "mean_diameter", 
                                          add = "reg.line", conf.int = TRUE,
                                          title = "Combined Types",
                                          xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Mean Diameter",
                                          shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.58) +
  coord_cartesian(ylim = c(0,0.6), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Mean Post Diameter - Main Type
SpearmanNewFloorMeanDiameterMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "mean_diameter", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Mean Diameter",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.58) +
  coord_cartesian(ylim = c(0,0.6), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Mean Post Diameter - MainWall Type
SpearmanNewFloorMeanDiameterMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "mean_diameter", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Mean Diameter",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.58) +
  coord_cartesian(ylim = c(0,0.6), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Post Diameter SD
SpearmanNewFloorDiameterSD <- ggscatter(sampleclean, x = "NewFloor", y = "sd_diameter", 
                                        add = "reg.line", conf.int = TRUE,
                                        title = "Combined Types",
                                        xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Diameter SD",
                                        shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.18) +
  coord_cartesian(ylim = c(0,0.2), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Post Diameter SD - Main Type
SpearmanNewFloorDiameterSDMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "sd_diameter", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Diameter SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.18) +
  coord_cartesian(ylim = c(0,0.2), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Post Diameter SD - MainWall Type
SpearmanNewFloorDiameterSDMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "sd_diameter", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Diameter SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.18) +
  coord_cartesian(ylim = c(0,0.2), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Mean Post Volume
SpearmanNewFloorMeanVolume <- ggscatter(sampleclean, x = "NewFloor", y = "mean_volume", 
                                        add = "reg.line", conf.int = TRUE,
                                        title = "Combined Types",
                                        xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Mean Volume",
                                        shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.225) +
  coord_cartesian(ylim = c(0,0.25), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Mean Post Volume - Main Type
SpearmanNewFloorMeanVolumeMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "mean_volume", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Mean Volume",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.225) +
  coord_cartesian(ylim = c(0,0.25), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Mean Post Volume - MainWall Type
SpearmanNewFloorMeanVolumeMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "mean_volume", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Mean Volume",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.225) +
  coord_cartesian(ylim = c(0,0.25), xlim = c(0,65)) +
  theme_minimal()


#Floor Space and Post Volume SD
SpearmanNewFloorVolumeSD <- ggscatter(sampleclean, x = "NewFloor", y = "sd_volume", 
                                      add = "reg.line", conf.int = TRUE,
                                      title = "Combined Types",
                                      xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Volume SD",
                                      shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.13) +
  coord_cartesian(ylim = c(0,0.15)) +
  theme_minimal()


#Floor Space and Post Volume SD - Main Type
SpearmanNewFloorVolumeSDMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "sd_volume", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Volume SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.13) +
  coord_cartesian(ylim = c(0,0.15)) +
  theme_minimal()


#Floor Space and Post Volume SD - MainWall Type
SpearmanNewFloorVolumeSDMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "sd_volume", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = expression(paste("Floor Space m"^"2")), ylab = "Post Volume SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 40, label.y = 0.13) +
  coord_cartesian(ylim = c(0,0.15)) +
  theme_minimal()



#Save Multiplots


#Floor Space Posthole Diameter Correlation Grid
pdf(file = here::here("figures/SpearmanNewFloorDiameter1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanNewFloorMeanDiameter,
                        SpearmanNewFloorDiameterSD,                       
                        SpearmanNewFloorMeanDiameterMain,
                        SpearmanNewFloorDiameterSDMain,                        
                        SpearmanNewFloorMeanDiameterMainWall, 
                        SpearmanNewFloorDiameterSDMainWall, 
                        ncol=2)
dev.off()

#Floor Space Posthole Volume Correlation Grid
pdf(file = here::here("figures/SpearmanNewFloorVolume1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanNewFloorMeanVolume, 
                        SpearmanNewFloorVolumeSD,                        
                        SpearmanNewFloorMeanVolumeMain,
                        SpearmanNewFloorVolumeSDMain,                         
                        SpearmanNewFloorMeanVolumeMainWall, 
                        SpearmanNewFloorVolumeSDMainWall, 
                        ncol=2)
dev.off()

