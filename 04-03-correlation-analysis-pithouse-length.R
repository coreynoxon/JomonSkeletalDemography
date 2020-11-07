###04-3 - correlation-analysis- pithouse length correlations

#These create correlation matrices for posthole mean and SD values

sampleclean <- readRDS(here::here("data/derived-data/sampleclean.rds"))



##Post Depth 



#Pithouse Length and Mean Post Depth
SpearmanLengthMeanDepth <- ggscatter(sampleclean, x = "Length", y = "mean_depth", 
                                     add = "reg.line", conf.int = TRUE,
                                     title = "Combined Types",
                                     xlab = "Pithouse Length (m)", ylab = "Mean Post Depth (m)",
                                     shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 1) +
  coord_cartesian(ylim = c(0,1.1)) +
  theme_minimal()


#Pithouse Length and Mean Post Depth - Main Type
SpearmanLengthMeanDepthMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "Length", y = "mean_depth", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = "Pithouse Length (m)", ylab = "Mean Post Depth (m)",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 1) +
  coord_cartesian(ylim = c(0,1.1)) +
  theme_minimal()


#Pithouse Length and Mean Post Depth - MainWall Type
SpearmanLengthMeanDepthMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "Length", y = "mean_depth", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = "Pithouse Length (m)", ylab = "Mean Post Depth (m)",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 1) +
  coord_cartesian(ylim = c(0,1.1)) +
  theme_minimal()


#Pithouse Length and Post Depth SD
SpearmanLengthDepthSD <- ggscatter(sampleclean, x = "Length", y = "sd_depth", 
                                   add = "reg.line", conf.int = TRUE,
                                   title = "Combined Types",
                                   xlab = "Pithouse Length (m)", ylab = "Post Depth SD",
                                   shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.45) +
  coord_cartesian(ylim = c(0,0.5)) +
  theme_minimal()


#Pithouse Length and Post Depth SD - Main Type
SpearmanLengthDepthSDMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "Length", y = "sd_depth", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = "Pithouse Length (m)", ylab = "Post Depth SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.45) +
  coord_cartesian(ylim = c(0,0.5)) +
  theme_minimal()


#Pithouse Length and Post Depth SD - MainWall Type
SpearmanLengthDepthSDMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "Length", y = "sd_depth", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = "Pithouse Length (m)", ylab = "Post Depth SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.45) +
  coord_cartesian(ylim = c(0,0.5)) +
  theme_minimal()




##Post Diameter



#Pithouse Length and Mean Post Diameter
SpearmanLengthMeanDiameter <- ggscatter(sampleclean, x = "Length", y = "mean_diameter", 
                                        add = "reg.line", conf.int = TRUE,
                                        title = "Combined Types",
                                        xlab = "Pithouse Length (m)", ylab = "Mean Post Diameter (m)",
                                        shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.7) +
  coord_cartesian(ylim = c(0,0.75)) +
  theme_minimal()


#Pithouse Length and Mean Post Diameter - Main Type
SpearmanLengthMeanDiameterMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "Length", y = "mean_diameter", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = "Pithouse Length (m)", ylab = "Mean Post Diameter (m)",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.7) +
  coord_cartesian(ylim = c(0,0.75)) +
  theme_minimal()


#Pithouse Length and Mean Post Depth - MainWall Type
SpearmanLengthMeanDiameterMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "Length", y = "mean_diameter", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = "Pithouse Length (m)", ylab = "Mean Post Diameter (m)",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.7) +
  coord_cartesian(ylim = c(0,0.75)) +
  theme_minimal()


#Pithouse Length and Post Diameter SD
SpearmanLengthDiameterSD <- ggscatter(sampleclean, x = "Length", y = "sd_diameter", 
                                      add = "reg.line", conf.int = TRUE,
                                      title = "Combined Types",
                                      xlab = "Pithouse Length (m)", ylab = "Post Diameter SD",
                                      shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.25) +
  coord_cartesian(ylim = c(0,0.3)) +
  theme_minimal()


#Pithouse Length and Post Diameter SD - Main Type
SpearmanLengthDiameterSDMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "Length", y = "sd_diameter", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = "Pithouse Length (m)", ylab = "Post Diameter SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.25) +
  coord_cartesian(ylim = c(0,0.3)) +
  theme_minimal()


#Pithouse Length and Post Diameter SD - MainWall Type
SpearmanLengthDiameterSDMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "Length", y = "sd_diameter", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = "Pithouse Length (m)", ylab = "Post Diameter SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.25) +
  coord_cartesian(ylim = c(0,0.3)) +
  theme_minimal()




##Post Volume



#Pithouse Length and Mean Post Volume
SpearmanLengthMeanVolume <- ggscatter(sampleclean, x = "Length", y = "mean_volume", 
                                      add = "reg.line", conf.int = TRUE,
                                      title = "Combined Types",
                                      xlab = "Pithouse Length (m)", ylab = "Mean Post Volume",
                                      shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.25) +
  coord_cartesian(ylim = c(0,0.3)) +
  theme_minimal()


#Pithouse Length and Mean Post Volume - Main Type
SpearmanLengthMeanVolumeMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "Length", y = "mean_volume", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = "Pithouse Length (m)", ylab = "Mean Post Volume",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.25) +
  coord_cartesian(ylim = c(0,0.3)) +
  theme_minimal()


#Pithouse Length and Mean Post Volume - MainWall Type
SpearmanLengthMeanVolumeMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "Length", y = "mean_volume", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = "Pithouse Length (m)", ylab = "Mean Post Volume",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.25) +
  coord_cartesian(ylim = c(0,0.3)) +
  theme_minimal()


#Pithouse Length and Post Volume SD
SpearmanLengthVolumeSD <- ggscatter(sampleclean, x = "Length", y = "sd_volume", 
                                    add = "reg.line", conf.int = TRUE,
                                    title = "Combined Types",
                                    xlab = "Pithouse Length (m)", ylab = "Post Volume SD",
                                    shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.11) +
  coord_cartesian(ylim = c(0,0.13)) +
  theme_minimal()


#Pithouse Length and Post Volume SD - Main Type
SpearmanLengthVolumeSDMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "Length", y = "sd_volume", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = "Pithouse Length (m)", ylab = "Post Volume SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.11) +
  coord_cartesian(ylim = c(0,0.13)) +
  theme_minimal()


#Pithouse Length and Post Volume SD - MainWall Type
SpearmanLengthVolumeSDMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "Length", y = "sd_volume", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = "Pithouse Length (m)", ylab = "Post Volume SD",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 7, label.y = 0.11) +
  coord_cartesian(ylim = c(0,0.13)) +
  theme_minimal()




##Save multiplots 

#Pithouse Length Posthole Depth Correlation Grid
pdf(file = here::here("figures/SpearmanLengthDepth1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthMeanDepth, 
                        SpearmanLengthDepthSD, 
                        SpearmanLengthMeanDepthMain,
                        SpearmanLengthDepthSDMain,                         
                        SpearmanLengthMeanDepthMainWall, 
                        SpearmanLengthDepthSDMainWall, 
                        ncol=2)
dev.off()

#Pithouse Length Posthole Diameter Correlation Grid
pdf(file = here::here("figures/SpearmanLengthDiameter1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthMeanDiameter, 
                        SpearmanLengthDiameterSD,                         
                        SpearmanLengthMeanDiameterMain,
                        SpearmanLengthDiameterSDMain,                        
                        SpearmanLengthMeanDiameterMainWall, 
                        SpearmanLengthDiameterSDMainWall, 
                        ncol=2)
dev.off()

#Pithouse Length Posthole Volume Correlation Grid
pdf(file = here::here("figures/SpearmanLengthVolume1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthMeanVolume, 
                        SpearmanLengthVolumeSD,                         
                        SpearmanLengthMeanVolumeMain,
                        SpearmanLengthVolumeSDMain,                         
                        SpearmanLengthMeanVolumeMainWall, 
                        SpearmanLengthVolumeSDMainWall, 
                        ncol=2)
dev.off()

#Pithouse Length Posthole Mean Depth Correlation
pdf(file = here::here("figures/SpearmanLengthMeanDepth1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthMeanDepth, 
                        SpearmanLengthMeanDepthMain,
                        SpearmanLengthMeanDepthMainWall, 
                        ncol=1)
dev.off()

#Pithouse Length Posthole SD Depth Correlation
pdf(file = here::here("figures/SpearmanLengthSDDepth1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthDepthSD, 
                        SpearmanLengthDepthSDMain,                         
                        SpearmanLengthDepthSDMainWall, 
                        ncol=1)
dev.off()

#Pithouse Length Posthole Mean Diameter Correlation
pdf(file = here::here("figures/SpearmanLengthMeanDiameter1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthMeanDiameter, 
                        SpearmanLengthMeanDiameterMain,
                        SpearmanLengthMeanDiameterMainWall, 
                        ncol=1)
dev.off()

#Pithouse Length Posthole SD Diameter Correlation Grid
pdf(file = here::here("figures/SpearmanLengthSDDiameter1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthDiameterSD,                         
                        SpearmanLengthDiameterSDMain,                        
                        SpearmanLengthDiameterSDMainWall, 
                        ncol=1)
dev.off()

#Pithouse Length Posthole Mean Volume Correlation
pdf(file = here::here("figures/SpearmanLengthMeanVolume1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthMeanVolume, 
                        SpearmanLengthMeanVolumeMain,
                        SpearmanLengthMeanVolumeMainWall, 
                        ncol=1)
dev.off()

#Pithouse Length Posthole Volume Correlation Grid
pdf(file = here::here("figures/SpearmanLengthSDVolume1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthVolumeSD,                         
                        SpearmanLengthVolumeSDMain,                         
                        SpearmanLengthVolumeSDMainWall, 
                        ncol=1)
dev.off()
