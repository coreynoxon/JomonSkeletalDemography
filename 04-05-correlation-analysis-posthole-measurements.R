###04-5 - correlation-analysis- posthole diamater and depth correlations

#These create correlation matrices for posthole diameter and depth measurements

cleanposts <- readRDS(here::here("data/derived-data/cleanposts.rds"))

##Posthole Diameter and Depth Correlations

#Posthole Diameter Posthole Depth
SpearmanDiameterDepth <- ggscatter(cleanposts, x = "D_Scaled", y = "FinalDepth", 
                                   add = "reg.line", conf.int = TRUE,
                                   title = "Posthole Diameter and Posthole Depth",
                                   xlab = "Posthole Diameter (m)", ylab = "Posthole Depth (m)",
                                   shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 0.45, label.y = 1.2) +
  coord_cartesian(ylim = c(0,1.25), xlim = c(0,0.7)) +
  theme_minimal()



SpearmanDiameterDepthMain <- filter(cleanposts, PillarPosition == 'Main') %>% 
  ggscatter(x = "D_Scaled", y = "FinalDepth", 
            add = "reg.line", conf.int = TRUE,
            title = "Posthole Diameter and Posthole Depth - Main Type",
            xlab = "Posthole Diameter (m)", ylab = "Posthole Depth (m)",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 0.45, label.y = 1.2) +
  coord_cartesian(ylim = c(0,1.25), xlim = c(0,0.7)) +
  theme_minimal()


SpearmanDiameterDepthMainWall <- filter(cleanposts, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "D_Scaled", y = "FinalDepth", 
            add = "reg.line", conf.int = TRUE,
            title = "Posthole Diameter and Posthole Depth - MainWall Type",
            xlab = "Posthole Diameter (m)", ylab = "Posthole Depth (m)",
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman", label.x = 0.45, label.y = 1.2) +
  coord_cartesian(ylim = c(0,1.25), xlim = c(0,0.7)) +
  theme_minimal()


#Save multiplot

#Posthole Correlations Grid
pdf(file = here::here("figures/SpearmanPostholes1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanDiameterDepth,
                        SpearmanDiameterDepthMain,
                        SpearmanDiameterDepthMainWall,
                        nrow=3)
dev.off()
