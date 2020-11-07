#time block correlations

library(RVAideMemoire)
library(viridis)
library(broom)

samplecleanroundneg <- readRDS(here::here("data/derived-data/samplecleanroundneg.rds"))

#Pithouse Length and Posthole Measures Combined Correlations
cor_depth <- samplecleanroundneg %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$mean_depth)))

cor_depthplot <- ggplot(cor_depth, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Combined Types") + xlab("Years calBP") + ylab("Length/Depth Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_depth$sampled, breaks = cor_depth$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_diameter <- samplecleanroundneg %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$mean_diameter)))

cor_diameterplot <- ggplot(cor_diameter, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Combined Types") + xlab("Years calBP") + ylab("Length/Diameter Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_diameter$sampled, breaks = cor_diameter$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_volume <- samplecleanroundneg %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$mean_volume)))

cor_volumeplot <- ggplot(cor_volume, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Combined Types") + xlab("Years calBP") + ylab("Length/Volume Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_volume$sampled, breaks = cor_volume$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



#Pithouse Length and Posthole Measures Correlations - Main Type
cor_depthMain <- samplecleanroundneg %>%
  filter(PillarPosition == "Main") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$mean_depth)))

cor_depthplotMain <- ggplot(cor_depthMain, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Main Type") + xlab("Years calBP") + ylab("Length/Depth Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_depthMain$sampled, breaks = cor_depthMain$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_diameterMain <- samplecleanroundneg %>%
  filter(PillarPosition == "Main") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$mean_diameter)))

cor_diameterplotMain <- ggplot(cor_diameterMain, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Main Type") + xlab("Years calBP") + ylab("Length/Diameter Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_diameterMain$sampled, breaks = cor_diameterMain$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_volumeMain <- samplecleanroundneg %>%
  filter(PillarPosition == "Main") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$mean_volume)))

cor_volumeplotMain <- ggplot(cor_volumeMain, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Main Type") + xlab("Years calBP") + ylab("Length/Volume Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_volumeMain$sampled, breaks = cor_volumeMain$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



#Pithouse Length and Posthole Measures Correlations - MainWall Type
cor_depthMainWall <- samplecleanroundneg %>%
  filter(PillarPosition == "MainWall") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$mean_depth)))

cor_depthplotMainWall <- ggplot(cor_depthMainWall, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("MainWall Type") + xlab("Years calBP") + ylab("Length/Depth Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_depthMainWall$sampled, breaks = cor_depthMainWall$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_diameterMainWall <- samplecleanroundneg %>%
  filter(PillarPosition == "MainWall") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$mean_diameter)))

cor_diameterplotMainWall <- ggplot(cor_diameterMainWall, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("MainWall Type") + xlab("Years calBP") + ylab("Length/Diameter Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_diameterMainWall$sampled, breaks = cor_diameterMainWall$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_volumeMainWall <- samplecleanroundneg %>%
  filter(PillarPosition == "MainWall") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$mean_volume)))

cor_volumeplotMainWall <- ggplot(cor_volumeMainWall, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("MainWall Type") + xlab("Years calBP") + ylab("Length/Volume Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_volumeMainWall$sampled, breaks = cor_volumeMainWall$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))




##Length and Posthole SD correlations


#Pithouse Length and Posthole Measures Combined Correlations - SD
cor_depthSD <- samplecleanroundneg %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$sd_depth)))

cor_depthplotSD <- ggplot(cor_depthSD, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Combined Types") + xlab("Years calBP") + ylab("Length/Depth SD Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_depthSD$sampled, breaks = cor_depthSD$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_diameterSD <- samplecleanroundneg %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$sd_diameter)))

cor_diameterplotSD <- ggplot(cor_diameterSD, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Combined Types") + xlab("Years calBP") + ylab("Length/Diameter SD Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_diameterSD$sampled, breaks = cor_diameterSD$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_volumeSD <- samplecleanroundneg %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$sd_volume)))

cor_volumeplotSD <- ggplot(cor_volumeSD, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Combined Types") + xlab("Years calBP") + ylab("Length/Volume SD Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_volumeSD$sampled, breaks = cor_volumeSD$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



#Pithouse Length and Posthole Measures Correlations - Main Type - SD
cor_depthMainSD <- samplecleanroundneg %>%
  filter(PillarPosition == "Main") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$sd_depth)))

cor_depthplotMainSD <- ggplot(cor_depthMainSD, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Main Type") + xlab("Years calBP") + ylab("Length/Depth SD Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_depthMainSD$sampled, breaks = cor_depthMainSD$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_diameterMainSD <- samplecleanroundneg %>%
  filter(PillarPosition == "Main") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$sd_diameter)))

cor_diameterplotMainSD <- ggplot(cor_diameterMainSD, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Main Type") + xlab("Years calBP") + ylab("Length/Diameter SD Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_diameterMainSD$sampled, breaks = cor_diameterMainSD$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_volumeMainSD <- samplecleanroundneg %>%
  filter(PillarPosition == "Main") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$sd_volume)))

cor_volumeplotMainSD <- ggplot(cor_volumeMainSD, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("Main Type") + xlab("Years calBP") + ylab("Length/Volume SD Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_volumeMainSD$sampled, breaks = cor_volumeMainSD$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



#Pithouse Length and Posthole Measures Correlations - MainWall Type - SD
cor_depthMainWallSD <- samplecleanroundneg %>%
  filter(PillarPosition == "MainWall") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$sd_depth)))

cor_depthplotMainWallSD <- ggplot(cor_depthMainWallSD, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("MainWall Type") + xlab("Years calBP") + ylab("Length/Depth SD Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_depthMainWallSD$sampled, breaks = cor_depthMainWallSD$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_diameterMainWallSD <- samplecleanroundneg %>%
  filter(PillarPosition == "MainWall") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$sd_diameter)))

cor_diameterplotMainWallSD <- ggplot(cor_diameterMainWallSD, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("MainWall Type") + xlab("Years calBP") + ylab("Length/Diameter SD Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_diameterMainWallSD$sampled, breaks = cor_diameterMainWallSD$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


cor_volumeMainWallSD <- samplecleanroundneg %>%
  filter(PillarPosition == "MainWall") %>%
  group_by(sampled) %>%
  group_modify(~ glance(spearman.ci(.x$Length, .x$sd_volume)))

cor_volumeplotMainWallSD <- ggplot(cor_volumeMainWallSD, aes(x = sampled, y = estimate, fill = as.factor(sampled))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=conf.low.Inf, ymax=conf.high.Sup), position=position_dodge(.9)) +
  scale_color_viridis(option = "D") +
  coord_cartesian(ylim = c(-.75,1)) +
  ggtitle("MainWall Type") + xlab("Years calBP") + ylab("Length/Volume SD Correlation") +
  theme_minimal() +
  scale_x_reverse(labels = cor_volumeMainWallSD$sampled, breaks = cor_volumeMainWallSD$sampled) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



#save multiplots

#Combined
pdf(file = here::here("figures/TimeBlockCorMulti.pdf"))
gridExtra::grid.arrange(cor_depthplot,
                        cor_diameterplot,
                        cor_volumeplot,
                        ncol=3)
dev.off()

#Main
pdf(file = here::here("figures/TimeBlockCorMultiMain.pdf"))
gridExtra::grid.arrange(cor_depthplotMain,
                        cor_diameterplotMain,
                        cor_volumeplotMain,
                        ncol=3)
dev.off()

#MainWall
pdf(file = here::here("figures/TimeBlockCorMultiMainWall.pdf"))
gridExtra::grid.arrange(cor_depthplotMainWall,
                        cor_diameterplotMainWall,
                        cor_volumeplotMainWall,
                        ncol=3)
dev.off()

#Combined SD
pdf(file = here::here("figures/TimeBlockCorMultiSD.pdf"))
gridExtra::grid.arrange(cor_depthplotSD,
                        cor_diameterplotSD,
                        cor_volumeplotSD,
                        ncol=3)
dev.off()

#Main SD
pdf(file = here::here("figures/TimeBlockCorMultiMainSD.pdf"))
gridExtra::grid.arrange(cor_depthplotMainSD,
                        cor_diameterplotMainSD,
                        cor_volumeplotMainSD,
                        ncol=3)
dev.off()

#MainWall SD
pdf(file = here::here("figures/TimeBlockCorMultiMainWallSD.pdf"))
gridExtra::grid.arrange(cor_depthplotMainWallSD,
                        cor_diameterplotMainWallSD,
                        cor_volumeplotMainWallSD,
                        ncol=3)
dev.off()

#Depth
pdf(file = here::here("figures/TimeBlockCorMultiDepth.pdf"))
gridExtra::grid.arrange(cor_depthplot,
                        cor_depthplotMain,
                        cor_depthplotMainWall,
                        ncol=3,
                        top = "Depth Correlations")
dev.off()

#Diameter
pdf(file = here::here("figures/TimeBlockCorMultiDiameter.pdf"))
gridExtra::grid.arrange(cor_diameterplot,
                        cor_diameterplotMain,
                        cor_diameterplotMainWall,
                        ncol=3,
                        top = "Diameter Correlations")
dev.off()

#Volume
pdf(file = here::here("figures/TimeBlockCorMultiVolume.pdf"))
gridExtra::grid.arrange(cor_volumeplot,
                        cor_volumeplotMain,
                        cor_volumeplotMainWall,
                        ncol=3,
                        top = "Volume Correlations")
dev.off()

#DepthSD
pdf(file = here::here("figures/TimeBlockCorMultiDepthSD.pdf"))
gridExtra::grid.arrange(cor_depthplotSD,
                        cor_depthplotMainSD,
                        cor_depthplotMainWallSD,
                        ncol=3,
                        top = "Depth Variation Correlations")
dev.off()

#DiameterSD
pdf(file = here::here("figures/TimeBlockCorMultiDiameterSD.pdf"))
gridExtra::grid.arrange(cor_diameterplotSD,
                        cor_diameterplotMainSD,
                        cor_diameterplotMainWallSD,
                        ncol=3,
                        top = "Diameter Variation Correlations")
dev.off()

#VolumeSD
pdf(file = here::here("figures/TimeBlockCorMultiVolumeSD.pdf"))
gridExtra::grid.arrange(cor_volumeplotSD,
                        cor_volumeplotMainSD,
                        cor_volumeplotMainWallSD,
                        ncol=3,
                        top = "Volume Variations Correlations")
dev.off()