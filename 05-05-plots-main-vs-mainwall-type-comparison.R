###05-5-main-vs-mainwall-type-comparison

#This section compares the Main and MainWall types of pithouses. Because they lack posthole data, other types are not able to be compared in this way.

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

MeanDepthPlotMain1000 <- ggplot(data = subset(sampleclean_trim, !is.na(mean_depth) & PillarPosition == "Main"), 
                                aes(x = -sampled, y = mean_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Depth - M-type") + xlab("Years calBP") + ylab("Depth (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0.4,0.8)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

MeanDepthPlotMainWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(mean_depth) & PillarPosition == "MainWall"), 
                                    aes(x = -sampled, y = mean_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Depth - MW-type") + xlab("Years calBP") + ylab("Depth (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0.4,0.8)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

##Post Depth SD
SDDepthPlotMain1000 <- ggplot(data = subset(sampleclean_trim, !is.na(sd_depth) & PillarPosition == "Main"), 
                              aes(x = -sampled, y = sd_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Depth SD - M-type") + xlab("Years calBP") + ylab("SD") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,0.25)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

SDDepthPlotMainWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(sd_depth) & PillarPosition == "MainWall"), 
                                  aes(x = -sampled, y = sd_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Depth SD - MW-type") + xlab("Years calBP") + ylab("SD") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,0.25)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400), xmax=c(4600), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()

##Mean Post Diameter
MeanDiameterPlotMain1000 <- ggplot(data = subset(sampleclean_trim, !is.na(mean_diameter) & PillarPosition == "Main"), 
                                   aes(x = -sampled, y = mean_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Diameter - M-type") + xlab("Years calBP") + ylab("Diameter (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0.05,0.35)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(5200), xmax=c(5400), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()

MeanDiameterPlotMainWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(mean_diameter) & PillarPosition == "MainWall"), 
                                       aes(x = -sampled, y = mean_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Diameter - MW-type") + xlab("Years calBP") + ylab("Diameter (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0.05,0.35)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

##Post Diameter SD
SDDiameterPlotMain1000 <- ggplot(data = subset(sampleclean_trim, !is.na(sd_diameter) & PillarPosition == "Main"), 
                                 aes(x = -sampled, y = sd_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Diameter SD - M-type") + xlab("Years calBP") + ylab("SD") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0.02,0.085)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

SDDiameterPlotMainWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(sd_diameter) & PillarPosition == "MainWall"), 
                                     aes(x = -sampled, y = sd_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Diameter SD - MW-type") + xlab("Years calBP") + ylab("SD") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0.02,0.085)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

##Mean Post Volume
MeanVolumePlotMain1000 <- ggplot(data = subset(sampleclean_trim, !is.na(mean_volume) & PillarPosition == "Main"), 
                                 aes(x = -sampled, y = mean_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Volume - M-type") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,0.07)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(5200), xmax=c(5400), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()

MeanVolumePlotMainWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(mean_volume) & PillarPosition == "MainWall"), 
                                     aes(x = -sampled, y = mean_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Volume - MW-type") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,0.07)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

##Post Volume SD
SDVolumePlotMain1000 <- ggplot(data = subset(sampleclean_trim, !is.na(sd_volume) & PillarPosition == "Main"), 
                               aes(x = -sampled, y = sd_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Volume SD - M-type") + xlab("Years calBP") + ylab("SD") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,0.035)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(5200), xmax=c(5400), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()

SDVolumePlotMainWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(sd_volume) & PillarPosition == "MainWall"), 
                                   aes(x = -sampled, y = sd_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Posthole Volume SD - MW-type") + xlab("Years calBP") + ylab("SD") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,0.035)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#save multiplot
pdf(file = here::here("figures/MainVSMainWallRegressions1000Multi1.pdf"))
gridExtra::grid.arrange(MeanDepthPlotMain1000, 
                        MeanDepthPlotMainWall1000, 
                        MeanDiameterPlotMain1000, 
                        MeanDiameterPlotMainWall1000,
                        MeanVolumePlotMain1000, 
                        MeanVolumePlotMainWall1000,
                        ncol=2)
dev.off()

pdf(file = here::here("figures/MainVSMainWallRegressions1000Multi2.pdf"))                        
gridExtra::grid.arrange(SDDepthPlotMain1000,
                        SDDepthPlotMainWall1000,
                        SDDiameterPlotMain1000,
                        SDDiameterPlotMainWall1000,
                        SDVolumePlotMain1000, 
                        SDVolumePlotMainWall1000,
                        ncol=2)
dev.off()
