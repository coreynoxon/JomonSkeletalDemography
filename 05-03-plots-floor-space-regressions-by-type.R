###05-03-floor-space-regressions-by-type

#These regressions compare floor space measurements between the different pithouse types

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

NewFloorPlotCombined1000 <- ggplot(data = subset(sampleclean_trim, !is.na(NewFloor)), 
                                   aes(x = -sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - Combined Types") + xlab("Years calBP") + labs(y = bquote("Area" ~ (m^2))) +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,35)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

NewFloorPlotPit1000 <- ggplot(data = subset(sampleclean_trim, !is.na(NewFloor) & PillarPosition == "Pit"), 
                              aes(x = -sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - P-type") + xlab("Years calBP") + labs(y = bquote("Area" ~ (m^2))) +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,35)) +
  scale_x_reverse() +
  theme_minimal()

NewFloorPlotWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(NewFloor) & PillarPosition == "Wall"), 
                               aes(x = -sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - W-type") + xlab("Years calBP") + labs(y = bquote("Area" ~ (m^2))) +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,35)) +
  scale_x_reverse() +
  theme_minimal()

NewFloorPlotMain1000 <- ggplot(data = subset(sampleclean_trim, !is.na(NewFloor) & PillarPosition == "Main"), 
                               aes(x = -sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - M-type") + xlab("Years calBP") + labs(y = bquote("Area" ~ (m^2))) +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,35)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

NewFloorPlotMainWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(NewFloor) & PillarPosition == "MainWall"), 
                                   aes(x = -sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - MW-type") + xlab("Years calBP") + labs(y = bquote("Area" ~ (m^2))) +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(0,35)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

NewFloorPlotMirror1000 <- ggplot(data = subset(sampleclean_trim, !is.na(NewFloor) & PillarPosition == "Mirror" & PithouseID != "P152-1029-1"), 
                                 aes(x = -sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - HM-type") + xlab("Years calBP") + labs(y = bquote("Area" ~ (m^2))) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(0,35)) +
  scale_x_reverse() +
  theme_minimal()

#save multiplot
pdf(file = here::here("figures/CombinedTypeRegressionsNewFloor1000Multi.pdf"))
gridExtra::grid.arrange(NewFloorPlotPit1000, 
                        NewFloorPlotWall1000,
                        NewFloorPlotMain1000, 
                        NewFloorPlotMainWall1000, 
                        NewFloorPlotMirror1000, 
                        NewFloorPlotCombined1000, 
                        ncol=2)
dev.off()