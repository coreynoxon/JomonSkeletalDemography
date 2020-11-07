###05-04-pithouse-length-regressions-by-type

#These regressions compare pithouse length measurements between the different pithouse types

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#negative test
LengthPlotCombined1000 <- ggplot(data = subset(sampleclean_trim, !is.na(Length)), 
                                 aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Pithouse Length - Combined Types") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

LengthPlotCombined1000 <- ggplot(data = subset(sampleclean_trim, !is.na(Length)), 
                                 aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Pithouse Length - Combined Types") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

LengthPlotPit1000 <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Pit"), 
                            aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Pithouse Length - P-type") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  theme_minimal()

LengthPlotWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Wall"), 
                             aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Pithouse Length - W-type") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  theme_minimal()

LengthPlotMain1000 <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Main"), 
                             aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Pithouse Length - M-type") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

LengthPlotMainWall1000 <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "MainWall"), 
                                 aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Pithouse Length - MW-type") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

LengthPlotMirror1000 <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Mirror"), 
                               aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Pithouse Length - HM-type") + xlab("Years calBP") + ylab("Length (m)") +
  coord_cartesian(ylim = c(2,8), xlim = c(5400,4400)) +
  scale_x_reverse() +
  theme_minimal()

LengthPlotMirror1000filter <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Mirror" & PithouseID != "P152-1029-1"), 
                                       aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Pithouse Length - HM-type") + xlab("Years calBP") + ylab("Length (m)") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(2,8)) +
  scale_x_reverse() +
  theme_minimal()



#save multiplot
pdf(file = here::here("figures/CombinedTypeRegressionsLength1000Multi.pdf"))
gridExtra::grid.arrange(LengthPlotPit1000, 
                        LengthPlotWall1000,
                        LengthPlotMain1000, 
                        LengthPlotMainWall1000, 
                        LengthPlotMirror1000filter, 
                        LengthPlotCombined1000, 
                        ncol=2)
dev.off()
