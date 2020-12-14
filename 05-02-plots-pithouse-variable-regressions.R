###05-02-multiple-variable-regressions

#These regressions compare diffrent variables related to pithouse and posthole size

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

NewFloorPlot1000 <- ggplot(data = subset(sampleclean_trim, !is.na(NewFloor)), 
                           aes(x = -sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  xlim(5400,4400) +
  ggtitle("Floor Space") + xlab("Years calBP") + labs(y = expression(m^2)) +
  scale_x_reverse() +
  theme_minimal()

MeanDepthPlot1000 <- ggplot(data = subset(sampleclean_trim, !is.na(mean_depth)), 
                            aes(x = -sampled, y = mean_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(5400,4400) +
  ggtitle("Posthole Depth - Combined Types") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

MeanDiameterPlot1000 <- ggplot(data = subset(sampleclean_trim, !is.na(mean_diameter)), 
                               aes(x = -sampled, y = mean_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(5400,4400) +
  ggtitle("Posthole Diameter - Combined Types") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

MeanVolumePlot1000 <- ggplot(data = subset(sampleclean_trim, !is.na(mean_volume)), 
                             aes(x = -sampled, y = mean_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(5400,4400) +
  ggtitle("Posthole Volume - Combined Types") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  annotate("rect", xmin=c(5200), xmax=c(5400), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()

SDDepthPlot1000 <- ggplot(data = subset(sampleclean_trim, !is.na(sd_depth)), 
                          aes(x = -sampled, y = sd_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(5400,4400) +
  ggtitle("Posthole Depth SD - Combined Types") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

SDDiameterPlot1000 <- ggplot(data = subset(sampleclean_trim, !is.na(sd_diameter)), 
                             aes(x = -sampled, y = sd_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(5400,4400) +
  ggtitle("Posthole Diameter SD - Combined Types") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

SDVolumePlot1000 <- ggplot(data = subset(sampleclean_trim, !is.na(sd_volume)), 
                           aes(x = -sampled, y = sd_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(5400,4400) +
  ggtitle("Posthole Volume SD - Combined Types") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  annotate("rect", xmin=c(5200), xmax=c(5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

PithouseLengthPlot1000 <- ggplot(data = subset(sampleclean_trim, !is.na(Length)), 
                                 aes(x = -sampled, y = as.numeric(Length), group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(5400,4400) +
  ggtitle("Pithouse Length") + xlab("Years calBP") + ylab("m") +
  scale_x_reverse() +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#save multiplot
pdf(file = here::here("figures/CombinedRegressions1000Multi.pdf"))
gridExtra::grid.arrange(NewFloorPlot1000, 
                        MeanDepthPlot1000,
                        MeanDiameterPlot1000, 
                        MeanVolumePlot1000, 
                        PithouseLengthPlot1000, 
                        SDDepthPlot1000, 
                        SDDiameterPlot1000, 
                        SDVolumePlot1000, 
                        ncol=2)
dev.off()
