###04-09 - correlation-analysis-pithouse-posthole-ratios

ratio_prep <- readRDS(here::here("data/derived-data/ratio_prep.rds"))

ratio_prep_M <- ratio_prep %>%
  filter(PillarPosition == "Main" & !is.na(Length))

ratio_prep_MW <- ratio_prep %>%
  filter(PillarPosition == "MainWall" & !is.na(Length))

##Pithouse Length to Posthole Mean Measures - M-type

#plot pithouse length to posthole depth ratio over time
LengthDepthRatioM <- ggplot(data = ratio_prep_M, 
                           aes(x = -sampled, y = length_depth_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Mean Depth Ratio - M-type") + xlab("Years calBP") + ylab("mean depth/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#plot pithouse length to posthole diameter ratio over time
LengthDiameterRatioM <- ggplot(data = ratio_prep_M, 
                              aes(x = -sampled, y = length_diameter_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Mean Diameter Ratio - M-type") + xlab("Years calBP") + ylab("mean diameter/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#plot pithouse length to posthole volume ratio over time
LengthVolumeRatioM <- ggplot(data = ratio_prep_M, 
                            aes(x = -sampled, y = length_volume_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Mean Volume Ratio - M-type") + xlab("Years calBP") + ylab("mean volume/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(5200), xmax=c(5400), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()

##Pithouse Length to Posthole Mean Measures - MW-type

#plot pithouse length to posthole depth ratio over time
LengthDepthRatioMW <- ggplot(data = ratio_prep_MW, 
                           aes(x = -sampled, y = length_depth_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Mean Depth Ratio - MW-type") + xlab("Years calBP") + ylab("mean depth/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#plot pithouse length to posthole diameter ratio over time
LengthDiameterRatioMW <- ggplot(data = ratio_prep_MW, 
                              aes(x = -sampled, y = length_diameter_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Mean Diameter Ratio - MW-type") + xlab("Years calBP") + ylab("mean diameter/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400), xmax=c(4600), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#plot pithouse length to posthole volume ratio over time
LengthVolumeRatioMW <- ggplot(data = ratio_prep_MW, 
                            aes(x = -sampled, y = length_volume_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Mean Volume Ratio - MW-type") + xlab("Years calBP") + ylab("mean volume/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400), xmax=c(4600), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()

##Pithouse Length to Posthole SD Measures - M-type

#plot pithouse length to posthole depth ratio over time
LengthDepthSDRatioM <- ggplot(data = ratio_prep_M, 
                             aes(x = -sampled, y = length_depth_sd_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Depth SD Ratio - M-type") + xlab("Years calBP") + ylab("depth sd/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#plot pithouse length to posthole diameter ratio over time
LengthDiameterSDRatioM <- ggplot(data = ratio_prep_M, 
                                aes(x = -sampled, y = length_diameter_sd_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Diameter SD Ratio - M-type") + xlab("Years calBP") + ylab("diameter sd/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#plot pithouse length to posthole volume ratio over time
LengthVolumeSDRatioM <- ggplot(data = ratio_prep_M, 
                              aes(x = -sampled, y = length_volume_sd_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Volume SD Ratio - M-type") + xlab("Years calBP") + ylab("volume sd/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(5200), xmax=c(5400), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()


##Pithouse Length to Posthole SD Measures - MW-type

#plot pithouse length to posthole depth ratio over time
LengthDepthSDRatioMW <- ggplot(data = ratio_prep_MW, 
                             aes(x = -sampled, y = length_depth_sd_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Depth SD Ratio - MW-type") + xlab("Years calBP") + ylab("depth sd/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400), xmax=c(4600), ymin=c(-Inf) , ymax=c(Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#plot pithouse length to posthole diameter ratio over time
LengthDiameterSDRatioMW <- ggplot(data = ratio_prep_MW, 
                                aes(x = -sampled, y = length_diameter_sd_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Diameter SD Ratio - MW-type") + xlab("Years calBP") + ylab("diameter sd/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()

#plot pithouse length to posthole volume ratio over time
LengthVolumeSDRatioMW <- ggplot(data = ratio_prep_MW, 
                              aes(x = -sampled, y = length_volume_sd_ratio, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Length to Volume SD Ratio - MW-type") + xlab("Years calBP") + ylab("volume sd/length ratio") +
  xlim(5400,4400) +
  annotate("rect", xmin=c(4400,5200), xmax=c(4600,5400), ymin=c(-Inf,-Inf) , ymax=c(Inf,Inf), alpha=0.4, fill="gray") +
  theme_minimal()