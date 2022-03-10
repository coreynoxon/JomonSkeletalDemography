#04-plots

#Main Plots

skeletalsummary2 <- readRDS(here::here("data/derived-data/skeletalsummary2.rds"))

##Confidence Envelope Plots

##Cartesian

#Show Confidence Envelopes and Points (trimmed using Cartesian coordinates)
PointsCIcartxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                   aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - Cartesian Trimmed") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (trimmed using Cartesian coordinates)
LoessCIcartxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                  aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - Cartesian Trimmed") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using Cartesian coordinates)
PointsLoessCIcartxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                        aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - Cartesian Trimmed") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

##Xlim

#Show Confidence Envelopes and Points (trimmed using xlim)
PointsCIxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (trimmed using xlim)
LoessCIxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                          aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using xlim)
PointsLoessCIxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                                aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

##Xlim Extended

#Show Confidence Envelopes and Points (trimmed using expanded xlim range with view limited by cartesian coordinates)
PointsCIxlimextend <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                             aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Extended") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (trimmed using expanded xlim range with view limited by cartesian coordinates)
LoessCIxlimextend <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                            aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Extended") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using expanded xlim range with view limited by cartesian coordinates)
PointsLoessCIxlimextend <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                                  aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Extended") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

##Weighted

##Cartesian (weighted)

#Show Confidence Envelopes and Points (trimmed using Cartesian coordinates and weighted)
PointsCIcartxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                       aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - Cartesian Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (trimmed using Cartesian coordinates and weighted)
LoessCIcartxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                      aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - Cartesian Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using Cartesian coordinates and weighted)
PointsLoessCIcartxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                            aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - Cartesian Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

##Xlim (weighted)

#Show Confidence Envelopes and Points (trimmed using xlim and weighted)
PointsCIxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (trimmed using xlim and weighted)
LoessCIxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                          aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using xlim and weighted)
PointsLoessCIxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                                aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

##Xlim Extended (weighted)

#Show Confidence Envelopes and Points (trimmed using expanded xlim range with view limited by cartesian coordinates)
PointsCIxlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                             aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Extended") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (trimmed using expanded xlim range with view limited by cartesian coordinates)
LoessCIxlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                            aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Extended") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using expanded xlim range with view limited by cartesian coordinates)
PointsLoessCIxlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                                  aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Extended") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()




#Compare Cartesian Trim, Xlim, Extended Xlim, plots and their weighted counterparts viewing points
pdf(file = here::here("figures/CombinedPointsCIx6.pdf"))
gridExtra::grid.arrange(PointsCIcartxlim,
                        PointsCIcartxlimW,
                        PointsCIxlim,
                        PointsCIxlimW,
                        PointsCIxlimextend,
                        PointsCIxlimextendW,
                        ncol=2)
dev.off()

#Compare Cartesian Trim, Xlim, Extended Xlim, plots and their weighted counterparts viewing LOESS regressions
pdf(file = here::here("figures/CombinedLoessCIx6.pdf"))
gridExtra::grid.arrange(LoessCIcartxlim,
                        LoessCIcartxlimW,
                        LoessCIxlim,
                        LoessCIxlimW,
                        LoessCIxlimextend,
                        LoessCIxlimextendW,
                        ncol=2)
dev.off()

#Compare Cartesian Trim, Xlim, Extended Xlim, plots and their weighted counterparts viewing points and LOESS regressions
pdf(file = here::here("figures/CombinedPointsLoessCIx6.pdf"))
gridExtra::grid.arrange(PointsLoessCIcartxlim,
                        PointsLoessCIcartxlimW,
                        PointsLoessCIxlim,
                        PointsLoessCIxlimW,
                        PointsLoessCIxlimextend,
                        PointsLoessCIxlimextendW,
                        ncol=2)
dev.off()


#This section is intended to automatically save and output the plots included in "plot_list"

plot_list <-
  c(
    "PointsCIcartxlim",
    "PointsCIcartxlimW",
    "PointsCIxlim",
    "PointsCIxlimW",
    "PointsCIxlimextend",
    "PointsCIxlimextendW",
    
    "LoessCIcartxlim",
    "LoessCIcartxlimW",
    "LoessCIxlim",
    "LoessCIxlimW",
    "LoessCIxlimextend",
    "LoessCIxlimextendW",
    
    "PointsLoessCIcartxlim",
    "PointsLoessCIcartxlimW",
    "PointsLoessCIxlim",
    "PointsLoessCIxlimW",
    "PointsLoessCIxlimextend",
    "PointsLoessCIxlimextendW"
  )

for(i in 1:length(plot_list)) {
  # print(paste0("now saving ", plot_list[i]))
  ggsave(filename=here::here(sprintf("figures/%s.pdf", plot_list[i])),
         plot=get(plot_list[i]))
}
