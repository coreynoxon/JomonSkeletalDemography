#04-plots

#Main Plots

skeletalsummary2 <- readRDS(here::here("data/derived-data/skeletalsummary2.rds"))




##Weighted Cartesian

#Show Confidence Envelopes and LOESS Regressions (trimmed using Cartesian coordinates and weighted)
LoessCIcartxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                      aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", span = 0.75, alpha = 0.006, size = 0.2, color = "blue") +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.6, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - 2000 sample runs") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal(base_size = 15) 

#Show Confidence Envelopes and LOESS Regressions (trimmed using Cartesian coordinates and weighted) - Run Limited Example
LoessCIcartxlimW100 <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19) & RunID<50), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", span = 0.75, alpha = 0.03, size = 0.25, color = "blue") +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.6, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - 100 Sample Runs") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal(base_size = 15)
  
#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using Cartesian coordinates and weighted)
PointsLoessCIcartxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                            aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", span = 0.75, alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.6, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - Loess, Points, and CI") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal(base_size = 15)

#This section is intended to automatically save and output the plots included in "plot_list"

plot_list <-
  c(
    "LoessCIcartxlimW",
    "LoessCIcartxlimW100",
    "PointsLoessCIcartxlimW"
  )

for(i in 1:length(plot_list)) {
  # print(paste0("now saving ", plot_list[i]))
  ggsave(filename=here::here(sprintf("figures/plots/%s.pdf", plot_list[i])),
         plot=get(plot_list[i]))
}
