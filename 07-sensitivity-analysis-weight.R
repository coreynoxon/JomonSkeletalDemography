

skeletalsummary2 <- readRDS(here::here("data/derived-data/skeletalsummary2.rds"))

##Unweighted Cartesian

#Show Confidence Envelopes and LOESS Regressions (trimmed using Cartesian coordinates)
LoessCIcartxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                          aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", span = 0.75, alpha = 0.006, size = 0.25, color = "blue") +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.6, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - Cartesian Trimmed") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using Cartesian coordinates)
PointsLoessCIcartxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                                aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", span = 0.75, alpha = 0.006, linewidth = 0.25, color = "blue") +
  geom_point(size = 0.25) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.6, color = "red") +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.5,0.8)) +
  ggtitle("15p5 - Cartesian Trimmed") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()



#This section is intended to automatically save and output the plots included in "plot_list"

plot_list <-
  c(
    "LoessCIcartxlim",
    "PointsLoessCIcartxlim"
  )

for(i in 1:length(plot_list)) {
  # print(paste0("now saving ", plot_list[i]))
  ggsave(filename=here::here(sprintf("figures/sensitivity/weight/%s.pdf", plot_list[i])),
         plot=get(plot_list[i]))
}
