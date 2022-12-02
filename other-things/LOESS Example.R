#Show Confidence Envelopes and LOESS Regressions (trimmed using xlim)
LoessCIxlim1 <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19) & RunID <= 1), 
                      aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", span = 0.75, alpha = 0.06, size = 0.25, color = "blue") +
  ggtitle("1 Sample Run") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

LoessCIxlim2 <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19) & RunID <= 5), 
                       aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", span = 0.75, alpha = 0.06, size = 0.25, color = "blue") +
  ggtitle("5 Sample Runs") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

LoessCIxlim3 <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19) & RunID <= 50), 
                       aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", span = 0.75, alpha = 0.06, size = 0.25, color = "blue") +
  ggtitle("50 Sample Runs") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

LoessCIxlim4 <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19) & RunID <= 500), 
                       aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", span = 0.75, alpha = 0.06, size = 0.25, color = "blue") +
  ggtitle("500 Sample Runs") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

pdf(file = here::here("other-things/LOESSx4.pdf"))
gridExtra::grid.arrange(LoessCIxlim1,
                        LoessCIxlim2,
                        LoessCIxlim3,
                        LoessCIxlim4,
                        ncol=2)
dev.off()