###05-06-floor-space-boxplots-type-comparison

#The data fot this analysis is the same as the "05-03-plots-floor-space-regressions-by-type", but displayed in boxplot form.

samplecleanround <- readRDS(here::here("data/derived-data/samplecleanround.rds"))

NewFloorBoxPlotCombined1000 <- samplecleanround %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Combined Types") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotPit1000 <- samplecleanround %>%
  filter(PillarPosition == "Pit") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Pit") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotWall1000 <- samplecleanround %>%
  filter(PillarPosition == "Wall") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Wall") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotMain1000 <- samplecleanround %>%
  filter(PillarPosition == "Main") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Main") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotMainWall1000 <- samplecleanround %>%
  filter(PillarPosition == "MainWall") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - MainWall") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotMirror1000 <- samplecleanround %>%
  filter(PillarPosition == "Mirror") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Mirror") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 70) +
  theme_minimal()


#save multiplot
pdf(file = here::here("figures/NewFloorBoxPlotMulti1000.pdf"))
gridExtra::grid.arrange(NewFloorBoxPlotPit1000, 
                        NewFloorBoxPlotWall1000,
                        NewFloorBoxPlotMain1000, 
                        NewFloorBoxPlotMainWall1000, 
                        NewFloorBoxPlotMirror1000, 
                        NewFloorBoxPlotCombined1000, 
                        ncol=2)
dev.off()

