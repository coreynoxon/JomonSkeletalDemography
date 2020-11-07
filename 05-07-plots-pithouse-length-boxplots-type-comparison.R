###05-07-pithouse-length-boxplots-type-comparison

#The data for this analysis is the same as the "05-04-plots-pithouse-length-regressions-by-type", but displayed in boxplot form.

samplecleanround <- readRDS(here::here("data/derived-data/samplecleanround.rds"))

LengthBoxPlotCombined1000 <- samplecleanround %>%
  ggplot(aes(x = sampled, y = Length, group = sampled)) +
  geom_boxplot() +
  ggtitle("Pithouse Length - Combined Types") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 10) +
  theme_minimal()

LengthBoxPlotPit1000 <- samplecleanround %>%
  filter(PillarPosition == "Pit") %>%
  ggplot(aes(x = sampled, y = Length, group = sampled)) +
  geom_boxplot() +
  ggtitle("Pithouse Length - Pit") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 10) +
  theme_minimal()

LengthBoxPlotWall1000 <- samplecleanround %>%
  filter(PillarPosition == "Wall") %>%
  ggplot(aes(x = sampled, y = Length, group = sampled)) +
  geom_boxplot() +
  ggtitle("Pithouse Length - Wall") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 10) +
  theme_minimal()

LengthBoxPlotMain1000 <- samplecleanround %>%
  filter(PillarPosition == "Main") %>%
  ggplot(aes(x = sampled, y = Length, group = sampled)) +
  geom_boxplot() +
  ggtitle("Pithouse Length - Main") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 10) +
  theme_minimal()

LengthBoxPlotMainWall1000 <- samplecleanround %>%
  filter(PillarPosition == "MainWall") %>%
  ggplot(aes(x = sampled, y = Length, group = sampled)) +
  geom_boxplot() +
  ggtitle("Pithouse Length - MainWall") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 10) +
  theme_minimal()

LengthBoxPlotMirror1000 <- samplecleanround %>%
  filter(PillarPosition == "Mirror") %>%
  ggplot(aes(x = sampled, y = Length, group = sampled)) +
  geom_boxplot() +
  ggtitle("Pithouse Length - Mirror") + xlab("Years calBP") + labs(y = expression(m^2)) +
  xlim(-5400,-4400) +
  ylim(0, 10) +
  theme_minimal()


#save multiplot
pdf(file = here::here("figures/LengthBoxPlotMulti1000.pdf"))
gridExtra::grid.arrange(LengthBoxPlotPit1000, 
                        LengthBoxPlotWall1000,
                        LengthBoxPlotMain1000, 
                        LengthBoxPlotMainWall1000, 
                        LengthBoxPlotMirror1000, 
                        LengthBoxPlotCombined1000, 
                        ncol=2)
dev.off()

