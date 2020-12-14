###15-extras
#This section is set aside for incidental plots not focused on specific aspects of the study

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#koyama population plot
koyama<- read_csv("other-things/Koyama Prefecture Site Counts Chart - Combined - Trim.csv")
koyamaclean <- koyama %>%
  group_by(Region) %>%
  summarise(Initial = sum(Initial), 
            Early = sum(Early), 
            Middle = sum(Middle), 
            Late = sum(Late), 
            Final = sum(Final))
koyamagather <- gather(koyamaclean, Period, Sites, c("Initial", "Early","Middle", "Late", "Final"))
koyamagather$Period <- factor(koyamagather$Period, level = c("Initial", "Early","Middle", "Late", "Final"))
koyamaplot <- ggplot(koyamagather, aes(x = Period, y = Sites, group = Region, color = Region)) +
  geom_line(size = 1) +
  theme_minimal()
ggsave(path = "figures", filename = "koyamasites.pdf")

#Sampling Effect Plot
PitSubset <- subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Pit" & RunID <= 5)

LengthPlotPitCompare <- ggplot(data = PitSubset, 
                         aes(x = -sampled, y = Length, group = RunID, color = factor(RunID))) +
  stat_smooth(geom = "line", se = FALSE, size = 1) + 
  ggtitle("Sampling Differences") + xlab("Years calBP") + ylab("Length (m)") + 
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  theme_minimal() +
  theme(legend.title = element_blank())


#MCMC run explanation using P-type pithouse length regressions
LengthPlotPit1 <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Pit" & RunID == 1), 
                         aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.3, se = FALSE) + 
  ggtitle("1 MCMC run") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  theme_minimal()

LengthPlotPit5 <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Pit" & RunID <= 5), 
                         aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.3, se = FALSE) + 
  ggtitle("5 MCMC runs") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  theme_minimal()

LengthPlotPit50 <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Pit" & RunID <= 50), 
                          aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.3, se = FALSE) + 
  ggtitle("50 MCMC runs") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  theme_minimal()

LengthPlotPit500 <- ggplot(data = subset(sampleclean_trim, !is.na(Length) & PillarPosition == "Pit" & RunID <= 500), 
                           aes(x = -sampled, y = Length, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.3, se = FALSE) + 
  ggtitle("500 MCMC runs") + xlab("Years calBP") + ylab("Length (m)") +
  xlim(5400,4400) +
  coord_cartesian(ylim = c(2,8)) +
  scale_x_reverse() +
  theme_minimal()


pdf(file = here::here("figures/MCMCexample.pdf"))
gridExtra::grid.arrange(LengthPlotPit1, 
                        LengthPlotPit5,
                        LengthPlotPit50, 
                        LengthPlotPit500, 
                        ncol=2)
dev.off()
