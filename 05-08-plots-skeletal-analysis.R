#05-08-skeletal-plots

#Plots for 15p5 and 5p0 

skeletalsummary2 <- readRDS(here::here("data/derived-data/skeletalsummary2.rds"))
skeletalsummary_trim <- readRDS(here::here("data/derived-data/skeletalsummary_trim.rds"))


#filter sites without any subadults
skeletalsummary2_subfilter <- skeletalsummary2 %>%
  filter(total_5_19 != 0 | total_0_4 != 0)

skeletalsummary_trim_subfilter <- skeletalsummary_trim %>%
  filter(total_5_19 != 0 | total_0_4 != 0)

skeletalsummary_trim10_subfilter <- skeletalsummary_trim_subfilter %>%
  filter(total_over_5 > 9)

skeletalsummary_trim25_subfilter <- skeletalsummary_trim_subfilter %>%
  filter(total_over_5 > 24)


#filter sites without subadults 5 to 19
skeletalsummary2_subfilter2 <- skeletalsummary2 %>%
  filter(total_5_19 != 0)
  
skeletalsummary_trim_subfilter2 <- skeletalsummary_trim %>%
  filter(total_5_19 != 0)

skeletalsummary_trim10 <- skeletalsummary_trim %>%
  filter(total_over_5 > 9)

skeletalsummary_trim10_subfilter2 <- skeletalsummary_trim_subfilter2 %>%
  filter(total_over_5 > 9)
  
skeletalsummary_trim25 <- skeletalsummary_trim %>%
  filter(total_over_5 > 24)

skeletalsummary_trim25_subfilter2 <- skeletalsummary_trim_subfilter2 %>%
  filter(total_over_5 > 24)


###Unweighted Plots
## Unweighted Plots from trimmed dates

Plot15p5filter <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_5_19)), 
                         aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Only") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot5p0filter <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_0_5)), 
                        aes(x = -sampled, y = ratio_0_5, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - Middle Jomon Only") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse() +
  theme_minimal()

## Unweighted Plots using subfilter

Plot15p5subfilter <- ggplot(data = subset(skeletalsummary_trim_subfilter, !is.na(ratio_5_19)), 
                         aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Only - \nsubfilter") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot15p5subfilter2 <- ggplot(data = subset(skeletalsummary_trim_subfilter2, !is.na(ratio_5_19)), 
                            aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Only - \nsubfilter 2") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()


## Unweighted Untrimmed Plots using coord-cartesian
Plot15p5cartxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Total Jomon Data") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot5p0cartxlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                          aes(x = -sampled, y = ratio_0_5, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  coord_cartesian(xlim = c(5400,4400)) +
  ggtitle("5p0 - Total Jomon Data") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse() +
  theme_minimal()

##Unweighted Untrimmed Plots using coord-cartesian and subfilters
Plot15p5cartxlim_subfilter <- ggplot(data = subset(skeletalsummary2_subfilter, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Total Jomon Data - \nsubfilter") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot15p5cartxlim_subfilter2 <- ggplot(data = subset(skeletalsummary2_subfilter2, !is.na(ratio_5_19)), 
                                     aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Total Jomon Data - \nsubfilter 2") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()


##Unweighted Untrimmed Plots using Middle Jomon range xlim
Plot15p5xlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                             aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Only") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

Plot5p0xlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                            aes(x = -sampled, y = ratio_0_5, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - Middle Jomon Only") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

##Unweighted Untrimmed Plots using Middle Jomon range xlim and subfilter
Plot15p5xlim_subfilter <- ggplot(data = subset(skeletalsummary2_subfilter, !is.na(ratio_5_19)), 
                       aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Only - \nsubfilter") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

Plot15p5xlim_subfilter2 <- ggplot(data = subset(skeletalsummary2_subfilter2, !is.na(ratio_5_19)), 
                                 aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Only - \nsubfilter 2") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()


##Unweighted Untrimmed Plots using extended xlim range
Plot15p5xlimextend <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                             aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Middle Jomon Expanded") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

Plot5p0xlimextend <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                            aes(x = -sampled, y = ratio_0_5, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("5p0 - Middle Jomon Expanded") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

##Unweighted Untrimmed Plots using extended xlim range and subfilter
Plot15p5xlimextend_subfilter <- ggplot(data = subset(skeletalsummary2_subfilter, !is.na(ratio_5_19)), 
                             aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Middle Jomon Expanded - \nsubfilter") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

Plot15p5xlimextend_subfilter2 <- ggplot(data = subset(skeletalsummary2_subfilter2, !is.na(ratio_5_19)), 
                                       aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Middle Jomon Expanded - \nsubfilter 2") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()


##Unweighted Plots total date range
Plot15p5alldates <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot5p0alldates <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                          aes(x = -sampled, y = ratio_0_5, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - total") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse() +
  theme_minimal()

##Unweighted Plots total date range and subfilter
Plot15p5alldates_subfilter <- ggplot(data = subset(skeletalsummary2_subfilter, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - subfilter total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot15p5alldates_subfilter2 <- ggplot(data = subset(skeletalsummary2_subfilter2, !is.na(ratio_5_19)), 
                                     aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - subfilter 2 total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

###Weighted Plots
## Weighted Plots from trimmed dates

Plot15p5filterW <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_5_19)), 
                         aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Only - \nweighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot5p0filterW <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_0_5)), 
                        aes(x = -sampled, y = ratio_0_5, group = RunID, weight = n)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - Middle Jomon Only - \nweighted") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse() +
  theme_minimal()


## Weighted Untrimmed Plots using coord-cartesian
Plot15p5cartxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Total Jomon Data - \nweighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot5p0cartxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                          aes(x = -sampled, y = ratio_0_5, group = RunID, weight = n)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  coord_cartesian(xlim = c(5400,4400)) +
  ggtitle("5p0 - Total Jomon Data - \nweighted") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse() +
  theme_minimal()


##Weighted Untrimmed Plots using Middle Jomon range xlim
Plot15p5xlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                       aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Only - \nweighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

Plot5p0xlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                      aes(x = -sampled, y = ratio_0_5, group = RunID, weight = n)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - Middle Jomon Only - \nweighted") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()


##Weighted Untrimmed Plots using extended xlim range
Plot15p5xlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                             aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Middle Jomon Expanded - \nweighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

Plot5p0xlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                            aes(x = -sampled, y = ratio_0_5, group = RunID, weight = n)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("5p0 - Middle Jomon Expanded - \nweighted") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()


##Weighted Plots total date range
Plot15p5alldatesW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - weighted total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot5p0alldatesW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                          aes(x = -sampled, y = ratio_0_5, group = RunID, weight = n)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - weighted total") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse() +
  theme_minimal()


#Hex and density plots for skeletal data
skeletalhexdata <- skeletalsummary_trim %>%
  select(ratio_5_19, sampled) %>%
  filter(ratio_5_19 != "uk")

skeletalhex519 <- ggplot(skeletalhexdata, aes(x = -sampled, y = ratio_5_19)) +
  geom_hex() +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  scale_fill_continuous(type = "viridis", labels = function(x) x/2000) +
  ggtitle("15p5 - total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

skeletalhex519density <- ggplot(data = skeletalhexdata, aes(x = -sampled, y = ratio_5_19)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  scale_fill_distiller(palette= "Spectral") +
  ggtitle("15p5 - total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

#Hex and density plots for skeletal data using subfilter
skeletalhexdata_subfilter <- skeletalsummary_trim_subfilter %>%
  select(ratio_5_19, sampled) %>%
  filter(ratio_5_19 != "uk")

skeletalhex519_subfilter <- ggplot(skeletalhexdata_subfilter, aes(x = -sampled, y = ratio_5_19)) +
  geom_hex() +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  scale_fill_continuous(type = "viridis", labels = function(x) x/2000) +
  ggtitle("15p5 - subfilter total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

skeletalhex519density_subfilter <- ggplot(data = skeletalhexdata_subfilter, aes(x = -sampled, y = ratio_5_19)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  scale_fill_distiller(palette= "Spectral") +
  ggtitle("15p5 - subfilter total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

skeletalhexdata_subfilter2 <- skeletalsummary_trim_subfilter2 %>%
  select(ratio_5_19, sampled) %>%
  filter(ratio_5_19 != "uk")

skeletalhex519_subfilter2 <- ggplot(skeletalhexdata_subfilter2, aes(x = -sampled, y = ratio_5_19)) +
  geom_hex() +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  scale_fill_continuous(type = "viridis", labels = function(x) x/2000) +
  ggtitle("15p5 - subfilter 2 total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

skeletalhex519density_subfilter2 <- ggplot(data = skeletalhexdata_subfilter2, aes(x = -sampled, y = ratio_5_19)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  scale_fill_distiller(palette= "Spectral") +
  ggtitle("15p5 - subfilter 2 total") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()


##Save Unweighted Combined15p5 plot 
pdf(file = here::here("figures/Combined15p5x4.pdf"))
gridExtra::grid.arrange(Plot15p5filter,
                        Plot15p5cartxlim,
                        Plot15p5xlimextend,
                        Plot15p5alldates,
                        ncol=2)
dev.off()

pdf(file = here::here("figures/Combined15p5x4_subfilter.pdf"))
gridExtra::grid.arrange(Plot15p5subfilter,
                        Plot15p5cartxlim_subfilter,
                        Plot15p5xlimextend_subfilter,
                        Plot15p5alldates_subfilter,
                        ncol=2)
dev.off()

pdf(file = here::here("figures/Combined15p5x4_subfilter2.pdf"))
gridExtra::grid.arrange(Plot15p5subfilter2,
                        Plot15p5cartxlim_subfilter2,
                        Plot15p5xlimextend_subfilter2,
                        Plot15p5alldates_subfilter2,
                        ncol=2)
dev.off()

pdf(file = here::here("figures/Combined15p5x3.pdf"))
gridExtra::grid.arrange(Plot15p5filter,
                        Plot15p5cartxlim,
                        Plot15p5xlimextend,
                        ncol=1)
dev.off()

pdf(file = here::here("figures/Combined15p5x3_subfilter.pdf"))
gridExtra::grid.arrange(Plot15p5subfilter,
                        Plot15p5cartxlim_subfilter,
                        Plot15p5xlimextend_subfilter,
                        ncol=1)
dev.off()

pdf(file = here::here("figures/Combined15p5x3_subfilter2.pdf"))
gridExtra::grid.arrange(Plot15p5subfilter2,
                        Plot15p5cartxlim_subfilter2,
                        Plot15p5xlimextend_subfilter2,
                        ncol=1)
dev.off()

pdf(file = here::here("figures/Combined15p5x6.pdf"))
gridExtra::grid.arrange(Plot15p5filter,
                        Plot15p5subfilter,
                        Plot15p5cartxlim,
                        Plot15p5cartxlim_subfilter,
                        Plot15p5xlimextend,
                        Plot15p5xlimextend_subfilter,
                        ncol=2)
dev.off()

pdf(file = here::here("figures/Combined15p5x6subfiltercompare.pdf"))
gridExtra::grid.arrange(Plot15p5subfilter,
                        Plot15p5subfilter2,
                        Plot15p5cartxlim_subfilter,
                        Plot15p5cartxlim_subfilter2,
                        Plot15p5xlimextend_subfilter,
                        Plot15p5xlimextend_subfilter2,
                        ncol=2)
dev.off()

#Save Weighted Combined15p5 plot 
pdf(file = here::here("figures/Combined15p5x4W.pdf"))
gridExtra::grid.arrange(Plot15p5filterW,
                        Plot15p5cartxlimW,
                        Plot15p5xlimextendW,
                        Plot15p5alldatesW,
                        ncol=2)
dev.off()


pdf(file = here::here("figures/Combined15p5x3W.pdf"))
gridExtra::grid.arrange(Plot15p5filterW,
                        Plot15p5cartxlimW,
                        Plot15p5xlimextendW,
                        ncol=1)
dev.off()


##Save Unweighted Combined5p0 plot
pdf(file = here::here("figures/Combined5p0x4.pdf"))
gridExtra::grid.arrange(Plot5p0filter,
                        Plot5p0cartxlim,
                        Plot5p0xlimextend,
                        Plot5p0alldates,
                        ncol=2)
dev.off()


pdf(file = here::here("figures/Combined5p0x3.pdf"))
gridExtra::grid.arrange(Plot5p0filter,
                        Plot5p0cartxlim,
                        Plot5p0xlimextend,
                        ncol=1)
dev.off()

##Save Weighted Combined5p0 plot
pdf(file = here::here("figures/Combined5p0x4W.pdf"))
gridExtra::grid.arrange(Plot5p0filterW,
                        Plot5p0cartxlimW,
                        Plot5p0xlimextendW,
                        Plot5p0alldatesW,
                        ncol=2)
dev.off()

pdf(file = here::here("figures/Combined5p0x3W.pdf"))
gridExtra::grid.arrange(Plot5p0filterW,
                        Plot5p0cartxlimW,
                        Plot5p0xlimextendW,
                        ncol=1)
dev.off()

#skeletal pithouse combined
pdf(file = here::here("figures/15p5PithouseCounts.pdf"))
gridExtra::grid.arrange(Plot15p5xlim,
                        PithouseCounts1000Plot,
                        nrow=2)
dev.off()

pdf(file = here::here("figures/15p5PithouseCounts_subfilter.pdf"))
gridExtra::grid.arrange(Plot15p5xlim_subfilter,
                        PithouseCounts1000Plot,
                        nrow=2)
dev.off()

pdf(file = here::here("figures/15p5PithouseCounts_subfilter2.pdf"))
gridExtra::grid.arrange(Plot15p5xlim_subfilter2,
                        PithouseCounts1000Plot,
                        nrow=2)
dev.off()

pdf(file = here::here("figures/15p5PithouseCountsxlim.pdf"))
gridExtra::grid.arrange(Plot15p5xlimextend,
                        PithouseCounts1000Plot,
                        nrow=2)
dev.off()

pdf(file = here::here("figures/15p5PithouseCounts_subfilterxlim.pdf"))
gridExtra::grid.arrange(Plot15p5xlimextend_subfilter,
                        PithouseCounts1000Plot,
                        nrow=2)
dev.off()

pdf(file = here::here("figures/15p5PithouseCounts_subfilter2xlim.pdf"))
gridExtra::grid.arrange(Plot15p5xlimextend_subfilter2,
                        PithouseCounts1000Plot,
                        nrow=2)
dev.off()

##Unweighted 15p5 and 5p0 comparison
pdf(file = here::here("figures/Combined15p5_5p0.pdf"))
gridExtra::grid.arrange(Plot5p0filter,
                        Plot15p5filter,
                        Plot5p0cartxlim,
                        Plot15p5cartxlim,
                        Plot5p0xlimextend,
                        Plot15p5xlimextend,
                        ncol=2)
dev.off()

##Weighted 15p5 and 5p0 comparison
pdf(file = here::here("figures/Combined15p5_5p0W.pdf"))
gridExtra::grid.arrange(Plot5p0filterW,
                        Plot15p5filterW,
                        Plot5p0cartxlimW,
                        Plot15p5cartxlimW,
                        Plot5p0xlimextendW,
                        Plot15p5xlimextendW,
                        ncol=2)
dev.off()

##Compare weighted and subfilter plots

pdf(file = here::here("figures/CompareWeightsFiltersXlim15p5x4.pdf"))
gridExtra::grid.arrange(Plot15p5xlim,
                        Plot15p5xlimW,
                        Plot15p5xlim_subfilter,
                        Plot15p5xlim_subfilter2,
                        ncol=2)
dev.off()

pdf(file = here::here("figures/CompareWeightsFiltersXlimExtend15p5x4.pdf"))
gridExtra::grid.arrange(Plot15p5xlimextend,
                        Plot15p5xlimextendW,
                        Plot15p5xlimextend_subfilter,
                        Plot15p5xlimextend_subfilter2,
                        ncol=2)
dev.off()

pdf(file = here::here("figures/CompareWeightsFiltersCartXlim15p5x4.pdf"))
gridExtra::grid.arrange(Plot15p5cartxlim,
                        Plot15p5cartxlimW,
                        Plot15p5cartxlim_subfilter,
                        Plot15p5cartxlim_subfilter2,
                        ncol=2)
dev.off()