#04-plots

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
  ggtitle("15p5 - Middle Jomon Data") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot5p0filter <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_0_5)), 
                        aes(x = -sampled, y = ratio_0_5, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - Middle Jomon Data") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse() +
  theme_minimal()

## Unweighted Plots using subfilter

Plot15p5subfilter <- ggplot(data = subset(skeletalsummary_trim_subfilter, !is.na(ratio_5_19)), 
                         aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Data - \nsubfilter") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot15p5subfilter2 <- ggplot(data = subset(skeletalsummary_trim_subfilter2, !is.na(ratio_5_19)), 
                            aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Data - \nsubfilter 2") + xlab("Years calBP") + ylab("15p5") +
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
  ggtitle("15p5 - Middle Jomon Data") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

Plot5p0xlim <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                            aes(x = -sampled, y = ratio_0_5, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - Middle Jomon Data") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

##Unweighted Untrimmed Plots using Middle Jomon range xlim and subfilter
Plot15p5xlim_subfilter <- ggplot(data = subset(skeletalsummary2_subfilter, !is.na(ratio_5_19)), 
                       aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Data - \nsubfilter") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

Plot15p5xlim_subfilter2 <- ggplot(data = subset(skeletalsummary2_subfilter2, !is.na(ratio_5_19)), 
                                 aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Data - \nsubfilter 2") + xlab("Years calBP") + ylab("15p5") +
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
  ggtitle("15p5 - Middle Jomon Only - weighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot5p0filterW <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_0_5)), 
                        aes(x = -sampled, y = ratio_0_5, group = RunID, weight = n)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - Middle Jomon Data - weighted") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse() +
  theme_minimal()


## Weighted Untrimmed Plots using coord-cartesian
Plot15p5cartxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Total Jomon Data - weighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse() +
  theme_minimal()

Plot5p0cartxlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                          aes(x = -sampled, y = ratio_0_5, group = RunID, weight = n)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  coord_cartesian(xlim = c(5400,4400)) +
  ggtitle("5p0 - Total Jomon Data - weighted") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse() +
  theme_minimal()


##Weighted Untrimmed Plots using Middle Jomon range xlim
Plot15p5xlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                       aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  ggtitle("15p5 - Middle Jomon Data - weighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

Plot5p0xlimW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                      aes(x = -sampled, y = ratio_0_5, group = RunID, weight = n)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("5p0 - Middle Jomon Data - weighted") + xlab("Years calBP") + ylab("5p0") +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()


##Weighted Untrimmed Plots using extended xlim range
Plot15p5xlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                             aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("15p5 - Middle Jomon Expanded - weighted") + xlab("Years calBP") + ylab("15p5") +
  scale_x_reverse(limits = c(6400,3400)) +
  theme_minimal()

Plot5p0xlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_0_5)), 
                            aes(x = -sampled, y = ratio_0_5, group = RunID, weight = n)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  coord_cartesian(xlim = c(5400,4400), ylim = c(-0.25,0.5)) +
  ggtitle("5p0 - Middle Jomon Expanded - weighted") + xlab("Years calBP") + ylab("5p0") +
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


##Confidence Envelope Plots

#Show Confidence Envelopes and Points (Data Trimmed to Middle Jomon Only)
PointsCItrim <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_5_19)), 
                   aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Data Trimmed") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse() +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (Data Trimmed to Middle Jomon Only)
LoessCItrim <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_5_19)), 
                  aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Data Trimmed") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse() +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (Data Trimmed to Middle Jomon Only)
PointsLoessCItrim <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_5_19)), 
                        aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Data Trimmed") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse() +
  theme_minimal()

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
PointsCIxlimextend <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (trimmed using xlim)
LoessCIxlimextend <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                          aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using xlim)
PointsLoessCIxlimextend <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                                aes(x = -sampled, y = ratio_5_19, group = RunID)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()


##Weighted

#Show Confidence Envelopes and Points (Data Trimmed to Middle Jomon Only and Weighted)
PointsCItrimW <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_5_19)), 
                   aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Data Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse() +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (Data Trimmed to Middle Jomon Only and Weighted)
LoessCItrimW <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_5_19)), 
                  aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Data Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse() +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (Data Trimmed to Middle Jomon Only and Weighted)
PointsLoessCItrimW <- ggplot(data = subset(skeletalsummary_trim, !is.na(ratio_5_19)), 
                        aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Data Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse() +
  theme_minimal()


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
PointsCIxlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                           aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", linetype = 0, alpha = 0.006) +
  geom_point(size = 0.25) +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

#Show Confidence Envelopes and LOESS Regressions (trimmed using xlim and weighted)
LoessCIxlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                          aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
   geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
  theme_minimal()

#Show Confidence Envelopes, Points, and LOESS Regressions (trimmed using xlim and weighted)
PointsLoessCIxlimextendW <- ggplot(data = subset(skeletalsummary2, !is.na(ratio_5_19)), 
                                aes(x = -sampled, y = ratio_5_19, group = RunID, weight = total_over_5)) +
  geom_smooth(method = "loess", alpha = 0.006, size = 0.25, color = "blue") +
  geom_point(size = 0.25) +
  geom_hline(yintercept = 0.173, linetype = "dashed", size = 0.25, color = "red") +
  ggtitle("15p5 - Xlim Trimmed - weighted") + xlab("Years calBP") + ylab("15p5") +
  coord_cartesian(ylim = c(-0.5,0.8)) +
  scale_x_reverse(limits = c(5400,4400)) +
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

#Compare Trimmed, Cartesian Trim, Xlim, and weighted counterparts
pdf(file = here::here("figures/CombinedCIx6.pdf"))
gridExtra::grid.arrange(PointsLoessCItrim,
                        PointsLoessCItrimW,
                        PointsLoessCIcartxlim,
                        PointsLoessCIcartxlimW,
                        PointsLoessCIxlimextend,
                        PointsLoessCIxlimextendW,
                        ncol=2)
dev.off()

