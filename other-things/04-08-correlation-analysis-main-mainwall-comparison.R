##Correlation matrices for Main and MainWall pithouse types

#These create correlation matrices for the comparison of posthole measures to pithouse length and floor space 
#for Main and MainWall pithouse types

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#create function for smooth regression
lowerFn <- function(data, mapping, method = "lm",...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = "blue", alpha = 0.3, size = 0.3) +
    geom_smooth(method = method, color = "red")
  p
}

##Correlations - Main Type
#compare posthole mean measurements
PostholeMeanMatrixDataMain <- sampleclean_trim %>%
  filter(PillarPosition == "Main") %>%
  select(mean_depth,mean_diameter,mean_volume)

#PostholeMeanMatrixMain <- GGally::ggpairs(data = PostholeMeanMatrixDataMain, 
#                                          upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                          diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                          lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare posthole sd measurements
PostholeSDMatrixDataMain <- sampleclean_trim %>%
  filter(PillarPosition == "Main") %>%
  select(sd_depth,sd_diameter,sd_volume)

#PostholeSDMatrixMain <- GGally::ggpairs(data = PostholeSDMatrixDataMain, 
#                                        upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                        diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                        lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare length and posthole mean measurements
LengthMeanMatrixDataMain <- sampleclean_trim %>%
  filter(PillarPosition == "Main") %>%
  select(Length, mean_depth,mean_diameter,mean_volume)

#LengthMeanMatrixMain <- GGally::ggpairs(data = LengthMeanMatrixDataMain, 
#                                        upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                        diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                        lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare length and posthole sd measurements
LengthSDMatrixDataMain <- sampleclean_trim %>%
  filter(PillarPosition == "Main") %>%
  select(Length, sd_depth,sd_diameter,sd_volume)

#LengthSDMatrixMain <- GGally::ggpairs(data = LengthSDMatrixDataMain, 
#                                      upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                      diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                      lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare floor space and posthole mean measurements
NewFloorMeanMatrixDataMain <- sampleclean_trim %>%
  filter(PillarPosition == "Main") %>%
  select(NewFloor, mean_depth,mean_diameter,mean_volume)

#NewFloorMeanMatrixMain <- GGally::ggpairs(data = NewFloorMeanMatrixDataMain, 
#                                          upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                          diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                          lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare floor space and posthole sd measurements
NewFloorSDMatrixDataMain <- sampleclean_trim %>%
  filter(PillarPosition == "Main") %>%
  select(NewFloor, sd_depth,sd_diameter,sd_volume)

#NewFloorSDMatrixMain <- GGally::ggpairs(data = NewFloorSDMatrixDataMain, 
#                                        upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                        diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                        lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


##save matrix plots

pdf(file = here::here("figures/PostholeMeanMatrixMain.pdf"))
print(PostholeMeanMatrixMain)
dev.off()

pdf(file = here::here("figures/PostholeSDMatrixMain.pdf"))
print(PostholeSDMatrixMain)
dev.off()

pdf(file = here::here("figures/LengthMeanMatrixMain.pdf"))
print(LengthMeanMatrixMain)
dev.off()

pdf(file = here::here("figures/LengthSDMatrixMain.pdf"))
print(LengthSDMatrixMain)
dev.off()

pdf(file = here::here("figures/NewFloorMeanMatrixMain.pdf"))
print(NewFloorMeanMatrixMain)
dev.off()

pdf(file = here::here("figures/NewFloorSDMatrixMain.pdf"))
print(NewFloorSDMatrixMain)
dev.off()



##Correlations - MainWall Type
#compare posthole mean measurements
PostholeMeanMatrixDataMainWall <- sampleclean_trim %>%
  filter(PillarPosition == "MainWall") %>%
  select(mean_depth,mean_diameter,mean_volume)

#PostholeMeanMatrixMainWall <- GGally::ggpairs(data = PostholeMeanMatrixDataMainWall, 
#                                              upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                              diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                              lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare posthole sd measurements
PostholeSDMatrixDataMainWall <- sampleclean_trim %>%
  filter(PillarPosition == "MainWall") %>%
  select(sd_depth,sd_diameter,sd_volume)

#PostholeSDMatrixMainWall <- GGally::ggpairs(data = PostholeSDMatrixDataMainWall, 
#                                            upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                            diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                            lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare length and posthole mean measurements
LengthMeanMatrixDataMainWall <- sampleclean_trim %>%
  filter(PillarPosition == "MainWall") %>%
  select(Length, mean_depth,mean_diameter,mean_volume)

#LengthMeanMatrixMainWall <- GGally::ggpairs(data = LengthMeanMatrixDataMainWall, 
#                                            upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                            diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                            lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare length and posthole sd measurements
LengthSDMatrixDataMainWall <- sampleclean_trim %>%
  filter(PillarPosition == "MainWall") %>%
  select(Length, sd_depth,sd_diameter,sd_volume)

#LengthSDMatrixMainWall <- GGally::ggpairs(data = LengthSDMatrixDataMainWall, 
#                                          upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                          diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                          lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare floor space and posthole mean measurements
NewFloorMeanMatrixDataMainWall <- sampleclean_trim %>%
  filter(PillarPosition == "MainWall") %>%
  select(NewFloor, mean_depth,mean_diameter,mean_volume)

#NewFloorMeanMatrixMainWall <- GGally::ggpairs(data = NewFloorMeanMatrixDataMainWall, 
#                                              upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                              diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                              lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare floor space and posthole sd measurements
NewFloorSDMatrixDataMainWall <- sampleclean_trim %>%
  filter(PillarPosition == "MainWall") %>%
  select(NewFloor, sd_depth,sd_diameter,sd_volume)

#NewFloorSDMatrixMainWall <- GGally::ggpairs(data = NewFloorSDMatrixDataMainWall, 
#                                            upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                            diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                            lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


##save matrix plots

pdf(file = here::here("figures/PostholeMeanMatrixMainWall.pdf"))
print(PostholeMeanMatrixMainWall)
dev.off()

pdf(file = here::here("figures/PostholeSDMatrixMainWall.pdf"))
print(PostholeSDMatrixMainWall)
dev.off()

pdf(file = here::here("figures/LengthMeanMatrixMainWall.pdf"))
print(LengthMeanMatrixMainWall)
dev.off()

pdf(file = here::here("figures/LengthSDMatrixMainWall.pdf"))
print(LengthSDMatrixMainWall)
dev.off()

pdf(file = here::here("figures/NewFloorMeanMatrixMainWall.pdf"))
print(NewFloorMeanMatrixMainWall)
dev.off()

pdf(file = here::here("figures/NewFloorSDMatrixMainWall.pdf"))
print(NewFloorSDMatrixMainWall)
dev.off()

