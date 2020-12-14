##Correlation matrices for posthole measures compared to pithouse length and floor space

#These create correlation matrices between pithouse length and posthole measures as well as pithouse floor space and posthole measures

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#create function for smooth regression
lowerFn <- function(data, mapping, method = "lm",...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = "blue", alpha = 0.3, size = 0.3) +
    geom_smooth(method = method, color = "red")
  p
}


##Pithouse Length

#compare length and posthole mean measurements
LengthMeanMatrixData <- sampleclean_trim %>%
  select(Length, mean_depth,mean_diameter,mean_volume)

#LengthMeanMatrix <- GGally::ggpairs(data = LengthMeanMatrixData, 
#                                    upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                    diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                    lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare length and posthole sd measurements
LengthSDMatrixData <- sampleclean_trim %>%
  select(Length, sd_depth,sd_diameter,sd_volume)

#LengthSDMatrix <- GGally::ggpairs(data = LengthSDMatrixData, 
#                                  upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                  diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                  lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()



##Pithouse Floor Space

#compare floor space and posthole mean measurements
NewFloorMeanMatrixData <- sampleclean_trim %>%
  select(NewFloor, mean_depth,mean_diameter,mean_volume)

#NewFloorMeanMatrix <- GGally::ggpairs(data = NewFloorMeanMatrixData, 
#                                      upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                      diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                      lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare floor space and posthole sd measurements
NewFloorSDMatrixData <- sampleclean_trim %>%
  select(NewFloor, sd_depth,sd_diameter,sd_volume)

#NewFloorSDMatrix <- GGally::ggpairs(data = NewFloorSDMatrixData, 
#                                    upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                    diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                    lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


##save matrix plots

pdf(file = here::here("figures/LengthMeanMatrix.pdf"))
print(LengthMeanMatrix)
dev.off()

pdf(file = here::here("figures/LengthSDMatrix.pdf"))
print(LengthSDMatrix)
dev.off()

pdf(file = here::here("figures/NewFloorMeanMatrix.pdf"))
print(NewFloorMeanMatrix)
dev.off()

pdf(file = here::here("figures/NewFloorSDMatrix.pdf"))
print(NewFloorSDMatrix)
dev.off()

