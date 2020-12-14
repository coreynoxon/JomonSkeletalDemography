##Correlation matrices for posthole mean and SD values 

#These create correlation matrices for posthole mean and SD values

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#create function for smooth regression
lowerFn <- function(data, mapping, method = "lm",...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = "blue", alpha = 0.3, size = 0.3) +
    geom_smooth(method = method, color = "red")
  p
}


#compare posthole mean measurements
PostholeMeanMatrixData <- sampleclean_trim %>%
  select(mean_depth,mean_diameter,mean_volume)

#PostholeMeanMatrix <- GGally::ggpairs(data = PostholeMeanMatrixData, 
#                                      upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                      diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                      lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


#compare posthole sd measurements
PostholeSDMatrixData <- sampleclean_trim %>%
  select(sd_depth,sd_diameter,sd_volume)

#PostholeSDMatrix <- GGally::ggpairs(data = PostholeSDMatrixData, 
#                                    upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 8)),
#                                    diag = list(continuous = GGally::wrap("barDiag", color = "blue")),
#                                    lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
#  theme_minimal()


##save matrix plots

pdf(file = here::here("figures/PostholeMeanMatrix.pdf"))
print(PostholeMeanMatrix)
dev.off()

pdf(file = here::here("figures/PostholeSDMatrix.pdf"))
print(PostholeSDMatrix)
dev.off()

