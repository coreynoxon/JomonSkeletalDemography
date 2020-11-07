###04-1-correlation-analysis-density

#These create density plots for various pithouse measurements in order to see if they exhibit normal distributions

sampleclean <- readRDS(here::here("data/derived-data/sampleclean.rds"))


#This section includes initial correlation correspondence analyses in addition to several tests for normal distributions


NewFloorDensity <- ggdensity(sampleclean$NewFloor,
                             main = "Floor Space Density Plot",
                             xlab = expression(paste("floor space in m"^"2")))
LengthDensity <- ggdensity(as.numeric(sampleclean$Length,
                           main = "Pithouse Length Density Plot",
                           xlab = "length in m"))
MeanDepthDensity <- ggdensity(sampleclean$mean_depth,
                              main = "Posthole Mean Depth Density Plot",
                              xlab = "depth in m")
MeanDiameterDensity <- ggdensity(sampleclean$mean_diameter,
                                 main = "Posthole Mean Diameter Density Plot",
                                 xlab = "depth in m")
MeanVolumeDensity <- ggdensity(sampleclean$mean_volume,
                               main = "Posthole Mean Volume Density Plot",
                               xlab = expression(paste("posthole volume in m"^"3")))

