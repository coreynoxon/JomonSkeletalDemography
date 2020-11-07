
#set colors
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

ratio_prep <- readRDS(here::here("data/derived-data/ratio_prep.rds"))

ratio_prep_M <- ratio_prep %>%
  filter(PillarPosition == "Main")

ratio_prep_MW <- ratio_prep %>%
  filter(PillarPosition == "MainWall")

##hexplots
##length/posthole size

LengthDepthRatioHexM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_depth_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean depth/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDepthRatioHexMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_depth_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean depth/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDiameterRatioHexM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_diameter_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean diameter/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDiameterRatioHexMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_diameter_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean diameter/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthVolumeRatioHexM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_volume_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean volume/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthVolumeRatioHexMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_volume_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean volume/length ratio") +
  scale_x_reverse() +
  theme_minimal()


##length/posthole variation

LengthDepthSDRatioHexM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_depth_sd_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth SD/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean depth SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDepthSDRatioHexMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_depth_sd_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth SD/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean depth SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDiameterSDRatioHexM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_diameter_sd_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter SD/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean diameter SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDiameterSDRatioHexMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_diameter_sd_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter SD/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean diameter SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthVolumeSDRatioHexM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_volume_sd_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume SD/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean volume SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthVolumeSDRatioHexMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_volume_sd_ratio)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume SD/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean volume SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()




#density rasters

LengthDepthRatioDensityM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_depth_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean depth/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDepthRatioDensityMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_depth_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean depth/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDiameterRatioDensityM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_diameter_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean diameter/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDiameterRatioDensityMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_diameter_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean diameter/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthVolumeRatioDensityM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_volume_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean volume/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthVolumeRatioDensityMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_volume_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean volume/length ratio") +
  scale_x_reverse() +
  theme_minimal()


#length/posthole variation density
LengthDepthSDRatioDensityM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_depth_sd_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth SD/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean depth SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDepthSDRatioDensityMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_depth_sd_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth SD/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean depth SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDiameterSDRatioDensityM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_diameter_sd_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter SD/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean diameter SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthDiameterSDRatioDensityMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_diameter_sd_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter SD/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean diameter SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthVolumeSDRatioDensityM <- ggplot(ratio_prep_M, aes(x = -sampled, y = length_volume_sd_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume SD/Pithouse Length Ratio - M-type") + xlab("Years calBP") + ylab("mean volume SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()

LengthVolumeSDRatioDensityMW <- ggplot(ratio_prep_MW, aes(x = -sampled, y = length_volume_sd_ratio)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume SD/Pithouse Length Ratio - MW-type") + xlab("Years calBP") + ylab("mean volume SD/length ratio") +
  scale_x_reverse() +
  theme_minimal()