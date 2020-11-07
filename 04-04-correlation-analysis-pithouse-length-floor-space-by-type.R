###04-4 - correlation-analysis- pithouse length and floor space correlations by type

#These create correlation matrices for comparing pithouse length and floor space for each pithouse type

sampleclean <- readRDS(here::here("data/derived-data/sampleclean.rds"))

##Floor Space Pithouse Length Correlations by Type

#Pit
#Pithouse Length and Mean Post Volume - MainWall Type
SpearmanLengthNewFloorPit <- filter(sampleclean, PillarPosition == 'Pit') %>% 
  ggscatter(x = "Length", y = "NewFloor", 
            add = "reg.line", conf.int = TRUE,
            title = "P-type",
            xlab = "Pithouse Length (m)", ylab = expression(paste("Floor Space in m"^"2")),
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman") +
  theme_minimal()

#Wall
SpearmanLengthNewFloorWall <- filter(sampleclean, PillarPosition == 'Wall') %>% 
  ggscatter(x = "Length", y = "NewFloor", 
            add = "reg.line", conf.int = TRUE,
            title = "W-type",
            xlab = "Pithouse Length (m)", ylab = expression(paste("Floor Space in m"^"2")),
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman") +
  theme_minimal()

#Main
SpearmanLengthNewFloorMain <- filter(sampleclean, PillarPosition == 'Main') %>% 
  ggscatter(x = "Length", y = "NewFloor", 
            add = "reg.line", conf.int = TRUE,
            title = "M-type",
            xlab = "Pithouse Length (m)", ylab = expression(paste("Floor Space in m"^"2")),
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman") +
  theme_minimal()

#MainWall
SpearmanLengthNewFloorMainWall <- filter(sampleclean, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "Length", y = "NewFloor", 
            add = "reg.line", conf.int = TRUE,
            title = "MW-type",
            xlab = "Pithouse Length (m)", ylab = expression(paste("Floor Space in m"^"2")),
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman") +
  theme_minimal()

#Mirror
SpearmanLengthNewFloorMirror <- filter(sampleclean, PillarPosition == 'Mirror') %>% 
  ggscatter(x = "Length", y = "NewFloor", 
            add = "reg.line", conf.int = TRUE,
            title = "HM-type",
            xlab = "Pithouse Length (m)", ylab = expression(paste("Floor Space in m"^"2")),
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman") +
  theme_minimal()

#Combined
SpearmanLengthNewFloorCombined <- sampleclean %>% 
  ggscatter(x = "Length", y = "NewFloor", 
            add = "reg.line", conf.int = TRUE,
            title = "Combined Types",
            xlab = "Pithouse Length (m)", ylab = expression(paste("Floor Space in m"^"2")),
            shape = 21, fill = "lightgray", color = "black", size = 3) +
  stat_cor(method = "spearman") +
  theme_minimal()



##save multiplots

#Pithouse Length and Floor Space Correlation Grid
pdf(file = here::here("figures/SpearmanLengthNewFloor1000Multi.pdf"))
gridExtra::grid.arrange(SpearmanLengthNewFloorPit, 
                        SpearmanLengthNewFloorWall,                         
                        SpearmanLengthNewFloorMain,
                        SpearmanLengthNewFloorMainWall,                         
                        SpearmanLengthNewFloorMirror, 
                        SpearmanLengthNewFloorCombined, 
                        ncol=2)
dev.off()