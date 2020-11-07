#001-libraries-and-functions

library("tidyverse")
library("ggthemes")
library("ggpubr")

#aoristic analysis function
time2aoristic<-function(data,timeData,resolution=100,minDate=NA,maxDate=NA,colphase)
{
  if (is.na(minDate)) {minDate=min(timeData[,2:3])-resolution}
  if (is.na(maxDate)) {maxDate=max(timeData[,2:3])+resolution}
  timeBlocks<-seq(minDate,maxDate,resolution)  
  
  tmpColnames<-colnames(timeData)
  timeData<-cbind(timeData,matrix(NA,nrow=dim(timeData)[1],ncol=length(timeBlocks)))
  
  for (x in 1:dim(timeData)[1])
  {
    timespan=timeData[x,3]-timeData[x,2] #compute time-span
    pp=1/timespan #compute probability of existence in a single year
    
    for (p in 1:length(timeBlocks))
    {
      start=timeData[x,2]
      end=timeData[x,3]
      if (timeBlocks[p]<start&(timeBlocks[p]+resolution)<=start) {timeData[x,p+3]=0} 
      if (timeBlocks[p]<start&(timeBlocks[p]+resolution)>start) {timeData[x,p+3]=pp*(resolution-abs(timeBlocks[p]-start))} 
      if (timeBlocks[p]>=start&timeBlocks[p]+resolution<=end){timeData[x,p+3]=pp*resolution} 
      if ((timeBlocks[p]>start)&(timeBlocks[p]<end)&timeBlocks[p]+resolution>=end){timeData[x,p+3]=pp*abs(timeBlocks[p]-end)} 
      if (timeBlocks[p]>=end) {timeData[x,p+3]=0}
      if (timeBlocks[p]<=start&(timeBlocks[p]+resolution)>=end){timeData[x,p+3]=1}
    }
  }
  
  colnames(timeData)<-c(tmpColnames,timeBlocks)
  res<-merge(x=data,y=timeData,by.x=colphase,by.y=1)
  return(res)
}

#multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#002-read-in-data

#import posthole and pithouse data
posts<- read_csv("data/raw-data/Tokyo PithouseDB V3.2 - PostHole.csv", na = c("#VALUE!", "#N/A"))
pits<- read_csv("data/raw-data/Tokyo PithouseDB V3.2 - Pithouse.csv", na = c("#VALUE!", "#N/A"))

#003-prepare-data

#Clean unnecessary columns
cleanpits <- select(pits, -c(PubPeriod, PhaseStart, PhaseEnd, PhaseStartRom, PhaseEndRom, NumPhaseStart, NumPhaseEnd, 
                             NumPhaseMed, Reference, Notes, TopologyNotes)) 

cleanpits$Length <- as.numeric(cleanpits$Length)
cleanpits$Width <- as.numeric(cleanpits$Width)
cleanpits$RecFloor <- as.numeric(cleanpits$RecFloor)

cleanposts <- select(posts, -c(Diameter, Major, Minor, Scale1m, PostType, D_Ref, Notes)) %>%
  mutate(FinalDepth = case_when(is.na(FlrLvlDepScale) ~ Depth,
                                FlrLvlDepScale > 0 ~ FlrLvlDepScale,
                                FlrLvlDepScale == 0 ~ Depth))

#Calculate oval shapted pithouse floor area
oval <- filter(cleanpits, ShapeFormula == "oval") %>%
  mutate(NewFloor = as.numeric(Length)*as.numeric(Width)*0.8)

#Calculate circle shaped pithouse floor area
circle <- filter(cleanpits, ShapeFormula == "circle") %>%
  mutate(NewFloor =  pi / 4 * as.numeric(Length)^2) 

#Combine oval and circle shaped pithouse floor areas back into dataframe  
pitfinal <- full_join(oval, circle)

#cleaning posthole data to provide averages and SDs for depth and diameter
 postfinal <- cleanposts %>%
  mutate(volume = pi * ((0.5 * D_Scaled)^2) * FinalDepth)%>%
  group_by(PithouseID) %>%
  summarise(mean_depth = mean(FinalDepth), 
            sd_depth = sd(FinalDepth), 
            mean_diameter = mean(D_Scaled), 
            sd_diameter = sd(D_Scaled),
            mean_volume = mean(volume),
            sd_volume = sd(volume),
            counts = n_distinct(PostholeID))

#combine pithouse and posthole data  
pithouse <- full_join(pitfinal, postfinal, by = "PithouseID")%>%
  drop_na("TotalPhaseRom") %>% 
  mutate_at(vars(topo_after1:topo_after13), ~replace(., which(!(. %in% PithouseID)), NA))

#reposition NA values after actual values  
pithouse[paste0("topo_after", 1:13)] <- t(apply(pithouse[paste0("topo_after", 1:13)], 1, function(x) c(x[!is.na(x)], x[is.na(x)])))

#add blank sampled column
pithouse["sampled"] <- NA_real_

#create potphase from pithouse data
potphase <- select(pits, c(TotalPhaseRom, NumPhaseStart, NumPhaseEnd)) %>%
  drop_na(TotalPhaseRom) %>%
  distinct(TotalPhaseRom, .keep_all = TRUE) 

#create data frames for analysis
tokyo_pithouse_join <- pithouse 
temp_pithouse_join<-tokyo_pithouse_join #temp_pithouse_join used for dynamically created samples
temp_pithouse_join$sampled <- NA_real_ #blanking out the sample column so I can check against NA for the dynamic detereminatination.
temp_pithouse_join %>% mutate_if(is.factor, as.character) -> temp_pithouse_join #change factors to characters

#003 - analysis

#aoristic analysis
tokyo_adata_test<-time2aoristic(data=pithouse,timeData=potphase,resolution=100,minDate=-5415,maxDate=-4050,colphase=14)
tokyo_mcprep <- tokyo_adata_test[,c(2,40:53)] #trim time2aoristic output and prep for further analysis

#MCMC
# 1000 iteration loop

for (i in 1:1000){ #determines how many iterations to run
  
  row_list<-as.list(1:nrow(temp_pithouse_join))
  q<-0
  
  while(length(row_list)!=0 & q<10){
    q<-q+1 #to make sure that we don't spinning off in an infinite loop
    for(j in row_list){ #this loop replaces the check values
      skip_flag<-FALSE #initialize skip flag used to check the replacement sampling
      for(k in 15:27){ #checking the topoafter columns
        if(is.na(temp_pithouse_join[j,k])){ 
          print("NA break")
          print(i)
          break
        } else if(is.na(as.integer(temp_pithouse_join[j,k]))==FALSE) { #if it's already an integer, well, a character vector containing an integer, we already did this, next
          print("integer next")
          next
          print("integer next")
        } else if(temp_pithouse_join[j,k]==""){ #check for blank values
          print("empty string next")
          temp_pithouse_join[j,k]<-NA_real_ #if blank value found, replace with NA
          print("fixed blank to NA")
          next 
        }
        else if(is.na(filter(temp_pithouse_join,PithouseID==as.character(temp_pithouse_join[j,k]))["sampled"])) { #if the replacement has not yet been generated, move on, but set flag to jump this to the end
          skip_flag<-TRUE
          print("skip flag set")
        } else {
          temp_pithouse_join[j,k]<-as.integer(filter(temp_pithouse_join,PithouseID==temp_pithouse_join[j,k])[37]) #replacing pithouse IDs with the sampled dates of those IDs
          print("successful check value grab")
        } #if-else
      } #k for loop
      if(skip_flag==FALSE){
        row_list<-row_list[row_list!=j]
      } else {
        next 
      }
      
      #sampling section
      if(skip_flag==FALSE){
        temp_pithouse_join[j,37]<-sample(as.numeric(names(tokyo_mcprep[,2:15])),1,
                                         replace=FALSE,
                                         filter(tokyo_mcprep,PithouseID==as.character(temp_pithouse_join[j,1]))[2:15])
        temp_pithouse_join[j,38]<-i #identifying the run number
        
        if(any(as.numeric(temp_pithouse_join[j,15:27])>as.numeric(temp_pithouse_join[j,37]),na.rm=TRUE)){
          print(j)
          while(any(as.numeric(temp_pithouse_join[j,15:27])>as.numeric(temp_pithouse_join[j,37]),na.rm=TRUE)){
            temp_pithouse_join[j,37]<-sample(as.numeric(names(tokyo_mcprep[,2:15])),1,
                                             replace=TRUE,filter(tokyo_mcprep,PithouseID==temp_pithouse_join[j,1])[2:15])
          } #while 
          temp_pithouse_join[j,38]=i 
        }#if
      }
    } #j for loop
  } #while loop wrapper around j loop
  if(i==1){
    tokyo_pithouse_join2<-temp_pithouse_join
  }else{
    tokyo_pithouse_join2<-rbind(tokyo_pithouse_join2,temp_pithouse_join)
  }#else
  
  #blank out temp_pithouse_join to prepare for another run
  temp_pithouse_join<-tokyo_pithouse_join
  temp_pithouse_join$sampled <- NA_real_ 
  temp_pithouse_join %>% mutate_if(is.factor, as.character) -> temp_pithouse_join 
  
}#i for loop

#tidy loop
colnames(tokyo_pithouse_join2)[38] <- "RunID" #renames the column containing the "i" value to RunID

#remove sampled Values not in Middle Jomon
tokyo_pithouse_join_trim <- tokyo_pithouse_join2 %>%
  filter(sampled < -4415)


###Correleation Analysis
#Floor Space and Mean Post Depth
PearsonNewFloorMeanDepth <- ggscatter(tokyo_pithouse_join, x = "NewFloor", y = "mean_depth", 
           add = "reg.line", conf.int = TRUE,
           xlab = "Floor Space m2", ylab = "Post Mean Depth") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()


#Floor Space and Mean Post Depth Main Subset
PearsonNewFloorMeanDepthMain <- filter(tokyo_pithouse_join, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "mean_depth", 
                                      add = "reg.line", conf.int = TRUE,
                                      xlab = "Floor Space m2", ylab = "Post Mean Depth") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorMeanDepthMain)


#Floor Space and Mean Post Depth MainWall Subset
PearsonNewFloorMeanDepthMainWall <- filter(tokyo_pithouse_join, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "mean_depth", 
                                      add = "reg.line", conf.int = TRUE,
                                      xlab = "Floor Space m2", ylab = "Post Mean Depth") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorMeanDepthMainWall)


#Floor Space and Post Depth SD
PearsonNewFloorDepthSD <- ggscatter(tokyo_pithouse_join, x = "NewFloor", y = "sd_depth", 
                                      add = "reg.line", conf.int = TRUE,
                                      xlab = "Floor Space m2", ylab = "Post Depth SD") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()


#Floor Space and Post Depth SD Main Subset
PearsonNewFloorDepthSDMain <- filter(tokyo_pithouse_join, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "sd_depth", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Depth SD") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorDepthSDMain)


#Floor Space and Post Depth SD MainWall Subset
PearsonNewFloorDepthSDMainWall <- filter(tokyo_pithouse_join, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "sd_depth", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Depth SD") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorDepthSDMainWall)


#Floor Space and Mean Post Diameter
PearsonNewFloorMeanDiameter <- ggscatter(tokyo_pithouse_join, x = "NewFloor", y = "mean_diameter", 
                                          add = "reg.line", conf.int = TRUE,
                                          ylim = c(0,.6), 
                                          xlab = "Floor Space m2", ylab = "Post Mean Diameter") +
  stat_cor(method = "pearson", label.x = 40, label.y = .5) +
  theme_minimal()


#Floor Space and Mean Post Diameter Main Subset
PearsonNewFloorMeanDiameterMain <- filter(tokyo_pithouse_join, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "mean_diameter", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Mean Diameter") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorMeanDiameterMain)


#Floor Space and Mean Post Depth MainWall Subset
PearsonNewFloorMeanDiameterMainWall <- filter(tokyo_pithouse_join, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "mean_diameter", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Mean Diameter") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorMeanDiameterMainWall)


#Floor Space and Post Diameter SD
PearsonNewFloorDiameterSD <- ggscatter(tokyo_pithouse_join, x = "NewFloor", y = "sd_diameter", 
                                         add = "reg.line", conf.int = TRUE,
                                         ylim = c(0,.6), 
                                         xlab = "Floor Space m2", ylab = "Post Diameter SD") +
  stat_cor(method = "pearson", label.x = 40, label.y = .5)


#Floor Space and Post Diameter SD Main Subset
PearsonNewFloorDiameterSDMain <- filter(tokyo_pithouse_join, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "sd_diameter", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Diameter SD") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorDiameterSDMain)


#Floor Space and Post Diameter SD MainWall Subset
PearsonNewFloorDiameterSDMainWall <- filter(tokyo_pithouse_join, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "sd_diameter", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Diameter SD") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorDiameterSDMainWall)


#Floor Space and Mean Post Volume
PearsonNewFloorMeanVolume <- ggscatter(tokyo_pithouse_join, x = "NewFloor", y = "mean_volume", 
                                         add = "reg.line", conf.int = TRUE,
                                         ylim = c(0,.6), 
                                         xlab = "Floor Space m2", ylab = "Post Mean Volume") +
  stat_cor(method = "pearson", label.x = 40, label.y = .5)


#Floor Space and Mean Post Volume Main Subset
PearsonNewFloorMeanVolumeMain <- filter(tokyo_pithouse_join, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "mean_volume", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Mean Volume") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorMeanVolumeMain)


#Floor Space and Mean Post Volume MainWall Subset
PearsonNewFloorMeanVolumeMainWall <- filter(tokyo_pithouse_join, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "mean_volume", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Mean Volume") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorMeanVolumeMainWall)

#Floor Space and Post Volume SD
PearsonNewFloorVolumeSD <- ggscatter(tokyo_pithouse_join, x = "NewFloor", y = "sd_volume", 
                                       add = "reg.line", conf.int = TRUE,
                                       ylim = c(0,.6), 
                                       xlab = "Floor Space m2", ylab = "Post Volume SD") +
  stat_cor(method = "pearson", label.x = 40, label.y = .5)


#Floor Space and Post Volume SD Main Subset
PearsonNewFloorVolumeSDMain <- filter(tokyo_pithouse_join, PillarPosition == 'Main') %>% 
  ggscatter(x = "NewFloor", y = "sd_volume", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Volume SD") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorVolumeSDMain)


#Floor Space and Post Volume SD MainWall Subset
PearsonNewFloorVolumeSDMainWall <- filter(tokyo_pithouse_join, PillarPosition == 'MainWall') %>% 
  ggscatter(x = "NewFloor", y = "sd_volume", 
            add = "reg.line", conf.int = TRUE,
            xlab = "Floor Space m2", ylab = "Post Volume SD") +
  stat_cor(method = "pearson", label.x = 40, label.y = 1) +
  theme_minimal()
plot(PearsonNewFloorVolumeSDMainWall)


##Correlation Tests
NewFloorMeanDepthCorTest <- cor.test(tokyo_pithouse_join$NewFloor, tokyo_pithouse_join$mean_depth, 
                                     method = "pearson")

NewFloorMeanDiameterCorTest <- cor.test(tokyo_pithouse_join$NewFloor, tokyo_pithouse_join$mean_diameter, 
                                     method = "pearson")

ShapiroNewFloor <- shapiro.test(tokyo_pithouse_join$NewFloor)
ShapiroMeanDepth <- shapiro.test(tokyo_pithouse_join$mean_depth)
ShapiroMeanDiameter <- shapiro.test(tokyo_pithouse_join$mean_diameter)
ggqqplot(tokyo_pithouse_join$NewFloor)
ggqqplot(tokyo_pithouse_join$mean_depth)
ggqqplot(tokyo_pithouse_join$mean_diameter)


###Main Plots for 1000 Runs

##Pithouse Count Plots

#Pithouse Counts - 2 parts
PithouseCounts1000 <- tokyo_pithouse_join_trim %>%
  group_by(sampled, RunID) %>%
  tally()

PithouseCounts1000Plot <- ggplot(data = PithouseCounts1000) +
  aes(x = sampled, y = n, group = RunID) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(-5415,-4415) +
  ggtitle("Pithouse Counts - 1000 runs") + xlab("Years calBP") + ylab("Pithouses") +
  theme_minimal()

#Pithouse Count Histogram - Barplot 
PithouseCounts1000Barplot <- ggplot(data = tokyo_pithouse_join_trim) +
  aes(x = sampled) +
  geom_bar() +
  ggtitle("Pithouse Counts - 1000 runs") + xlab("Years calBP") + ylab("pithouses") +
  theme_minimal()

#Stacked Barplot
PithouseCounts1000BarplotStack <- ggplot(data = tokyo_pithouse_join_trim) +
  aes(x = sampled, fill = PillarPosition) +
  geom_bar(position = "stack") +
  ggtitle("Pithouse Counts - 1000 runs") + xlab("Years calBP") + ylab("pithouses") +
  theme_minimal()

#Barplot Series
PithouseCounts1000BarplotSeries <- ggplot(data = tokyo_pithouse_join_trim) +
  aes(x = sampled, fill = PillarPosition) +
  geom_bar(position = "dodge") +
  ggtitle("Pithouse Counts - 1000 runs") + xlab("Years calBP") + ylab("pithouses") +
  theme_minimal()

#Pithouse Line Plot By Type
PithouseLineCounts1000 <- tokyo_pithouse_join_trim %>%
  group_by(sampled, RunID, PillarPosition) %>%
  tally() %>%
  group_by(PillarPosition, sampled) %>%
  summarise_at(vars(n), sum)

PithouseCounts1000LineType <- ggplot(data = PithouseLineCounts1000) +
  aes(x = sampled, y = n, group = PillarPosition) +
  geom_line(aes(color = PillarPosition)) +
  ggtitle("Pithouse Counts - 1000 runs - Total") + xlab("Years calBP") + ylab("pithouses") +
  theme_minimal()

#Kernel Density Estimate
PithouseCounts1000KDE <- ggplot(tokyo_pithouse_join_trim) +
  aes(sampled, color = PillarPosition, fill = PillarPosition, alpha = .2) +
  stat_density(adjust = 4)

#Combinded histogram and KDE
PithouseCounts1000HistogramKDE <- ggplot(tokyo_pithouse_join_trim, aes(x=sampled)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=100,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666", bw=55)


###Pithouse Floor Space Regressions


NewFloorPlot1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(NewFloor)), 
                           aes(x = sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  xlim(-5415,-4415) +
  ggtitle("Floor Space - 1000 runs") + xlab("Years calBP") + ylab("m^2") +
  theme_minimal()

MeanDepthPlot1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(mean_depth)), 
                            aes(x = sampled, y = mean_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(-5415,-4415) +
  ggtitle("Mean Post Depth - 1000 runs") + xlab("Years calBP") + ylab("m") +
  theme_minimal()

MeanVolumePlot1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(mean_volume)), 
                             aes(x = sampled, y = mean_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(-5415,-4415) +
  ggtitle("Mean Post Volume - 1000 runs") + xlab("Years calBP") + ylab("m^3") +
  theme_minimal()

MeanDiameterPlot1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(mean_diameter)), 
                               aes(x = sampled, y = mean_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(-5415,-4415) +
  ggtitle("Mean Post Diameter - 1000 runs") + xlab("Years calBP") + ylab("m") +
  theme_minimal()

SDDepthPlot1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(sd_depth)), 
                          aes(x = sampled, y = sd_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(-5415,-4415) +
  ggtitle("Post Depth SD - 1000 runs") + xlab("Years calBP") + ylab("SD") +
  theme_minimal()

SDDiameterPlot1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(sd_diameter)), 
                             aes(x = sampled, y = sd_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(-5415,-4415) +
  ggtitle("Post Diameter SD - 1000 runs") + xlab("Years calBP") + ylab("SD") +
  theme_minimal()

SDVolumePlot1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(sd_volume)), 
                           aes(x = sampled, y = sd_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(-5415,-4415) +
  ggtitle("Post Volume SD - 1000 runs") + xlab("Years calBP") + ylab("SD") +
  theme_minimal()

PithouseLengthPlot1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(Length)), 
                                 aes(x = sampled, y = as.numeric(Length), group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  xlim(-5415,-4415) +
  ggtitle("Pithouse Length - 1000 runs") + xlab("Years calBP") + ylab("m") +
  theme_minimal()

CombinedRegressions1000Multi <- multiplot(NewFloorPlot1000, MeanDepthPlot1000,MeanDiameterPlot1000, MeanVolumePlot1000, 
                                     PithouseLengthPlot1000, SDDepthPlot1000, SDDiameterPlot1000, SDVolumePlot1000, 
                                     cols=2)

###Pithouse Floor Space Regressions - Separated by Type

NewFloorPlotCombined1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(NewFloor)), 
                                   aes(x = sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - 1000 runs - Combined") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,35)) +
  theme_minimal()

NewFloorPlotPit1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(NewFloor) & PillarPosition == "Pit"), 
                           aes(x = sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - 1000 runs - Pit") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,35)) +
  theme_minimal()

NewFloorPlotWall1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(NewFloor) & PillarPosition == "Wall"), 
                               aes(x = sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - 1000 runs - Wall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,35)) +
  theme_minimal()

NewFloorPlotMain1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(NewFloor) & PillarPosition == "Main"), 
                               aes(x = sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - 1000 runs - Main") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,35)) +
  theme_minimal()

NewFloorPlotMainWall1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(NewFloor) & PillarPosition == "MainWall"), 
                               aes(x = sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - 1000 runs - MainWall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,35)) +
  theme_minimal()

NewFloorPlotMirror1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(NewFloor) & PillarPosition == "Mirror"), 
                               aes(x = sampled, y = NewFloor, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Floor Space - 1000 runs - Mirror") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,35)) +
  theme_minimal()

CombinedTypeRegressionsNewFloor1000Multi <- multiplot(NewFloorPlotPit1000, NewFloorPlotWall1000,
                                                      NewFloorPlotMain1000, NewFloorPlotMainWall1000, 
                                                      NewFloorPlotMirror1000, NewFloorPlotCombined1000,
                                                      cols=2)

###Main and MainWall Pithouse Type Comparison
MeanDepthPlotMain1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(mean_depth) & PillarPosition == "Main"), 
                               aes(x = sampled, y = mean_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Mean Depth - 1000 runs - Main") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0.4,0.8)) +
  theme_minimal()

MeanDepthPlotMainWall1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(mean_depth) & PillarPosition == "MainWall"), 
                                   aes(x = sampled, y = mean_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Mean Depth - 1000 runs - MainWall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0.4,0.8)) +
  theme_minimal()

##Post Depth SD
SDDepthPlotMain1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(sd_depth) & PillarPosition == "Main"), 
                                aes(x = sampled, y = sd_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Post Depth SD - 1000 runs - Main") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,0.25)) +
  theme_minimal()

SDDepthPlotMainWall1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(sd_depth) & PillarPosition == "MainWall"), 
                                    aes(x = sampled, y = sd_depth, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Post Depth SD - 1000 runs - MainWall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,0.25)) +
  theme_minimal()

##Mean Post Diameter
MeanDiameterPlotMain1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(mean_diameter) & PillarPosition == "Main"), 
                                aes(x = sampled, y = mean_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Mean Diameter - 1000 runs - Main") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,0.7)) +
  theme_minimal()

MeanDiameterPlotMainWall1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(mean_diameter) & PillarPosition == "MainWall"), 
                                    aes(x = sampled, y = mean_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Mean Diameter - 1000 runs - MainWall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0,0.7)) +
  theme_minimal()

##Post Diameter SD
SDDiameterPlotMain1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(sd_diameter) & PillarPosition == "Main"), 
                              aes(x = sampled, y = sd_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Post Diameter SD - 1000 runs - Main") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0.02,0.085)) +  
  theme_minimal()

SDDiameterPlotMainWall1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(sd_diameter) & PillarPosition == "MainWall"), 
                                  aes(x = sampled, y = sd_diameter, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Post Diameter SD - 1000 runs - MainWall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(0.02,0.085)) +
  theme_minimal()

##Mean Post Volume
MeanVolumePlotMain1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(mean_volume) & PillarPosition == "Main"), 
                                   aes(x = sampled, y = mean_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Mean Volume - 1000 runs - Main") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(-2,4)) +
  theme_minimal()

MeanVolumePlotMainWall1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(mean_volume) & PillarPosition == "MainWall"), 
                                       aes(x = sampled, y = mean_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Mean Volume - 1000 runs - MainWall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  coord_cartesian(ylim = c(-2,4)) +
  theme_minimal()

##Post Volume SD
SDVolumePlotMain1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(sd_volume) & PillarPosition == "Main"), 
                                 aes(x = sampled, y = sd_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Post Volume SD - 1000 runs - Main") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()

SDVolumePlotMainWall1000 <- ggplot(data = subset(tokyo_pithouse_join_trim, !is.na(sd_volume) & PillarPosition == "MainWall"), 
                                     aes(x = sampled, y = sd_volume, group = RunID)) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) + 
  ggtitle("Post Volume SD - 1000 runs - MainWall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()

MainVSMainWallRegressions1000Multi <- multiplot(MeanDepthPlotMain1000, SDDepthPlotMain1000,
                                           MeanDiameterPlotMain1000, SDDiameterPlotMain1000,
                                           MeanVolumePlotMain1000, SDVolumePlotMain1000,
                                           MeanDepthPlotMainWall1000, SDDepthPlotMainWall1000,
                                           MeanDiameterPlotMainWall1000, SDDiameterPlotMainWall1000,
                                           MeanVolumePlotMainWall1000, SDVolumePlotMainWall1000,cols=2)

###Pithouse Floor Space Box Plots - Seaparated by Type

NewFloorBoxPlotCombined1000 <- tokyo_pithouse_join_trim %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Combined Types") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5600,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotPit1000 <- tokyo_pithouse_join_trim %>%
  filter(PillarPosition == "Pit") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Pit") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5600,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotWall1000 <- tokyo_pithouse_join_trim %>%
  filter(PillarPosition == "Wall") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Wall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5600,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotMain1000 <- tokyo_pithouse_join_trim %>%
  filter(PillarPosition == "Main") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Main") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5600,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotMainWall1000 <- tokyo_pithouse_join_trim %>%
  filter(PillarPosition == "MainWall") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - MainWall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5600,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotMirror1000 <- tokyo_pithouse_join_trim %>%
  filter(PillarPosition == "Mirror") %>%
  ggplot(aes(x = sampled, y = NewFloor, group = sampled)) +
  geom_boxplot() +
  ggtitle("Floor Space - Mirror") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5600,-4400) +
  ylim(0, 70) +
  theme_minimal()

NewFloorBoxPlotMulti <-  multiplot(NewFloorBoxPlotPit1000, NewFloorBoxPlotWall1000,NewFloorBoxPlotMain1000, NewFloorBoxPlotMainWall1000, 
          NewFloorBoxPlotMirror1000, NewFloorBoxPlotCombined1000, cols=2)

###Save Plots
plot_list <- c("MeanDepthPlot1000", "MeanDepthPlotMain1000", "MeanDepthPlotMainWall1000", "MeanDiameterPlot1000", 
               "MeanDiameterPlotMain1000", "MeanDiameterPlotMainWall1000", "MeanVolumePlot1000", 
               "MeanVolumePlotMain1000", "MeanVolumePlotMainWall1000", "NewFloorBoxPlotCombined1000", 
               "NewFloorBoxPlotMain1000", "NewFloorBoxPlotMainWall1000", "NewFloorBoxPlotMirror1000", 
               "NewFloorBoxPlotMulti", "NewFloorBoxPlotPit1000", "NewFloorBoxPlotWall1000", "NewFloorMeanDepthCorTest",
               "NewFloorMeanDiameterCorTest", "NewFloorPlot1000", "NewFloorPlotCombined1000", "NewFloorPlotMain1000",
               "NewFloorPlotMainWall1000", "NewFloorPlotMirror1000", "NewFloorPlotPit1000", "NewFloorPlotWall1000", 
               "PearsonNewFloorDepthSD", "PearsonNewFloorDepthSDMain", "PearsonNewFloorDepthSDMainWall", 
               "PearsonNewFloorDiameterSD", "PearsonNewFloorDiameterSDMain", "PearsonNewFloorDiameterSDMainWall", 
               "PearsonNewFloorMeanDepth", "PearsonNewFloorMeanDepthMain", "PearsonNewFloorMeanDepthMainWall", 
               "PearsonNewFloorMeanDiameter", "PearsonNewFloorMeanDiameterMain", "PearsonNewFloorMeanDiameterMainWall",
               "PearsonNewFloorMeanVolume", "PearsonNewFloorMeanVolumeMain", "PearsonNewFloorMeanVolumeMainWall", 
               "PearsonNewFloorVolumeSD", "PearsonNewFloorVolumeSDMain", "PearsonNewFloorVolumeSDMainWall", 
               "PithouseCounts1000Barplot", "PithouseCounts1000BarplotSeries", "PithouseCounts1000BarplotStack", 
               "PithouseCounts1000HistogramKDE", "PithouseCounts1000KDE", "PithouseCounts1000LineType", 
               "PithouseCounts1000Plot", "PithouseLengthPlot1000", "SDDepthPlot1000", "SDDepthPlotMain1000", 
               "SDDepthPlotMainWall1000", "SDDiameterPlot1000", "SDDiameterPlotMain1000", "SDDiameterPlotMainWall1000",
               "SDVolumePlot1000", "SDVolumePlotMain1000", "SDVolumePlotMainWall1000", "SDVolumePlotMainWall1000")

for(i in 1:length(plot_list)) {
  ggsave(filename=sprintf("%s.jpg", plot_list[i]),
         plot=get(plot_list[i]))
}

###Tables

PithouseTypeTable <- tokyo_pithouse_join %>%
  count(PillarPosition)

unique(tokyo_pithouse_join_trim$sampled)

test <- tokyo_pithouse_join %>%
  filter(!is.na(NewFloor))

unique <- unique(test$SiteID)


###Additional Plots for Presentations

##Plots for Pithouse Types using Points
tokyo_pithouse_join_trim %>%
  filter(RunID == 1 & PillarPosition == "Pit") %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 1 - Pit") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal() 

tokyo_pithouse_join_trim %>%
  filter(RunID == 1 & PillarPosition == "Wall") %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 1 - Wall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal() 

tokyo_pithouse_join_trim %>%
  filter(RunID == 1 & PillarPosition == "Main") %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 1 - Main") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal() 

tokyo_pithouse_join_trim %>%
  filter(RunID == 1 & PillarPosition == "MainWall") %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 1 - MainWall") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal() 

tokyo_pithouse_join_trim %>%
  filter(RunID == 1 & PillarPosition == "Mirror") %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 1 - Mirror") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal() 


#point versions
tokyo_pithouse_join_trim %>%
  filter(RunID == 1) %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 1") + xlab("Years calBP") + ylab("m^2") +
  theme_minimal() 

tokyo_pithouse_join_trim %>%
  filter(RunID == 1) %>%
  ggplot(aes(x = sampled, y = mean_depth, group = PithouseID)) +
  geom_point() +
  ggtitle("Mean Post Depth - Run 1") + xlab("Years calBP") + ylab("m") +
  theme_minimal()

tokyo_pithouse_join_trim %>%
  filter(RunID == 1) %>%
  ggplot(aes(x = sampled, y = mean_diameter, group = PithouseID)) +
  geom_point()

tokyo_pithouse_join_trim %>%
  filter(RunID == 1) %>%
  ggplot(aes(x = sampled, y = sd_depth, group = PithouseID)) +
  geom_point() +
  ggtitle("Post Depth SD - Run 1") + xlab("Years calBP") + ylab("SD") +
  theme_minimal()

tokyo_pithouse_join_trim %>%
  filter(RunID == 1) %>%
  ggplot(aes(x=sampled))+ geom_histogram() +
  xlim(-5300,-4750) +
  stat_bin(aes(y=..count.., label=..count..), binwidth = 100, geom="text", vjust=-.5) 
ggtitle("Pithouse Counts - Run 1") + xlab("Years calBP") + ylab("Pithouses") +
  theme_minimal()

#quick dirty plots for presentation
tokyo_pithouse_join_trim %>%
  filter(RunID == 1) %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 1") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()

tokyo_pithouse_join_trim %>%
  filter(RunID == 2) %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 2") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()

tokyo_pithouse_join_trim %>%
  filter(RunID == 3) %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 3") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()

tokyo_pithouse_join_trim %>%
  filter(RunID == 4) %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 4") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()

tokyo_pithouse_join_trim %>%
  filter(RunID == 5) %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 5") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()

tokyo_pithouse_join_trim %>%
  filter(RunID == 6) %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 6") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()

tokyo_pithouse_join_trim %>%
  filter(RunID == 7) %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 7") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()

tokyo_pithouse_join_trim %>%
  filter(RunID == 8) %>%
  ggplot(aes(x = sampled, y = NewFloor)) +
  geom_point() +
  ggtitle("Floor Space - Run 8") + xlab("Years calBP") + ylab("m^2") +
  xlim(-5415,-4415) +
  theme_minimal()
sessionInfo()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      