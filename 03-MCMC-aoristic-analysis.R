#03 - MCMC-aoristic-analysis



#MCMC and subsequent clean up

#pithouse
# 1000 iteration loop

for (i in 1:1000){ #determines how many iterations to run
  
  row_list<-as.list(1:nrow(temp_sampleclean))
  q<-0
  
  while(length(row_list)!=0 & q<10){
    q<-q+1 #to make sure that we don't spinning off in an infinite loop
    for(j in row_list){ #this loop replaces the check values
      skip_flag<-FALSE #initialize skip flag used to check the replacement sampling
      for(k in 18:30){ #checking the topoafter columns
        if(is.na(temp_sampleclean[j,k])){ 
          # print("NA break")
          # print(i)
          break
        } else if(is.na(as.integer(temp_sampleclean[j,k]))==FALSE) { #if it's already an integer, well, a character vector containing an integer, we already did this, next
          # print("integer next")
          next
          # print("integer next")
        } else if(temp_sampleclean[j,k]==""){ #check for blank values
          # print("empty string next")
          temp_sampleclean[j,k]<-NA #if blank value found, replace with NA
          # print("fixed blank to NA")
          next 
        }
        else if(is.na(filter(temp_sampleclean,PithouseID==as.character(temp_sampleclean[j,k]))["sampled"])) { #if the replacement has not yet been generated, move on, but set flag to jump this to the end
          skip_flag<-TRUE
          # print("skip flag set")
        } else {
          temp_sampleclean[j,k]<-as.integer(filter(temp_sampleclean,PithouseID==temp_sampleclean[j,k])[40]) #replacing pithouse IDs with the sampled dates of those IDs
          # print("successful check value grab")
        } #if-else
      } #k for loop
      if(skip_flag==FALSE){
        row_list<-row_list[row_list!=j]
      } else {
        next 
      }
      
      #sampling section
      if(skip_flag==FALSE){
        temp_sampleclean[j,40]<-mapply(function(x, y) if(any(is.na(x) || is.na(y))) NA else 
          sample(seq(x, y, na.rm = TRUE), 1), temp_sampleclean[j,"NumPhaseStart"], temp_sampleclean[j,"NumPhaseEnd"])
        temp_sampleclean[j,41]<-i #identifying the run number
        
        if(any(as.numeric(temp_sampleclean[j,18:30])>as.numeric(temp_sampleclean[j,40]),na.rm=TRUE)){
          # print(j)
          while(any(as.numeric(temp_sampleclean[j,18:30])>as.numeric(temp_sampleclean[j,40]),na.rm=TRUE)){
            temp_sampleclean[j,40]<-mapply(function(x, y) if(any(is.na(x) || is.na(y))) NA else 
              sample(seq(x, y, na.rm = TRUE), 1), temp_sampleclean[j,"NumPhaseStart"], temp_sampleclean[j,"NumPhaseEnd"])
          } #while 
          temp_sampleclean[j,41]=i 
        }#if
      }
    } #j for loop
  } #while loop wrapper around j loop
  if(i==1){
    sampleclean2<-temp_sampleclean
  }else{
    sampleclean2<-rbind(sampleclean2,temp_sampleclean)
  }#else
  
  #blank out temp_sampleclean to prepare for another run
  temp_sampleclean<-sampleclean
  temp_sampleclean$sampled <- NA 
  temp_sampleclean %>% mutate_if(is.factor, as.character) -> temp_sampleclean 
  
}#i for loop

#tidy loop
colnames(sampleclean2)[41] <- "RunID" #renames the column containing the "i" value to RunID

#remove sampled Values not in Middle Jomon
sampleclean_trim <- sampleclean2 %>%
  filter(sampled < -4400 & sampled >= -5400)

#Round values for aoristic analysis
samplecleanround <- sampleclean_trim %>%
  mutate(sampled = floor(sampleclean_trim$sampled/100) * 100)

#create pithouse to posthole size ratios for later analysis
ratio_prep <- sampleclean_trim %>%
  mutate(length_depth_ratio = mean_depth/Length, 
         length_diameter_ratio = mean_diameter/Length, 
         length_volume_ratio = mean_volume/Length,
         NewFloor_depth_ratio = mean_depth/NewFloor, 
         NewFloor_diameter_ratio = mean_diameter/NewFloor, 
         NewFloor_volume_ratio = mean_volume/NewFloor,
         length_depth_sd_ratio = sd_depth/Length, 
         length_diameter_sd_ratio = sd_diameter/Length, 
         length_volume_sd_ratio = sd_volume/Length,
         NewFloor_depth_sd_ratio = sd_depth/NewFloor, 
         NewFloor_diameter_sd_ratio = sd_diameter/NewFloor, 
         NewFloor_volume_sd_ratio = sd_volume/NewFloor)

#cancel negative values for samplecleanround
samplecleanroundneg <- samplecleanround 
samplecleanroundneg$sampled <- samplecleanroundneg$sampled * -1

# save to disk to so we can reuse this:
saveRDS(sampleclean, here::here("data/derived-data/sampleclean.rds"))
saveRDS(sampleclean_trim, here::here("data/derived-data/sampleclean_trim.rds"))
saveRDS(samplecleanround, here::here("data/derived-data/samplecleanround.rds"))
saveRDS(samplecleanroundneg, here::here("data/derived-data/samplecleanroundneg.rds"))
saveRDS(ratio_prep, here::here("data/derived-data/ratio_prep.rds"))

#skeletal
#MCMC
# 2000 iteration loop

for (i in 1:2000){ #determines how many iterations to run
  
  row_list<-as.list(1:nrow(temp_skeletalsummary))
  q<-0
  
  while(length(row_list)!=0 & q<10){
    q<-q+1 #to make sure that we don't spinning off in an infinite loop
    for(j in row_list){ #this loop replaces the check values
      #sampling section
      if(is.na(temp_skeletalsummary[j,11])){
        temp_skeletalsummary[j,11]<-mapply(function(x, y) if(any(is.na(x) || is.na(y))) NA else 
          sample(seq(x, y, na.rm = TRUE), 1), temp_skeletalsummary[j,"phase_start"], temp_skeletalsummary[j,"phase_end"])
        temp_skeletalsummary[j,12]<-i #identifying the run number
      }
    } #j for loop
  } #while loop wrapper around j loop
  if(i==1){
    skeletalsummary2<-temp_skeletalsummary
  }else{
    skeletalsummary2<-rbind(skeletalsummary2,temp_skeletalsummary)
  }#else
  
  #blank out temp_pithouse_join to prepare for another run
  temp_skeletalsummary<-skeletalsummary
  temp_skeletalsummary$sampled <- NA_real_ 
  temp_skeletalsummary %>% mutate_if(is.factor, as.character) -> temp_skeletalsummary 
  
}#i for loop

#tidy loop
colnames(skeletalsummary2)[12] <- "RunID" #renames the column containing the "i" value to RunID

#remove sampled Values not in Middle Jomon
skeletalsummary_trim <- skeletalsummary2 %>%
  filter(sampled < -4400 & sampled >= -5400)

#Round values for aoristic analysis
sampleround <- floor(skeletalsummary2$sampled/100) * 100
skeletalsummaryround <- cbind(skeletalsummary2,sampleround)

#Round values for aoristic analysis
sampleround_trim <- floor(skeletalsummary_trim$sampled/100) * 100
skeletalsummary_trimround <- cbind(skeletalsummary_trim, sampleround_trim)

# save to disk to so we can reuse this:
saveRDS(skeletalsummary2, here::here("data/derived-data/skeletalsummary2.rds"))
saveRDS(skeletalsummary_trim, here::here("data/derived-data/skeletalsummary_trim.rds"))
saveRDS(skeletalsummaryround, here::here("data/derived-data/skeletalsummaryround.rds"))
saveRDS(skeletalsummary_trimround, here::here("data/derived-data/skeletalsummary_trimround.rds"))