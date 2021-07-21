#03-Aoristic Analysis and MCMC

#aoristic analysis
#pithouse
tokyo_adata_test<-time2aoristic(data=pithouse,timeData=potphase,resolution=100,minDate=-5415,maxDate=-4050,colphase=14)
tokyo_mcprep <- tokyo_adata_test[,c(2,40:53)] #trim time2aoristic output and prep for further analysis

#skeletal
skeletal_aoristic<-time2aoristic(data=skeletalsummary,timeData=skeletaltimephase,resolution=100,minDate=-15515,maxDate=-2385,colphase=3)
skeletal_mcprep <- skeletal_aoristic[,c(2,12:143)] #trim time2aoristic output and prep for further analysis


#MCMC and subsequent clean up

#pithouse
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
          # print("NA break")
          # print(i)
          break
        } else if(is.na(as.integer(temp_pithouse_join[j,k]))==FALSE) { #if it's already an integer, well, a character vector containing an integer, we already did this, next
          # print("integer next")
          next
          # print("integer next")
        } else if(temp_pithouse_join[j,k]==""){ #check for blank values
          # print("empty string next")
          temp_pithouse_join[j,k]<-NA #if blank value found, replace with NA
          # print("fixed blank to NA")
          next 
        }
        else if(is.na(filter(temp_pithouse_join,PithouseID==as.character(temp_pithouse_join[j,k]))["sampled"])) { #if the replacement has not yet been generated, move on, but set flag to jump this to the end
          skip_flag<-TRUE
          # print("skip flag set")
        } else {
          temp_pithouse_join[j,k]<-as.integer(filter(temp_pithouse_join,PithouseID==temp_pithouse_join[j,k])[37]) #replacing pithouse IDs with the sampled dates of those IDs
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
        temp_pithouse_join[j,37]<-sample(as.numeric(names(tokyo_mcprep[,2:15])),1,
                                         replace=FALSE,
                                         filter(tokyo_mcprep,PithouseID==as.character(temp_pithouse_join[j,1]))[2:15])
        temp_pithouse_join[j,38]<-i #identifying the run number
        
        if(any(as.numeric(temp_pithouse_join[j,15:27])>as.numeric(temp_pithouse_join[j,37]),na.rm=TRUE)){
          # print(j)
          while(any(as.numeric(temp_pithouse_join[j,15:27])>as.numeric(temp_pithouse_join[j,37]),na.rm=TRUE)){
            temp_pithouse_join[j,37]<-sample(as.numeric(names(tokyo_mcprep[,2:15])),1,replace=TRUE,filter(tokyo_mcprep,PithouseID==temp_pithouse_join[j,1])[2:15])
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
  temp_pithouse_join$sampled <- NA 
  temp_pithouse_join %>% mutate_if(is.factor, as.character) -> temp_pithouse_join 
  
}#i for loop

#tidy loop
colnames(tokyo_pithouse_join2)[38] <- "RunID" #renames the column containing the "i" value to RunID

#remove sampled Values not in Middle Jomon
tokyo_pithouse_join_trim <- tokyo_pithouse_join2 %>%
  filter(sampled < -4415 & sampled >= -5415)

#create pithouse to posthole size ratios for later analysis
ratio_prep <- tokyo_pithouse_join_trim %>%
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

# save to disk to so we can reuse this:
saveRDS(tokyo_pithouse_join, here::here("data/derived-data/tokyo_pithouse_join.rds"))
saveRDS(tokyo_pithouse_join_trim, here::here("data/derived-data/tokyo_pithouse_join_trim.rds"))
saveRDS(ratio_prep, here::here("data/derived-data/ratio_prep.rds"))

#skeletal
#MCMC
# 1000 iteration loop

for (i in 1:2000){ #determines how many iterations to run
  
  row_list<-as.list(1:nrow(temp_skeletalsummary))
  q<-0
  
  while(length(row_list)!=0 & q<10){
    q<-q+1 #to make sure that we don't spinning off in an infinite loop
    for(j in row_list){ #this loop replaces the check values
      #      skip_flag<-FALSE #initialize skip flag used to check the replacement sampling
      #      for(k in 15:27){ #checking the topoafter columns
      #        if(is.na(temp_pithouse_join[j,k])){ 
      #          print("NA break")
      #          print(i)
      #          break
      #        } else if(is.na(as.integer(temp_pithouse_join[j,k]))==FALSE) { #if it's already an integer, well, a character vector containing an integer, we already did this, next
      #          print("integer next")
      #          next
      #          print("integer next")
      #        } else if(temp_pithouse_join[j,k]==""){ #check for blank values
      #          print("empty string next")
      #          temp_pithouse_join[j,k]<-NA_real_ #if blank value found, replace with NA
      #          print("fixed blank to NA")
      #          next 
      #        }
      #        else if(is.na(filter(temp_pithouse_join,PithouseID==as.character(temp_pithouse_join[j,k]))["sampled"])) { #if the replacement has not yet been generated, move on, but set flag to jump this to the end
      #          skip_flag<-TRUE
      #          print("skip flag set")
      #        } else {
      #          temp_pithouse_join[j,k]<-as.integer(filter(temp_pithouse_join,PithouseID==temp_pithouse_join[j,k])[37]) #replacing pithouse IDs with the sampled dates of those IDs
      #          print("successful check value grab")
      #        } #if-else
      #      } #k for loop
      #      if(skip_flag==FALSE){
      #        row_list<-row_list[row_list!=j]
      #      } else {
      #        next 
      #      }
      #      
      #sampling section
      if(is.na(temp_skeletalsummary[j,10])){
        temp_skeletalsummary[j,10]<-sample(as.numeric(names(skeletal_mcprep[,13:133])),1,
                                           replace=FALSE,
                                           filter(skeletal_mcprep,UniqueID==as.character(temp_skeletalsummary[j,1]))[13:133])
        temp_skeletalsummary[j,11]<-i #identifying the run number
        
        #        if(any(as.numeric(temp_pithouse_join[j,15:27])>as.numeric(temp_pithouse_join[j,37]),na.rm=TRUE)){
        #          print(j)
        #          while(any(as.numeric(temp_pithouse_join[j,15:27])>as.numeric(temp_pithouse_join[j,37]),na.rm=TRUE)){
        #            temp_pithouse_join[j,37]<-sample(as.numeric(names(tokyo_mcprep[,2:15])),1,
        #                                             replace=TRUE,filter(tokyo_mcprep,PithouseID==temp_pithouse_join[j,1])[2:15])
        #          } #while 
        #          temp_pithouse_join[j,38]=i 
        #        }#if
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
colnames(skeletalsummary2)[11] <- "RunID" #renames the column containing the "i" value to RunID

#remove sampled Values not in Middle Jomon
skeletalsummary_trim <- skeletalsummary2 %>%
  filter(sampled < -4415 & sampled >= -5415)

# save to disk to so we can reuse this:
saveRDS(skeletalsummary2, here::here("data/derived-data/skeletalsummary2.rds"))
saveRDS(skeletalsummary_trim, here::here("data/derived-data/skeletalsummary_trim.rds"))
