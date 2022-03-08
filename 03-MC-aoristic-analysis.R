#03 - MC-aoristic-analysis

# 2000 iteration loop

for (i in 1:2000){ #specifies how many iterations to run
  
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