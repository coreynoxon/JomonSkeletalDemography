dfsource <- read_csv("other-things/Loop Example DF.csv", na = c("#VALUE!", "#N/A"))

df <- dfsource
temp_df<-df #temp_pithouse_join used for dynamically created samples
temp_df$sampled <- NA #blanking out the sample column so I can check against NA for the dynamic detereminatination.
temp_df %>% mutate_if(is.factor, as.character) -> temp_df #change factors to characters

for (i in 1:100){ #determines how many iterations to run
  
  row_list<-as.list(1:nrow(temp_df))
  q<-0
  
  while(length(row_list)!=0 & q<10){
    q<-q+1 #to make sure that we don't spinning off in an infinite loop
    for(j in row_list){ #this loop replaces the check values
      skip_flag<-FALSE #initialize skip flag used to check the replacement sampling
      for(k in 4:5){ #checking the topoafter columns
        if(is.na(temp_df[j,k])){ 
          # print("NA break")
          # print(i)
          break
        } else if(is.na(as.integer(temp_df[j,k]))==FALSE) { #if it's already an integer, well, a character vector containing an integer, we already did this, next
          # print("integer next")
          next
          # print("integer next")
        } else if(temp_df[j,k]==""){ #check for blank values
          # print("empty string next")
          temp_df[j,k]<-NA #if blank value found, replace with NA
          # print("fixed blank to NA")
          next 
        }
        else if(is.na(filter(temp_df,ID==as.character(temp_df[j,k]))["sampled"])) { #if the replacement has not yet been generated, move on, but set flag to jump this to the end
          skip_flag<-TRUE
          # print("skip flag set")
        } else {
          temp_df[j,k]<-as.integer(filter(temp_df,ID==temp_df[j,k])[6]) #replacing IDs with the sampled dates of those IDs
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
        temp_df[j,6]<-mapply(function(x, y) if(any(is.na(x) || is.na(y))) NA else 
          sample(seq(x, y, na.rm = TRUE), 1), temp_df[j,"Start"], temp_df[j,"End"])
        temp_df[j,7]<-i #identifying the run number
        
        if(any(as.numeric(temp_df[j,4:5])>as.numeric(temp_df[j,6]),na.rm=TRUE)){
          # print(j)
          while(any(as.numeric(temp_df[j,4:5])>as.numeric(temp_df[j,6]),na.rm=TRUE)){
            temp_df[j,6]<-mapply(function(x, y) if(any(is.na(x) || is.na(y))) NA else 
              sample(seq(x, y, na.rm = TRUE), 1), temp_df[j,"Start"], temp_df[j,"End"])
          } #while 
          temp_df[j,7]=i 
        }#if
      }
    } #j for loop
  } #while loop wrapper around j loop
  if(i==1){
    df2<-temp_df
  }else{
    df2<-rbind(df2,temp_df)
  }#else
  
  #blank out temp_df to prepare for another run
  temp_df<-df
  temp_df$sampled <- NA 
  temp_df %>% mutate_if(is.factor, as.character) -> temp_df 
  
}#i for loop