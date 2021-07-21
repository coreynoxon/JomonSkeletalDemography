df<- read_csv("df.csv", na = c("#VALUE!", "#N/A"))
df1<- read_csv("df1.csv", na = c("#VALUE!", "#N/A"))
df$sampled <- NA_real_ 
for (i in 1:100){ #determines how many iterations to run
  
  row_list<-as.list(1:nrow(df))
  q<-0
  
  while(length(row_list)!=0 & q<10){
    q<-q+1 #to make sure that we don't spinning off in an infinite loop
    for(j in row_list){ #this loop replaces the check values
      skip_flag<-FALSE #initialize skip flag used to check the replacement sampling
      for(k in 2:6){ #checking the after columns
        if(is.na(df[j,k])){ 
          print("NA break")
          print(i)
          break
        } else if(is.na(as.integer(df[j,k]))==FALSE) { #if it's already an integer, well, a character vector containing an integer, we already did this, next
          print("integer next")
          next
          print("integer next")
        } else if(df[j,k]==""){ #check for blank values
          print("empty string next")
          df[j,k]<-NA #if blank value found, replace with NA
          print("fixed blank to NA")
          next 
        }
        else if(is.na(filter(df,ID==as.character(df[j,k]))["sampled"])) { #if the replacement has not yet been generated, move on, but set flag to jump this to the end
          skip_flag<-TRUE
          print("skip flag set")
        } else {
          df[j,k]<-as.integer(filter(df,ID==df[j,k])[7]) #replacing IDs with the sampled dates of those IDs
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
        df[j,7]<-sample(as.numeric(names(df1[,2:15])),1,
                                         replace=FALSE,
                                         filter(df1,ID==as.character(df[j,1]))[2:15])
        df[j,8]<-i #identifying the run number
        
        if(any(as.numeric(df[j,2:6])>as.numeric(df[j,7]),na.rm=TRUE)){
          print(j)
          while(any(as.numeric(df[j,2:6])>as.numeric(df[j,7]),na.rm=TRUE)){
            df[j,7]<-sample(as.numeric(names(df1[,2:15])),1,replace=TRUE,filter(df1,ID==as.character(df[j,1]))[2:15])
          } #while 
          df[j,8]=i 
        }#if
      }
    } #j for loop
  } #while loop wrapper around j loop
  if(i==1){
    df2<-df
  }else{
    df2<-rbind(df2,df)
  }#else
  
  #blank out df to prepare for another run
  df<-df3
  df$sampled <- NA_real_ 
  df %>% mutate_if(is.factor, as.character) -> df 
  
}#i for loop



df[j,1]<-sample(as.numeric(names(df1[,2:15])),1,replace=FALSE,filter(df1,ID==as.character(df[j,1]))[2:15])



if(any(as.numeric(df3[1,2:6])>as.numeric(df3[1,7]),na.rm=TRUE))print("ok") else print("not ok")

df-temp_pithouse_join
df1 - mcprepdf1

require(devtools)
install_version("ggpubr", version = "0.2.5", repos = "http://cran.us.r-project.org")
install_version("magrittr", version = "1.5", repos = "http://cran.us.r-project.org")
install_version("ggthemes", version = "4.2.0", repos = "http://cran.us.r-project.org")
install_version("forcats", version = "0.4.0", repos = "http://cran.us.r-project.org")
install_version("stringr", version = "1.4.0", repos = "http://cran.us.r-project.org")
install_version("dplyr", version = "0.8.3", repos = "http://cran.us.r-project.org")
install_version("purrr", version = "0.3.3", repos = "http://cran.us.r-project.org")
install_version("readr", version = "1.3.1", repos = "http://cran.us.r-project.org")
install_version("tidyr", version = "1.0.0", repos = "http://cran.us.r-project.org")
install_version("tibble", version = "2.1.3", repos = "http://cran.us.r-project.org")
install_version("ggplot2", version = "3.2.1", repos = "http://cran.us.r-project.org")
install_version("tidyverse", version = "1.3.0", repos = "http://cran.us.r-project.org")


install_version("Rcpp", version = "1.0.2", repos = "http://cran.us.r-project.org")
install_version("cellranger", version = "1.1.0", repos = "http://cran.us.r-project.org")
install_version("pillar", version = "1.4.2", repos = "http://cran.us.r-project.org")
install_version("compiler", version = "3.6.0", repos = "http://cran.us.r-project.org")
install_version("dbplyr", version = "1.4.2", repos = "http://cran.us.r-project.org")
install_version("tools", version = "3.6.0", repos = "http://cran.us.r-project.org")
install_version("lubridate", version = "1.7.4", repos = "http://cran.us.r-project.org")
install_version("jsonlite", version = "1.6", repos = "http://cran.us.r-project.org")
install_version("lifecycle", version = "0.1.0", repos = "http://cran.us.r-project.org")
install_version("nlme", version = "3.1-139", repos = "http://cran.us.r-project.org")
install_version("gtable", version = "0.3.0", repos = "http://cran.us.r-project.org")
install_version("lattice", version = "0.20-38", repos = "http://cran.us.r-project.org")
install_version("pkgconfig", version = "2.0.3", repos = "http://cran.us.r-project.org")
install_version("rlang", version = "0.4.5", repos = "http://cran.us.r-project.org")
install_version("reprex", version = "0.3.0", repos = "http://cran.us.r-project.org")
install_version("cli", version = "1.1.0", repos = "http://cran.us.r-project.org")
install_version("DBI", version = "1.0.0", repos = "http://cran.us.r-project.org")
install_version("rstudioapi", version = "0.10", repos = "http://cran.us.r-project.org")
install_version("haven", version = "2.2.0", repos = "http://cran.us.r-project.org")
install_version("xfun", version = "0.10", repos = "http://cran.us.r-project.org")
install_version("withr", version = "2.1.2", repos = "http://cran.us.r-project.org")
install_version("xml2", version = "1.2.2", repos = "http://cran.us.r-project.org")
install_version("httr", version = "1.4.1", repos = "http://cran.us.r-project.org")
install_version("knitr", version = "1.25", repos = "http://cran.us.r-project.org")
install_version("fs", version = "1.3.1", repos = "http://cran.us.r-project.org")
install_version("generics", version = "0.0.2", repos = "http://cran.us.r-project.org")
install_version("vctrs", version = "0.2.4", repos = "http://cran.us.r-project.org")
install_version("hms", version = "0.5.3", repos = "http://cran.us.r-project.org")
install_version("grid", version = "3.6.0", repos = "http://cran.us.r-project.org")
install_version("tidyselect", version = "0.2.5", repos = "http://cran.us.r-project.org")
install_version("glue", version = "1.3.1", repos = "http://cran.us.r-project.org")
install_version("R6", version = "2.4.0", repos = "http://cran.us.r-project.org")
install_version("readxl", version = "1.3.1", repos = "http://cran.us.r-project.org")
install_version("modelr", version = "0.1.5", repos = "http://cran.us.r-project.org")
install_version("ellipsis", version = "0.3.0", repos = "http://cran.us.r-project.org")
install_version("backports", version = "1.1.5", repos = "http://cran.us.r-project.org")
install_version("scales", version = "1.0.0", repos = "http://cran.us.r-project.org")
install_version("rvest", version = "0.3.5", repos = "http://cran.us.r-project.org")
install_version("assertthat", version = "0.2.1", repos = "http://cran.us.r-project.org")
install_version("colorspace", version = "1.4-1", repos = "http://cran.us.r-project.org")
install_version("ggsignif", version = "0.6.0", repos = "http://cran.us.r-project.org")
install_version("stringi", version = "1.4.3", repos = "http://cran.us.r-project.org")
install_version("lazyeval", version = "0.2.2", repos = "http://cran.us.r-project.org")
install_version("munsell", version = "0.5.0", repos = "http://cran.us.r-project.org")
install_version("broom", version = "0.5.2", repos = "http://cran.us.r-project.org")
install_version("crayon", version = "1.3.4", repos = "http://cran.us.r-project.org")










