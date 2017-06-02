
library(dplyr)
download_file <- function(){
  url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  if(!file.exists("StormData.csv.bz2")){
    
    file <- download.file(url, destfile = "./StormData.csv.bz2", method = "curl")
    return(file)
  }
}
  
read_file <- function(){
  
    df <- read.csv(bzfile("StormData.csv.bz2"))
  
    return(df)
}

df <- df

df_variables <- select(df,BGN_DATE, EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP) 

df_variables$BGN_DATE <- as.character(df_variables$BGN_DATE)
df_variables$BGN_DATE <- as.Date(df_variables$BGN_DATE,format =  "%m/%d/%Y")

df_dated <- filter(df_variables, BGN_DATE >= "1996-01-01") 



############## Need to arrange PROPDMGEXP CROPDMGEXP and then sum
#####group data

df_grouped <- group_by(df_dated, EVTYPE) %>% summarize(freq = length(EVTYPE),
                                                       FATALITIES = sum(FATALITIES),
                                                       INJURIES = sum(INJURIES)
                                                       
                                                       ) %>% arrange(desc(freq))