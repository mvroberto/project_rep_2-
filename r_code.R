
library(dplyr)
download_file <- function(){
  url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  if(!file.exists("StormData.csv.bz2")){
    
    file <- download.file(url, destfile = "./StormData.csv.bz2", method = "curl")
    return(file)
  }
}
  
read_file <- function(){
  
    df <- read.csv(bzfile("StormData.csv.bz2"), strip.white=TRUE)
    df
}

df <- df

df_variables <- select(df,BGN_DATE, EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)

df_variables$BGN_DATE <- as.character(df_variables$BGN_DATE)
df_variables$BGN_DATE <- as.Date(df_variables$BGN_DATE,format =  "%m/%d/%Y")

df_dated <- filter(df_variables, BGN_DATE >= "1996-01-01")



######################
############### Exponetials

K <- 100000
M <-1000000
B <- 1000000000
zero <-  10
blanK <- 0



df_exponent_crop <- mutate(df_dated, cropValue = ifelse(CROPDMGEXP == "K",CROPDMG * K,
                                                  ifelse(CROPDMGEXP == "M",CROPDMG * M,
                                                  ifelse(CROPDMGEXP == "B",CROPDMG * B,     
                                                  ifelse(CROPDMGEXP == "0",CROPDMG * zero,0    
                                                          )))))

df_exponent_crop_property <- mutate(df_exponent_crop, propValue = ifelse(PROPDMGEXP == "K",PROPDMG * K,
                                                                 ifelse(PROPDMGEXP == "M",PROPDMG * M,
                                                                  ifelse(PROPDMGEXP == "B",PROPDMG * B,     
                                                                  ifelse(PROPDMGEXP == "0",PROPDMG * zero,0    
                                                                               )))))

############## Need to arrange PROPDMGEXP CROPDMGEXP and then sum
#####group data
# 
df_grouped <- group_by(df_exponent_crop_property, EVTYPE) %>% summarize(freq = length(EVTYPE),
                                                       FATALITIES = sum(FATALITIES),
                                                       INJURIES = sum(INJURIES),
                                                       TotalCropDamage = sum(cropValue),
                                                       TotalPropDamage = sum(propValue)
                                                       

                                                       ) %>% arrange(desc(freq)) %>% mutate(TotalMonetaryDamage = TotalCropDamage + TotalPropDamage)




#################
############### get event type

##### clean summary

df_grouped$EVTYPE <- toupper(df_grouped$EVTYPE)

summary_index <- grepl("SUMMARY.+",df_grouped$EVTYPE)
NOT_summary_names <-  df_grouped$EVTYPE[!summary_index]
df_grouped <- filter(df_grouped, EVTYPE %in% NOT_summary_names)


summary_index <- grepl("RECORD+.",df_grouped$EVTYPE)
NOT_summary_names <-  df_grouped$EVTYPE[!summary_index]
df_grouped <- filter(df_grouped, EVTYPE %in% NOT_summary_names)



##########
########## analyize event names

read_events <- read.csv("event_names.csv", header = FALSE, strip.white=TRUE)
read_events <- toupper(read_events$V1)
names_event <- df_grouped$EVTYPE

 events_data_index <- which(read_events %in% names_event)
 events_in_data <- read_events[events_data_index]
 events_NOT_in_data <- read_events[-events_data_index]
############# Get events




# names_splitted <- unlist(strsplit(names_event," ", fixed = TRUE))
# names_splitted <- unlist(strsplit(names_event," ", fixed = TRUE))
# df_names <- arrange(as.data.frame(table(names_splitted)), desc(Freq))

df_grouped$EVTYPE <- as.factor(df_grouped$EVTYPE)

# df_grouped_types <- mutate(df_grouped, EventTypeCons = ifelse( EVTYPE %in% names[grep("*HAIL.*",df_grouped$EVTYPE)], "HAIL", "OTHER"),
#                            ifelse()
#                                                  )

df_grouped_types <- mutate(df_grouped, EventTypeCons_boolean = EVTYPE  %in%  read_events) 

i <- 1
df_grouped_types$EventTypeCons <- c(1:length(df_grouped_types$EVTYPE))
for(x in df_grouped_types$EVTYPE){
  # index <- which(read_events %in% names_event[i])
  # my_list[i] <- read_events[index]
  # 
  
  if(names_event[i] %in% read_events == TRUE ){
    index <- which(read_events %in% names_event[i])
    df_grouped_types$EventTypeCons[i] <- read_events[[index]]
  }
  else{
     df_grouped_types$EventTypeCons[i] <- "NO NAME"
     
   }
  i <- i + 1
}
  
# df_two <- group_by(df_grouped_types,EventTypeCons)  %>% summarize(freq = length(EVTYPE),
#                                                                   FATALITIES = sum(FATALITIES),
#                                                                   INJURIES = sum(INJURIES),
#                                                                   TotalCropDamage = sum(TotalCropDamage),
#                                                                   TotalPropDamage = sum(TotalPropDamage),
#                                                                   TotalMonetaryDamage = sum(TotalMonetaryDamage)
#                                                                   
#                                                                   ) %>% arrange(desc(freq))
#   
  
