
library(dplyr)
library(ggplot2)
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

K <- 1000
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
  
df_last <- group_by(df_grouped_types,EventTypeCons)  %>% summarize(
                                                                  FATALITIES = sum(FATALITIES),
                                                                   INJURIES = sum(INJURIES),
                                                                   TotalCropDamage = sum(TotalCropDamage),
                                                                   TotalPropDamage = sum(TotalPropDamage),
                                                                   TotalMonetaryDamage = sum(TotalMonetaryDamage)
                                                                   
                                                                   ) %>% arrange(desc(FATALITIES)) %>% filter(EventTypeCons != "NO NAME")  
  
check_table <- function(df){ #### USE      df_grouped_types AS PARAMETER
  
  df_names <- filter(df,EventTypeCons != "NO NAME")
  df_N0_names <- filter(df,EventTypeCons == "NO NAME")
  
  
  df_name_FATALITIES <-df_names$FATALITIES
  df_name_INJURIES <-df_names$INJURIES
  df_name_TotalCropDamage <-df_names$TotalCropDamage
  df_name_TotalPropDamage<-df_names$TotalPropDamage
  df_name_TotalMonetaryDamage<-df_names$TotalMonetaryDamage

  df_N0_names_FATALITIES <-df_N0_names$FATALITIES
  df_N0_names_INJURIES <-df_N0_names$INJURIES
  df_N0_names_TotalCropDamage <-df_N0_names$TotalCropDamage
  df_N0_names_TotalPropDamage<-df_N0_names$TotalPropDamage
  df_N0_names_TotalMonetaryDamage<-df_N0_names$TotalMonetaryDamage
  
  # ####### fatalities
   fatalities_sum <- sum(df_name_FATALITIES) + sum(df_N0_names_FATALITIES)
   fatalities_percentage <- c((sum(df_name_FATALITIES)/fatalities_sum)*100,(sum(df_N0_names_FATALITIES)/fatalities_sum)*100)
   
   
   # ####### injuries
   injuries_sum <- sum(df_name_INJURIES) + sum(df_N0_names_INJURIES)
   injuries_percentage <- c((sum(df_name_INJURIES)/injuries_sum)*100,(sum(df_N0_names_INJURIES)/injuries_sum)*100)
   
   
   # ####### totalCrop
   crop_sum <- sum(df_name_TotalCropDamage) + sum(df_N0_names_TotalCropDamage)
   crop_percentage <- c((sum(df_name_TotalCropDamage)/crop_sum)*100,(sum(df_N0_names_TotalCropDamage)/crop_sum)*100)
   
   
   # ####### totalProperty
   prop_sum <- sum(df_name_TotalPropDamage) + sum(df_N0_names_TotalPropDamage)
   prop_percentage <- c((sum(df_name_TotalPropDamage)/prop_sum)*100,(sum(df_N0_names_TotalPropDamage)/prop_sum)*100)
   
   
   # ####### totalMonetary
   monetary_sum <- sum(df_name_TotalMonetaryDamage) + sum(df_N0_names_TotalMonetaryDamage)
   monetary_percentage <- c((sum(df_name_TotalMonetaryDamage)/monetary_sum)*100,(sum(df_N0_names_TotalMonetaryDamage)/monetary_sum)*100)
   
   my_list <- list(fatalities_percentage,injuries_percentage,crop_percentage,prop_percentage,monetary_percentage)
   names(my_list) <- c("fatalities","injuries","totalCrop","totalProperty","totalMonetary")
   my_list
}

