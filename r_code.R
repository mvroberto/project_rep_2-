
download_file <- function(){
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  file <- download.file(url, destfile = "./StormData.csv.bz2", method = "curl")
  
  df <- read.csv(bzfile("stormdata.csv.bz2"))
  
  return(df)
}