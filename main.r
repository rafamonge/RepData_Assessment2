library(dplyr)
library(stringdist)
library(ggplot2)

file <- "repdata_data_StormData.csv.bz2"
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists(file)){
  download.file(url)
}
stormData <- read.csv(file, stringsAsFactors = FALSE)
stormData <-tbl_df(stormData)

vectorMultiplier <- list(K = 1000, M= 1000000,B=1000000000, H=100,"+" = 100, "-" = 1, "?" = 1, "0" = 1)
ValidEventTypes <- toupper(c('Astronomical Low Tide ','Avalanche ','Blizzard ','Coastal Flood','Cold/Wind Chill ','Debris Flow','Dense Fog','Dense Smoke ','Drought ','Dust Devil ','Dust Storm ','cessive Heat ','Extreme Cold/Wind Chill','Flash Flood','Flood ','Freezing Fog','Frost/Freeze','Funnel Cloud','Hail ','Heat ','Heavy Rain','Heavy Snow','High Surf','High Wind','Hurricane/Typhoon','Ice Storm ','Lakeshore Flood','Lake-Effect Snow','Lightning ','Marine Hail ','Marine High Wind ','Marine Strong Wind','Marine Thunderstorm Wind','Rip Current ','Seiche ','Sleet ','Storm Surge/Tide','Strong Wind','Thunderstorm Wind','Tornado ','Tropical Depression ','Tropical Storm','Tsunami ','Volcanic Ash','Waterspout ','Wildfire ','Winter Storm ','Winter Weather'))


ExpTransform <- function(value, exp){
  if(exp == ""){
    1000*value
  }
  else if(exists(exp,where=vectorMultiplier))
    value *vectorMultiplier[[exp]]
  
  else
    value *as.numeric(exp)
}

ExpTransformVectorized <-  Vectorize(ExpTransform, vectorize.args = c("value", "exp"))
TransformedEventType<- function(x){
  
  index<- which.min(stringdist(x, ValidEventTypes, method ="lcs"))
  ValidEventTypes[index]
}
VectorizedTransformedEventType <- Vectorize(TransformedEventType, vectorize.args = c("x"))


VectorizedPopulationDamage <- Vectorize(PopulationDamage, vectorize.args = c("injuries", "fatalities"))


stormDataTrim <- stormData %>%
  select (BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
  arrange(EVTYPE) %>%
  mutate (PROPDMGEXP = toupper(PROPDMGEXP)
          ,CROPDMGEXP= toupper(CROPDMGEXP)
          ,EVTYPE= toupper(EVTYPE)
          ,PropertyDamage = ExpTransformVectorized(PROPDMG, PROPDMGEXP)
          ,CropDamage = ExpTransformVectorized(CROPDMG, CROPDMGEXP)
          ,EventType = VectorizedTransformedEventType(EVTYPE)
  ) %>%
  group_by(EventType) %>%
  summarize(INJURIES = sum(INJURIES)
            , FATALITIES=sum(FATALITIES)
            , CropDamage=sum(CropDamage)
            , PropertyDamage=sum(PropertyDamage)) %>%
  mutate(PopulationCost = INJURIES + FATALITIES *5
         , MonetaryCost = CropDamage  + PropertyDamage
         , PopulationFactor = reorder(EventType,PopulationCost )
         , MonetaryFactor = reorder(EventType,MonetaryCost )) %>%
  select (EventType,PopulationCost, MonetaryCost,PopulationFactor,MonetaryFactor )
  
View(stormDataTrim)
stormDataTrim %>% top_n(5,PopulationCost) %>%
ggplot(aes(PopulationFactor, PopulationCost))+
  geom_bar(stat = "identity") +
  xlab("Event Type") +
  ylab("Population Cost (Injuries + Fatalities * 5) ") +
  ggtitle("Population Cost by Event Type (top 5 event types)")

stormDataTrim %>% top_n(5,MonetaryCost) %>%
  ggplot(aes(MonetaryFactor, MonetaryCost))+
  geom_bar(stat = "identity") +
  xlab("Event Type") +
  ylab("Monetary Cost ") +
  ggtitle("Monetary Cost by Event Type (top 5 event types)")
