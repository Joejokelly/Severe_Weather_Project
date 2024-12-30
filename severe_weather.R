library(scales)
library(tidyverse)
library(lubridate)

library(knitr)

# Download data

if(!file.exists("repdata_data_StormData.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                "repdata_data_StormData.csv.bz2") }

# Read data

noaa <- read.csv("repdata_data_StormData.csv.bz2",
                 stringsAsFactors = F,
                 sep = ",",
                 strip.white = T,
                 na.strings = "")



noaa.subset <- select(noaa,
                      EVTYPE,
                      BGN_DATE,
                      FATALITIES,
                      INJURIES,
                      PROPDMG,
                      PROPDMGEXP,
                      CROPDMG,
                      CROPDMGEXP)
  
  # Filter weather events from 1996 onwards
  
  mutate(BGN_DATE = mdy_hms(BGN_DATE)) 
  filter(year(BGN_DATE) >= 1996)

# Permittted storm events as listed in data documentation

permitted.storm.events <- toupper(c("Astronomical Low Tide",    "Avalanche",
                                    "Blizzard",                 "Coastal Flood",
                                    "Cold/Wind Chill",          "Debris Flow",
                                    "Dense Fog",                "Dense Smoke",
                                    "Drought",                  "Dust Devil",
                                    "Dust Storm",               "Excessive Heat",
                                    "Extreme Cold/Wind Chill",  "Flash Flood",
                                    "Flood",                    "Frost/Freeze",
                                    "Funnel Cloud",             "Freezing Fog",
                                    "Hail",                     "Heat",
                                    "Heavy Rain",               "Heavy Snow",
                                    "High Surf",                "High Wind",
                                    "Hurricane (Typhoon)",      "Ice Storm",
                                    "Lake-Effect Snow",         "Lakeshore Flood",
                                    "Lightning",                "Marine Hail",
                                    "Marine High Wind",         "Marine Strong Wind",
                                    "Marine Thunderstorm Wind", "Rip Current",
                                    "Seiche",                   "Sleet",
                                    "Storm Surge/Tide",         "Strong Wind",
                                    "Thunderstorm Wind",        "Tornado",
                                    "Tropical Depression",      "Tropical Storm",
                                    "Tsunami",                  "Volcanic Ash",
                                    "Waterspout",               "Wildfire",
                                    "Winter Storm",             "Winter Weather"))

# Split data based on allowed and not allowed event types

noaa.subset.event.allowed <- filter(noaa.subset,  EVTYPE %in% permitted.storm.events)
noaa.subset.event.recode  <- filter(noaa.subset, !EVTYPE %in% permitted.storm.events)

# Recode event types with more than 100 occurances into allowed event types

noaa.subset.event.recode <- mutate(noaa.subset.event.recode,
                                   EVTYPE = case_when(
                                     grepl("MARINE",              EVTYPE) ~ "MARINE THUNDERSTORM WIND",
                                     grepl("TSTM|THUNDE",         EVTYPE) ~ "THUNDERSTORM",
                                     grepl("FIRE",                EVTYPE) ~ "WILDFIRE",
                                     grepl("COASTAL",             EVTYPE) ~ "COASTAL FLOOD",
                                     grepl("FLD|FLOOD",           EVTYPE) ~ "FLOOD",
                                     grepl("WINTER|SNOW",         EVTYPE) ~ "WINTER WEATHER",
                                     grepl("COLD|CHILL",          EVTYPE) ~ "COLD/WIND CHILL",
                                     grepl("FOG",                 EVTYPE) ~ "DENSE FOG",
                                     grepl("HURRIC|TYPHOO",       EVTYPE) ~ "HURRICANE (TYPHOON)",
                                     grepl("WARM|HEAT|HOT",       EVTYPE) ~ "HEAT",
                                     grepl("WIND",                EVTYPE) ~ "STRONG WIND",
                                     grepl("RIP ",                EVTYPE) ~ "RIP CURRENT",
                                     grepl("SURGE",               EVTYPE) ~ "STORM SURGE/TIDE",
                                     grepl("SURF",                EVTYPE) ~ "HIGH SURF",
                                     grepl("BLIZZ",               EVTYPE) ~ "BLIZZARD",
                                     grepl("FROST|FREEZ|ICY|ICE", EVTYPE) ~ "FROST/FREEZE",
                                     TRUE ~ "OTHER"))

# Join data frames

noaa.subset <- rbind(noaa.subset.event.allowed, noaa.subset.event.recode)

noaa.subset <- mutate(noaa.subset,
                      
                      # Capitalize multiplier codes
                      
                      PROPDMGEXP = toupper(PROPDMGEXP),
                      CROPDMGEXP = toupper(CROPDMGEXP),
                      
                      # Multiply base damage with appropriate multiplier
                      
                      PROPDMG.TOTAL = case_when(
                        PROPDMGEXP == "H" ~ PROPDMG * 1e+02,         # Hundreds
                        PROPDMGEXP == "K" ~ PROPDMG * 1e+03,         # Thousands
                        PROPDMGEXP == "M" ~ PROPDMG * 1e+06,         # Millions
                        PROPDMGEXP == "B" ~ PROPDMG * 1e+09,         # Billions
                        grepl("[0-9]", PROPDMGEXP) ~ PROPDMG * 10^as.numeric(PROPDMGEXP),
                        TRUE ~ PROPDMG),
                      CROPDMG.TOTAL = case_when(
                        CROPDMGEXP == "H" ~ CROPDMG * 1e+02,         # Hundreds
                        CROPDMGEXP == "K" ~ CROPDMG * 1e+03,         # Thousand
                        CROPDMGEXP == "M" ~ CROPDMG * 1e+06,         # Millions
                        CROPDMGEXP == "B" ~ CROPDMG * 1e+09,         # Billions
                        grepl("[0-9]", CROPDMGEXP) ~ PROPDMG * 10^as.numeric(CROPDMGEXP),
                        TRUE ~ CROPDMG))


results.fatalities <- group_by(noaa.subset, EVTYPE) 
  summarise(FATALITIES = sum(FATALITIES))
  top_n(10, FATALITIES)

ggplot(results.fatalities, aes(reorder(EVTYPE, FATALITIES), FATALITIES)) +
  geom_col(fill = "darkolivegreen4") +
  coord_flip() +
  labs(y = "Fatalities",
       x = "Severe Weather",
       title = "Figure 1. Fatalities in the US from severe weather events from 1996-2011") +
  scale_y_continuous(labels = comma) +
  theme_light()+
  theme(axis.title.y = element_blank(),
        plot.title   = element_text(size = 12,
                                    face = "bold"))

# Calculate total injuries by event type

results.injuries <- group_by(noaa.subset, EVTYPE)
  summarise(INJURIES = sum(INJURIES))
  top_n(10, INJURIES)

ggplot(results.injuries, aes(reorder(EVTYPE, INJURIES), INJURIES)) +
  geom_col(fill = "skyblue4") +
  coord_flip() +
  labs(y = "Injuries",
       x = "Severe Weather",
       title = "Figure 2. Injuries in the US from severe weather events from 1996-2011") +
  scale_y_continuous(labels = comma) +
  theme_light() +
  theme(axis.title.y = element_blank(),
        plot.title   = element_text(size = 12,
                                    face = "bold"))


results.economic <- group_by(noaa.subset, EVTYPE)
  summarise(TOTAL.PROP.DMG = sum(PROPDMG.TOTAL),
            TOTAL.CROP.DMG = sum(CROPDMG.TOTAL))
  mutate(TOTAL.DMG = TOTAL.PROP.DMG + TOTAL.CROP.DMG) 
  top_n(10, TOTAL.DMG)

# Prepare data for plotting and scale to billions

results.economic <- select(results.economic, -TOTAL.DMG) 
  gather("DMG.TYPE", "DMG.DOLLARS.BILLION", -EVTYPE) 
  mutate(DMG.DOLLARS.BILLION = round(DMG.DOLLARS.BILLION / 1e+09, 2),
         DMG.TYPE = recode_factor(DMG.TYPE,
                                  TOTAL.CROP.DMG = "Crop",
                                  TOTAL.PROP.DMG = "Property"))

ggplot(results.economic, aes(reorder(EVTYPE, DMG.DOLLARS.BILLION),
                             DMG.DOLLARS.BILLION, fill = DMG.TYPE)) +
  geom_col() +
  coord_flip() +
  labs(y = "Billions of Dollars",
       x = "Severe Weather",
       fill = "Damage",
       title = "Figure 3. Damages in the US from severe weather events from 1996-2011") +
  guides(fill = guide_legend(reverse=T)) +
  scale_y_continuous(labels = comma) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.title.y    = element_blank(),
        plot.title      = element_text(size = 12,
                                       face = "bold"))

