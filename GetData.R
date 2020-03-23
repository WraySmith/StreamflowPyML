rm(list = ls())
# Load Packages
library(tidyhydat)
library(weathercan)
library(dplyr)
library(formattable)
library(lubridate)
library(rlang)

# Load R script for a hydrometri station search
# You will need to modify the path as required
source("./Hydat_Search.R")

# Load list of Hydat database stations from the tidyhydat package
# Search for the Stave River Station which is the station we'll be assessing
Hydat_List <- tidyhydat::allstations
Stave_List <- dplyr::pull((tidyhydat::search_stn_name("Stave River")[,1]), STATION_NUMBER)
Stave_List

# We can see that there are two stations with the name Stave River
# Look for the station record lengths to select which station to use for the study
Stave_Summary <- tidyhydat::hy_stn_data_range(Stave_List) %>% 
        filter(DATA_TYPE == "Q")
formattable(Stave_Summary)

## "08MH147" has the longer and more current record and will therefore be used for the study
# Extract station coordinates and station start and end year
Stave_Coords <- filter(Hydat_List, STATION_NUMBER == "08MH147") %>% 
        select(LATITUDE, LONGITUDE)
Stave_Coords <- c(as.numeric(Stave_Coords[1,1]), as.numeric(Stave_Coords[1,2]))
Stave_Start_Year <- Stave_Summary$Year_from[2]
Stave_End_Year <- Stave_Summary$Year_to[2]

# Complete a 100km radius hydromet search using Hydat_Search.R function
Streams_Proximity <- hydat_loc_search(Stave_Coords, dist = 100)
formattable(Streams_Proximity)

# Pull the station numbers for the hydromet radius search to pass into the record length function
Streams_Numbers <- dplyr::pull(Streams_Proximity, STATION_NUMBER)
Streams_Range <- tidyhydat::hy_stn_data_range(Streams_Numbers) %>% 
        filter(DATA_TYPE == "Q")
formattable(Streams_Range)

# Filter stations if they contain the Stave data range
Streams_Range <- Streams_Range[which(Streams_Range$Year_from <= Stave_Start_Year &
                                             Streams_Range$Year_to >= Stave_End_Year),]
Streams_Count <- nrow(Streams_Range)


# Complete a 100km radius climate station search using the weathercan package
Climate_Numbers <- weathercan::stations_search(coords = Stave_Coords, 
                                             dist = 100, interval = "day")
formattable(Climate_Numbers)

# Filter stations if they contain the Stave station data range
Climate_Numbers <- Climate_Numbers[which(Climate_Numbers$start <= Stave_Start_Year &
                                                 Climate_Numbers$end >= Stave_End_Year),]
formattable(Climate_Numbers)
Climate_Count <- nrow(Climate_Numbers)

# Extract Stave Station Data
Output <- tidyhydat::hy_daily_flows(station_number = "08MH147") %>%
        filter(Parameter == "Flow") %>%
        mutate(DayofYear = yday(Date)) %>%
        select(Date, DayofYear, "08MH147" = Value)

# Add hydromet stations in radius
for (i in 1:Streams_Count) {
        temp_station <- Streams_Range[i, 1]
        if (!(temp_station == "08MH147")) {
                temp <- tidyhydat::hy_daily_flows(station_number = temp_station) %>%
                        filter(Parameter == "Flow") %>%
                        select(Date, Value)

                Output <- left_join(Output, temp, by = "Date")
                Output <- rename(Output, !!paste(temp_station) := Value)
        }
}
Output1 <- Output

# Add climate stations in radius
for (i in 1:Climate_Count) {
        temp_station <- Climate_Numbers[i, 3]
        temp <- weathercan::weather_dl(station_ids = temp_station, 
                                       interval = "day",
                                       start = ymd(paste0(Stave_Start_Year,"0101")),
                                       end = ymd(paste0(Stave_End_Year,"1231"))) %>%
                select(date, min_temp, max_temp, total_precip)
        
        temp <- rename(temp, Date = date, 
               !!paste0(temp_station,"_mint") := min_temp,
               !!paste0(temp_station,"_maxt") := max_temp,
               !!paste0(temp_station,"_precip") := total_precip)
        
        Output1 <- left_join(Output1, temp, by = "Date")
}

##### Save Dataset
write.csv(Output1, file = "./Dataset.csv", row.names = FALSE)
Dataset <- read.csv(file = "./Dataset.csv")

