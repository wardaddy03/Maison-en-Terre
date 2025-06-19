setwd("C:/Summer")

#DATA IMPORT
#library (readr)

install.packages("tidyverse")
library(tidyverse)
temp2024 <- read_csv("temperatures.csv", 
            col_types = cols(Time = col_character()))

#library(stringr)
temp2024 <- temp2024 %>%
  mutate(Time = str_trim(Time), Time = dmy_hm(Time)) 

#SMOOTHING
install.packages("zoo")
library(zoo)
temp2024 <- temp2024 %>%
  mutate(SM_tempsud = rollmean(temp_sud, k = 10, fill = NA, align = "center"))
temp2024 <- temp2024 %>%
  mutate(SM_tempnord = rollmean(temp_nord, k = 10, fill = NA, align = "center"))
temp2024 <- temp2024 %>%
  mutate(SM_tempe4000 = rollmean(temp_e4000, k = 10, fill = NA, align = "center"))
temp2024 <- temp2024 %>%
  mutate(SM_tempstation = rollmean(temp_station, k = 10, fill = NA, align = "center"))

#SUMMARY
resumen_sm <- temp2024 %>%
  mutate(Date = as_date(Time)) %>%
  group_by(Date) %>%
  summarise(
    max_sm_sud = max(SM_tempsud, na.rm = TRUE),
    min_sm_sud = min(SM_tempsud, na.rm = TRUE),
    max_sm_nord = max(SM_tempnord, na.rm = TRUE),
    min_sm_nord = min(SM_tempnord, na.rm = TRUE),
    max_sm_e4000 = max(SM_tempe4000, na.rm = TRUE),
    min_sm_e4000 = min(SM_tempe4000, na.rm = TRUE),
    max_sm_station = max(SM_tempstation, na.rm = TRUE),
    min_sm_station = min(SM_tempstation, na.rm = TRUE))

#AMPLITUD
daily_delta<-temp2024%>%
  mutate(Date=as_date(Time)) %>%
  group_by(Date)%>%
  summarise(
    amplitude_sud   = max(SM_tempsud,   na.rm = TRUE) - min(SM_tempsud,   na.rm = TRUE),
    amplitude_nord  = max(SM_tempnord,  na.rm = TRUE) - min(SM_tempnord,  na.rm = TRUE),
    amplitude_e4000 = max(SM_tempe4000, na.rm = TRUE) - min(SM_tempe4000, na.rm = TRUE),
    amplitude_station = max(SM_tempstation, na.rm = TRUE) - min(SM_tempstation, na.rm = TRUE)
  )

#EXPORT
#install.packages("writexl")
#library(writexl)
#write_xlsx(daily_delta, "daily_amplitude.xlsx")

#CANICULE DAYS
temp2024 <- temp2024 %>%
  mutate(hour = hour(Time)
  )

night_station<-filter(temp2024,hour>= 22 | hour<5) %>%
  select(Time,temp_station,hour)

night_station <- night_station %>%
  mutate(night_date = if_else(hour < 5,
      as.Date(Time) - 1,  
      as.Date(Time))      
    )

warm_nights <- night_station %>%
      group_by(night_date) %>%
      summarise(
        warm_nights = any(temp_station>21, na.rm = TRUE)  #we can also use all
      )
    
 

#plotdata
