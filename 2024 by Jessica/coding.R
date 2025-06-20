setwd("C:/Summer/Maison-en-Terre/2024 by Jessica")

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

#SMOOTHING FOR 10 POINTS
temp2024 <- temp2024 %>%
  mutate(SM10_tempsud = rollmean(temp_sud, k = 10, fill = NA, align = "center"))
temp2024 <- temp2024 %>%
  mutate(SM10_tempnord = rollmean(temp_nord, k = 10, fill = NA, align = "center"))
temp2024 <- temp2024 %>%
  mutate(SM10_tempe4000 = rollmean(temp_e4000, k = 10, fill = NA, align = "center"))
temp2024 <- temp2024 %>%
  mutate(SM10_tempstation = rollmean(temp_station, k = 10, fill = NA, align = "center"))

#SMOOTHING FOR 50 POINTS
temp2024 <- temp2024 %>%
  mutate(SM50_tempsud = rollmean(temp_sud, k = 50, fill = NA, align = "center"))
temp2024 <- temp2024 %>%
  mutate(SM50_tempnord = rollmean(temp_nord, k = 50, fill = NA, align = "center"))
temp2024 <- temp2024 %>%
  mutate(SM50_tempe4000 = rollmean(temp_e4000, k = 50, fill = NA, align = "center"))
temp2024 <- temp2024 %>%
  mutate(SM50_tempstation = rollmean(temp_station, k = 50, fill = NA, align = "center"))

#PLOTS
# 1st plot - Temperature_Interieur_Sud
p1 <- ggplot(temp2024, aes(x = Time)) +
  geom_line(aes(y = temp_sud, color = "Original")) +
  geom_line(aes(y = SM10_tempsud, color = "k=10")) +
  geom_line(aes(y = SM50_tempsud, color = "k=50")) +
  labs(title = "Temperature Interieur Sud", y = "Temp (°C)", color = "") +
  theme_minimal()

# 2nd plot - Temperature_Interieur_Nord
p2 <- ggplot(temp2024, aes(x = Time)) +
  geom_line(aes(y = temp_nord, color = "Original")) +
  geom_line(aes(y = SM10_tempnord, color = "k=10")) +
  geom_line(aes(y = SM50_tempnord, color = "k=50")) +
  labs(title = "Temperature Interieur Nord", y = "Temp (°C)", color = "") +
  theme_minimal()

# 3rd plot - Temperature_E4000
p3 <- ggplot(temp2024, aes(x = Time)) +
  geom_line(aes(y = temp_e4000, color = "Original")) +
  geom_line(aes(y = SM10_tempe4000, color = "k=10")) +
  geom_line(aes(y = SM50_tempe4000, color = "k=50")) +
  labs(title = "Temperature E4000", y = "Temp (°C)", color = "") +
  theme_minimal()

# 4th plot - Station_Meteo
p4 <- ggplot(temp2024, aes(x = Time)) +
  geom_line(aes(y = temp_station, color = "Original")) +
  geom_line(aes(y = SM10_tempstation, color = "k=10")) +
  geom_line(aes(y = SM50_tempstation, color = "k=50")) +
  labs(title = "Station Meteo", y = "Temp (°C)", color = "") +
  theme_minimal()

# Combine all 4 plots into one view
install.packages("patchwork")
library(patchwork)

(p1 / p2) / (p3 / p4)


#SUMMARY
resumen <- temp2024 %>%
  select(Time, SM10_tempsud,SM10_tempnord,SM10_tempe4000,SM10_tempstation)
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
