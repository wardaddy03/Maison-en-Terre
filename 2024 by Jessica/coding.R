setwd("C:/Users/Emma/Desktop/Maison_en_terre")

#DATA IMPORT
#library (readr)

install.packages("tidyverse")
library(tidyverse)
temp2022 <- read_csv("2022_summer_CLEANED.csv", 
            col_types = cols(Time = col_character()))

#library(stringr)
temp2022 <- temp2022 %>%
  mutate(Time = str_trim(Time), Time = dmy_hm(Time)) 

#SMOOTHING
#install.packages("zoo")
library(zoo)
temp2022 <- temp2022 %>%
  mutate(SM_tempsud = rollmean(temp_sud, k = 10, fill = NA, align = "center"))
temp2022 <- temp2022 %>%
  mutate(SM_tempnord = rollmean(temp_nord, k = 10, fill = NA, align = "center"))
temp2022 <- temp2022 %>%
  mutate(SM_tempe4000 = rollmean(temp_e4000, k = 10, fill = NA, align = "center"))
temp2022 <- temp2022 %>%
  mutate(SM_tempstation = rollmean(temp_station, k = 10, fill = NA, align = "center"))

#SUMMARY
resumen_sm <- temp2022 %>%
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

#AMPLITUD with smoothening
daily_delta<-temp2022%>%
  mutate(Date=as_date(Time)) %>%
  group_by(Date)%>%
  summarise(
    amplitude_sud   = max(SM_tempsud,   na.rm = TRUE) - min(SM_tempsud,   na.rm = TRUE),
    amplitude_nord  = max(SM_tempnord,  na.rm = TRUE) - min(SM_tempnord,  na.rm = TRUE),
    amplitude_e4000 = max(SM_tempe4000, na.rm = TRUE) - min(SM_tempe4000, na.rm = TRUE),
    amplitude_station = max(SM_tempstation, na.rm = TRUE) - min(SM_tempstation, na.rm = TRUE)
  )

#AMPLITUD without smoothening
daily_delta1<-temp2022%>%
  mutate(Date=as_date(Time)) %>%
  group_by(Date)%>%
  summarise(
    amplitude_sud   = max(temp_sud,   na.rm = TRUE) - min(temp_sud,   na.rm = TRUE),
    amplitude_nord  = max(temp_nord,  na.rm = TRUE) - min(temp_nord,  na.rm = TRUE),
    amplitude_e4000 = max(temp_e4000, na.rm = TRUE) - min(temp_e4000, na.rm = TRUE),
    amplitude_station = max(temp_station, na.rm = TRUE) - min(temp_station, na.rm = TRUE)
  )

#EXPORT
install.packages("writexl")
library(writexl)
write_xlsx(daily_delta1, "daily_amplitudek0.xlsx")


#Ratio for k=10
daily_delta <- daily_delta %>%
  mutate(
    south = amplitude_sud / amplitude_station,
    north = amplitude_nord / amplitude_station,
    middle = amplitude_e4000 / amplitude_station
  )

mean_ratios <- daily_delta %>%
  summarise(
    mean_south = mean(south, na.rm = TRUE),
    mean_north = mean(north, na.rm = TRUE),
    mean_middle = mean(middle, na.rm = TRUE)
  )

mean_ratios


#Ratio for k=0
daily_delta1 <- daily_delta1 %>%
  mutate(
    south = amplitude_sud / amplitude_station,
    north = amplitude_nord / amplitude_station,
    middle = amplitude_e4000 / amplitude_station
  )

mean_ratios1 <- daily_delta1 %>%
  summarise(
    mean_south = mean(south, na.rm = TRUE),
    mean_north = mean(north, na.rm = TRUE),
    mean_middle = mean(middle, na.rm = TRUE)
  )

mean_ratios1

#Graph for k=10
# Reshape to long format
daily_delta_long <- daily_delta %>%
  select(Date, south, north, middle) %>%
  pivot_longer(cols = c(south, north, middle),
               names_to = "location",
               values_to = "amplitude_ratio")

# Plot
ggplot(daily_delta_long, aes(x = Date, y = amplitude_ratio, color = location)) +
  geom_line(size = 1) +
  labs(
    title = "Daily Amplitude Ratios over Time for K=10",
    x = "Date",
    y = "Amplitude Ratio",
    color = "Location"
  ) +
  theme_minimal()

#Graph for k=0
# Reshape to long format
daily_delta1_long <- daily_delta1 %>%
  select(Date, south, north, middle) %>%
  pivot_longer(cols = c(south, north, middle),
               names_to = "location",
               values_to = "amplitude_ratio")

# Plot
ggplot(daily_delta1_long, aes(x = Date, y = amplitude_ratio, color = location)) +
  geom_line(size = 1) +
  labs(
    title = "Daily Amplitude Ratios over Time for K=0",
    x = "Date",
    y = "Amplitude Ratio",
    color = "Location"
  ) +
  theme_minimal()


#graph of temperature and their smoothened values
# Select the relevant columns and reshape to long format
temp_long <- temp2022 %>%
  select(Time, temp_sud, temp_nord, temp_e4000, temp_station,
         SM_tempsud, SM_tempnord, SM_tempe4000, SM_tempstation) %>%
  pivot_longer(
    cols = -Time,
    names_to = "variable",
    values_to = "temperature"
  ) %>%
  mutate(
    Location = case_when(
      str_detect(variable, "sud") ~ "South",
      str_detect(variable, "nord") ~ "North",
      str_detect(variable, "e4000") ~ "Middle",
      str_detect(variable, "station") ~ "Station"
    ),
    Smoothing = if_else(str_starts(variable, "SM_"), "Smoothed (k=10)", "Raw (k=0)")
  )

ggplot(temp_long, aes(x = Time, y = temperature, color = Smoothing)) +
  geom_line() +
  facet_wrap(~Location, scales = "free_y") +
  labs(
    title = "Temperature Over Time (Raw vs Smoothed)",
    x = "Time",
    y = "Temperature (°C)",
    color = "Data"
  ) +
  theme_minimal()


# lets do three points graph
# Add month column to make it easier
temp2022 <- temp2022 %>%
  mutate(month = month(Time), date = as.Date(Time))

# Get 3 random dates per month (May = 5 to September = 9)
set.seed(123)  # For reproducibility
selected_dates <- temp2022 %>%
  filter(month >= 5 & month <= 9) %>%
  distinct(date, month) %>%
  group_by(month) %>%
  slice_sample(n = 3) %>%
  ungroup()

selected_dates
subset_temp <- temp2022 %>%
  filter(date %in% selected_dates$date)

subset_long <- subset_temp %>%
  select(Time, temp_sud, temp_nord, temp_e4000, temp_station,
         SM_tempsud, SM_tempnord, SM_tempe4000, SM_tempstation) %>%
  pivot_longer(
    cols = -Time,
    names_to = "variable",
    values_to = "temperature"
  ) %>%
  mutate(
    Location = case_when(
      str_detect(variable, "sud") ~ "South",
      str_detect(variable, "nord") ~ "North",
      str_detect(variable, "e4000") ~ "Middle",
      str_detect(variable, "station") ~ "Station"
    ),
    Smoothing = if_else(str_starts(variable, "SM_"), "Smoothed (k=10)", "Raw (k=0)")
  )

ggplot(subset_long, aes(x = Time, y = temperature, color = Smoothing)) +
  geom_line() +
  facet_wrap(~Location, scales = "free_y") +
  labs(
    title = "Temperature Over 3 Random Days per Month (May–September)",
    x = "Time",
    y = "Temperature (°C)",
    color = "Data"
  ) +
  theme_minimal()


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
