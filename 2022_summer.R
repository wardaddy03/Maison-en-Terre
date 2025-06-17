setwd("Z:/maison en terre")
library(readr)
data <- read_csv("2022_summer.csv")


library(lubridate)

data$datetime <- as.POSIXct(data$Time, format="%d/%m/%Y %H:%M")




data$datetime <- as.POSIXct(data$Time, format="%d/%m/%Y %H:%M")

# Check number of missing values for each temperature column
df <- read.csv("2022_summer.csv")
colSums(is.na(df[
  , c("Temperature_Interieur_Sud.mean", "Temperature_Interieur_Nord.mean", 
      "Temperature_E4000.mean", "Station_Meteo_Text")]))

df[!complete.cases(df[, c("Temperature_Interieur_Sud.mean",
                          "Temperature_Interieur_Nord.mean",
                          "Temperature_E4000.mean",
                          "Station_Meteo_Text")]), ]

# Interpolation of empty cells
install.packages("zoo")
library(zoo)
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")


# Define your actual time range: 1 May 2022 to 30 Sep 2022, hourly data
Time <- seq.POSIXt(from = as.POSIXct("2022-05-01 00:00"), 
                   to = as.POSIXct("2022-09-30 23:00"), by = "hour")

  # Create zoo object indexed by Time for interpolation
  zoo_df <- zoo(
  df[, c("Temperature_Interieur_Sud.mean", "Temperature_Interieur_Nord.mean", "Temperature_E4000.mean")],
  order.by = df$Time
)
print(zoo_df)

# Linear interpolation of missing values based on time index
#zoo_interp <- na.approx(zoo_df, x = index(zoo_df), na.rm = FALSE)

library(zoo)

df <- df[order(df$Time), ]

df_interp <- df  # copy original

for (col in c("Temperature_Interieur_Sud.mean", "Temperature_Interieur_Nord.mean", "Temperature_E4000.mean")) {
  df_interp[[col]] <- na.approx(df[[col]], na.rm = FALSE)
}


#Smoothening of data
library(zoo)

for (col in c("Temperature_Interieur_Sud.mean", 
              "Temperature_Interieur_Nord.mean", 
              "Temperature_E4000.mean", 
              "Station_Meteo_Text")) {
  
  df_interp[[paste0(col, "_smoothed")]] <- rollmean(df_interp[[col]], 
                                                    k = 10,
                                                    fill = NA, 
                                                    align = "center")
}

#Plotting of DAtas

install.packages("patchwork")
library(patchwork)


library(ggplot2)
library(patchwork)  # to combine multiple ggplots into one layout

# 1st plot - Temperature_Interieur_Sud
p1 <- ggplot(df_interp, aes(x = Time)) +
  geom_line(aes(y = Temperature_Interieur_Sud.mean, color = "Original")) +
  geom_line(aes(y = Temperature_Interieur_Sud.mean_smoothed, color = "Smoothed")) +
  labs(title = "Temperature Intérieur Sud", y = "Temp (°C)", color = "") +
  theme_minimal()

# 2nd plot - Temperature_Interieur_Nord
p2 <- ggplot(df_interp, aes(x = Time)) +
  geom_line(aes(y = Temperature_Interieur_Nord.mean, color = "Original")) +
  geom_line(aes(y = Temperature_Interieur_Nord.mean_smoothed, color = "Smoothed")) +
  labs(title = "Temperature Intérieur Nord", y = "Temp (°C)", color = "") +
  theme_minimal()

# 3rd plot - Temperature_E4000
p3 <- ggplot(df_interp, aes(x = Time)) +
  geom_line(aes(y = Temperature_E4000.mean, color = "Original")) +
  geom_line(aes(y = Temperature_E4000.mean_smoothed, color = "Smoothed")) +
  labs(title = "Temperature E4000", y = "Temp (°C)", color = "") +
  theme_minimal()

# 4th plot - Station_Meteo_Text
p4 <- ggplot(df_interp, aes(x = Time)) +
  geom_line(aes(y = Station_Meteo_Text, color = "Original")) +
  geom_line(aes(y = Station_Meteo_Text_smoothed, color = "Smoothed")) +
  labs(title = "Station Meteo Text", y = "Value", color = "") +
  theme_minimal()

# Combine all 4 plots into one view
library(patchwork)

(p1 / p2) / (p3 / p4)



# Get min and max for each smoothed temperature column
temp_summary <- data.frame(
  Variable = c(
    "Temperature_Interieur_Sud.mean_smoothed",
    "Temperature_Interieur_Nord.mean_smoothed",
    "Temperature_E4000.mean_smoothed",
    "Station_Meteo_Text_smoothed"
  ),
  Min = c(
    min(df_interp$Temperature_Interieur_Sud.mean_smoothed, na.rm = TRUE),
    min(df_interp$Temperature_Interieur_Nord.mean_smoothed, na.rm = TRUE),
    min(df_interp$Temperature_E4000.mean_smoothed, na.rm = TRUE),
    min(df_interp$Station_Meteo_Text_smoothed, na.rm = TRUE)
  ),
  Max = c(
    max(df_interp$Temperature_Interieur_Sud.mean_smoothed, na.rm = TRUE),
    max(df_interp$Temperature_Interieur_Nord.mean_smoothed, na.rm = TRUE),
    max(df_interp$Temperature_E4000.mean_smoothed, na.rm = TRUE),
    max(df_interp$Station_Meteo_Text_smoothed, na.rm = TRUE)
  )
)


print(temp_summary)


# Computation of daily amplitude
library(dplyr)

# Create a Date column from your Time datetime
df_interp <- df_interp %>%
  mutate(Date = as.Date(Time))

# Calculate daily amplitude (max - min) for each variable grouped by Date
daily_amplitude <- df_interp %>%
  group_by(Date) %>%
  summarise(
    Temp_Interieur_Sud_Amplitude = if(all(is.na(Temperature_Interieur_Sud.mean_smoothed))) NA_real_ else max(Temperature_Interieur_Sud.mean_smoothed, na.rm = TRUE) - min(Temperature_Interieur_Sud.mean_smoothed, na.rm = TRUE),
    Temp_Interieur_Nord_Amplitude = if(all(is.na(Temperature_Interieur_Nord.mean_smoothed))) NA_real_ else max(Temperature_Interieur_Nord.mean_smoothed, na.rm = TRUE) - min(Temperature_Interieur_Nord.mean_smoothed, na.rm = TRUE),
    Temp_E4000_Amplitude = if(all(is.na(Temperature_E4000.mean_smoothed))) NA_real_ else max(Temperature_E4000.mean_smoothed, na.rm = TRUE) - min(Temperature_E4000.mean_smoothed, na.rm = TRUE),
    Station_Meteo_Text_Amplitude = if(all(is.na(Station_Meteo_Text_smoothed))) NA_real_ else max(Station_Meteo_Text_smoothed, na.rm = TRUE) - min(Station_Meteo_Text_smoothed, na.rm = TRUE)
  ) %>%
  ungroup()


# View the resulting daily amplitude dataset
print(daily_amplitude)

library(writexl)

write_xlsx(daily_amplitude, path = "daily_amplitude2022.xlsx")

