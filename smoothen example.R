# Use read.csv with your file path
data <- read.csv("C:/Users/Emma/Downloads/Mur Nord -sol-data-2025-06-14 16_12_21.csv", stringsAsFactors = FALSE)

# Check the first few rows and structure
head(data)
str(data)


library(lubridate)
data$time <- ymd_hms(data$time)

smoothed_exterieur <- rollmean(data$exterieur, k = 500, fill = NA, align = "center")

library(zoo)

# combine it into a data frame
smoothed_data <- data.frame(
  Time = data$Time,
  exterieur = data$exterieur,
  smoothed_exterieur = smoothed_exterieur
)

View(smoothed_data)

# Plot original data first (as a line)
plot(data$Time, data$exterieur, type = "l", col = "blue",
     xlab = "Time", ylab = "Exterieur",
     main = "Original vs Smoothed Exterieur")

# Add smoothed data on top (red line)
lines(data$Time, smoothed_exterieur, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Original", "Smoothed"),
       col = c("blue", "red"), lty = 1, lwd = 2)

