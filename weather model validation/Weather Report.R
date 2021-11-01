
library(riem)
library(lubridate)
library(tidyverse)
library(stringr)

forecast <- read.csv("C:\\Users\\Ryan\\Desktop\\weather model validation\\forecast_to_merge.csv")

forecast <- select(forecast, c(3, 6, 9, 10, 12, 14))


#######################################
#          ACTUAL
#######################################

yesterday_date <- Sys.Date()-1

weather <- riem::riem_measures(station = "AVL", date_start = yesterday_date, date_end = yesterday_date)



weather$valid <- with_tz(weather$valid, tzone = "America/Indianapolis")


weather2 <- weather %>% 
  filter(tmpf != "NA") 


weather3 <- weather2[weather2$valid > paste(yesterday_date, " 06:00:00") &    # Extract data frame subset
                       weather2$valid < paste(yesterday_date, " 16:00:00"), ]
   

weather3$startTime <-str_sub(weather3$valid, start = 1L, end = 10L)
weather3$startTime <- paste(weather3$startTime, "T06:00:00-04:00", sep = "")

weather3[, 8][weather3[, 8] == 0] <- NA # make zeros NA




startTime <- unique( weather3$startTime )
date <-str_sub(startTime, start = 1L, end = 10L)
max_temp <- max(na.omit(weather3$tmpf))
avg_wind_dir <- round(mean(na.omit(weather3$drct)), 0)
avg_wind_speed <- round(mean(na.omit(weather3$sknt)), 0)
max_wind_speed <- max(na.omit(weather3$sknt))
precip <- sum(weather3$p01i)




t <- weather3 %>% 
  count(skyc1)

r <- t[which.max(t$n),]

skyCover <- r$skyc1

weather4 <- data.frame(startTime, date,  max_temp, avg_wind_dir, avg_wind_speed, max_wind_speed, precip, skyCover)

##################################
#            MERGE
#################################


test <- left_join(forecast, weather4, by = "startTime")
test$timestamp <- mdy(test$timestamp)
test$date <- ymd(test$date)
test$dif <- test$date - test$timestamp
test$tempDif <- test$max_temp - test$temperature

test$windDirection_num <- ifelse(test$windDirection == "NW", 315, 
                                 ifelse(test$windDirection == "NNW", 337.5, 
                                 ifelse(test$windDirection == "W", 270, 
                                 ifelse(test$windDirection == "WNW", 292.5,
                                 ifelse(test$windDirection == "SW", 225, 
                                 ifelse(test$windDirection == "WSW", 247.5,
                                 ifelse(test$windDirection == "N", 360, 
                                 ifelse(test$windDirection == "SSW", 202.5,
                                 ifelse(test$windDirection == "ESE", 112.5,
                                 ifelse(test$windDirection == "S", 180, 
                                 ifelse(test$windDirection == "SSE", 157.5,
                                 ifelse(test$windDirection == "NE", 045 ,
                                 ifelse(test$windDirection == "SE", 135, 
                                 ifelse(test$windDirection == "E", 090,
                                 ifelse(test$windDirection == "ENE", 067.5, NA)))))))))))))))
test$windDif <- test$windDirection_num - test$avg_wind_dir


r <- test[!is.na(test$date),]

view(r)



r$windSpeed



tt <- str_extract_all(r$windSpeed, "\\d+")
ed <- do.call(rbind.data.frame, tt)

names(ed) <- c("min", "max_wind_forecast")


r$max_wind_forecast <- ed$max_wind_forecast

View(r)

###################################
#          TEMP PLOTS
###################################

plot <- ggplot(data = r, mapping = aes(x = timestamp, y = temperature)) 

plot + geom_point() + geom_line( y = r$max_temp, color = 'red')

plot(y = r$temperature, x = r$timestamp)





####################################
#           WIND PLOTS
####################################

ggplot(r, aes(x = r$windDirection_num, fill = r$max_wind_forecast)) +
  
  coord_polar(theta = "x", start = 0, clip = "on") +
  scale_x_continuous(limits = c(0,360))


#######################3

ggplot(r,
       aes(x = r$windDirection_num, y = r$max_wind_forecast)) +
  geom_col(width = .1, fill = "steelblue", color = "steelblue") +
  coord_polar(start = -pi/12) + # change start value if you want a different orientation
  theme_light() +
  theme(axis.title = element_blank(),
        panel.ontop = TRUE, # change to FALSE for grid lines below the wind rose
        panel.background = element_blank())



#####################3

library(plotly)

data <- data.frame(distance = r$max_wind_forecast,
                   rotation  = r$windDirection_num,
                   id = paste0(1:6))


fig <- plot_ly(
  type = 'scatterpolar',
  r = c(data$distance),
  theta = c(data$rotation),
  text = r$timestamp,
  hovertemplate = paste('<i>Speed</i>: %{r}',
                        '<br><b>Direction</b>: %{theta}<br>',
                        '<b>%{text}</b>'),
  mode = 'markers',
  name = "Forecasts"
) %>%
  layout(polar = list(
    angularaxis  = list(
      rotation = 90,
      direction = "clockwise"
    )
  )) 
  
fig <- fig %>%
  add_trace(
    r = r$max_wind_speed,
    theta = r$avg_wind_dir,
    name = "Actual",
    marker = list(
      color = "maroon",
      symbol = 'diamond',
      size = 8
    ),
    text = "sector: 135->225<br>rotation: 90<br>direction: counterclockwise"
  ) 

fig


