rm(list=ls()) 

library(tidyverse)
library(Hmisc)
library(corrplot)
library(effects)

setwd("masterAFI/10. Regresion Avanzada/javier_herraez_albarran_regresion_avanzada/")

booking <- read.csv("hotel_bookings.csv", header = TRUE)

str(booking)
summary(booking)

barplot(colSums(is.na(booking)), las=2)

booking[is.na(booking$children),]$children <- 0 

booking <- booking %>% select(-arrival_date_year,
                              -arrival_date_week_number,
                              -arrival_date_day_of_month,
                              -meal,
                              -country,
                              -market_segment,
                              -distribution_channel,
                              -reserved_room_type,
                              -assigned_room_type,
                              -agent,
                              -company,
                              -days_in_waiting_list,
                              -reservation_status,
                              -reservation_status_date)
str(booking)

booking <- booking %>% 
  mutate(season = ifelse(arrival_date_month %in% c("December", "January", "February"), "Winter",
                  ifelse(arrival_date_month %in% c("March", "April", "May"), "Spring",
                  ifelse(arrival_date_month %in% c("June", "July", "August"), "Summer",
                  "Autumm")))) %>% 
  select(-arrival_date_month)

booking <- booking %>% 
  mutate(children = children + babies,
         total_nights = stays_in_week_nights + stays_in_weekend_nights) %>% 
  select(-babies, -stays_in_week_nights, -stays_in_weekend_nights)

booking$hotel <- as.factor(booking$hotel)
booking$is_canceled <- as.factor(booking$is_canceled)
booking$is_repeated_guest <- as.factor(booking$is_repeated_guest)
booking$deposit_type <- as.factor(booking$deposit_type)
booking$customer_type <- as.factor(booking$customer_type)
booking$season <- as.factor(booking$season)

str(booking)
summary(booking)
hist.data.frame(booking)

table(booking$is_canceled)

booking %>% 
  ggplot(aes(x= customer_type, fill = is_canceled)) + geom_bar(position = "fill") +
  labs(title = "Canceled by customer type", x = "", y = "", col = "")

booking %>% 
  ggplot(aes(x= customer_type, y = adults + children)) + geom_boxplot() +
  labs(title = "People by customer type", x = "", y = "", col = "")

booking %>% 
  ggplot(aes(x= customer_type, y = adults + children)) + geom_boxplot() +
  labs(title = "People by customer type", x = "", y = "", col = "") +
  coord_cartesian(ylim=c(0,10))

booking %>% 
  ggplot(aes(x= deposit_type, fill = is_canceled)) + geom_bar() +
  labs(title = "Canceled by customer type", x = "", y = "", col = "")

table(booking$deposit_type, booking$is_canceled)

booking <- booking %>% select(-deposit_type)

# la columna deposit_type 'Non Refund' y 'is_canceled' están correlacionadas de manera contraria a la intuición. 
# Más del 99 % de las personas que pagaron el monto total por adelantado cancelaron.

booking %>% 
  ggplot(aes(x= is_canceled, y = adr, fill = is_canceled)) + geom_boxplot() +
  labs(title = "ADR - is_cancelled", x = "", y = "", col = "")

booking %>% 
  ggplot(aes(x= is_canceled, y = adr, fill = is_canceled)) + geom_boxplot() +
  labs(title = "ADR - is_cancelled", x = "", y = "", col = "") +  coord_cartesian(ylim=c(0,600))

booking %>% 
  ggplot(aes(x= is_canceled, y = total_nights, fill = is_canceled)) + geom_boxplot() +
  labs(title = "Nights - is_cancelled", x = "", y = "", col = "")

booking %>% 
  ggplot(aes(x= season, fill = is_canceled)) + geom_bar() +
  labs(title = "Canceled by Season", x = "", y = "", col = "")

numeric_columns <- unlist(lapply(booking, is.numeric))
numeric_columns <- booking[, numeric_columns]

corrplot(cor(numeric_columns), method='number')

partition <- sort(sample(nrow(booking), nrow(booking)*.7))

booking.train <- booking[partition,]
booking.test <- booking[-partition,]

booking.mod <- glm(is_canceled ~ ., family=binomial(link="logit"), data=booking.train) 
summary(booking.mod)

plot(effect("booking_changes", booking.mod), ci.style="band", rescale.axis=FALSE, multiline=TRUE, ylab="P(released)", rug=FALSE, main="")

