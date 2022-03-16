rm(list=ls()) 

library(tidyverse)
library(Hmisc)
library(corrplot)
library(effects)
library(caret)

setwd("masterAFI/10. Regresion Avanzada/javier_herraez_albarran_regresion_avanzada/")

set.seed(1404)

booking <- read.csv("hotel_bookings.csv", header = TRUE)

str(booking)
summary(booking)

prop.table(table(booking$is_canceled))

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
         total_nights = stays_in_week_nights + stays_in_weekend_nights,
         previous_cancellations = ifelse(previous_cancellations > 0, 1, 0)) %>% 
  select(-babies, -stays_in_week_nights, -stays_in_weekend_nights)

booking$hotel <- as.factor(booking$hotel)
booking$is_canceled <- as.factor(booking$is_canceled)
booking$is_repeated_guest <- as.factor(booking$is_repeated_guest)
booking$deposit_type <- as.factor(booking$deposit_type)
booking$customer_type <- as.factor(booking$customer_type)
booking$season <- as.factor(booking$season)
booking$previous_cancellations <- as.factor(booking$previous_cancellations)

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
  ggplot(aes(x= deposit_type, fill = is_canceled)) + geom_bar() +
  labs(title = "Canceled by customer type", x = "", y = "", col = "")

table(booking$deposit_type, booking$is_canceled)

booking <- booking %>% select(-deposit_type)

# la columna deposit_type 'Non Refund' y 'is_canceled' están correlacionadas de manera contraria a la intuición. 
# Más del 99 % de las personas que pagaron el monto total por adelantado cancelaron.

booking %>% 
  ggplot(aes(x= is_canceled, y = adr, fill = is_canceled)) + geom_boxplot() +
  labs(title = "ADR - is_cancelled", x = "", y = "", col = "")

booking[(booking$adr > 4000),]
booking[(booking$adr < 0),]
booking <- booking[!(booking$adr>4000 | booking$adr < 0),]

booking %>% 
  ggplot(aes(x= is_canceled, y = adr, fill = is_canceled)) + geom_boxplot() +
  labs(title = "ADR - is_cancelled", x = "", y = "", col = "")

booking %>% 
  ggplot(aes(x= is_canceled, y = total_nights, fill = is_canceled)) + geom_boxplot() +
  labs(title = "Nights - is_cancelled", x = "", y = "", col = "")

booking %>% 
  ggplot(aes(x= season, fill = is_canceled)) + geom_bar() +
  labs(title = "Canceled by Season", x = "", y = "", col = "")

booking %>% 
  ggplot(aes(x= previous_cancellations, fill = is_canceled)) + geom_bar(position = "fill") +
  labs(title = "Canceled by previous_cancellations", x = "", y = "", col = "")
table(booking$previous_cancellations, booking$is_canceled)


numeric_columns <- unlist(lapply(booking, is.numeric))
numeric_columns <- booking[, numeric_columns]

corrplot(cor(numeric_columns), method='number')

partition <- sort(sample(nrow(booking), nrow(booking)*.7))

booking.train <- booking[partition,]
booking.test <- booking[-partition,]

mod_general <- glm(is_canceled ~ ., family=binomial(link="logit"), data=booking.train) 
summary(mod_general)
cbind(coef(mod_general), confint(mod_general))
mod_general.predict <- predict(mod_general, newdata = booking.test, type = "response")
mod_general.predict <- ifelse(mod_general.predict > 0.5, 1, 0)
confusionMatrix(table(data=mod_general.predict, reference=booking.test$is_canceled))

mod_1 <- glm(is_canceled ~ . - hotel - adr +
               log(adr + 1) +
               adr:total_nights,
             family=binomial(link="logit"), data=booking.train) 
summary(mod_1)

mod_1.predict <- predict(mod_1, newdata = booking.test, type = "response")
mod_1.predict <- ifelse(mod_1.predict > 0.5, 1, 0)
confusionMatrix(table(data=mod_1.predict, reference=booking.test$is_canceled))

plot(allEffects(mod_general), ci.style="band", rescale.axis=FALSE, multiline=TRUE, ylab="P(released)", rug=FALSE, main="")

roc.plot(
  booking.train$is_canceled,
  mod_1.predict,
  threshold = seq(0,max(mod_1.predict),0.01)
)

