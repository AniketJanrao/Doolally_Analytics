"""
Created on Thu June 27 12:05:21 2018

@author: Aditya Lashkare
"""
#============================ *Forecasting* ==================================


#Input 
#set working directory
setwd("D:\\practice_r\\doolally")
#Enter location name:
location_n = "andheri"
#Enter category name:
category_n = "beer"
#Enter date upto which the model has to be trained
train_upto_date = "2018-04-30"
#Enter future prediction days
future_Days_n = 100




#-------------------*loading libraries*----------------------

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}
if(!require(prophet)){
  install.packages("prophet")
  library(prophet)
}
if(!require(forecast)){
  install.packages("forecast")
  library(forecast)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(tidyquant)){
  install.packages("tidyquant")
  library(tidyquant)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(RMySQL)){
  install.packages("RMySQL")
  library(RMySQL)
}


#-----------------------*Data Collection*--------------------------------------------------------------------


cn <- dbConnect(drv      = RMySQL::MySQL(), 
                username = "DoolallyBeer", 
                password = "doolallybeer123", 
                host     = "doolallybeer.ckkw1eidl5ao.ap-south-1.rds.amazonaws.com", 
                port     = 3306, 
                dbname   = "DoolallySales")

POS_data <- dbGetQuery(cn, "SELECT * FROM doolally_pos_master_staging WHERE order_status = 'printed' ;")

# setwd("D:\\practice_r\\doolally")
# POS_data <- read_csv("doolally_pos_master_staging.csv")


#-----------------------*Data Preproccesing*--------------------------------------------------------------------

names(POS_data)
POS_data <- POS_data[,c( "order_id" ,"order_number", "order_date", "location_name"    ,
                         "category_name",  "sub_category_name", "item_name", "item_quantity",    
                         "selling_price" , "subtotal"   ,    "tax_amount",  "discount_price",   
                         "final_total",  "order_status", "server_name",    "table_name",   
                         "correct_beer_name", "beer_litres" )]
POS_data$correct_beer_name <- as.character(POS_data$correct_beer_name)
POS_data$correct_item_name <- POS_data$correct_beer_name
POS_data$correct_item_name[is.na(POS_data$correct_item_name)] <- as.character(POS_data$item_name[is.na(POS_data$correct_item_name)])


#seperating date and time
new <- do.call(rbind,strsplit(as.character(POS_data$order_date), " " ))
pos <- cbind(POS_data, Time = new[,2], Date = new[,1] )
pos$Date <- as.Date(pos$Date)




#seperating year and month
xn  <-  as.POSIXct(pos$Date, format = "%Y-%m-%d")
pos["year"] = as.numeric(format(xn,"%Y"))
pos["month"] = as.numeric(format(xn,"%m"))
pos$month <- month.abb[pos$month]
#getting DAY
pos$day = strftime(pos$Date,'%A')

#removing unwanted obs 
pos <- pos[(which(pos$final_total != 0)),]
pos <- pos[which(pos$order_status == "printed") ,]
pos$beer_litres <- pos$beer_litres * pos$item_quantity 



#-----------------------Forecasting--------------------------------------------------

df_pos <- pos[pos$location_name == location_n & pos$category_name == category_n,]



names(df_pos)

df_pos <- df_pos[,c("pos_order_date" ,"beer_litres")]
#Summarizing day wise beer consumption
df_pos <- df_pos %>%
  group_by(pos_order_date) %>%
  summarise(beer_litres = sum(beer_litres) )

df_pos$beer_litres <- as.numeric(df_pos$beer_litres)
df_pos$y <- sqrt(df_pos$beer_litres)

df <- df_pos
summary(sqrt(df$beer_litres))
boxplot(sqrt(df$beer_litres), boxwex = 0.5, col = c("orange", "yellow"),
        main = "andheri Beer litres Boxplot",
        ylab = "log( Beer litres)")
#outliers are beer_litres bellow 10.618 or 40863.81 litres

outlier <- function(x){
  qnt <-  summary(x)['3rd Qu.'] - summary(x)['1st Qu.']
  y = summary(x)['1st Qu.'] - (1.5 * qnt)
  x = summary(x)['3rd Qu.'] + (1.5 * qnt)
  return(x)
}  
outlier = outlier(sqrt(df$beer_litres))

df <- df[df$beer_litres < (outlier^2), ]
df$day <- strftime(df$pos_order_date,"%A")

d <- density(sqrt(df_pos$beer_litres))
plot(d, main="beer_litres")
polygon(d, col="red", border="blue")



df_pos <- as.data.frame(df_pos)
names(df_pos)[1] <- "Date"
df_pos$Date <- as.Date(df_pos$Date)

#Dividing data in test and train sets
doolally_forecast <- df_pos %>%
  mutate(mode = ifelse(Date <= train_upto_date, "train", "test"))

colnames(doolally_forecast)[grep("^[0-9]+", colnames(doolally_forecast))] <- paste0("P_", colnames(doolally_forecast)[grep("^[0-9]+", colnames(doolally_forecast))])


#plotting train-test dataset
doolally_forecast %>%
  ggplot(aes(x = Date, y = beer_litres, color = mode)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = palette_dark()) +
  theme_tq()





train <- filter(doolally_forecast, mode == "train") %>%
  select(Date, y) %>%
  rename(ds = Date,
         y = y)

test <- filter(doolally_forecast, mode == "test") %>%
  select(Date, y) %>%
  rename(ds = Date)



#missing dates
ts <- seq(ymd(df_pos[1,"Date"]), ymd(df_pos[nrow(df_pos),"Date"]), by="day")

df <- data.frame(Date=ts)

data_with_missing_times <- full_join(df,df_pos)
data_with_missing_times[is.na(data_with_missing_times$beer_litres), "Date"] 

data_with_missing_times <- as.data.frame(data_with_missing_times[is.na(data_with_missing_times$beer_litres), "Date"] )




#holidays
#Holidays <- read_csv("festivals.csv")

#Holidays$ds  <-  as.POSIXct(Holidays$ds, format = "%m/%d/%Y")


#training forecast model
prophet_model_test <- prophet(train,
                              growth = "linear", # growth curve trend
                              #n.changepoints = 100, # Prophet automatically detects changes in trends by selecting changepoints from the data
                              yearly.seasonality = TRUE, # yearly seasonal component using Fourier series
                              daily.seasonality = TRUE) # weekly seasonal component using dummy variables
#prophet_model_test <- prophet(train, holidays = Holidays) 
#growth = "linear", # growth curve trend
#n.changepoints = 100, # Prophet automatically detects changes in trends by selecting changepoints from the data
#yearly.seasonality = FALSE, # yearly seasonal component using Fourier series
#daily.seasonality = TRUE) # weekly seasonal component using dummy variables 





#applying on test set
forecast_test <- predict(prophet_model_test, test)


#plotting residuals
forecast_test %>%
  mutate(resid = test$y - yhat) %>%
  ggplot(aes(x = ds, y = resid)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_smooth() +
  theme_tq()

forecast_test$beer_litres <- test$y




# ploting actual vs predicted 
forecast_test %>%
  gather(x, y, beer_litres,  yhat) %>%
  ggplot(aes(x = ds, y = (y^2), color = x)) +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) +
  scale_color_manual(values = palette_green()) +
  ggtitle("Actual vs Predicted") +
  xlab("date") +
  ylab("beer litres in ml") +
  scale_color_manual(labels = c("Actual", "Predicted"), values = c("blue", "red")) +
  theme_tq()

forecast_test$predicted <- (forecast_test$yhat)^2
forecast_test$actual <- (forecast_test$beer_litres)^2


#making future predictions for 7 days
future <- make_future_dataframe(prophet_model_test, periods = future_Days_n)
forecast <- predict(prophet_model_test, future)

plot(prophet_model_test, forecast) +
  theme_tq()
#extracting seasonal and trend components
prophet_plot_components(prophet_model_test, forecast)

prophet:::plot_yearly(prophet_model_test)
m <- prophet_model_test
fp <- predict(m, future)[,c('yhat')]
fp_fit <- ts(fp[1:(length(fp)-364)],frequency = 365)
fp_hist <- msts(m$history[,'y'],seasonal.periods = c(7,365.25))
fp_mean <- ts(fp[length(fp)-364:0],frequency = 365,start = end(fp_hist))
fprophet <- list(mean = fp_mean,fitted = fp_fit,x = fp_hist)
class(fprophet) <- "forecast"
MAPE = accuracy(fprophet)[5]
cat("MAPE:"  ,MAPE)



#
forecast$predicted_beer_total <- (forecast$yhat)^2
forecast$predicted_beer_total <- round(forecast$predicted_beer_total)
forecast$day = strftime(forecast$ds,'%A')
forecast = forecast[,c("ds","predicted_beer_total","day")]
forecast = forecast[nrow(doolally_forecast):nrow(forecast),]
rownames(forecast) <- NULL

forecast_week <- forecast[1:7,]
forecast_month <- forecast[1:30,]


#predicted graph next 8 days
ggplot(forecast_week, aes(x = ds,y =predicted_beer_total), fill = day) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = palette_dark()) +
  ggtitle("future prediction") +
  theme_tq()
ggplot(forecast_month, aes(x = ds,y =predicted_beer_total), fill = day) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = palette_dark()) +
  ggtitle("future prediction") +
  theme_tq()


