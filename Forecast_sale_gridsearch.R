library(tidyverse)
library(forecast)
library(lubridate)
library(furrr)
library(tsibble)
library(brotools)
library(data.table)

ihs <- function(x){
  log(x + sqrt(x**2 + 1))
}

forecast_sample$TransactionDate =as.Date(as.character(forecast_sample$TransactionDate),format="%d/%m/%Y")
forecast_sample= as.data.table(forecast_sample)
forecast_sample2= forecast_sample[order(TransactionDate),]
mytrain= forecast_sample2[TransactionDate< '2019-04-01',]
mytest=forecast_sample2[TransactionDate>= '2019-04-01',]


mytrain2=  mytrain$fuel_vol  %>%
   ts(.,frequency = 365)
  
mytest2=mytest$fuel_vol  %>% ts(.,frequency = 365)


logged_train_data <- ihs(mytrain2)

logged_test_data <- ihs(mytest2)

##### Check seasonality and trend ####
plot.ts(mytrain2)

decompese_fuel= decompose(mytrain2,'additive')

plot(as.ts(decompese_fuel$seasonal))
plot(as.ts(decompese_fuel$trend))
plot(as.ts(decompese_fuel$random))
####### Check the Diffrencing 
logged_train_data_seas_diff=diff(logged_train_data-decompese_fuel$seasonal)
plot.ts(logged_train_data_seas_diff)
logged_train_data_seas_diff1=diff(logged_train_data_seas_diff,1)
plot.ts(logged_train_data_seas_diff1)
acf(logged_train_data_seas_diff,lag.max = 20)

to_tibble <- function(forecast_object){
  point_estimate <- forecast_object$mean %>%
    as_tsibble() %>%
    rename(point_estimate = value,
           date = index)
  
  upper <- forecast_object$upper %>%
    as_tsibble() %>%
    spread(key, value) %>%
    rename(date = index,
           upper80 = `80%`,
           upper95 = `95%`)
  
  lower <- forecast_object$lower %>%
    as_tsibble() %>%
    spread(key, value) %>%
    rename(date = index,
           lower80 = `80%`,
           lower95 = `95%`)
  
  reduce(list(point_estimate, upper, lower), full_join)
}

##### Creating Grid serch 
order_list <- list("p" = seq(1, 3),
                   "d" = seq(1, 2),
                   "q" = seq(5, 7)) %>%
  cross() %>%
  map(lift(c))


season_list <- list("P" = seq(0, 3),
                    "D" = seq(0, 2),
                    "Q" = seq(0, 3),
                    "period" = 365)  %>%
  cross() %>%
  map(lift(c))

orderdf <- tibble("order" = order_list)

seasondf <- tibble("season" = season_list)

hyper_parameters_df <- crossing(orderdf, seasondf)

plan(multiprocess, workers = 8)

tic <- Sys.time()
models_df <- hyper_parameters_df %>%
  mutate(models = future_map2(.x = order,
                              .y = season,
                              ~possibly(arima, otherwise = NULL)(x = logged_train_data,
                                                                 order = .x, seasonal = .y)))
running_time <- Sys.time() - tic

nrows <- nrow(hyper_parameters_df)


models_df_2 = models_df %>%
  mutate(forecast = map(models, ~possibly(forecast, otherwise = NULL)(., h = 39))) %>%
  mutate(point_forecast = map(forecast, ~.$`mean`)) %>%
  mutate(true_value = rerun(nrows, logged_test_data)) %>%
  mutate(rmse = map2_dbl(point_forecast, true_value,
                         ~sqrt(mean((.x - .y) ** 2))))

best_model <- models_df_2 %>%
  filter(rmse == min(rmse, na.rm = TRUE))

best_model_forecast <- to_tibble(best_model$forecast[[1]])


#### Plot

forecast_sample %>%
  mutate(total_ihs = ihs(fuel_vol)) %>%
  ggplot() +
  ggtitle("Logged data") +
  geom_line(aes(y = total_ihs, x = TransactionDate), colour = "#82518c") +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%Y") +
  geom_ribbon(data = best_model_forecast, aes(x = date, ymin = lower95, ymax = upper95), 
              fill = "#666018", alpha = 0.2) +
  geom_line(data = best_model_forecast, aes(x = date, y = point_estimate), linetype = 2, colour = "#8e9d98") 
#theme_blog()

