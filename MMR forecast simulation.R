#### Sara Shakouri July 2022
#### MMR vaccine Simulations and Forecast for 17-32 yo that we don't have any data to use for the forecasting.

library(forecast)
library(dplyr)
library(data.table)
library(lubridate)
library(GGally)
set.seed(123)


# Simulation of 13+ uptake in vaccination events --------------------------


upwards_rate <- 0.002
downwards_rate <- 0.002
flat_rate <- 0.003
N <- 3000




myDate1 <- seq(as.Date("2022-06-01"), as.Date("2022-06-20"), by = "day")
x <- (1:length(myDate1)) / 2



ts.df <- c()
for (i in 1:100) {
  # model1 <- Arima(ts(rnorm(500,mean = 10,sd = 20) ,freq=1), order=c(2,1,1), seasonal=c(1,0,1))
  #   ts.sim1 <- simulate(model1, nsim=length(myDate1)) + 3 *x
  # ts.df_temp =data.frame("ds"= myDate1, "y"= tsbox::ts_df(ts.sim1)$value)
  ts.df_temp <- data.frame("ds" = myDate1, "y" = rnorm(length(myDate1), mean = N * upwards_rate, sd = 2) + (x^0.5))
  ts.df_temp$id <- i
  ts.df <- bind_rows(ts.df, ts.df_temp)
  ts.df <- as.data.frame(ts.df)
}


ts_df <- ts.df %>%
  group_by(ds) %>%
  mutate(y2 = mean(y))
ts_df <- ts_df %>%
  mutate(y = if_else(y2 < 0, 0, y2)) %>%
  select(ds, y) %>%
  unique()
################# sep to feb
myDate1 <- seq(as.Date("2022-06-21"), as.Date("2022-07-10"), by = "day")
x <- (1:length(myDate1)) / 2


ts.df <- c()
for (i in 1:100) {
  #  model1 <- Arima(ts(rnorm(500,mean = 50,sd = 20) ,freq=1), order=c(2,1,1), seasonal=c(1,0,1))
  #  ts.sim1 <- simulate(model1, nsim=length(myDate1)) -2*x
  
  #  ts.df_temp =data.frame("ds"= myDate1, "y"= tsbox::ts_df(ts.sim1)$value)
  ts.df_temp <- data.frame("ds" = myDate1, "y" = rnorm(length(myDate1), mean = N * flat_rate, sd = 5))
  ts.df_temp$id <- i
  ts.df <- bind_rows(ts.df, ts.df_temp)
  ts.df <- as.data.frame(ts.df)
}


ts_df2 <- ts.df %>%
  group_by(ds) %>%
  mutate(y2 = mean(y))
ts_df2 <- ts_df2 %>%
  mutate(y = if_else(y2 < 0, 0, y2)) %>%
  select(ds, y) %>%
  unique()
#############
myDate1 <- seq(as.Date("2022-07-11"), as.Date("2022-07-31"), by = "day")
x <- (1:length(myDate1)) / 2


ts.df <- c()
for (i in 1:100) {
  #  model1 <- Arima(ts(rnorm(500,mean = 50,sd = 20) ,freq=1), order=c(2,1,1), seasonal=c(1,0,1))
  #  ts.sim1 <- simulate(model1, nsim=length(myDate1)) -2*x
  
  #  ts.df_temp =data.frame("ds"= myDate1, "y"= tsbox::ts_df(ts.sim1)$value)
  ts.df_temp <- data.frame("ds" = myDate1, "y" = rnorm(length(myDate1), mean = N * downwards_rate, sd = 5) - 2 * log(x))
  ts.df_temp$id <- i
  ts.df <- bind_rows(ts.df, ts.df_temp)
  ts.df <- as.data.frame(ts.df)
}


ts_df3 <- ts.df %>%
  group_by(ds) %>%
  mutate(y2 = mean(y))
ts_df3 <- ts_df3 %>%
  mutate(y = if_else(y2 < 0, 0, y2), trend = "sim 10") %>%
  select(ds, y) %>%
  unique()

df <- bind_rows(ts_df, ts_df2, ts_df3)
df$y <- floor(df$y)
################ Shifting the distribution above every two weeks when  they open to new cohort,dhb .... till end of March 2023
df2 <- as.data.table(df)

for (i in 1:19) {
  myDate <- seq(as.Date(max(df2$ds) - 47), by = "day", length.out = nrow(df))
  
  temp <- data.frame("ds" = myDate, "y" = df$y)
  
  df2 <- bind_rows(df2, temp)
}

df_final <- df2 %>%
  group_by(ds) %>%
  mutate(y = sum(y)) %>%
  unique()

df_final <- df_final %>%
  mutate(y2 = case_when(
    ds <= "2022-07-11" ~ y,
    ds > "2022-07-11" & ds <= "2023-02-16" ~ 3 * y,
    ds > "2023-02-16" ~ 0.5 * y
  )) %>%
  as.data.table() %>%
  select(ds, y)

TSstudio::ts_plot(df_final)

ggplot2::ggplot(df, mapping = aes(x = ds, y = y)) +
  ggplot2::geom_line()
