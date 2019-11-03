library(data.table)

library(prophet)
library(dplyr)
library(ggpubr)
library(Metrics)

dt=data.table(read.csv('sample_fb.csv'))
offers=data.table(read.csv('offers.csv'))

dt$Day=as.Date(as.character(dt$Day))
offers$Date=as.Date(as.character(offers$Date))
names(dt)=c('ds','y')
dt= dt[order(ds),]
############# Daily Forecasting #####
dt_train = dt[ds< '2019-05-01',]
##### Adding NZ holidays and promos as extra events #######
#holidays=data.table(prophet::generated_holidays)
#holidays= holidays[year %in% c('2018','2019','2020') & country=='NZ',]
#xmass_eve=data.table(holiday='xmass_Eve',ds=as.Date(c('2018-12-24','2019-12-24','2020-12-24')))

easter_break_19=data.table(holiday='easter_break_19',ds=as.Date('2019-04-19'),lower_window = 0,
upper_window = 10,prior_scale=40)

newyear_break=data.table(holiday='newyear_break',ds=as.Date('2017-12-22','2018-12-22','2019-12-22','2020-12-22'),lower_window = 0,
upper_window = 16,prior_scale=40
)


school_hols <- data.table(
holiday = 'School_holiday',ds = as.Date(c('2017-04-13', '2017-06-08', '2017-10-01',
'2018-04-13', '2018-06-08', '2018-10-01',
'2019-04-13', '2019-06-08', '2019-10-01',
'2020-04-13', '2020-06-08', '2020-10-01',
'2021-04-13', '2021-06-08', '2021-10-01',
'2022-04-13', '2022-06-08', '2022-10-01',
'2023-04-13', '2023-06-08', '2023-10-01',
'2024-04-13', '2024-06-08', '2024-10-01',
'2025-04-13', '2025-06-08', '2025-10-01'
,'2026-04-13', '2026-06-08', '2026-10-01')),
lower_window = -2,
upper_window = 14,
prior_scale=15
)

offer= data.table(holiday='offer',ds=as.Date(as.character(offers$Date)),lower_window = 0,
upper_window = 0,prior_scale=100)



holidays=rbind(easter_break_19,newyear_break,school_hols)

holidays_offers=rbind(easter_break_19,newyear_break,school_hols,offer)


m = prophet(holidays = holidays_offers,changepoint.prior.scale = 5,daily.seasonality=FALSE)
#holidays.prior.scale = 15
m <- add_country_holidays(m, country_name = 'NZ')
#m <- add_seasonality(m, name='weekly', period=7, fourier.order=3, prior.scale=0.1)
m <- fit.prophet(m, dt_train)

future <- make_future_dataframe(m, periods =91 )
forecast <- predict(m, future)
prophet_plot_components(m, forecast)


plot(m, forecast)


##### Preparing dataframe for plotting ######
forecast_plot = forecast %>% select('ds','yhat')
#,'yhat_upper','yhat_lower')
forecast_plot$Type="Forecast"
forecast_plot$ds = as.Date(forecast_plot$ds)

names(forecast_plot)[2]="y"

dt$Type="Actuals"

df_plot= rbind(dt, forecast_plot)
df_plot= as.data.table(df_plot)
df_plot=df_plot[ds > '2019-01-01',]

 ggline(df_plot, x = 'ds', y = 'y',color = 'Type',palette = c("#00AFBB", "#E7B800")) 

### The accuracy of the model using RMSE and MAPE






