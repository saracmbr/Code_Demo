#### Sara Shakouri
#### July 2022
#### This code forecasts MMR dose 1 & 2 for the normal schedule

# Load in all the libraries required


load("mmr.Rdata")


mmr <- as.data.table(mmr)
mmr <- mmr %>%
  dplyr::select(-antigens) %>%
  unique()



mmr %>% filter(vaccine_dose==1 & event_sub_status_description=="Completed") %>%
  mutate(year=year(vaccine_date)) %>%
  filter(year >2015 & year< 2022) %>%
  select(year) %>%
  group_by(year) %>% 
  summarise(count=n()) 

mmr %>% filter(vaccine_dose==2 & event_sub_status_description=="Completed") %>%
  mutate(year=year(vaccine_date)) %>%
  filter(year >2015 & year< 2021) %>%
  select(year) %>%
  group_by(year) %>% 
  summarise(count=n()) 

dose1_cnt = mmr %>% filter(vaccine_dose==1 & event_sub_status_description=="Completed") %>%
  mutate(ethnicity2 = if_else(ethnicity_priority_l1 !='Maori' & ethnicity_priority_l1 !='Pacific Peoples','Other',ethnicity_priority_l1 )) %>%
  mutate(year=year(vaccine_date)) %>%
  filter(year >2016 ) %>%
  group_by(vaccine_date) %>% 
  summarise(count=n()) %>%
  as.data.table()

dose2_cnt = mmr %>% filter(vaccine_dose==2 & event_sub_status_description=="Completed") %>%
  mutate(ethnicity2 = if_else(ethnicity_priority_l1 !='Maori' & ethnicity_priority_l1 !='Pacific Peoples','Other',ethnicity_priority_l1 )) %>%
  mutate(year=year(vaccine_date)) %>%
  filter(year >2016 ) %>%
  unique() %>%
  group_by(vaccine_date) %>% 
  summarise(count=n()) %>%
  as.data.table()



horizon = 9*30
n_months_validation = 3



dose1_cnt = dose1_cnt %>%
  arrange(vaccine_date) %>%
  complete(vaccine_date = seq.Date(min(vaccine_date), max(vaccine_date), by = "day")) %>%
  mutate(count = if_else(is.na(count), as.integer(0), count)) %>% 
  as.data.table()


dose2_cnt = dose2_cnt %>%
  arrange(vaccine_date) %>%
  complete(vaccine_date = seq.Date(min(vaccine_date), max(vaccine_date), by = "day")) %>%
  mutate(count = if_else(is.na(count), as.integer(0), count)) %>%
  as.data.table() %>%
  unique()

### Create train, test
names(dose1_cnt)[1]='ds'
names(dose1_cnt)[2]='y'


names(dose2_cnt)[1]='ds'
names(dose2_cnt)[2]='y'


#Augmented Dickey-Fuller Test
#adf.test(diff(log(dose1_cnt$y)), alternative="stationary", k=0)

#adf.test(log(dose1_cnt$y), alternative="stationary", k=0)



# Create train and test sets for the log-transformed 

train_data <- dose1_cnt %>% filter(ds<= max(dose1_cnt$ds) -months(n_months_validation))

test_data <- dose1_cnt %>% filter(ds> max(dose1_cnt$ds) - months(n_months_validation))


train_data_ts = ts_ts(train_data) 
test_data_ts = ts_ts(test_data) 

#### Visualise data
dose1_cnt %>%
   filter(ds>='2021-01-01') %>%
  ggplot(aes(x = ds, y = y)) +
  geom_line(color = "steelblue") 




############### fable 

fable_fit= as_tsibble(train_data) %>%
  model(
    ets = ETS(y, opt_crit = "mae" ),
    ets2 = ETS(y ~ error("M") + trend("Ad") + season("A")),
    ## Model 3: Drift ----
    drift_mod = RW(y ~ drift()),
    ## Model 4: SES ----
    ses_mod = ETS(y ~ error("A") + trend("N") + season("N"), opt_crit = "mse"),
    ## Model 5: Holt's Linear ----
    hl_mod = ETS(y ~ error("A") + trend("A") + season("N"), opt_crit = "mse"),
    ## Model 6: Damped Holt's Linear ----
    hldamp_mod = ETS(y ~ error("A") + trend("Ad") + season("N"), opt_crit = "mse"),
    ## Model 7: STL decomposition with ETS ----
    # stl_ets_mod = decomposition_model(STL(y), ETS(season_adjust ~ season("N")))
    ## Model 8: ARIMA ----
    
    
    Arima1 = ARIMA (y ~ 0 + pdq(0:3,0,0:3) + PDQ(0:2,0,0:2), stepwise = FALSE) ,
    Arima2 = ARIMA (y ~ 0 + pdq(0:3,1,0:3) + PDQ(0:2,0,0:2), stepwise = FALSE) ,
    Arima3 = ARIMA (y ~ 0 + pdq(0:3,0,0:3) + PDQ(0:2,1,0:2), stepwise = FALSE) ,
    Arima4 = ARIMA (y ~ 0 + pdq(0:3,1,0:3) + PDQ(0:2,1,0:2), stepwise = FALSE) ,
    
    Prophet_default = fable.prophet::prophet( y ),
    
    prophet1 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.01 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    
    prophet2 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.05,  changepoint_range = 1 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    
    prophet3 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.03 , changepoint_range = 1 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    
    prophet4 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.07 , changepoint_range = 1 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    prophet5 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.09 , changepoint_range = 1 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    snaive_mod = SNAIVE(y)        
    
  ) %>%
  mutate(combined = (ets +  Arima3 ) / 2)

fable_forecast <- fable_fit %>%
  forecast(h = paste(nrow(test_data) , "days"))

fable_forecast %>% filter(.model %in% c('Arima4','Arima3'))  %>% autoplot(test_data, level =NULL)

joined <- inner_join(as_tibble(fable_forecast) %>% 
  dplyr::select(-y), test_data %>% 
    dplyr::select(ds, y), by = "ds")



accuracy = joined %>%
  group_by(.model) %>%
yardstick::rmse(y, .mean) %>%
  mutate(RMSE = .estimate) %>%
  dplyr::select(-.metric, -.estimator, -.estimate) %>%
  ungroup() %>%
  mutate(min_rmse =min(RMSE))

best_model = accuracy %>%
 filter(RMSE==min_rmse)


#### save  the best model and re-use to predict
saveRDS(best_model, "MMR_1st_dose_best_model.rds")
my_model <- readRDS("MMR_1st_dose_best_model.rds")
new_forecast_1st =  (h = paste(horizon , "days"))

####visulization 
fable_forecast_final <- fable_fit %>%
  forecast(h = paste(nrow(test_data)+horizon , "days")) %>%
  filter(.model==best_model$.model)
# 
# fable_forecast_final$rand=as.integer(floor(rnorm(nrow(fable_forecast_final),mean=0,sd=15)))
# 
# fable_forecast_final= as_tibble(fable_forecast_final) %>%
#   dplyr::select(-y) %>%
#   left_join( test_data %>% 
#         dplyr::select(ds, y), by = "ds") %>%
#   mutate(type= factor(if_else(!is.na(y),'Actual','Forecast'))) %>%
#   mutate(y_final = if_else(type=='Actual',y,  as.integer(floor(.mean)+rand))) %>%
#   mutate(y_final = if_else(y_final<0,as.integer(0),y_final))
#   
# fable_forecast_final = as.data.table(fable_forecast_final)
# fable_forecast_final[,'year_month':=as.Date(paste0("01/",month(ds),'/',year(ds)),format = "%d/%m/%Y")]
# 
# 
# 
# 
# 
# forecast_dose1_sum = fable_forecast_final[type=='Forecast', sum(y_final),by= year_month] %>% rename( 'Total Dose 1' = V1)
# forecast_dose1_sum$type='Forecast'
# 
# ggtexttable(forecast_dose1_sum, rows = NULL, theme = ttheme("blank")) %>%
#   tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)
# 
# ggsave('dose1_table.svg', width = 9, height = 9,plot = last_plot())
# 
# ggline(fable_forecast_final,x= 'ds',y='y_final',color='type',
#        palette = c("#00AFBB", "#E7B800"),plot_type = 'l',ylab= 'MMR dose 1',xlab ='')
# 
# ggsave('dose1_schedule_forecast.svg', width = 9, height = 9,plot = last_plot())
# 
# dose1_actual_sum= dose1_cnt[,sum(y),by= list(as.Date(paste0("01/",month(ds),'/',year(ds)),format = "%d/%m/%Y"))] %>% rename( year_month= as.Date, 'Total Dose 1' = V1)
# dose1_actual_sum$type= 'Actual'
# 
# tsbox::ts_plot(rbind(dose1_actual_sum[ year_month>='2018-01-01' & year_month<'2022-07-01',],forecast_dose1_sum))
# 
# 
# 
# p <- ggplot(aes(x=as.yearmon(year_month), y=`Total Dose 1`), 
# data=rbind(dose1_actual_sum[ year_month>='2018-01-01' & year_month<'2022-07-01',],forecast_sum))
# 
# p + geom_line()+
#   zoo::scale_x_yearmon()+
#   theme(
# panel.background = element_blank())+
#   labs(x='Year',y='Total Dose 1')
# 
# ggsave('dose1_forecast_monthly.svg', width = 9, height = 9)


# Dose 2 Forecast ---------------------------------------------------------

train_data <- dose2_cnt %>% filter(ds<=max(dose2_cnt$ds) - months(n_months_validation))

test_data <- dose2_cnt %>% filter(ds>max(dose2_cnt$ds) - months(n_months_validation))

#TSstudio::ts_plot(dose1_cnt)

############### fable 

fable_fit= as_tsibble(train_data) %>%
  model(
    ets = ETS(y, opt_crit = "mae" ),
    ets2 = ETS(y ~ error("M") + trend("Ad") + season("A")),
    ## Model 3: Drift ----
    drift_mod = RW(y ~ drift()),
    ## Model 4: SES ----
    ses_mod = ETS(y ~ error("A") + trend("N") + season("N"), opt_crit = "mse"),
    ## Model 5: Holt's Linear ----
    hl_mod = ETS(y ~ error("A") + trend("A") + season("N"), opt_crit = "mse"),
    ## Model 6: Damped Holt's Linear ----
    hldamp_mod = ETS(y ~ error("A") + trend("Ad") + season("N"), opt_crit = "mse"),
    ## Model 7: STL decomposition with ETS ----
    # stl_ets_mod = decomposition_model(STL(y), ETS(season_adjust ~ season("N")))
    ## Model 8: ARIMA ----
    
    
    Arima1 = ARIMA (y ~ 0 + pdq(0:3,0,0:3) + PDQ(0:2,0,0:2), stepwise = FALSE) ,
    Arima2 = ARIMA (y ~ 0 + pdq(0:3,1,0:3) + PDQ(0:2,0,0:2), stepwise = FALSE) ,
    Arima3 = ARIMA (y ~ 0 + pdq(0:3,0,0:3) + PDQ(0:2,1,0:2), stepwise = FALSE) ,
    Arima4 = ARIMA (y ~ 0 + pdq(0:3,1,0:3) + PDQ(0:2,1,0:2), stepwise = FALSE) ,
    
    Prophet_default = fable.prophet::prophet( y ),
    
    prophet1 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.01 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    
    prophet2 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.05,  changepoint_range = 1 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    
    prophet3 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.03 , changepoint_range = 1 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    
    prophet4 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.07 , changepoint_range = 1 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    prophet5 = fable.prophet::prophet( y~ growth(type = "linear" , changepoint_prior_scale  = 0.09 , changepoint_range = 1 ) +
                                         season( prior_scale = 10.0 ,type = c("additive", "multiplicative") )  ),
    snaive_mod = SNAIVE(y)        
    
  ) %>%
  mutate(combined = (ets +  Arima3 ) / 2)

fable_forecast <- fable_fit %>%
  forecast(h = paste(nrow(test_data) , "days"))

fable_forecast %>% filter(.model %in% c('Arima3','prophet3'))  %>% autoplot(test_data, level =NULL)


joined <- inner_join(as_tibble(fable_forecast) %>% 
                       dplyr::select(-y), test_data %>% 
                       dplyr::select(ds, y), by = "ds")



accuracy = joined %>%
  group_by(.model) %>%
  yardstick::rmse(y, .mean) %>%
  mutate(RMSE = .estimate) %>%
  dplyr::select(-.metric, -.estimator, -.estimate) %>%
  ungroup() %>%
  mutate(min_rmse =min(RMSE))

best_model = accuracy %>%
  filter(RMSE==min_rmse)

fable_forecast_final <- fable_fit %>% 
  forecast(h = paste(nrow(test_data)+horizon , "days")) %>%
  filter(.model==best_model$.model)

#### save  the best model and re-use to predict
saveRDS(best_model, "MMR_2nd_dose_best_model.rds")
my_model <- readRDS("MMR_2nd_dose_best_model.rds")
new_forecast_2nd =  (h = paste(horizon , "days"))

#### add randomness to the forecast, this is more for visualization
# fable_forecast_final$rand=as.integer(floor(rnorm(nrow(fable_forecast_final),mean=0,sd=15)))
# 
# fable_forecast_final= as_tibble(fable_forecast_final) %>%
#   dplyr::select(-y) %>%
#   left_join( test_data %>% 
#                dplyr::select(ds, y), by = "ds") %>%
#   mutate(type= factor(if_else(!is.na(y),'Actual','Forecast'))) %>%
#   mutate(y_final = if_else(type=='Actual',y,  as.integer(floor(.mean)+rand))) %>%
#   mutate(y_final = if_else(y_final<0,as.integer(0),y_final))
# 
# fable_forecast_final = as.data.table(fable_forecast_final)
# fable_forecast_final[,'year_month':=as.Date(paste0("01/",month(ds),'/',year(ds)),format = "%d/%m/%Y")]
# 
# 
# forecast_sum = fable_forecast_final[type=='Forecast', sum(y_final),by= year_month] %>% rename( 'Total Dose 2' = V1)
# 
# ggtexttable(forecast_sum, rows = NULL, theme = ttheme("blank")) %>%
#   tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)
# 
# ggsave('dose2_table.svg', width = 9, height = 9,plot = last_plot())
# 
# ggline(fable_forecast_final,x= 'ds',y='y_final',color='type',
#        palette = c("#00AFBB", "#E7B800"),plot_type = 'l',ylab= 'MMR dose 2',xlab ='')
# 
# ggsave('dose2_schedule_forecast.svg', width = 9, height = 9,plot = last_plot())
# 
# dose2_actual_sum= dose2_cnt[,sum(y),by= list(as.Date(paste0("01/",month(ds),'/',year(ds)),format = "%d/%m/%Y"))] %>% rename( year_month= as.Date, 'Total Dose 2' = V1)
# dose2_actual_sum$type= 'Actual'
# forecast_sum$type='Forecast'
# 
# tsbox::ts_plot(rbind(dose2_actual_sum[ year_month>='2018-01-01' & year_month<'2022-07-01',],forecast_sum))
# 
# 
# p <- ggplot(aes(x=as.yearmon(year_month), y=`Total Dose 2`), 
#             data=rbind(dose2_actual_sum[ year_month>='2018-01-01' & year_month<'2022-07-01',],forecast_sum))
# 
# p + geom_line()+
#   zoo::scale_x_yearmon()+
#   theme(
#     panel.background = element_blank())+
#   labs(x='Year',y='Total Dose 2')
# 
# ggsave('dose2_forecast_monthly.svg', width = 9, height = 9)
# 
# ggtexttable(forecast_dose1_sum %>% left_join(forecast_sum,by='year_month') %>% dplyr::select(-type.x,-type.y) , rows = NULL, theme = ttheme("blank")) %>%
#   tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)
# 
# ggsave('dose1&2_table.svg', width = 9, height = 9,plot = last_plot())
# 
# all_forecast =forecast_dose1_sum %>% left_join(forecast_sum,by='year_month') %>% dplyr::select(-type.x,-type.y) 
