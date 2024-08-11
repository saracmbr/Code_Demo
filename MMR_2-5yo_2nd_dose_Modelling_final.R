
# Load Packages ----------------------------------------------------------------
pacman::p_load(
  tidyverse, DBI, odbc, keyring, httr, janitor,
  gridExtra, grid, lubridate,
  rprojroot, tidymodels, lightgbm, rsample, data.table, tune, dplyr, recipes,
  parsnip, tune, dials, workflows, yardstick, Matrix, rstanarm,
  MASS, doParallel,
  install = FALSE
)
library(SHAPforxgboost)

the_root_file <- find_root(has_file(".here"))

# source(file.path(the_root_file, "projects", "mmr" , "module_get_oracle_vacc_data.r"))
source(file.path(the_root_file, "utilities", "r", "connect_to_snowflake.r"))
source(file.path(the_root_file, "utilities", "r", "connect_to_oracle.r"))

query <- "
SELECT master_hcu_id, gender_code, ethnicity_priority_l1,nzdepdecile2018,
IUR2018_NAME as rural_urban, date_of_birth
FROM DWH_PRD_REP.COMMON.HEALTH_SERVICE_USER_2021FY hsu21
left join
    DWH_PRD_REP.DWH.DIM_ANNUAL_AREAS_2018 g ON g.MB2018_CODE = ifnull(meshblock_2018,meshblock_2018)

WHERE date_of_birth > '1989-01-01'
"
conn <- DBI::dbConnect(odbc::odbc(),
  "Snowflake",
  token = snowflake_token$access_token
)
population <- dbGetQuery(conn, query) %>%
  clean_names() %>%
  as.data.table()

dbDisconnect(conn)

id_query <-
  "select
      dim_hcu_identifiable_key,
      Master_HCU_ID
      from
      dssadm_rep.dim_hcu_identifiable"

id <- dbGetQuery(con, id_query) %>%
  clean_names() %>%
  as.data.table()

dbDisconnect(con)

load("mmr.Rdata")
load("enrolments.Rdata")
load("rv.Rdata")
load("var.Rdata")


mmr <- as.data.table(mmr)
mmr <- mmr %>%
  dplyr::select(-antigens) %>%
  unique()
mmr <- mmr %>% filter(mmr$event_sub_status_description == "Completed")

rv <- rv %>%
  dplyr::select(-antigens) %>%
  unique() %>%
  filter(event_sub_status_description == "Completed")



var <- var %>%
  dplyr::select(-antigens) %>%
  unique() %>%
  filter(event_sub_status_description == "Completed")


mmr <- mmr %>%
  left_join(rv %>% ungroup() %>% filter(vaccine_dose == 2) %>%
    dplyr::mutate(Rotavirus = vaccine) %>%
    dplyr::select(master_hcu_id, Rotavirus), by = "master_hcu_id") %>%
  left_join(var %>% ungroup() %>%
    dplyr::mutate(Chicken_pox = vaccine) %>%
    dplyr::select(master_hcu_id, Chicken_pox), by = "master_hcu_id") %>%
  unique()

mmr <- mmr %>%
  mutate(had_varicella = if_else(!is.na(Chicken_pox), "Y", "N")) %>%
  mutate(had_rota = if_else(!is.na(Rotavirus), "Y", "N")) %>%
  dplyr::select(-Chicken_pox, -Rotavirus)

mmr <- mmr %>%
  left_join(id, by = "master_hcu_id") %>%
  left_join(population, by = "master_hcu_id") %>%
  left_join(enrolments, by = c("dim_hcu_identifiable_key" = "dim_health_care_user_key"))



mmr <- mmr[, "Current_age" := trunc((date_of_birth %--% Sys.Date()) / years(1))]


mmr <- mmr %>% filter(!is.na(ethnicity_priority_l1) & !is.na(nzdepdecile2018))

mmr <- mmr[
  vaccine_dose == 2,
  "age_at_dose2" := trunc((date_of_birth %--% vaccine_date) / days(1))
]

mmr <- mmr[
  vaccine_dose == 1,
  "age_at_dose1" := trunc((date_of_birth %--% vaccine_date) / days(1))
]

mmr <- mmr[, "age_at_dose2" := max(age_at_dose2, na.rm = TRUE), by = list(master_hcu_id)] %>%
  mutate(age_at_dose2 = if_else(is.finite(age_at_dose2), age_at_dose2, NULL))


mmr <- mmr[, "age_at_dose1" := max(age_at_dose1, na.rm = TRUE), by = list(master_hcu_id)] %>%
  mutate(age_at_dose1 = if_else(is.finite(age_at_dose1), age_at_dose1, NULL))



mmr <- mmr %>%
  mutate("dose1_elligible_date" = if_else(date_of_birth >= ymd("2020-10-01"), date_of_birth + days(366), date_of_birth + days(366 + 180))) %>%
  mutate("dose2_elligible_date" = if_else(date_of_birth >= ymd("2020-10-01"), date_of_birth + days(366 + 180), date_of_birth + days(366 + 365 * 3)))

mmr <- mmr %>%
  mutate("days_past_dose1_elligibility" = if_else(vaccine_dose == 1, difftime(vaccine_date, dose1_elligible_date, units = "days"), NULL)) %>%
  mutate("days_past_dose2_elligibility" = if_else(vaccine_dose != 1, difftime(vaccine_date, dose2_elligible_date, units = "days"), NULL))


mmr <- mmr[, "days_past_dose1_elligibility" := max(days_past_dose1_elligibility, na.rm = TRUE), by = list(master_hcu_id)] %>%
  mutate(days_past_dose1_elligibility = if_else(is.finite(days_past_dose1_elligibility), days_past_dose1_elligibility, NULL))

mmr <- mmr[, "days_past_dose2_elligibility" := max(days_past_dose2_elligibility, na.rm = TRUE), by = list(master_hcu_id)] %>%
  mutate(days_past_dose2_elligibility = if_else(is.finite(days_past_dose2_elligibility), days_past_dose2_elligibility, NULL))

mmr$days_past_dose1_elligibility <- as.numeric(mmr$days_past_dose1_elligibility)
mmr$days_past_dose2_elligibility <- as.numeric(mmr$days_past_dose2_elligibility)
# mmr$less_6mo_dose2=if_else(mmr$days_past_dose2_elligibility<=180,'Y','N')
dose2 <- mmr %>%
  filter(vaccine_dose == 2 & event_status_description == "Completed") %>%
  mutate(age_at_second_dose = trunc((date_of_birth %--% vaccine_date) / years(1))) %>%
  mutate(age_at_change = trunc((date_of_birth %--% '2020-10-01') / years(1))) %>%
  mutate(Current_age = trunc((date_of_birth %--% Sys.Date()) / years(1))) %>%
  mutate(year_month = substr(vaccine_date,1,7)) %>% 
  filter(age_at_second_dose >= 0) %>%
  mutate(had_varicella=if_else(date_of_birth>='2016-04-01',had_varicella,if_else(had_varicella=='Y','Y','N/A'))) %>%
  mutate(had_rota=if_else(date_of_birth>='2014-04-01',had_rota,if_else(had_rota=='Y','Y','N/A'))) %>%
  as.data.table()%>%
  mutate(dose2_days_category=case_when(days_past_dose2_elligibility <=-100 ~'<-100',
                                       days_past_dose2_elligibility %between% c(-100,-30) ~ '[-100,-30]',
                                       days_past_dose2_elligibility %between% c(-29,30) ~ '(-30,30]',
                                       days_past_dose2_elligibility %between% c(31,90) ~ '(30,90]',
                                       days_past_dose2_elligibility %between% c(91,180) ~ '(90,180]',
                                       days_past_dose2_elligibility>180 ~ '>180'
  ) ) %>%
  mutate(ethnicity2 = if_else(ethnicity_priority_l1 != "Maori" & ethnicity_priority_l1 != "Pacific Peoples", "Other", ethnicity_priority_l1))

dose2$dose2_days_category = factor(dose2$dose2_days_category,ordered = TRUE,levels = c('<-100','[-100,-30]','(-30,30]','(30,90]','(90,180]','>180'))         


dose2$dose2_days_category = factor(dose2$dose2_days_category,ordered = TRUE,levels = c('<-100','[-100,-30]','(-30,30]','(30,90]','(90,180]','>180'))         



mmr_sample <- dose2 %>%
  dplyr::select(
    -dose2_elligible_date, -gender_code,-dose2_elligible_date,-age_at_change,
    -event_sub_status_description,
    -vaccine, -vaccine_dose, -Current_age,
    -vaccine_date, -event_id,
    -rural_urban, -event_status_description, -indication, -date_last_updated,
    -year_month,-dose2_days_category,-ethnicity_priority_l1,-age_at_second_dose,-date_of_birth
  ,-dose1_elligible_date
    ) %>%
  unique()



Training_sample <- dose2[Current_age %between% c(4, 11) &
 
  !is.na(days_past_dose2_elligibility), master_hcu_id] %>%
  unique()

mmr_sample <- mmr_sample[master_hcu_id %in% Training_sample, ] %>%
  filter(!is.na(ethnicity2) & !is.na(nzdepdecile2018))



mmr_sample <- mmr_sample %>% dplyr::select( -master_hcu_id)


mmr_sample <- mmr_sample %>% mutate_if(is.character, factor)
mmr_sample <- mmr_sample %>% dplyr::filter(!is.na(days_past_dose1_elligibility))

### shuffle data
mmr_sample <- mmr_sample[sample(1:nrow(mmr_sample)), ]
# split non-test set into training and validation set - consider cross validation


mmr_sample = mmr_sample %>% 
  mutate(had_varicella=factor(had_varicella,levels =c('Y','N','N/A'))) %>%
  mutate(had_rota=factor(had_rota,levels =c('Y','N','N/A'))) %>%
  mutate(ethnicity2= factor(ethnicity2,levels=c('Maori','Pacific Peoples','Other')))

split <- initial_split(
  mmr_sample,
  prop = 0.8,
  strata = "days_past_dose2_elligibility"
)




train <- data.frame(training(split)) 
  
  test <- data.frame(testing(split))


recipe <- recipe(days_past_dose2_elligibility ~ ., data = train) %>%
  step_normalize(days_past_dose1_elligibility) %>%
  step_dummy(all_nominal_predictors()) %>%
  prep()


train <- as.data.frame(bake(recipe, new_data = NULL))
test <- as.data.frame(bake(recipe, new_data = test))



# speed up computation with parallel processing
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# set the random seed so we can reproduce any simulated results.
set.seed(1234)

####################### XGBOOST regression

xgb_mod <-
  parsnip::boost_tree(
    trees = 3800, min_n = 5,
    tree_depth = 3,
    learn_rate = 0.01,
  ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


xgb_fit <-
  xgb_mod %>%
  fit(days_past_dose2_elligibility ~ ., data = train)


p <- floor(predict(xgb_fit, test))

p_naive <- mean(test$days_past_dose2_elligibility)

# accuracy check


rmse <- caret::RMSE(test$days_past_dose2_elligibility, p$.pred)
rmse_naive <- caret::RMSE(test$days_past_dose2_elligibility, p_naive)


cat("\nRMSE: ", rmse, "\nRMSE_Naive: ", rmse_naive)


res <- cbind(test, "prediction" = p$.pred)
res$less_6mo_dose2 <- factor(if_else(res$days_past_dose2_elligibility <= 180, "Y", "N"))
res$less_6mo_dose2_pred <- factor(if_else(res$prediction <= 180, "Y", "N"))

caret::confusionMatrix(data = res$less_6mo_dose2_pred, reference = res$less_6mo_dose2)
caret::recall(data = res$less_6mo_dose2_pred, reference = res$less_6mo_dose2, relevant = levels(res$less_6mo_dose2)[2], na.rm = TRUE)
caret::recall(data = res$less_6mo_dose2_pred, reference = res$less_6mo_dose2, relevant = levels(res$less_6mo_dose2)[1], na.rm = TRUE)


res$nzdepdecile2018 <- factor(res$nzdepdecile2018, ordered = FALSE)

ggplot(res, aes(x = nzdepdecile2018, y = prediction)) +
  geom_boxplot(fill = "green")

# mmr_sample$nzdepdecile2018 =factor(mmr_sample$nzdepdecile2018,ordered = FALSE,levels = c('10','9','8','7','6','5','4','3','2','1'))
ggplot(
  train,
  aes(x = as.factor(nzdepdecile2018), y = days_past_dose2_elligibility)
) +
  geom_boxplot(fill = "green")

ggplot(train, aes(x = factor(ethnicity_priority_l1_Maori), y = days_past_dose2_elligibility)) +
  geom_boxplot(fill = "green")

ggplot(res, aes(x = as.factor(ethnicity_priority_l1_Maori), y = days_past_dose2_elligibility)) +
  geom_boxplot(fill = "green")


ggplot(mmr_sample, aes(
  x = days_past_dose2_elligibility,
  fill = ethnicity_priority_l1, color = ethnicity_priority_l1
)) +
  geom_histogram(position = "identity")

############## XGBOOST Classification

train$less_6mo_dose2 <- factor(if_else(train$days_past_dose2_elligibility <= 180, "Y", "N"))
test$less_6mo_dose2 <- factor(if_else(test$days_past_dose2_elligibility <= 180, "Y", "N"))



xgb_mod <-
  parsnip::boost_tree(
    trees = 1800, min_n = 5,
    tree_depth = 3,
    learn_rate = 0.01,
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")


xgb_fit <-
  xgb_mod %>%
  fit(less_6mo_dose2 ~ nzdepdecile2018 + had_rota_Y + ethnicity_priority_l1_Maori +
    ethnicity_priority_l1_Other + days_past_dose1_elligibility + had_varicella_Y + ethnicity_priority_l1_European
    + ethnicity_priority_l1_MELAA + ethnicity_priority_l1_Pacific.Peoples, data = train)

prop.table(table(train$less_6mo_dose2))
p <- predict(xgb_fit, test)


p <- predict(xgb_fit, test, type = "prob")
p$.pred_class <- as.factor(if_else(p$.pred_Y <= 0.15, "N", "Y"))

caret::confusionMatrix(data = p$.pred_class, reference = test$less_6mo_dose2)
caret::recall(data = p$.pred_class, reference = test$less_6mo_dose2, relevant = levels(test$less_6mo_dose2)[2], na.rm = TRUE)
caret::recall(data = p$.pred_class, reference = test$less_6mo_dose2, relevant = levels(test$less_6mo_dose2)[1], na.rm = TRUE)



##################
glmnet_fit <-
  linear_reg(penalty = 0.001, mixture = 0.5) %>%
  set_engine("glmnet") %>%
  fit(days_past_dose2_elligibility ~ ., data = train)

p <- predict(glmnet_fit, test)



p_naive <- mean(test$days_past_dose2_elligibility)

# accuracy check

rmse <- caret::RMSE(test$days_past_dose2_elligibility, p$.pred)
rmse_naive <- caret::RMSE(test$days_past_dose2_elligibility, p_naive)

res <- data.frame("actual" = test$days_past_dose2_elligibility, "prediction" = floor(p$.pred))

cat("\nRMSE: ", rmse, "\nRMSE_Naive: ", rmse_naive)
############################################# Lightgbm
trainm <- as.matrix(train %>% dplyr::select(-days_past_dose2_elligibility))
train_label <- train[, "days_past_dose2_elligibility"]

valm <- as.matrix(test %>% dplyr::select(-days_past_dose2_elligibility))
val_label <- test[, "days_past_dose2_elligibility"]


train_matrix <- lgb.Dataset(data = trainm, label = train_label)
val_matrix <- lgb.Dataset(data = valm, label = val_label)


# define parameters
params <- list(
  objective = "regression",
  min_data = 1L,
  learning_rate = 0.01,
  n_estimators = 2000,
  num_leaves = 1000,
  num_iterations = 10
)
# 0.01 230
# validataion data
valid <- list(test = val_matrix)


# model <- lgb.cv(
#   params = params
#   , data = train_matrix
#
#  # ,label = train_y
#   , nrounds = 10L
#   , nfold = 3L
#  #,valids = valid
# )

# lgb.get.eval.result(model, "test", "l2")



bst <- lightgbm(params = params, train_matrix, valid)



# prediction
p <- floor(predict(bst, valm))

p_naive <- mean(val_label)

# accuracy check
# mse = mean((test_y - p)^2)
mae <- caret::MAE(val_label, p)
rmse <- caret::RMSE(val_label, p)
rmse_naive <- caret::RMSE(val_label, p_naive)

res <- data.frame("actual" = val_label, "prediction" = floor(p))

cat("\nRMSE: ", rmse, "\nRMSE_Naive: ", rmse_naive)
