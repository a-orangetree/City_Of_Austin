library(glmnetUtils)
library(leaps)
library(gridExtra)
library(randomForest)
library(xgboost)
library(knitr)


data_without_NA <- drop_na(austin_zip_codes) %>%
  select(-crimes_2015, -crimes_2014) %>%
  ungroup() %>%
  as_tibble()

train_data <- sample_frac(data_without_NA, .75)
test_data <- anti_join(data_without_NA, train_data)

# View(cor(data_without_NA))
# Matrix::rankMatrix(pracma::rref(cor(data_without_NA)))


####### Feature Selection #####################


num_predictors <- dim(data_without_NA)[2] - 1

regsubset_model <- regsubsets(crimes_2016 ~ .
                              ,data = data_without_NA
                              ,nvmax = num_predictors
                              ,method = 'forward')

reg_subset_summary <- summary(regsubset_model)

num_predictors <- regsubset_model$nvmax - 1

reg_subset_results <- tibble(num_pred = 1:num_predictors
                             ,rss = reg_subset_summary$rss
                             ,rsquared = reg_subset_summary$rsq
                             ,adj_rsquared = reg_subset_summary$adjr2
                             ,cp = reg_subset_summary$cp
                             ,bic = reg_subset_summary$bic)


# RSS
plot1_q1 <- reg_subset_results %>% 
  ggplot(aes(num_pred, rss)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(reg_subset_results$rss)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('RSS')

# ADJ R-SQUARED
plot2_q1 <- reg_subset_results %>% 
  ggplot(aes(num_pred, adj_rsquared)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.max(reg_subset_results$adj_rsquared)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Adj R-squared')

# CP
plot3_q1 <- reg_subset_results %>% 
  ggplot(aes(num_pred, cp)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(reg_subset_results$cp)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Cp')

# BIC
plot4_q1 <- reg_subset_results %>% 
  ggplot(aes(num_pred, bic)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(reg_subset_results$bic)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('BIC')

grid.arrange(plot1_q1, plot2_q1, plot3_q1, plot4_q1, nrow = 2, ncol = 2)

coef(regsubset_model, 5)


####### Ridge Regression ################


ridge_model <- glmnet(crimes_2016 ~ venues + Non_White_Non_Hispanic + Hispanic + Passive_Park + MultiFamily
                      , data = train_data
                      , alpha = 0)

ridge_cv <- cv.glmnet(crimes_2016 ~ venues + Non_White_Non_Hispanic + Hispanic + Passive_Park + MultiFamily
                      , data = train_data
                      , alpha = 0)

best_lambda_ridge <- ridge_cv$lambda.min

ridge_predictions <- test_data %>%
  mutate(ridge_pred = predict(ridge_model, newdata = test_data, s = best_lambda_ridge)
         ,diff_sq = (crimes_2016 - ridge_pred)^2)

(ridge_accuracy <- tibble(sqrt(mean(ridge_predictions$diff_sq))))


####### Lasso ################


lasso_model <- glmnet(crimes_2016 ~ venues + Non_White_Non_Hispanic + Hispanic + Passive_Park + MultiFamily
                      , data = train_data
                      , alpha = 1)

lasso_cv <- cv.glmnet(crimes_2016 ~ venues + Non_White_Non_Hispanic + Hispanic + Passive_Park + MultiFamily
                      , data = train_data
                      , alpha = 1)

best_lambda <- lasso_cv$lambda.min

lasso_predictions <- test_data %>%
  mutate(lasso_pred = predict(lasso_model, newdata = test_data, s = best_lambda)
         ,diff_sq = (crimes_2016 - lasso_pred)^2)

lasso_accuracy <- tibble(sqrt(mean(lasso_predictions$diff_sq)))


###### Random Forest ######


# rf_grid_search <- read_csv('data/rf_grid_search.csv')
# rf_grid_search <- tibble(number_of_trees = 0, mtry = 0, min_observations = 0, terminal_nodes = 0, rmse = 0)

rf_function <- function(number_of_trees, mtries, min_observations, terminal_nodes) {
  
  for (tree_size in number_of_trees) {
    for (n_pred in mtries) {
      for (min_obs in min_observations) {
        for (term_nodes in terminal_nodes) {
          
          rf_model <- randomForest(crimes_2016 ~ venues + Non_White_Non_Hispanic + Hispanic + Passive_Park + MultiFamily
                                   , data = train_data
                                   , ntrees = tree_size
                                   , mtry = n_pred
                                   , nodesize = min_obs
                                   , maxnodes = term_nodes)
          
          test_data_rf <- test_data %>% 
            mutate(pred_rf = predict(rf_model, test_data)
                   ,diff_sq = (crimes_2016 - pred_rf)^2)
          
          accuracy <- sqrt(mean(test_data_rf$diff_sq))
          
          rf_grid_search <- add_row(rf_grid_search, number_of_trees = tree_size
                                    ,mtry = n_pred
                                    ,terminal_nodes = term_nodes
                                    ,min_observations = min_obs
                                    , rmse = accuracy)
        }
      }
    }
    write_csv(rf_grid_search, 'data/rf_grid_search.csv')
  }
}

num_of_predictors_rf <- length(names(train_data))

mtries <- seq(1, num_of_predictors_rf - 2)
min_observations <- list(1, 2, 5, 10, 15)
terminal_nodes <- list(1, 2, 5, 10, 15)
number_of_trees <- list(10, 50, 100, 250, 500)

# rf_function(number_of_trees, mtries, min_observations, terminal_nodes)

rf_grid_search <- read_csv('data/rf_grid_search.csv')

rf_grid_search <- rf_grid_search %>% 
  filter(rmse != 0 & !is.na(number_of_trees)) %>% 
  arrange(rmse) %>% 
  head(1)

rf_model <- randomForest(crimes_2016 ~ venues + Non_White_Non_Hispanic + Hispanic + Passive_Park + MultiFamily
                          , data = train_data
                          , ntrees = rf_grid_search$number_of_trees
                          , mtry = rf_grid_search$mtry
                          , nodesize = rf_grid_search$min_observations
                          , maxnodes = rf_grid_search$terminal_nodes)

rf_predictions <- test_data %>% 
  mutate(pred_rf = predict(rf_model, test_data)
         ,diff_sq = (crimes_2016 - pred_rf)^2)

(rf_accuracy <- tibble(sqrt(mean(rf_predictions$diff_sq))))


###### Boosted Tree #####

# boost_accuracies <- read_csv('data/boost_accuracies.csv')
# boost_accuracies <- tibble(number_of_rounds = 0, best_iteration = 0, best_ntreelimit = 0
                           # , eta = 0, gamma = 0, max_depth = 0, rmse = 0)

x <- select(train_data, -crimes_2016)
y <- train_data$crimes_2016 %>% as.matrix()

boosted_func <- function(number_of_rounds, learning_rates, list_of_gammas, max_depths) {

  for (round in number_of_rounds) {
    for (rate in learning_rates) {
      for (gamma in list_of_gammas) {
        for (depth in max_depths) {
          
          xgboost_cv <- xgb.cv(data = model.matrix(~ ., data = x)
                               , label = y
                               , early_stopping_rounds = 10
                               , nfold = 10
                               , objective = "reg:linear"
                               , verbose = FALSE
                               , nrounds = round
                               , eta = rate
                               , gamma = gamma
                               , max_depth = depth)
          
          print(round)
          print(xgboost_cv$best_iteration)
          print(xgboost_cv$best_ntreelimit)
          print(rate)
          print(gamma)
          print(depth)
          print(xgboost_cv$evaluation_log$test_rmse_mean[xgboost_cv$best_iteration])
          
          boost_accuracies <- add_row(boost_accuracies
                  , number_of_rounds = round
                  , best_iteration = xgboost_cv$best_iteration
                  , best_ntreelimit = xgboost_cv$best_ntreelimit
                  , eta = rate
                  , gamma = gamma
                  , max_depth = depth
                  , rmse = xgboost_cv$evaluation_log$test_rmse_mean[xgboost_cv$best_iteration])
          
        }
      }
    }
  }
  write_csv(boost_accuracies, 'data/boost_accuracies.csv')
}


number_of_rounds <- list(5, 10, 50, 100, 500, 1000)
learning_rates <- list(.001, .003, .005, .01, .03, .05, .1)
list_of_gammas <- list(1, 5, 20, 50, 100)
max_depths <- list(2, 3, 5, 8, 10)

# boosted_func(number_of_rounds, learning_rates, list_of_gammas, max_depths)
      
boost_accuracies <- read_csv('data/boost_accuracies.csv') %>% 
  filter(rmse != 0) %>% 
  arrange(rmse) %>% 
  head(1)
  
xgboost_model <- xgboost(data = model.matrix(~ ., data = x)
                         , label = y
                         , objective = "reg:linear"
                         , verbose = FALSE
                         , nrounds = boost_accuracies$number_of_rounds
                         , eta = boost_accuracies$eta
                         , gamma = boost_accuracies$gamma
                         , max_depth = boost_accuracies$max_depth)

dtrain <- xgb.DMatrix(data = model.matrix(~ ., data = select(test_data, -crimes_2016)))

testing_data_boost <- test_data %>% 
  mutate(pred_boost = predict(xgboost_model, dtrain)
         ,diff_sq = (crimes_2016 - pred_boost)^2)

(boost_accuracy <- tibble(sqrt(mean(testing_data_boost$diff_sq))))


######### Combined Accuracies ##################

# combined_accuracies <- tibble(Boosted_Tree = sqrt(mean(testing_data_boost$diff_sq))
#                               , Random_Forest = sqrt(mean(rf_predictions$diff_sq))
#                               , Ridge_Regression = sqrt(mean(ridge_predictions$diff_sq))
#                               , Lasso = sqrt(mean(lasso_predictions$diff_sq)))
# 
# 
# combined_accuracies_wo2015 <- combined_accuracies %>%
#   gather(key = 'model', value = 'error') %>%
#   arrange(desc(error))

# write_csv(combined_accuracies_wo2015, 'data/combined_accuracies_wo2015')

combined_accuracies_wo2015 <- read_csv('data/combined_accuracies_wo2015')

kable(combined_accuracies_wo2015)
