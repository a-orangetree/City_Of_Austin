library(keras)

data_without_NA <- drop_na(austin_zip_codes) %>%
  # select(-crimes_2014, -crimes_2015) %>%
  ungroup() %>%
  as_tibble()

train_data <- sample_frac(data_without_NA, .75)
test_data <- anti_join(data_without_NA, train_data)

# Training data
x_train <- model.matrix(~ ., data = select(train_data, -crimes_2016, -zip_code))[,-1]
dim(x_train)
y_train <- select(train_data, crimes_2016, -zip_code) %>% as.matrix()
dim(y_train)

# Test data
x_test <- model.matrix(~ ., data = select(test_data, -crimes_2016, -zip_code))[,-1]
dim(x_test)
y_test <- select(test_data, crimes_2016, -zip_code) %>% as.matrix()
dim(y_test)

# Standardize
mean <- apply(x_train, 2, mean)
std <- apply(x_train, 2, sd)
x_train <- scale(x_train, center = mean, scale = std)
x_test <- scale(x_test, center = mean, scale = std)

build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 32, activation = "relu", input_shape = dim(x_train)[[2]]) %>% 
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model %>% compile(
    optimizer = "rmsprop", 
    loss = "mse", 
    metrics = c("mse")
  )
}

k <- 2
num_epochs <- 100
indices <- sample(1:nrow(x_train))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
all_mse_histories <- NULL

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- x_train[val_indices,]
  val_targets <- y_train[val_indices]
  
  partial_train_data <- x_train[-val_indices,]
  partial_train_targets <- y_train[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 1, verbose = 0
  )
  
  mse_history <- history$metrics$val_mean_squared_error
  all_mse_histories <- rbind(all_mse_histories, mse_history)
}

average_mse_history <- data.frame(
  epoch = seq(1:ncol(all_mse_histories)),
  validation_mse = apply(all_mse_histories, 2, mean)
)

ggplot(average_mse_history, aes(x = epoch, y = validation_mse)) + geom_point()

model <- build_model()

model %>% fit(x_train, y_train,
              epochs = 60, batch_size = 1, verbose = 0)


result <- model %>% evaluate(x_test, y_test)
sqrt(result$mean_squared_error)
