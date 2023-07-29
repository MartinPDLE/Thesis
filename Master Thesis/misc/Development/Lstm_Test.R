install.packages("keras")
library(keras)

dat <- runif(n=1000, min = 1, max = 50)
dat <- as.matrix(dat)
dat <- scale(dat)

train_size <- floor(0.7* nrow(dat))
train_data <- dat[1:train_size,]
test_data <- dat[(train_size +1):nrow(dat), ]

create_sequences <- function(dat, seq_length) {
  sequences <- matrix(0, nrow = nrow(dat) - seq_length, ncol = seq_length)
  targets <- matrix(0, nrow = nrow(dat) - seq_length)
  
  for (i in 1:(nrow(dat) - seq_length)) {
    sequences[i,] <- dat[i:(i + seq_length -1), ]
    targets[i] <- dat[i + seq_length, ]
  }
  
  return(list(sequences, targets))
}

seq_length <- 10
train_seq <- create_sequences(train_data, seq_length)
test_seq <- create_sequences(test_data, seq_length)

model <- keras_model_sequential()
model%>%
  layer_lstm(units = 50, input_shape= c(seq_length,1)) %>%
  layer_dense(units = 1)

model%>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam()
)

history <- model %>% fit(
  train_seq$sequences, train_size$targets,
  epochs = 10,
  batch_size =32,
  validation_split = 0.2
)

predictions <- model %>% predict(test_seq$sequences)

