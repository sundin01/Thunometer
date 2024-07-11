# Define the function for stepwise backward selection and training the model
LM_Model <- function(pp, data, training_data) {

  # Remove rows with missing values
  training_data <- training_data |> drop_na()

  # Preprocess the data using the recipe
  pp <- pp |> prep(data = data)

  # Fit the full model
  full_model <- lm(pp, data = training_data)

  # Stepwise backward selection (fit AIC)
  step_model <- stepAIC(full_model, direction = "backward")

  # Get the selected formula
  selected_formula <- formula(step_model)

  # Set up parallel processing
  cores <- makeCluster(detectCores())
  registerDoParallel(cores)

  # Train the model using the selected features
  mod_cv <- caret::train(
    selected_formula, data = training_data,
    method = "glmnet",
    trControl = caret::trainControl(method = "cv", number = 3, savePredictions = "final"),
    metric = "RMSE",
    tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 100))
  )

  # Stop parallel processing
  stopCluster(cores)

  return(mod_cv)
}
