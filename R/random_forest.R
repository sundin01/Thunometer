random_forest <- function(pp, data, training_data, tuning = FALSE) {
  # Preprocess the data using the recipe
  pp <- pp |> prep(data = data)

  # Remove rows with missing values
  training_data <- training_data |> drop_na()

  # Fit the full model
  full_model <- lm(pp, data = training_data)

  # Stepwise backward selection (fit AIC)
  step_model <- stepAIC(full_model, direction = "backward")

  # Get the selected formula
  selected_formula <- formula(step_model)

  print('generate new recipe')
  pp <- recipes::recipe(selected_formula, data = data) |>
    # Yeo-Johnson transformation (includes Box Cox and an extension. Now it can handle x ≤ 0)
    recipes::step_YeoJohnson(all_numeric(), -all_outcomes()) |>
    # Subtracting the mean from each observation/measurement
    recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
    # Transforming numeric variables to a similar scale
    recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())

  # Set up parallel processing
  cores <- makeCluster(detectCores())
  registerDoParallel(cores)

  # Define the tuning grid for glmnet
  if (tuning == FALSE) {
    print('The model is currently being generated with default alpha and lambda. Please be patient...')
    grid <- expand.grid(
      alpha = 0,
      lambda = seq(0.0001, 1, length = 100)
    )
  } else {
    print('The model is now in the tuning process. Please be patient...')
    grid <- expand.grid(
      alpha = seq(0, 1, length = 10),
      lambda = seq(0.0001, 1, length = 100)
    )
  }

  mod_cv <- caret::train(
    pp,
    data = training_data,
    method = "glmnet",
    metric = "RMSE",
    trControl = trainControl(
      method = "cv",
      number = 3,
      savePredictions = "final"
    ),
    parallel = 'foreach',
    tuneGrid = grid
  )

  stopCluster(cores)
  return(mod_cv)
}
