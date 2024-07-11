KNN_Model <- function(pp, data, training_data, tuning = F, vector = NULL){

  training_data <- training_data |> drop_na()

  # Preprocess the data using the recipe
  pp <- pp |> prep(data = data)

  # Fit the full model
  full_model <- lm(pp, data = training_data)

  # Stepwise backward selection (fit AIC)
  step_model <- stepAIC(full_model, direction = "backward")

  # Get the selected formula
  selected_formula <- formula(step_model)

  print('generate new recipe')
  pp <- recipes::recipe(selected_formula,
                        data = data) |>
    # Yeo-Johnsen transformation (includes Box Cox and an extansion. Now it can handle x ≤ 0)
    recipes::step_YeoJohnson(all_numeric(), -all_outcomes()) |>
    # subsracting the mean from each observation/measurement
    recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
    # transforming numeric variables to a similar scale
    recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())

  cores <- makeCluster(detectCores())
  registerDoParallel(cores)

  if(tuning == F){
    print('The model is currently being generated with k = 10. Please be patient...')
    model <- caret::train(pp, data = training_data |> tidyr::drop_na(),
                        # We want a KNN model
                        method = "knn",
                        # we use cross validation as method
                        trControl = caret::trainControl(method = "cv",
                                                        number = 3,
                                                        savePredictions = "final"),
                        parallel = 'foreach',
                        # we set k = k to optimize the hyperparameter k. We substitute it later with a vector
                        tuneGrid = data.frame(k = 10),
                        # we want the RMSE as our metrics
                        metric = "RMSE")
    }else{print('The model is now in the tuning process. Please be patient...')
        model <- caret::train(pp, data = training_data |> tidyr::drop_na(),
                        # We want a KNN model
                        method = "knn",
                        # we use cross validation as method
                        trControl = caret::trainControl(method = "cv",
                                                        number = 3,
                                                        savePredictions = "final"),
                        parallel = 'foreach',
                        # we set k = k to optimize the hyperparameter k. We substitute it later with a vector
                        tuneGrid = data.frame(k = vector),
                        # we want the RMSE as our metrics
                        metric = "RMSE")

        best.tune <- knn_model$bestTune$k
        print(paste('The model was fine-tuned with values of',tuning,'The optimal k is now:',best.tune))
        }


  return(model)
}
