
sup_boot_rf <- function(dataframe, response_variable, predictores, boots, train_size, mtry, ntree, projection) {
  
      validation <- data.frame(MAE = numeric(boots), RMSE = numeric(boots))
         metrics <- data.frame(VarExpl = numeric(boots), MSR = numeric(boots))
      importance <- data.frame(Bootstrap = integer(), Variable = character(), IncNodePurity = numeric(), IncMSE = numeric())
        adjusted <- matrix(NA, nrow = nrow(dataframe), ncol = boots)
    future_preds <- if (!is.null(projection)) matrix(NA, nrow = nrow(projection), ncol = boots) else NULL
  

    for (i in 1:boots) {
       train_indices <- sample(1:nrow(dataframe), size = floor(train_size * nrow(dataframe)))
               train <- dataframe[train_indices, ]
                test <- dataframe[-train_indices, ]
        

        formulaRF <- as.formula(paste(response_variable, "~", paste(predictores, collapse = "+")))
            model <- randomForest(formulaRF, data = train, importance = TRUE, mtry = mtry, ntree = ntree)
        

                   all_preds <- predict(model, dataframe)
               adjusted[, i] <- all_preds 
        

                  teste_preds <- predict(model, test)
         validation[i, "MAE"] <- mean(abs(teste_preds - test[[response_variable]]))
        validation[i, "RMSE"] <- sqrt(mean((teste_preds - test[[response_variable]])^2))
        

        metrics[i, "VarExpl"] <- 100 * (1 - sum((model$y - model$pred)^2) / sum((model$y - mean(model$y))^2))
            metrics[i, "MSR"] <- model$mse[ntree]
        

               var_importance <- importance(model)
                   importance <- rbind(importance, data.frame(Bootstrap = i,
                                 Variable = rownames(var_importance),
                                 IncNodePurity = var_importance[, "IncNodePurity"],
                                 IncMSE = var_importance[, "%IncMSE"]))
        
          if (!is.null(projection)) {
            future_preds[, i] <- predict(model, newdata = projection)}
    }

    return(list(adjusted = adjusted, validation = validation, metrics = metrics, importance = importance, future_predictions = future_preds))
}
