#' Predictions for king
#' 
#' @param king_model the models produced for the data
#' @param prediction_data the dataset which is going to be used in prediction
#' 
#' @export
predict <- function(king_model, prediction_data){
  checkmate::assert_list(king_model)
  checkmate::assert_class(prediction_data, "tbl_df")

  prediction_data <- dplyr::group_by(prediction_data, playerid)
  prediction_data <- dplyr::arrange(prediction_data, playerid, desc(activity_date))
  prediction_data <- dplyr::slice(prediction_data, 1)
  prediction_data <- dplyr::ungroup(prediction_data)
  
  test_a <- dplyr::filter(prediction_data, abtest_group == "A")
  test_data_a <- dplyr::select(test_a, tot_gameends)
  pred_a <- stats::predict(king_model$a, newdata = test_data_a, type = c("response"))
  
  test_b <- dplyr::filter(prediction_data, abtest_group == "B")
  test_data_b <- dplyr::select(test_b, tot_gameends)
  pred_b <- stats::predict(king_model$b, newdata = test_data_b, type = c("response"))
  
  test_a$pred_a <- pred_a
  test_b$pred_b <- pred_b
    
  predictions <- list()
  predictions$a <- test_a
  predictions$b <- test_b

  return(predictions)
  
  }