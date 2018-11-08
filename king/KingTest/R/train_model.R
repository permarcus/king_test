#' Train an arbitrary model
#'
#' @param train_set a training data set
#' @param cfg a \code{king_kongfig} object.
#' 
#' @export
train_model <- function(train_set, cfg){
  checkmate::assert_class(data_set, "tbl_df")
  checkmate::assert_class(cfg, "king_bot_args")
  
  # Remove company_id, sale_process_stage, company_to and company_from
  train_set <- king_df_list$train
  train_set <- dplyr::group_by(train_set, playerid)
  train_set <- dplyr::arrange(train_set, playerid, desc(activity_date))
  train_set <- dplyr::slice(train_set, 1)
  train_set <- dplyr::ungroup(train_set)
  a_train <- dplyr::filter(train_set, abtest_group == "A")
  tot_gameends <- a_train$tot_gameends
  log_reg_a <- stats::glm(a_train$conversion ~ tot_gameends, family = stats::binomial(link = "logit"))
  
  
  b_train <- dplyr::filter(train_set, abtest_group == "B")
  tot_gameends <- b_train$tot_gameends
  log_reg_b <- stats::glm(b_train$conversion ~ tot_gameends, family = stats::binomial(link = "logit"))
  
  class(log_reg_a) <- c("king_model", class(log_reg_a))
  class(log_reg_b) <- c("king_model", class(log_reg_b))
  
  mod <- list()
  mod$a <- log_reg_a
  mod$b <- log_reg_b
  
  return(mod)
}

