#' Modelling data set
#' 
#' @param ass_df assignment data frame
#' @param act_df activity data frame
#' @param train_prop proportion of data to be used in the modelling
#' 
#' @export
modelling_df <- function(act_df, ass_df, train_prop){
  checkmate::assert_class(ass_df, "tbl_df")
  checkmate::assert_class(act_df, "tbl_df")
  checkmate::assert_numeric(train_prop)
  
  modell_df <- dplyr::left_join(act_df, ass_df, by = "playerid")
  
  modell_df <- dplyr::group_by(modell_df, playerid)
  modell_df <- dplyr::arrange(modell_df, playerid, desc(activity_date))
  modell_df <- dplyr::mutate(modell_df, tot_gameends = sum(gameends))
  modell_df <- dplyr::mutate(modell_df, tot_purchases = sum(purchases))
  
  train_test <- dplyr::slice(modell_df, 1)
  train_test <- dplyr::ungroup(train_test)
  train <- dplyr::sample_frac(train_test, size = train_prop)
  test  <- dplyr::anti_join(train_test, train, by = 'playerid')
  
  
  modell_df <- dplyr::ungroup(modell_df)
  
  modell_df <- dplyr::select(modell_df, 
                             playerid,
                             install_date,
                             assignment_date,
                             conversion_date,
                             activity_date,
                             purchases,
                             tot_purchases, 
                             gameends,
                             tot_gameends,
                             abtest_group,   
                             conversion
                             )
  
  modell_train <- dplyr::filter(modell_df, playerid %in% train$playerid)
  modell_test <- dplyr::filter(modell_df, playerid %in% test$playerid)
  
  modell_df <- list()
  modell_df$train <- modell_train
  modell_df$test <- modell_test
  
  return(modell_df)
}