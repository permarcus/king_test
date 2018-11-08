#' Statistics 
#' 
#' @param dat a data set containg all data
#' 
#' @export
statistics_king <- function(dat){
  checkmate::assert_list(dat)
  
  data_set <- dplyr::bind_rows(dat$train, dat$test)
  group_a <- dplyr::filter(data_set, abtest_group == "A")
  group_b <- dplyr::filter(data_set, abtest_group == "B")
  
  king_ttest_conv <- t.test(group_a$conversion, group_b$conversion)
  king_ttest_purchase <- t.test(group_a$purchases, group_b$purchases)
  
  king_statistics <- list()
  king_statistics$king_ttest_conv <- king_ttest_conv
  king_statistics$king_ttest_purchases <- king_ttest_purchase
  
  return(king_statistics)
}