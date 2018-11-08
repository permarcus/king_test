#' King kongversion proportion
#' 
#' @param cfg a king kongfig
#' 
#' @export
king_konv_prop <- function(cfg){
  checkmate::assert_class(cfg, "king_bot_args")
  
  conv_prop_tot <- paste0("SELECT COUNT(conversion_date IS NULL) AS tot, (conversion_date IS NULL) " , 
                      "AS isnull FROM `king-ds-recruit-candidate-91.abtest.assignment` ",
                      "GROUP BY isnull")
  
  conv_prop_a <- paste0("SELECT COUNT(conversion_date IS NULL) AS a, (conversion_date IS NULL) " , 
                          "AS isnull FROM `king-ds-recruit-candidate-91.abtest.assignment` ",
                          "WHERE abtest_group = 'A' GROUP BY isnull")
  
  conv_prop_b <- paste0("SELECT COUNT(conversion_date IS NULL) AS b, (conversion_date IS NULL) " , 
                          "AS isnull FROM `king-ds-recruit-candidate-91.abtest.assignment` ",
                          "WHERE abtest_group = 'B' GROUP BY isnull")
  
  
  conv_tot <- king_query(project = cfg$io$google$project,
                         query = conv_prop_tot)
  
  conv_a   <- king_query(project = cfg$io$google$project,
                         query = conv_prop_a)
  
  conv_b   <- king_query(project = cfg$io$google$project,
                         query = conv_prop_b)
  
  conv_rates <- dplyr::left_join(conv_tot, conv_a, by = "isnull")
  conv_rates <- dplyr::left_join(conv_rates, conv_b, by = "isnull")  
  conv_rates <- dplyr::select(conv_rates, isnull, tot, a, b)

  add <- as.data.frame(c(conv_rates[2, 2:4]/(conv_rates[2, 2:4]+conv_rates[1, 2:4])))
  
  conv_rates <- dplyr::bind_rows(conv_rates, add)
    
  return(conv_rates)  
  
}