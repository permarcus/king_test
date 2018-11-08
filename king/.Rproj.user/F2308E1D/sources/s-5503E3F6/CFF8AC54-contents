#' collect activity data from BIG google
#' 
#' @param cfg a King Kongfig
#' @param df a datafram containing the user ID needed for extraction of activity data
#' 
#' @export
collect_act_df <- function(cfg, df){
  checkmate::assert_class(cfg, "king_bot_args")
  checkmate::assert_class(df, "tbl_df")

  query_act = paste0("SELECT * FROM `king-ds-recruit-candidate-91.abtest.activity` ",
                     "WHERE playerid IN ", 
                     paste0("(", toString(df$playerid),")"))
  
  
  act_df <- king_query(project = cfg$io$google$project,
                       query = query_act)
  
  return(act_df)  
}