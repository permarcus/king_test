#' Collect assignment data from BigGoogle
#' 
#' @param cfg a King Kongfig object
#' @param rows integer of wanted number of rows in query
#' 
#' @export
collect_ass_df <- function(cfg, rows){
  checkmate::assert_class(cfg, "king_bot_args")
  
  query_null = paste0("SELECT * FROM `king-ds-recruit-candidate-91.abtest.assignment`", 
                      " WHERE conversion_date IS NULL LIMIT ", rows/2)
  
  query_not_null = paste0("SELECT * FROM `king-ds-recruit-candidate-91.abtest.assignment`", 
                          " WHERE conversion_date IS NOT NULL LIMIT ", rows/2)
  
  
  # Collect data for specified data
  assignment_null <- king_query(project = cfg$io$google$project,
                                query = query_null)
  
  assignment_not_null <- king_query(project = cfg$io$google$project,
                                    query = query_not_null)
  
  assignment <- dplyr::bind_rows(assignment_not_null, assignment_null)
  assignment$conversion[!is.na(assignment$conversion_date)] <- 1
  assignment$conversion[is.na(assignment$conversion_date)] <- 0
  
  return(assignment)
  
  }