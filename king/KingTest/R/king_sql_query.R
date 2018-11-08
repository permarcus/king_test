#' King query
#' 
#' @param query SQL query for calling BIGdata
#' @param project the project from which data should be collected
#' 
#' @export
king_query <- function(project, query){
  checkmate::assert_character(query)
  checkmate::assert_character(project)

  tb <- bq_project_query(x = cfg$io$google$project, 
                         query = query)
  
  data <- bq_table_download(tb)
  
  return(data)
    
}