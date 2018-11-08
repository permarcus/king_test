#' Collect data from Google BigQuery
#' 
#' @param project The google project which data will be collected from
#' @param dataset The data set from which the tables will be choosen
#' @param table The tables of choice from which one can colect data
#' @param rows Simple function for number of rows to extract, if NULL al rows will be collected
#' 
#' @export
data_tables <- function(project, dataset, table, rows = NULL){
  checkmate::assert_character(project)
  checkmate::assert_character(dataset)
  checkmate::assert_character(table)
  checkmate::assert_int(rows, null.ok = TRUE)

    # Project name
    project <- project

    # Set tables to work with
    table <- bigrquery::bq_table(project = project, dataset = dataset, table = table)
    
    
    # Query dataset
    table <- bigrquery::bq_table_download(x = table, max_results = rows)
    
    # Return from function
    return(table)
}