#' King Tables exists
#' 
#' @param cfg king kongfig containing all tables to check
#' 
#' @export
check_king_tables <- function(cfg){
  checkmate::assert_class(cfg, "king_bot_args")
  
  for(i in 1:length(cfg$io$google$table)){
    cat(paste(i, ": "))
    cat(paste(cfg$io$google$table[i]), ": ")
    cat(paste(bigrquery::exists_table(project = cfg$io$google$project, 
                                      dataset = cfg$io$google$dataset, 
                                      table = cfg$io$google$table[i])), " \n")
  }
  
}