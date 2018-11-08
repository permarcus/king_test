#' Get king kongfig
#' 
#' @param cfg_path location to construct
#' 
#' @export
get_king_kongfig <- function(cfg_path){
  checkmate::assert_file_exists(cfg_path)
  
  cfg <- yaml::read_yaml(file = cfg_path)
  
  cfg <- set_king_config(cfg)
  
  return(cfg)
}