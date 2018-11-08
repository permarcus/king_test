#' King config assertion
#' 
#' @param args character vector to assert.
#' 
#' @export
set_king_config <- function(args){
  checkmate::assert_list(args)
  
    class(args) <- c("king_bot_args", class(args))
    return(args)
}  