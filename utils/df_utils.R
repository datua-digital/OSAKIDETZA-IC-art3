print('df utils OK')

#' longtowide. reshape function from base built in library
#'
#' @param df 
#' @param idvar_ the variable that identifies your groups
#' @param timevar_ the variables that will become multiple columns in wide format
#' @param v.names_ the variable containing the values that will be appended to v.names in wide format
#' @param direction_ wide/long
#'
#' @return
#' @export
#'
#' @examples
longtowide <- function(df, idvar_, timevar_, v.names_, direction_){
  
  return(reshape(df,
                 idvar = idvar_,
                 timevar = timevar_,
                 v.names = v.names_,
                 sep = "_",
                 direction = direction_)
         )
}

#' vector_tocollapsedstring
#'
#' @param vector 
#'
#' @return
#' @export
#'
#' @examples
vector_tocollapsedstring <- function(vector){
  return(paste(vector, sep="", collapse=","))
}


#' collapsedstring_tovector
#'
#' @param string 
#'
#' @return
#' @export
#'
#' @examples
collapsedstring_tovector <- function(string){
  
  return(unlist(strsplit(string, split=',')))
}



