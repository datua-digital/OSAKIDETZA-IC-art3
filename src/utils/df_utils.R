print('df utils OK')

#' longtowide. reshape function from base built in library
#'
#' @param df 
#' @param idvar_ the variable that identifies your groups
#' @param timevar_ the variables that will become multiple columns in wide format
#' @param v.names_ the variable containing the values that will be appended to v.names in wide format
#'
#' @return df reshaped in wide format
longtowide <- function(df, idvar_, timevar_, v.names_, direction_){
  
  return(reshape(df,
                 idvar = idvar_,
                 timevar = timevar_,
                 v.names = v.names_,
                 sep = "_",
                 direction = 'wide')
         )
}

#' vector_tocollapsedstring
#'
#' @param vector (character vector)
#'
#' @return character
vector_tocollapsedstring <- function(vector){
  return(paste(vector, sep = "", collapse = ","))
}


#' collapsedstring_tovector
#'
#' @param string (character)
#'
#' @return character vector
#' @export
#'
#' @examples
collapsedstring_tovector <- function(string){
  
  return(unlist(strsplit(string, split = ',')))
}

#' deletemultiplecolumns
#'
#' @param df (data.frame)
#' @param columns (character vector) columns to be removed
#' 
#' @return (data.frame) df without 'columns' columns
deletemultiplecolumns <- function(df, columns){
  df[ ,columns] <- list(NULL)
  return(df)
}

