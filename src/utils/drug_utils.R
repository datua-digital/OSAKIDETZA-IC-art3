#' get_principio_activo: get active principle of some families
#'
#' @param drugs: (character) Global variable DRUGS
#'
#' @return ppa active principles of dlobal variable DRUGS

# TODO: Asegurar que estos son solo arm...
get_principio_activo <- function(drugs) {
  ppa <- c()
  if (c("arm") %in% drugs) {
    ppa <- c(ppa, c("espironolactona", "eplerenona"))
  }
  if (c("arm") %in% drugs) {
    ppa <- c(ppa, c("dapagliflozina", "empagliflozina"))
  }
  return(ppa)
}
