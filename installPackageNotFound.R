#setup: install packages if not installed already
installPackageNotFound <- function(package_name){

  possibleError <- tryCatch(
    suppressPackageStartupMessages(library(package_name, character.only = TRUE)),
    error=function(e) e
  )
  if(inherits(possibleError, "error")){
    install.packages(package_name, repos="http://cran.stat.ucla.edu")
    suppressPackageStartupMessages(library(package_name, character.only = TRUE))
  }
  
}