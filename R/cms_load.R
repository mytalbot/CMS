#' Loads Data 
#'
#' Data from a *.txt file will be read into the function so that it can be fed to the algorithm.
#'
#' @param file path and filename as a string (e.g. C:/testfolder/testdata.txt)
#' @param remove_this specification of columheaders that shall be removed from the data 
#' 
#' @importFrom utils read.table
#'
#' @return data.frame
#'
#' @export
#'

cms_load <- function(file, remove_this=NA){
  raw         <- read.table(file, header=T)
  raw         <- raw[,colSums(is.na(raw))<nrow(raw)]
  raw         <- raw[!raw[,"groups"]== remove_this, ]
  return(raw)
}