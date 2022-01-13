#' Loads data in the cms format into R
#'
#' Data from a *.txt file will be read into the function so that it can be fed to the algorithm. For the tabular structure see the Vignette.
#'
#' @param file path and filename as a string (e.g. C:/testfolder/testdata.txt)
#' @param remove_this specification of columheaders that shall be removed from the data 
#' 
#' @importFrom utils read.table
#'
#' @return Data frame with the experimental raw data.
#'
#' @export
#'

cms_load <- function(file, remove_this=NULL){
  raw         <- read.table(file, header=T, fill = TRUE)
  raw         <- raw[,colSums(is.na(raw))<nrow(raw)]
  
  if(is.null(remove_this)){
  }else{
    raw         <- raw[!raw[,"groups"]== remove_this, ]
  }
   
  return(raw)
}