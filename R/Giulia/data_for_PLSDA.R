#' @title data_for_PLSDA
#' @description
#' \code{\link{data_for_PLSDA}} Function for preparing dataset for PLSDA
#' @param data data.frame object with the variables (numeric or categorical) on which performing the PLSDA
#' 
#' @name data_for_PLSDA
#'
#' @return
#'  \item{final_list}{a data.frame object with results unlisted (with bindrow function}
#'  
#'  
#' @examples
#' \dontrun{
#' 
#' library(readxl)
#' 
#' # Reading the data
#' data<- read_excel("Data/examination_dataset1.xlsx")
#'  
#' # Using the function
#' 
#'
#'  }                             
#'
#'
#'  
#'  
#'  @export
data_for_PLSDA <- function(data, cat_var){
  
  suppressWarnings({
  require(mixOmics)
  require(tidyverse)
  require(dplyr)
  require(ggplot2)
      })
}

  results <- list()
  
  data <- data %>% 
    mutate(across(where(is.character), as.factor))
  
  
  data.num <- data %>% select_if(is.numeric)
  data.cat <- data %>% select_if(is.factor)
  
return(data.cat)


}

res <- data_for_PLSDA(data)

library(tidyverse)
