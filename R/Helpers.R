
# load packages 
library(pacman)

pacman::p_load("haven", "readr", "dplyr", "tidyr", "hrbrthemes", "echarts4r", "shinydashboard", "shinydashboardPlus", "stringr",
               "shinyjs", "openxlsx", "DT", "htmlwidgets", "readxl", "ggplot2", "highcharter")


my_rowSums <- function(x) {
  if (is.data.frame(x)) x <- as.matrix(x)
  z <- base::rowSums(x, na.rm = TRUE)
  z[!base::rowSums(!is.na(x))] <- NA
  z
}




#071A2D: This is a very dark blue, almost black.

#14477A: This is a medium-dark blue. 

#F5F5F5: This is a very light gray, almost white. 

#DCC159: This is a muted gold color. 

#FFD469: This is a light, warm yellow. 

