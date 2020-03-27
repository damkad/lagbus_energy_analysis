library(lubridate)
library(tidyverse)
#ata = readxl::excel_sheets("thsc-nhts17-caltrans-codebook.xlsx")

data = readxl::read_excel("lagbus.xlsx", sheet = "2016")

data = drop_na(data)

data$OD = paste(data$ORIGIN, data$DESTINATION, sep = "-")


remove_year = function(x){
  y = str_split(x, " ")
  return (y[[1]][2])
}


data$`AVERAGE TIME TAKEN PER TRIP` = lapply(data$`AVERAGE TIME TAKEN PER TRIP`, remove_year)

data$`AVERAGE TIME TAKEN PER TRIP` = hms(data$`AVERAGE TIME TAKEN PER TRIP`)



clean_origin = function(location){
  if (str_detect(location, "[-]") == TRUE){
    z_ = str_split(location, "[-]")
    
    if (str_detect(z_[[1]][1], "[()]") == TRUE) {
      z = str_split(z_[[1]][1], "[()]")
      zz = str_c(z[[1]][1], z[[1]][2], sep=", ")
      
      return(zz)
      
    } else {return (z_[[1]][1])}
  } else {
    if (str_detect(location, "[()]") == TRUE){
      z = str_split(location, "[()]")
      zz = str_c(z[[1]][1], z[[1]][2], sep=", ")
      
      return(zz)
    }
    
  } 
  return(location)
  
  
}

clean_destination = function(location){
  location = str_replace(location, "^[()]", "")
  
  if (str_detect(location, "[-]") == TRUE && (location != "U-TURN" && location != "IYANA-IPAJA")){
    z_ = str_split(location, "[-]")
    
    if (str_detect(z_[[1]][2], "[()]") == TRUE) {
      z = str_split(z_[[1]][2], "[()]")
      zz = str_c(z[[1]][1], z[[1]][2], sep=", ")
      
      return(zz)
      
    } else {return (z_[[1]][2])}
  } else {
    if (str_detect(location, "[()]") == TRUE){
      z = str_split(location, "[()]")
      zz = str_c(z[[1]][1], z[[1]][2], sep=", ")
      
      return(zz)
    }
    
  } 
  return(location)
  
}


data$ORIGIN_clean <- lapply(data$ORIGIN, clean_origin)
data$DESTINATION_clean <- lapply(data$DESTINATION, clean_destination)



