### Functions

## Create census divisions ----------------------------

divisioning <- function(variable){
  
  x <- variable
  
  x[x %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")] <-  "New England"
  x[x %in% c("New Jersey", "New York", "Pennsylvania")] <- "Middle Atlantic"
  x[x %in% c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin")] <-   "East North Central"
  x[x %in% c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")] <- "West North Central"
  x[x %in% c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia")] <- "South Atlantic" 
  x[x %in% c("Alabama", "Kentucky", "Mississippi", "Tennessee")] <- "East South Central"
  x[x %in% c("Arkansas", "Louisiana", "Oklahoma", "Texas")] <- "West South Central"
  x[x %in% c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana", "Utah", "Wyoming", "Nevada")] <- "Mountain"
  x[x %in% c("Alaska", "California", "Hawaii", "Oregon", "Washington")] <- "Pacific"
  
  census_division <- x
  
  return(x)
}