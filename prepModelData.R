##Function to just prep the data, but not run the models
prepModelData <- function(dta, 
                          fbi.name = "jl.codes.mdc6", 
                          control.name = "control.jl.codes", 
                          years = 1995:1999,
                          control.grp = TRUE,
                          filter = FALSE){
  
  #subset data to years of interest
  dta <- subset(dta, year%in%years)
  
  #standardize geographical unit name
  names(dta)[names(dta)%in% c("zip", "county")] <- "geo"
  
  #if filter is true, use filtered version of variables
  if(filter){
    fbi.name <- paste0(fbi.name,".filter")
    control.name <- paste0(control.name, ".filter")
  }
  
  #log count (add one to deal with zeros)
  dta$logCount <- log(dta[[fbi.name]] + 1)
  
  #add control group  
  if(control.grp){
    dta$logCountControl <- log(dta[[control.name]] + 1)
    melt.measures <- c("logCountControl", "logCount")
  }else{
    melt.measures <- c("logCount")
  }
  
  ## Melt
  dta <- melt(dta, measure.vars = melt.measures, 
              variable.name = "disease.type.label", value.name = "logCount")
  dta$disease.type <- ifelse(dta$disease.type.label=="logCount", 1, 0)
  dta$geo.type.pk <- apply(cbind(dta$geo, dta$disease.type),1,paste,collapse="-")
  dta$year.qtr.type <- apply(cbind(dta$year.qtr, dta$disease.type),1,paste,collapse="-")
  
  return(dta)
}