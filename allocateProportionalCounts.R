allocateProportionalCounts <- function(this.data, vars, prop.var){
  
  #Create reverse of proportional variable for CA
  this.data$prop.ca <- 1-this.data[[c(prop.var)]]

  #Loop through vars
  for(this.var in vars){
    this.data$this.var.la <- this.data[[c(prop.var)]]*this.data[[c(this.var)]]
    this.data$this.var.ca <- this.data$prop.ca*this.data[[c(this.var)]]
    
    #Take weighted averages of LA and CA counts by year and disease type
    log.count.fbi.sum <- aggregate(formula(paste0("cbind(this.var.la, this.var.ca,", prop.var, ", prop.ca) ~ year + disease.type")), 
                                   data=this.data, FUN=sum)
    log.count.fbi.sum$this.var_0 <- log.count.fbi.sum$this.var.ca/log.count.fbi.sum$prop.ca
    log.count.fbi.sum$this.var_1 <- log.count.fbi.sum$this.var.la/log.count.fbi.sum[[c(prop.var)]]
    
    #Reshape to long by group
    log.count.fbi.sum <- reshape(log.count.fbi.sum,
                                 idvar = c("year", "disease.type"),
                                 direction = "long",
                                 drop = c("this.var.la", "this.var.ca",prop.var, 
                                          "prop.ca"),
                                 varying = c("this.var_0", "this.var_1"),
                                 sep = "_")
    #Rename groups
    group.name <- gsub("prop.", "", prop.var, fixed = TRUE)
    names(log.count.fbi.sum)[names(log.count.fbi.sum)=="this.var"] <- this.var
    names(log.count.fbi.sum)[names(log.count.fbi.sum)=="time"] <- group.name
    
    #Save
    if(this.var == vars[1]){
      final.df <- log.count.fbi.sum
    }else{
      final.df <- merge(final.df, log.count.fbi.sum, by = c("year", "disease.type", group.name))
    }
    
  }
  
  return(final.df)
  
}

