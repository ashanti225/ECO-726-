#Calculate F ratio for nested models
calculateFRatio <- function(full.model, reduced.model){
  #Get residual sum of squares and degrees of freedom
  full.sse <- sum(full.model$resid^2)
  full.df <- full.model$df.residual
  red.sse <- sum(reduced.model$resid^2)
  red.df <- reduced.model$df.residual
  
  #F ratio test
  f.ratio <- ((red.sse-full.sse)/(red.df-full.df))/(full.sse/full.df)
  
  #P value
  p.value <- pf(f.ratio, (red.df-full.df), full.df, lower.tail=F)
  
  return(c(f.ratio, p.value))
}
