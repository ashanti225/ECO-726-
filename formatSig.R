## Function to format number to a specific number of sig digits
formatSig <- function(number, n){
  return(formatC(round(number, n), digits = n, format="f", flag="#"))
}