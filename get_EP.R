get_EP <- function(fp) {
  # fp goes 0-120
  #  under 100 = in the field (no score change)
  #  100-110= TD
  #  110-120= FG
  
  if (fp > 100 && fp <= 110) {
    return(7)
  } else if (fp > 110 && fp <= 120) {
    return(3)
  } else if (fp <0) {
    return(-2)
  }else {
    
    # No score
    return(NA)
  }
}
