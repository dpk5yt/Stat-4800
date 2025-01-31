run_drive <- function(down, ytg, fp) {
 
  # Random fp
  new_fp <- runif(1, min = 0, max = 120)
  
  #starting new set of downs 
  new_down <- 1
  new_ytg  <- 10
  
  # Return new states 
  return(list(down = new_down, ytg = new_ytg, fp = new_fp))
}
