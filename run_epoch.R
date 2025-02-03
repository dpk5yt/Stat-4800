run_epoch <- function(down, ytg, fp, max_drives) {
  
  
  # team = 1= home 
  # team = -1 = opponent
  team <- 1
  
  drive_count <- 0
  
  current_state <- list(down = down, ytg = ytg, fp = fp)
  
  # until  a score or exceed max drives
  while (drive_count < max_drives) {
    
    drive_count <- drive_count + 1
    
    # Sim a drive 
    next_state <- run_drive(
      down = current_state$down,
      ytg  = current_state$ytg,
      fp   = current_state$fp
    )
    
    # Check if someone scored 
    pts <- get_EP(next_state$fp)
    
    if (!is.na(pts)) {
      # assign score to team
      return(pts * team)
    }
    
    # If no score, reset and give to the other team
    current_state <- next_state
    team <- team * -1   # Switch from 1 to -1, or -1 to 1
    
  } 
  
  
  return(0)
}