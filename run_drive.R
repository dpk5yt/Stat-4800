source("run_play.R")

run_drive <- function(D, YTG, FP) {
  # list to store number of plays 
  states_log <- list()
  
  exit_drive <- 0
  
  # start wit state provided
  current_state <- list(D = D, YTG = YTG, FP = FP, exit_drive = exit_drive)
  
  # Loop until drive ends
  while (current_state$exit_drive == 0) {
    
    # Run a drive using the current down, yards-to-gain, field position.
    next_state <- run_play(
      D   = current_state$D, 
      YTG = current_state$YTG, 
      FP  = current_state$FP
    )
    
   
    states_log[[length(states_log) + 1]] <- next_state
    
    # exit drive = 0, update current state and continue.
    if (next_state$exit_drive == 0) {
      current_state <- next_state
    } else {
      # The drive is finished
      current_state <- next_state
    }
  }
  
  return(list(
    final_state = list(
      D   = current_state$D,
      YTG = current_state$YTG,
      FP  = current_state$FP
    ),
    states_log = states_log
  ))
}

