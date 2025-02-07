

# Source the play-level functions 
source("run_play.R")

run_drive <- function(D, YTG, FP) {
  
  # new state 
  new_state <- run_play(D, YTG, FP)
  
  # Check to continue or return
  if (new_state$exit_drive == 0) {
    # call with updated state
    run_drive(new_state$D, new_state$YTG, new_state$FP)
  } else {

    list(D = new_state$D, YTG = new_state$YTG, FP = new_state$FP)
  }
}

