run_play <- function(D, YTG, FP) {
  if (D == 1) {
    out <- down_one(D, YTG, FP)
  } else if (D == 2) {
    out <- down_two(D, YTG, FP)
  } else if (D == 3) {
    out <- down_three(D, YTG, FP)
  } else {
    out <- down_four(D, YTG, FP)
  }
  out
}

# 1st down 
down_one <- function(D, YTG, FP) {
  
  yards_gained <- sample(-5:30, size = 1)
  
  # 2% chance of turnover
  forced_turnover <- runif(1) < 0.02
  
  # TD if FP >100
  new_FP <- FP + yards_gained
  is_touchdown <- (new_FP >= 100)
  
  # exit drive for turnover 
  if (forced_turnover) {
    list(D = D, YTG = YTG, FP = FP, exit_drive = 1)
  } else if (is_touchdown) {
    # If touchdown set FP=105 as a marker
    list(D = D, YTG = YTG, FP = 105, exit_drive = 1)
  } else {
    # check if first down earned 
    new_YTG <- YTG - yards_gained
    if (new_YTG <= 0) {
      # Reset downs to 1st and 10
      list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
    } else {
      # go to 2nd down 
      list(D = 2, YTG = new_YTG, FP = new_FP, exit_drive = 0)
    }
  }
}

# 2nd Down
down_two <- function(D, YTG, FP) {
  # Sample yards gained
  yards_gained <- sample(-5:30, size = 1)
  forced_turnover <- runif(1) < 0.02
  new_FP <- FP + yards_gained
  is_touchdown <- (new_FP >= 100)
  
  # turnover
  if (forced_turnover) {
    list(D = D, YTG = YTG, FP = FP, exit_drive = 1)
  } else if (is_touchdown) {
    # Td scenario
    list(D = D, YTG = YTG, FP = 105, exit_drive = 1)
  } else {
    # Check for a first down
    new_YTG <- YTG - yards_gained
    if (new_YTG <= 0) {
      # Reset downs
      list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
    } else {
      # go to to 3rd 
      list(D = 3, YTG = new_YTG, FP = new_FP, exit_drive = 0)
    }
  }
}

# 3rd Down 
down_three <- function(D, YTG, FP) {
  yards_gained <- sample(-5:30, size = 1)
  forced_turnover <- runif(1) < 0.02
  new_FP <- FP + yards_gained
  is_touchdown <- (new_FP >= 100)
  
  if (forced_turnover) {
    list(D = D, YTG = YTG, FP = FP, exit_drive = 1)
  } else if (is_touchdown) {
    list(D = D, YTG = YTG, FP = 105, exit_drive = 1)
  } else {
    new_YTG <- YTG - yards_gained
    if (new_YTG <= 0) {
      list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
    } else {
      # go to 4th 
      list(D = 4, YTG = new_YTG, FP = new_FP, exit_drive = 0)
    }
  }
}

# 4th Down
down_four <- function(D, YTG, FP) {
  
  if (YTG < 2) {
    # go for it if under 2 yds, see if attempt is successful
    yards_gained <- sample(-5:10, size = 1)  
    new_FP <- FP + yards_gained
    
    forced_turnover <- runif(1) < 0.02
    is_touchdown <- (new_FP >= 100)
    
    # check for turnover
    if (forced_turnover) {
      list(D = D, YTG = YTG, FP = FP, exit_drive = 1)
    } else if (is_touchdown) {
      list(D = D, YTG = YTG, FP = 105, exit_drive = 1)
    } else {
      # Check for conversion 
      if (yards_gained >= YTG) {
        # Conversion successful, go back to 1st 
        list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
      } else {
        # Didn’t convert, turnover 
        list(D = D, YTG = YTG, FP = new_FP, exit_drive = 1)
      }
    }
    
  } else if (FP > 70) {
    # fg attempt 
    # 55% rate for fg being good
    made_fg <- runif(1) < 0.55
    if (made_fg) {
      # good fg = fp of 115
      list(D = D, YTG = YTG, FP = 115, exit_drive = 1)
    } else {
      # Missed FG otherwise, turnover 
      list(D = D, YTG = YTG, FP = FP, exit_drive = 1)
    }
    
  } else if (FP > 80) {
    # fg attempt 
    # 65% rate for fg being good
    made_fg <- runif(1) < 0.65
    if (made_fg) {
      # good fg = fp of 115
      list(D = D, YTG = YTG, FP = 115, exit_drive = 1)
    } else {
      # Missed FG otherwise, turnover 
      list(D = D, YTG = YTG, FP = FP, exit_drive = 1)
    }    
    
  } else {
    # Punt
    punting_distance <- sample(30:50, 1)
    new_FP <- FP + punting_distance
    
    #possibility of muffed punt 5%
    muffed_punt <- runif(1) < 0.05
    
    if (muffed_punt) {
      #possession doesn't flip, new downs given 
      list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
    } else {
      # Normal punt, possession flips
      list(D = D, YTG = YTG, FP = new_FP, exit_drive = 1)
    }
  }
}
