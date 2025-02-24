# logistic regression to estimate FG success 
predict_fg_success <- function(FP) {
  # Ex. formula:
  plogis(-5 + 0.08 * FP)
}

#multinomial regression to estimate the probabilities

predict_4th_down_decision <- function(FP, YTG) {
  
  if (FP < 50) {
    go_prob <- 0.1
    fg_prob <- 0.1
    punt_prob <- 0.8
  } else if (FP < 70) {
    go_prob <- 0.3
    fg_prob <- 0.2
    punt_prob <- 0.5
  } else {
    go_prob <- 0.2
    fg_prob <- 0.5
    punt_prob <- 0.3
  }
  
  # adjust for large YTG
  if (YTG > 10) {
    go_prob <- go_prob * 0.5
    # fg_prob stays the same
    # punt_prob stays the same
  }
  
  total <- go_prob + fg_prob + punt_prob
  c(go_for_it = go_prob / total,
    fg        = fg_prob / total,
    punt      = punt_prob / total)
}

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
  
  # Sample yards gained
  yards_gained <- sample(-5:30, size = 1)
  
  # 2% chance of turnover
  forced_turnover <- runif(1) < 0.02
  
  # TD if FP >100
  new_FP <- FP + yards_gained
  is_touchdown <- (new_FP >= 100)
  
  # check for safety
  if (new_FP <= 0) {
    # if safety, exit drive
    list(D = D, YTG = YTG, FP = -5, exit_drive = 1)
  } else if (forced_turnover) {
    # exit drive for turnover
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
  
  # check for safety
  if (new_FP <= 0) {
    list(D = D, YTG = YTG, FP = -5, exit_drive = 1)
  } else if (forced_turnover) {
    # turnover
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
  
  # check for safety
  if (new_FP <= 0) {
    list(D = D, YTG = YTG, FP = -5, exit_drive = 1)
  } else if (forced_turnover) {
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
  
  decision_probs <- predict_4th_down_decision(FP, YTG)
  decision <- sample(c("go_for_it", "fg", "punt"), size = 1, prob = decision_probs)
  
  if (decision == "go_for_it") {
    # go for it if under 2 yds, see if attempt is successful
    # (We keep the same yardage distribution from your original code)
    yards_gained <- sample(-5:10, size = 1)  
    new_FP <- FP + yards_gained
    
    # 2% chance of forced turnover (as in original go-for-it code)
    forced_turnover <- runif(1) < 0.02
    is_touchdown <- (new_FP >= 100)
    
    # check for safety
    if (new_FP <= 0) {
      list(D = D, YTG = YTG, FP = -5, exit_drive = 1)
    } else if (forced_turnover) {
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
    
  } else if (decision == "fg") {
    # fg attempt 
    p_fg <- predict_fg_success(FP)
    made_fg <- runif(1) < p_fg
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
