library(dplyr)


data <- readRDS("data.rds")



#transition matrix 

# 2a) Identify unique teams
teams <- sort(unique(c(data$Home_Team, data$Visiting_Team)))
n     <- length(teams)

# lookup system 
team_index <- setNames(seq_along(teams), teams)

# 2b)  loss count matrix
# M[i, j] = number of times i lost to  j
M <- matrix(0, nrow = n, ncol = n)


# Loop over each game 
for (k in seq_len(nrow(data))) {
  
  home_team <- data$Home_Team[k]
  away_team <- data$Visiting_Team[k]
  home_idx  <- team_index[home_team]
  away_idx  <- team_index[away_team]
  
  home_score <- data$Home_Score[k]
  away_score <- data$Visiting_Score[k]
  
  # If home lost 
  if (home_score < away_score) {
    M[home_idx, away_idx] <- M[home_idx, away_idx] + 1
  }
  # If away lost
  if (away_score < home_score) {
    M[away_idx, home_idx] <- M[away_idx, home_idx] + 1
  }
}

# 2e) Convert  raw counts to normalized prob
Tmat <- M
for (i in seq_len(n)) {
  row_sum <- sum(Tmat[i, ])
  if (row_sum > 0) {
    Tmat[i, ] <- Tmat[i, ] / row_sum
  } else {
    
    Tmat[i, ] <- 1 / n
  }
}


# 3. MARKOV CHAIN STEADY STATE 


#uniform distribution 
b <- rep(1/n, n)

#  Multiply b by Tmat 
niter <- 10000
for (step in 1:niter) {
  b <- b %*% Tmat
}

b_steady <- as.numeric(b)

# Rank teams 
ranking_all_years <- order(b_steady, decreasing = TRUE)
team_ranks        <- teams[ranking_all_years]


cat("Steady state distribution (all years):\n")
print(b_steady)
cat("\nRanking of teams (best to worst):\n")
print(team_ranks)

#steady state( [1] 0.03219857 0.03200487 0.03445328 0.03316139 0.03584793 0.02930756 0.03489770 0.03064543
#[9] 0.03454301 0.03045380 0.02959835 0.03721329 0.02999425 0.03948072 0.02969201 0.03498321
#[17] 0.03187847 0.03830918 0.03341753 0.03197763 0.03243858 0.03138499 0.03235099 0.03394242
#[25] 0.03400499 0.03536689 0.03660620 0.03184672 0.03532205 0.03267800)

# team rankings( [1] "LAN" "NYA" "HOU" "TBA" "BOS" "SLN" "TOR" "MIL" "CHN" "CLE" "ATL" "SFN" "SEA" "NYN" "BAL"
# [16] "WAS" "PHI" "SDN" "ANA" "ARI" "OAK" "MIN" "TEX" "PIT" "CIN" "COL" "KCA" "MIA" "DET" "CHA" )


# we have decided to see how team rankings have changed year to year

years <- sort(unique(data$season))

rankings_by_year <- list()

for (yr in years) {
  #  Subset data for that year
  data_yr <- data %>% filter(season == yr)
  
  # Build the matrix  of losses
  M_yr <- matrix(0, nrow = n, ncol = n)
  
  for (k in seq_len(nrow(data_yr))) {
    hteam <- data_yr$Home_Team[k]
    ateam <- data_yr$Visiting_Team[k]
    hidx  <- team_index[hteam]
    aidx  <- team_index[ateam]
    
    hscore <- data_yr$Home_Score[k]
    ascore <- data_yr$Visiting_Score[k]
    
    if (hscore < ascore) M_yr[hidx, aidx] <- M_yr[hidx, aidx] + 1
    if (ascore < hscore) M_yr[aidx, hidx] <- M_yr[aidx, hidx] + 1
  }
  
  # Normalize by row
  Tmat_yr <- M_yr
  for (i in seq_len(n)) {
    row_sum <- sum(Tmat_yr[i, ])
    if (row_sum > 0) {
      Tmat_yr[i, ] <- Tmat_yr[i, ] / row_sum
    } else {
      Tmat_yr[i, ] <- 1 / n
    }
  }
  
  # Compute steady state
  b_yr <- rep(1/n, n)
  for (step in 1:niter) {
    b_yr <- b_yr %*% Tmat_yr
  }
  
  b_yr <- as.numeric(b_yr)
  
  #  Sort teams by that year's steady‐state prob
  ranking_yr <- order(b_yr, decreasing = TRUE)
  team_rank_yr <- teams[ranking_yr]
  
  # Store results
  rankings_by_year[[as.character(yr)]] <- list(
    year         = yr,
    steady_state = b_yr,
    ranking      = team_rank_yr
  )
}

# print the top 5 teams for each year
cat("\nYear-by-Year Top 5:\n")
for (yr in names(rankings_by_year)) {
  cat("\nYear:", yr, "\n")
  # Print top 5
  cat(head(rankings_by_year[[yr]]$ranking, 5), sep = ", ")
  cat("\n-------------------------------------\n")
}

# rankings through the years, a vizulation will be attached in github:
#Year: 2014 
#BAL, ANA, SEA, DET, OAK
#-------------------------------------

#  Year: 2015 
#KCA, TOR, SLN, CHN, PIT
#-------------------------------------

#  Year: 2016 
#TEX, BOS, TOR, BAL, CLE
#-------------------------------------

#  Year: 2017 
#CLE, HOU, LAN, BOS, ARI
#-------------------------------------

#  Year: 2018 
#HOU, BOS, NYA, LAN, COL
#-------------------------------------

#  Year: 2019 
#LAN, ATL, MIL, HOU, SLN
#-------------------------------------

#  Year: 2020 
#LAN, SDN, TBA, MIN, CLE
#-------------------------------------

#  Year: 2021 
#HOU, TBA, NYA, SEA, BOS
#-------------------------------------

#  Year: 2022 
#LAN, HOU, NYA, NYN, ATL
#-------------------------------------

#  Year: 2023 
#ATL, BAL, LAN, TBA, PHI
#-------------------------------------

#  Year: 2024 
#LAN, PHI, SDN, MIL, ATL
#-------------------------------------

#using this we can see which teams are consistently performing at the top of the leauge. 


