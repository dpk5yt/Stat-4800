
game_data <- readRDS("data.rds")

# create matrix
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
team_index <- setNames(seq_along(teams), teams)
n_teams <- length(teams)

# transition matrix
transition_matrix <- matrix(0, nrow = n_teams, ncol = n_teams)

# Fill transition matrix
for (i in 1:nrow(game_data)) {
  visiting_team <- game_data$Visiting_Team[i]
  home_team <- game_data$Home_Team[i]
  visiting_score <- game_data$Visiting_Score[i]
  home_score <- game_data$Home_Score[i]
  
  if (visiting_score > home_score) {
    transition_matrix[team_index[home_team], team_index[visiting_team]] <- 
      transition_matrix[team_index[home_team], team_index[visiting_team]] + 1
  } else {
    transition_matrix[team_index[visiting_team], team_index[home_team]] <- 
      transition_matrix[team_index[visiting_team], team_index[home_team]] + 1
  }
}

# Normalize 
transition_matrix <- sweep(transition_matrix, 1, rowSums(transition_matrix), "/")
transition_matrix[is.na(transition_matrix)] <- 0

# make steady state
steady_state <- rep(1 / n_teams, n_teams)
for (i in 1:10000) {
  steady_state <- steady_state %*% transition_matrix
}

# making numeric
steady_state <- as.numeric(steady_state)

# ranking teams
ranked_teams <- data.frame(
  Team = teams,
  Rank = steady_state
)
ranked_teams <- ranked_teams[order(ranked_teams$Rank, decreasing = TRUE), ]

# steady_state: [1] 0.03947862 0.03394116 0.03584639 0.03187718 0.02999296 0.03454139 0.03532067 0.03243700
#[9] 0.03400317 0.03536549 0.03045194 0.03445172 0.03267662 0.03490097 0.03830736 0.03498181
#[17] 0.03316018 0.03219724 0.02930542 0.03184536 0.03200338 0.03234929 0.03064405 0.03660463
#[25] 0.03197636 0.03721167 0.03341625 0.03142372 0.02959753 0.02969048

# ranked_teams: "Team","Rank"
#"LAN",0.039478617601807
#"NYA",0.0383073586942638
#"HOU",0.0372116706999659
#"TBA",0.036604627884067
#"BOS",0.0358463949856882
#"SLN",0.0353654866139859
#"TOR",0.0353206658189705
#"MIL",0.0349818057735044
#CHN",0.0349009673596236
#"CLE",0.03454138619398
#"ATL",0.0344517243819711
#"SFN",0.0340031697742031
#"SEA",0.0339411551336553
#"NYN",0.0334162500386418
#"BAL",0.033160184023264
#"WAS",0.0326766193040761
#"PHI",0.0324370000296221
#"SDN",0.0323492949525094
#"ANA",0.0321972443100924
#"ARI",0.0320033798403351
#"OAK",0.0319763604725629
#"MIN",0.0318771791932998
#"TEX",0.0318453640408472
#"PIT",0.0314237154608868
#"CIN",0.0306440464792446
#"COL",0.0304519444347548
#"KCA",0.0299929590849049
#"MIA",0.0296904837365093
#"DET",0.0295975274170201
#"CHA",0.0293054162657438




