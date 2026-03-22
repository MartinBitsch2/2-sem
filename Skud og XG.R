### ## nu kigger vi på skud og XG 

WSL_shots <- WSL[WSL$type.name == "Shot", ]
PremierLeague_shots <- PremierLeague[PremierLeague$type.name == "Shot", ]

##### SKUD PR. KAMP #####

WSL_shots_per_game <- nrow(WSL_shots) / WSL_matches
PL_shots_per_game  <- nrow(PremierLeague_shots) / PL_matches

WSL_shots_per_game
# 25.52454
PL_shots_per_game
# 25.92584

##### SKUD PÅ MÅL #####

WSL_shots_on_target <- sum(WSL_shots$shot.outcome.name %in% 
                             c("Goal", "Saved", "Saved to Post"))

PL_shots_on_target <- sum(PremierLeague_shots$shot.outcome.name %in% 
                            c("Goal", "Saved", "Saved to Post"))

WSL_shots_on_target
# 3010
PL_shots_on_target
# 3551

##### SKUDPRÆCISION #####

WSL_shot_accuracy <- WSL_shots_on_target / nrow(WSL_shots)
PL_shot_accuracy  <- PL_shots_on_target  / nrow(PremierLeague_shots)

WSL_shot_accuracy
# 0.3617354
PL_shot_accuracy
# 0.3276737

WSL_missed_shots <- nrow(WSL_shots) - WSL_shots_on_target
PL_missed_shots  <- nrow(PremierLeague_shots) - PL_shots_on_target

WSL_missed_shots
# 5311
PL_missed_shots
# 7286

##### MÅL #####

WSL_goals <- sum(WSL_shots$shot.outcome.name == "Goal")
PL_goals  <- sum(PremierLeague_shots$shot.outcome.name == "Goal")

WSL_goals
# 962
PL_goals
#1082

##### KONVERTERINGSRATE #####

WSL_conversion_rate <- WSL_goals / nrow(WSL_shots)
PL_conversion_rate  <- PL_goals  / nrow(PremierLeague_shots)

WSL_conversion_rate
# 0.1156111
PL_conversion_rate
# 0.09984313

### Så er det lige tid til at kigge på vores elskede XG 
mean(WSL_shots$shot.statsbomb_xg, na.rm = TRUE)
# 0.1109551
mean(PremierLeague_shots$shot.statsbomb_xg, na.rm = TRUE)
# 0.09810714

##### xG PR. KAMP #####

WSL_xg_per_game <- sum(WSL_shots$shot.statsbomb_xg, na.rm = TRUE) / WSL_matches
PL_xg_per_game  <- sum(PremierLeague_shots$shot.statsbomb_xg, na.rm = TRUE) / PL_matches

WSL_xg_per_game
# 2.832077
PL_xg_per_game
# 2.54351

##### MÅL VS xG #####

WSL_total_xg <- sum(WSL_shots$shot.statsbomb_xg, na.rm = TRUE)
PL_total_xg  <- sum(PremierLeague_shots$shot.statsbomb_xg, na.rm = TRUE)

WSL_overperformance <- WSL_goals - WSL_total_xg
PL_overperformance  <- PL_goals  - PL_total_xg

WSL_overperformance
# 38.74283
PL_overperformance
# 18.81289


############# PLOTS #############
##### GRAF: MISSEDE SKUD PR. KAMP #####

missed_shots_data <- data.frame(
  league = c("WSL", "Premier League"),
  missed_per_game = c(
    WSL_missed_shots / WSL_matches,
    PL_missed_shots  / PL_matches
  )
)

ggplot(missed_shots_data, aes(x = league, y = missed_per_game, fill = league)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(missed_per_game, 1)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("WSL" = "#E69F00", "Premier League" = "#0072B2")) +
  labs(
    title = "Premier League spillere misser flere skud pr. kamp",
    x = "",
    y = "Missede skud pr. kamp"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")
  )


#############
##### GRAF: xG PR. SKUD #####

xg_data <- data.frame(
  league = c("WSL", "Premier League"),
  xg_per_shot = c(
    mean(WSL_shots$shot.statsbomb_xg, na.rm = TRUE),
    mean(PremierLeague_shots$shot.statsbomb_xg, na.rm = TRUE)
  )
)

ggplot(xg_data, aes(x = league, y = xg_per_shot, fill = league)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(xg_per_shot, 3)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("WSL" = "#E69F00", "Premier League" = "#0072B2")) +
  labs(
    title = "Gennemsnitlig xG pr. skud",
    x = "",
    y = "xG pr. skud"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")
  )

