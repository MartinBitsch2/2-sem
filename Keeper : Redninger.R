#### Nu kigger vi sgu på de højt diskuterede målvogtere

WSL_keeper <- WSL[WSL$type.name == "Goal Keeper", ]
PremierLeague_keeper <- PremierLeague[PremierLeague$type.name == "Goal Keeper", ]
dim(WSL_keeper)
dim(PremierLeague_keeper)

names(WSL_keeper)[grepl("goalkeeper", names(WSL_keeper))]
unique(WSL_keeper$goalkeeper.type.name)
unique(PremierLeague_keeper$goalkeeper.type.name)

##### REDNINGER #####

WSL_saves <- sum(WSL_keeper$goalkeeper.type.name %in% 
                   c("Shot Saved", "Shot Saved to Post", "Penalty Saved", 
                     "Saved to Post", "Save"))

PL_saves <- sum(PremierLeague_keeper$goalkeeper.type.name %in% 
                  c("Shot Saved", "Shot Saved to Post", "Penalty Saved", 
                    "Saved to Post", "Save"))

WSL_saves
# 2069
PL_saves
# 2493

##### MÅL IMOD #####

WSL_goals_conceded <- sum(WSL_keeper$goalkeeper.type.name == "Goal Conceded")
PL_goals_conceded  <- sum(PremierLeague_keeper$goalkeeper.type.name == "Goal Conceded")

WSL_goals_conceded
# 902
PL_goals_conceded
# 998

##### REDNINGSPROCENT #####

WSL_save_percentage <- WSL_saves / (WSL_saves + WSL_goals_conceded)
PL_save_percentage  <- PL_saves  / (PL_saves  + PL_goals_conceded)

WSL_save_percentage
# 0.6963985
PL_save_percentage
# 0.714122

####################
##### GRAF: REDNINGER VS MÅL IMOD (MED LABELS) #####

keeper_data <- data.frame(
  league = c("WSL", "WSL", "Premier League", "Premier League"),
  outcome = c("Redninger", "Mål imod", "Redninger", "Mål imod"),
  count = c(WSL_saves, WSL_goals_conceded, PL_saves, PL_goals_conceded)
)

ggplot(keeper_data, aes(x = league, y = count, fill = outcome)) +
  
  # Selve søjlerne
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  
  # Labels inde i søjlerne
  geom_text(
    aes(label = scales::percent(count / ave(count, league, FUN = sum), accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  
  # Farver
  scale_fill_manual(values = c(
    "Redninger" = "#2E8B57",
    "Mål imod" = "#C44E52"
  )) +
  
  scale_y_continuous(labels = scales::percent) +
  
  labs(
    title = "Fordeling af skud imod",
    subtitle = "Redninger vs mål lukket ind",
    x = "",
    y = "Andel",
    fill = ""
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 11)
  )
###############
##### GRAF: REDNINGSPROCENT #####

save_data <- data.frame(
  league = c("WSL", "Premier League"),
  save_percentage = c(WSL_save_percentage, PL_save_percentage)
)

ggplot(save_data, aes(x = league, y = save_percentage, fill = league)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(save_percentage, accuracy = 0.1)), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c(
    "WSL" = "#E69F00",
    "Premier League" = "#0072B2"
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Redningsprocent er højere i Premier League",
    x = "",
    y = "Redningsprocent"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")
  )
