source("util.R")
matches <- getAllMatches()

readRDS("femalePremLeage.rds")

readRDS("malePremLeage.rds")


##### Nu har vi så en dataframe for 
WSL <- readRDS("femalePremLeage.rds")
PremierLeague <- readRDS("malePremLeage.rds")


# Kolonner der findes i WSL men IKKE i PremierLeague
setdiff(names(WSL), names(PremierLeague))

# Kolonner der findes i PremierLeague men IKKE i WSL
setdiff(names(PremierLeague), names(WSL))


#Match id passer ikke helt sammen i de to datasæt, så vi laver lige navnene om i Premier League
library(dplyr)

PremierLeague <- PremierLeague %>%
  rename(
    match_id = matchId
  )

#### Vi starter med at kigge på afleveringer.
# Så nu laver vi to data frames, hvor vi kun har afleveringer med.
WSL_passes <- WSL[WSL$type.name == "Pass", ]
PremierLeague_passes <- PremierLeague[PremierLeague$type.name == "Pass", ]


#################
##### DATA IMPORT #####

WSL <- readRDS("femalePremLeage.rds")
PremierLeague <- readRDS("malePremLeage.rds")


##### DATA KLARGØRING #####

# Match-id hedder forskelligt → ensret
library(dplyr)

PremierLeague <- PremierLeague %>%
  rename(
    match_id = matchId
  )


##### FILTRERING: AFLEVERINGER #####

# Behold kun pass-events
WSL_passes <- WSL[WSL$type.name == "Pass", ]
PremierLeague_passes <- PremierLeague[PremierLeague$type.name == "Pass", ]


##### OVERBLIK #####

# Antal afleveringer (totalt)
nrow(WSL_passes)            # WSL
#298436
nrow(PremierLeague_passes)  # Premier League
#404785

# Antal kampe
length(unique(WSL_passes$match_id))
# 326
length(unique(PremierLeague_passes$match_id))
# 418

##### SUCCES VS FEJL #####

# Succesfulde afleveringer (NA = succes)
WSL_success <- sum(is.na(WSL_passes$pass.outcome.name))
PL_success  <- sum(is.na(PremierLeague_passes$pass.outcome.name))

# Fejlafleveringer (ikke NA = fejl)
WSL_fail <- sum(!is.na(WSL_passes$pass.outcome.name))
PL_fail  <- sum(!is.na(PremierLeague_passes$pass.outcome.name))


##### FEJLRATE #####

# Fejlrate = fejl / totale afleveringer
WSL_error_rate <- WSL_fail / nrow(WSL_passes)

PL_error_rate  <- PL_fail  / nrow(PremierLeague_passes)

WSL_error_rate
# 0.2633731
PL_error_rate
# 0.2361105

##### AFLEVERINGER PR. KAMP #####

WSL_matches <- length(unique(WSL_passes$match_id))
PL_matches  <- length(unique(PremierLeague_passes$match_id))

WSL_passes_per_game <- nrow(WSL_passes) / WSL_matches
PL_passes_per_game  <- nrow(PremierLeague_passes) / PL_matches

##### RESULTATER #####

WSL_error_rate
# 0.2633731
PL_error_rate
# 0.2361105

WSL_passes_per_game
# 915.4479
PL_passes_per_game
# 968.3852

##### SUCCESRATE #####

# Succesrate = succesfulde afleveringer / totale afleveringer
WSL_success_rate <- WSL_success / nrow(WSL_passes)
PL_success_rate  <- PL_success  / nrow(PremierLeague_passes)

WSL_success_rate
# 0.7366269
PL_success_rate
# 0.7638895

##### Nu tager vi længden på afleveringerne ##

mean(WSL_passes$pass.length, na.rm = TRUE)
# 21.67335
mean(PremierLeague_passes$pass.length, na.rm = TRUE)
# 22.03467



##### LANGE AFLEVERINGER (PRÆCIS DEFINITION) #####

WSL_long_passes <- WSL_passes[
  (WSL_passes$pass.height.name == "Ground Pass" & WSL_passes$pass.length > 45) |
    (WSL_passes$pass.height.name == "High Pass" & WSL_passes$pass.length > 25),
]

PL_long_passes <- PremierLeague_passes[
  (PremierLeague_passes$pass.height.name == "Ground Pass" & PremierLeague_passes$pass.length > 45) |
    (PremierLeague_passes$pass.height.name == "High Pass" & PremierLeague_passes$pass.length > 25),
]

nrow(WSL_long_passes)
# 46576
nrow(PL_long_passes)
# 66917

##### ANDEL LANGE AFLEVERINGER #####

WSL_long_share <- nrow(WSL_long_passes) / nrow(WSL_passes)
PL_long_share  <- nrow(PL_long_passes)  / nrow(PremierLeague_passes)

WSL_long_share
# 0.156067
PL_long_share
# 0.1653149

##### GENNEMSNIT LÆNGDE (LANGE AFLEVERINGER) #####

mean(WSL_long_passes$pass.length, na.rm = TRUE)
# 45.5236
mean(PL_long_passes$pass.length, na.rm = TRUE)
# 48.64083





#################### PLOTS #################
##### GRAF: AFLEVERINGER PR. KAMP #####

passes_per_game <- data.frame(
  league = c("WSL", "Premier League"),
  passes_per_game = c(WSL_passes_per_game, PL_passes_per_game)
)


ggplot(passes_per_game, aes(x = league, y = passes_per_game, fill = league)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(passes_per_game, 0)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("WSL" = "#E69F00", "Premier League" = "#0072B2")) +
  labs(
    title = "Der er flest afleveringer pr. kamp i Premier League",
    x = "",
    y = "Afleveringer pr. kamp"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11)
  )

###########################
##### GRAF: FEJLRATE #####

error_rate_data <- data.frame(
  league = c("WSL", "Premier League"),
  error_rate = c(WSL_error_rate, PL_error_rate)
)

ggplot(error_rate_data, aes(x = league, y = error_rate, fill = league)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(error_rate, accuracy = 0.1)), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("WSL" = "#E69F00", "Premier League" = "#0072B2")) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "WSL har flere fejlafleveringer end Premier League",
    x = "",
    y = "Fejlrate"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11)
  )

###################
##### GRAF: LÆNGDE AF LANGE AFLEVERINGER #####

long_length_data <- data.frame(
  league = c("WSL", "Premier League"),
  avg_length = c(
    mean(WSL_long_passes$pass.length, na.rm = TRUE),
    mean(PL_long_passes$pass.length, na.rm = TRUE)
  )
)

ggplot(long_length_data, aes(x = league, y = avg_length, fill = league)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(avg_length, 1)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("WSL" = "#E69F00", "Premier League" = "#0072B2")) +
  labs(
    title = "Den gennemsnitlig længde af lange afleveringer er højere i Premier League",
    x = "",
    y = "Meter"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11)
  )
