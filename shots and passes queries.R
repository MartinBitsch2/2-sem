library(RMariaDB)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(dplyr)


#forbindelse
con <- dbConnect(MariaDB(),
                 host="talmedos.com",
                 port="3306",
                 db="superliga2",
                 user="dalremote",
                 password="OttoRehagel123456789Long2026!")
                 
common <- dbGetQuery(con, "SELECT * FROM superliga2.wyscout_matchevents_common;")

matches301 <- dbGetQuery(con, "  SELECT MATCH_WYID, SEASON_WYID
FROM wyscout_matches
WHERE competition_wyid = 335
  AND date BETWEEN '2024-07-19' AND CURRENT_DATE;")

passes <- dbGetQuery(con, "select MATCH_WYID, primarytype, accurate from wyscout_matchevents_passes;")

shots <- dbGetQuery(con, "select MATCH_WYID, EVENT_WYID, PRIMARYTYPE, SHOTBODYPART, SHOTISGOAL, SHOTONTARGET from wyscout_matchevents_shots;")

################################################################################
#PASSES
################################################################################

#primarytypes for 24/25 og 25/26 fra matchevents_passes
passes_season <- left_join(matches301, passes, by="MATCH_WYID")
table(passes_season$primarytype)

#primarytypes for 24/25 og 25/26 fra matchevents_common
season <- common %>% filter(SEASON_WYID %in% c(189918, 191611))
table(season$PRIMARYTYPE)

################################################################################
#SHOTS
################################################################################

#shots for 24/25 og 25/26 fra matchevents_shots
shots_season <- left_join(matches301, shots, by="MATCH_WYID")
shots_med_xy <- left_join(shots_season,common[, c(3,12,13)] %>% distinct(EVENT_WYID, .keep_all = TRUE),by = "EVENT_WYID")


#27 Own-goals for begge sæsoner
own_goals <- common %>% filter(grepl("own_goal", PRIMARYTYPE), SEASON_WYID %in% c(189918, 191611))
own_goals <- cbind(own_goals, SHOTBODYPART=rep(0, nrow(own_goals)), SHOTISGOAL=rep(0, nrow(own_goals)))





odd_ones <- anti_join(matches301, season, by = "MATCH_WYID")

all_goals_df <- rbind(own_goals[,c(1,3,4,11,12,13,26,27)], shots_med_xy[,c(2,3,1,4,8,9,5,6)])





length(unique(season$MATCH_WYID))
length(unique(matches301$MATCH_WYID))
