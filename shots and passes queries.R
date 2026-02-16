library(RMariaDB)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(dplyr)
options(digits = 5)


#forbindelse
con <- dbConnect(MariaDB(),
                 host="talmedos.com",
                 port="3306",
                 db="superliga2",
                 user="dalremote",
                 password="OttoRehagel123456789Long2026!")
                 
common <- dbGetQuery(con, "SELECT * FROM superliga2.wyscout_matchevents_common;")

matches301 <- dbGetQuery(con, "SELECT MATCH_WYID, SEASON_WYID
FROM wyscout_matches
WHERE competition_wyid IN (335, 328)
  AND date BETWEEN '2024-07-19' AND CURRENT_DATE;")

passes <- dbGetQuery(con, "select MATCH_WYID, primarytype, accurate from wyscout_matchevents_passes;")

shots <- dbGetQuery(con, "select MATCH_WYID, EVENT_WYID, PRIMARYTYPE, SHOTBODYPART, SHOTISGOAL, SHOTONTARGET, SHOTXG, SHOTPOSTSHOTXG from wyscout_matchevents_shots;")

################################################################################
#SELVMÅL
################################################################################
#27 Own-goals for begge sæsoner
own_goals <- common %>% filter(grepl("own_goal", PRIMARYTYPE), SEASON_WYID %in% c(189918, 191611))
own_goals <- cbind(own_goals, SHOTBODYPART=rep(0, nrow(own_goals)), SHOTISGOAL=rep(0, nrow(own_goals)))


################################################################################
#PASSES OG ODD_ONES
################################################################################

#primarytypes for 24/25 og 25/26 fra matchevents_passes. Både Superliga og 1. div
passes_season <- left_join(matches301, passes, by="MATCH_WYID")
table(passes_season$primarytype)

#primarytypes for 24/25 og 25/26 fra matchevents_common
season <- common %>% filter(SEASON_WYID %in% c(189918, 191611))
table(season$PRIMARYTYPE)

odd_ones <- anti_join(matches301, season, by = "MATCH_WYID")

################################################################################
#SHOTS
################################################################################

#shots for 24/25 og 25/26 fra matchevents_shots. Både Superliga og 1. div
shots_season <- left_join(matches301, shots, by="MATCH_WYID")
shots_med_xy <- left_join(shots_season, common[, c(2,3,14,16,12,13)] %>% distinct(EVENT_WYID, .keep_all = TRUE),by = "EVENT_WYID")
shots_med_xy <- shots_med_xy[,c(1,2,3,10,11,12,4,5,6,7,8,9,13,14)]#rearrangerer kolonner 



#DF vi arbejder med
View(shots_med_xy)


