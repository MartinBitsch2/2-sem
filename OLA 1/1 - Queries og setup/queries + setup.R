library(RMariaDB)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(dplyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(caret)
library(scales)
library(randomForest)
options(digits = 5)


#forbindelse
con <- dbConnect(MariaDB(),
                 host="talmedos.com",
                 port="3306",
                 db="superliga2",
                 user="dalremote",
                 password="OttoRehagel123456789Long2026!")

##########################################################################################################################################
#QUERIES FRA SQL OG RDS
##########################################################################################################################################

common <- dbGetQuery(con, "SELECT * FROM superliga2.wyscout_matchevents_common;")

matches301 <- dbGetQuery(con, "SELECT MATCH_WYID, SEASON_WYID
FROM wyscout_matches
WHERE competition_wyid IN (335, 328)
  AND date BETWEEN '2024-07-19' AND CURRENT_DATE;")

passes <- dbGetQuery(con, "select * from wyscout_matchevents_passes;")

shots <- dbGetQuery(con, "select MATCH_WYID, EVENT_WYID, PRIMARYTYPE, SHOTBODYPART, SHOTISGOAL, SHOTONTARGET, SHOTXG, SHOTPOSTSHOTXG 
                    from wyscout_matchevents_shots;")

players <- dbGetQuery(con, "SELECT * FROM superliga2.wyscout_players;")

secondarytype <- dbGetQuery(con, "SELECT * from superliga2.wyscout_matchevents_secondarytype;")

teams <- dbGetQuery(con, "SELECT DISTINCT TEAM_WYID, TEAMNAME FROM wyscout_teams;")




##########################################################################################################################################
#SELVMÅL
##########################################################################################################################################

#27 Own-goals for begge sæsoner (kun superliga)
own_goals <- common %>% filter(grepl("own_goal", PRIMARYTYPE), SEASON_WYID %in% c(189918, 191611))
own_goals <- common %>% filter(grepl("own_goal", PRIMARYTYPE), SEASON_WYID %in% c(189918, 189933))
own_goals <- cbind(own_goals, SHOTBODYPART=rep(0, nrow(own_goals)), SHOTISGOAL=rep(0, nrow(own_goals)))


##########################################################################################################################################
#PASSES OG ODD_ONS
##########################################################################################################################################

#primarytypes for 24/25 og 25/26 fra matchevents_passes. Både Superliga og 1. div
passes_season <- left_join(matches301, passes, by="MATCH_WYID")
passes_med_xy <- left_join(passes_season, common[, c(3,5,6,14,16,12,13)] %>% distinct(EVENT_WYID, .keep_all = TRUE),by = "EVENT_WYID")

#Kun succesfulde afleveringer
teamz <- unique(na.omit(passes_med_xy$TEAM_WYID[passes_med_xy$SEASON_WYID %in% c(191611,189918)]))
filtered_df <- c()
for(i in 1:length(teamz)){
  filtered_df2 <- passes_med_xy %>% filter(TEAM_WYID==teamz[i])
  filtered_df1 <- filtered_df2 %>% filter(RECIPIENT_WYID %in% unique(filtered_df2$PLAYER_WYID))
  
  filtered_df <- rbind(filtered_df, filtered_df1)
}
passes_succes_super <- filtered_df %>% filter(SEASON_WYID %in% c(191611,189918), PRIMARYTYPE=="pass", PLAYER_WYID != 0, RECIPIENT_WYID != 0)
passes_super <- passes_med_xy %>% filter(SEASON_WYID %in% c(191611,189918), PRIMARYTYPE=="pass")
passes_unsucces_super <- anti_join(passes_super, passes_succes_super, by="EVENT_WYID")

#odd ones
season <- common %>% filter(SEASON_WYID %in% c(189918,189933, 191611, 191620))
odd_ones <- anti_join(matches301, season, by = "MATCH_WYID")

##########################################################################################################################################
#SHOTS
##########################################################################################################################################

#shots for 24/25 og 25/26 fra matchevents_shots. Både Superliga og 1. div
shots_season <- left_join(matches301, shots, by="MATCH_WYID")
shots_med_xy <- left_join(shots_season, common[, c(2,3,5,6,14,16,12,13)] %>% distinct(EVENT_WYID, .keep_all = TRUE),by = "EVENT_WYID")

#sererat df med vinkler og afstande. koordinater før i procent, laves om til meter
afstand_vinkel <- cbind(shots_med_xy, x_meter=shots_med_xy$LOCATIONX*0.01*105, y_meter=shots_med_xy$LOCATIONY*0.01*68) 

#pytagoras (a^2+b^2=c^2)
afstand_vinkel$afstand_til_mål <- sqrt(((105-afstand_vinkel$x_meter)^2)+((34-afstand_vinkel$y_meter)^2))

#afstande til venstre og højre målstolpe. skal bruges til udregning af vinkel som bold kan gå ind fra skudposition (uden skrue)
left_post <- c(105, 34+(7.32/2))
right_post <- c(105, 34-(7.32/2))
afstand_vinkel$afstand_left <- sqrt((left_post[1] - afstand_vinkel$x_meter)^2 + (left_post[2] - afstand_vinkel$y_meter)^2) 
afstand_vinkel$afstand_right <- sqrt((right_post[1] - afstand_vinkel$x_meter)^2 + (right_post[2] - afstand_vinkel$y_meter)^2) 

a <- afstand_vinkel$afstand_left
b <- afstand_vinkel$afstand_right
c <- 7.32 #afstand mellem inderstolperne

#cosinusrelation til udregning af vinkel i grader mellem stolperne fra skudposition.
afstand_vinkel$vinkel_mellem_stolper <- acos((a^2+b^2-c^2)/(2*a*b))*180/pi

#Tilføjer det vigtigste tilbage til hoved-df shots_med_xy
shots_med_xy <- afstand_vinkel[,c(1:14,17:19,22)]
