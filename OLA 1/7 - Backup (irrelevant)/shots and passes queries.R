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

#900 hovedstød for begge sæsoner



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

common <- readRDS("C://Users//marti//Documents//Git//EK//2. sem//OLA1//filer//common.rds")
matches301 <- readRDS("C://Users//marti//Documents//Git//EK//2. sem//OLA1//filer//matches301.rds")
passes <- readRDS("C://Users//marti//Documents//Git//EK//2. sem//OLA1//filer//passes.rds")
shots <- readRDS("C://Users//marti//Documents//Git//EK//2. sem//OLA1//filer//shots.rds")


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

##########################################################################################################################################
#BESLUTNINGSTRÆ
##########################################################################################################################################

#laver ny kolonne hvor der defineres om et skud er blevet til at mål eller ej.
super <- shots_med_xy %>% filter(COMPETITION_WYID==335)
super <- super %>% filter(afstand_til_mål<35, PRIMARYTYPE=="shot")
supermål <- super %>% filter(str_detect(SHOTBODYPART, '"goal"') | SHOTISGOAL == 1)
supermål <- cbind(supermål, skud=as.factor(rep("mål", nrow(supermål))))
superikkemål <- anti_join(super, supermål, by="EVENT_WYID")
superikkemål <- cbind(superikkemål, skud=as.factor(rep("ikke-mål", nrow(superikkemål))))
super <- rbind(supermål, superikkemål)
  
#beslutningstræet
set.seed(123)
train_index <- createDataPartition(super$skud, p = 0.8, list = FALSE)
train_data <- super[train_index, ]
test_data <- super[-train_index, ]

tree_model <- rpart(skud ~ vinkel_mellem_stolper + afstand_til_mål, 
                    data = train_data, 
                    method = "class",  # For classification
                    control = rpart.control(minsplit = 10, cp = 0.0037425)) #cp=complexity parameter. bruges til at prunne.

rpart.plot(tree_model, box.palette = "auto", nn = TRUE)

predictions <- predict(tree_model, test_data, type = "class")
confusionMatrix(predictions, test_data$skud)

printcp(tree_model)  # Identify optimal cp (xerror is minimized)
optimal_cp <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(tree_model, cp = optimal_cp)
rpart.plot(pruned_tree)


##########################################################################################################################################
#PLOTS
##########################################################################################################################################


#Plot over fordling af mål og ikke mål
super_plot <- super %>% count(skud, name = "n") %>% mutate(pct = n / sum(n), label = paste0(n, " (", percent(pct, accuracy = 1), ")"))

ggplot(super_plot, aes(x = skud, y = pct, fill = skud)) + geom_col(width = 0.55, show.legend = FALSE) + 
  geom_text(aes(label = label),vjust = -0.6,fontface = "bold",size = 4.5) +
  scale_fill_manual(values = c("ikke-mål" = "#D55E00", "mål" = "#009E73")) +
  scale_y_continuous(labels = percent,expand = expansion(mult = c(0, 0.15))) +
  labs(title = "12% af skud i frit spil under 35 m fra målet, bliver til mål",
       subtitle = "Fordeling mellem afslutninger der giver mål og ikke mål",x = NULL,y = "Andel afslutninger") +
  theme_minimal(base_size = 14) + theme(plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, color = "grey40"), axis.text.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"), panel.grid.major.x = element_blank())



# Plot over gns længde på skud der bliver til mål og ikke mål
df_mean <- super %>% summarise(mean_dist = mean(afstand_til_mål, na.rm = TRUE), sd_dist = sd(afstand_til_mål, na.rm = TRUE), .by = skud)

ggplot(df_mean, aes(x = skud, y = mean_dist)) + geom_col(fill = "grey30", width = 0.45) + geom_text(
    aes(label = round(mean_dist, 1)), vjust = -0.4, fontface = "bold", size = 4) +
  labs(title = "Den gns. distance til mål er lavere ved målscoringer - målt indenfor 35 meter fra målet og i frit spil",
    subtitle = "Mål vs skud der ikke bliver mål - målt i meter", x = NULL, y = "Afstand til mål (meter)") +
  theme_minimal(base_size = 14) + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(face = "bold"))



# Tester p-værdi, gennemsnit og standardafvigelsen
#afstand
sd(super %>% filter(skud == "mål") %>% pull(afstand_til_mål))
sd(super %>% filter(skud == "ikke-mål") %>% pull(afstand_til_mål))

#vinkel:
sd(super %>% filter(skud == "mål") %>% pull(vinkel_mellem_stolper))
sd(super %>% filter(skud == "ikke-mål") %>% pull(vinkel_mellem_stolper))



#Skudvinkler
df_mean2 <- super %>% summarise(mean_dist2 = mean(vinkel_mellem_stolper, 
            na.rm = TRUE), sd_dist2= sd(vinkel_mellem_stolper, na.rm = TRUE), .by = skud)

ggplot(df_mean2, aes(x = skud, y = mean_dist2)) + geom_col(fill = "grey30", width = 0.45) +
  geom_text(aes(label = round(mean_dist2, 1)), vjust = -0.4, fontface = "bold", size = 4) +
  labs(title = "Den gns. vinkel til mål er højere ved målscoringer - \nmålt indenfor 35 meter fra målet og i frit spil",
    subtitle = "Mål vs skud der ikke bliver mål - målt i grader", x = NULL, y = "Vinkel fra afslutter (grader)") +
  theme_minimal(base_size = 14) + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(face = "bold"))


