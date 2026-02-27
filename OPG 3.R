library(plotly)
library(factoextra)


#Kun succesfulde afleveringer
teamz_sup_div <- unique(na.omit(passes_med_xy$TEAM_WYID[passes_med_xy$SEASON_WYID %in% c(191611,189918, 189933, 191620)]))
filtered_df <- c()
for(i in 1:length(teamz_sup_div)){
  filtered_df2 <- passes_med_xy %>% filter(TEAM_WYID==teamz_sup_div[i])
  filtered_df1 <- filtered_df2 %>% filter(RECIPIENT_WYID %in% unique(filtered_df2$PLAYER_WYID))
  
  filtered_df <- rbind(filtered_df, filtered_df1)
}
passes_succes <- filtered_df %>% filter(PRIMARYTYPE=="pass", PLAYER_WYID != 0, RECIPIENT_WYID != 0)
passes_super_div <- passes_med_xy %>% filter(PRIMARYTYPE=="pass")
passes_unsucces <- anti_join(passes_super_div, passes_succes, by="EVENT_WYID")
passes_true_false <- rbind(cbind(passes_succes, succesfuld_aflevering=rep("TRUE", nrow(passes_succes))), 
                                 cbind(passes_unsucces, succesfuld_aflevering=rep("FALSE", nrow(passes_unsucces))))






spiller_statistik_passes <- passes_true_false %>%
  group_by(PLAYER_WYID) %>%
  summarise(
    # Gennemsnitlig position
    gns_X = mean(LOCATIONX, na.rm = TRUE),
    gns_Y = mean(LOCATIONY, na.rm = TRUE),
    
    # Antal forsøg (HELE)
    antal_forsøg = n(),
    
    # Antal succesfulde (DELEN)
    antal_succes = sum(succesfuld_aflevering == TRUE, na.rm = TRUE),
    
    # Udregning: (del / hel) * 100
    succes_procent = (antal_succes / antal_forsøg) * 100,
    
    # Gennemsnitlig længde for kun succesfulde
    gns_længde_succes = mean(LENGTH[succesfuld_aflevering == TRUE], na.rm = TRUE)
  ) %>%
  filter(antal_forsøg > 50) %>%
  arrange(desc(succes_procent))

players1 <- players %>% filter(SEASON_WYID %in% c(191620, 191611, 189918, 189933)) %>% distinct(PLAYER_WYID, .keep_all = TRUE)
passes_med_xy1 <- passes_med_xy %>% filter(SEASON_WYID %in% c(191620, 191611, 189918, 189933)) %>% distinct(PLAYER_WYID, .keep_all = TRUE)

spiller_statistik_passes <- left_join(spiller_statistik_passes, players1[,c("SEASON_WYID", "PLAYER_WYID", "SHORTNAME", "ROLENAME")], by ="PLAYER_WYID")
spiller_statistik_passes <- na.omit(spiller_statistik_passes)
spiller_statistik_passes <- left_join(spiller_statistik_passes, passes_med_xy1[,c("PLAYER_WYID", "TEAM_WYID")], by="PLAYER_WYID")
spiller_statistik_passes <- left_join(spiller_statistik_passes, teams, by="TEAM_WYID")


# kmeans
vars <- spiller_statistik_passes[, c(
  "gns_X",
  "gns_Y",
  "succes_procent",
  "gns_længde_succes"
)]


vars$original_row <- as.numeric(rownames(vars))
vars_scaled_df <-as.data.frame(scale(vars[, !colnames(vars) %in% "original_row"]))
vars_scaled <-as.data.frame(scale(vars[, !colnames(vars) %in% "original_row"]))
wss <- list()





for (k in 1:10) {
  wss[k] <- sum(kmeans(vars_scaled, centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type="b")
kmodel <- kmeans(vars_scaled, centers = 6, nstart = 25)
kval=kmodel$betweenss / kmodel$totss
kval

# ADD CLUSTER TO DATA
spiller_statistik_passes$cluster <- as.factor(kmodel$cluster)
###

nucol=colnames(vars_scaled_df)
nucol

#### SUMMARY PR CLUSTER
profilesup <- spiller_statistik_passes %>% group_by(cluster) %>% 
  summarize(
    gns_X=mean(gns_X),
    gns_Y=mean(gns_Y),
    succes_procent=mean(succes_procent),
    gns_længde_succes=mean(gns_længde_succes),
    size=n()
  )


# FOR VIZ - PCA or ORIGINAL DATA?
fviz_cluster(kmodel,
             data = vars_scaled,
             geom = "point",
             stand = FALSE,
             ellipse.type = "norm",
             ggtheme = theme_minimal())


pca <- prcomp(vars_scaled)
summary(pca)

# dominans
pca$rotation
pca$x[,1]

pca_df <- data.frame(
  # PCA koordinater (de nye akser)
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  PC3 = pca$x[,3],
  
  # Metadata fra din aggregering
  cluster = factor(spiller_statistik_passes$cluster),
  PLAYER_WYID = spiller_statistik_passes$PLAYER_WYID,
  SHORTNAME = spiller_statistik_passes$SHORTNAME,
  ROLENAME = spiller_statistik_passes$ROLENAME,
  TEAMNAME = spiller_statistik_passes$TEAM_WYID,
  
  
  # Dine originale cluster-variable (så du kan se dem i hover-info)
  gns_X = spiller_statistik_passes$gns_X,
  gns_Y = spiller_statistik_passes$gns_Y,
  succes_procent = spiller_statistik_passes$succes_procent,
  gns_længde_succes = spiller_statistik_passes$gns_længde_succes,
  antal_forsøg = spiller_statistik_passes$antal_forsøg
)

ggplot(pca_df, aes(x=PC1, y=PC2, colour =cluster))+
  geom_point()+
  geom_text(aes(label=SHORTNAME),size=1.5 )


plot_ly(pca_df,
        x = ~PC1,
        y = ~PC2,
        z = ~PC3,
        color = ~cluster,
        colors = "Set1",
        type = "scatter3d",
        mode = "markers",  # Fjernet '+text' for at undgå rod på banen, navne ses ved hover
        marker = list(size = 5, opacity = 0.8),
        hovertext = ~paste(
          "<b>Navn:</b>", SHORTNAME,
          "<b>Hold:</b>", TEAMNAME,
          "<br><b>Rolle:</b>", ROLENAME,
          "<br>---------------------------",
          "<br><b>Gns. Position (X):</b>", round(gns_X, 1),
          "<br><b>Succesrate_passes i procent:</b>", round(succes_procent, 1), "%",
          "<br><b>gns_længde afleveringer:</b>", round(gns_længde_succes, 1), "m",
          "<br><b>Antal Forsøg:</b>", antal_forsøg
        ),
        hoverinfo = "text"
) %>% layout(
  title = " \n3D Clustering over afleveringer i Superligaen \nog 1. division fordelt på spillere",
  scene = list(
    xaxis = list(title = "PC1 (Offensiv position vs. Lange succesfulde bolde)"),
    yaxis = list(title = "       PC2 (Højre vs. Venstre side)"),
    zaxis = list(title = "PC3 (Generel boldpræcision vs. Lange succesfulde bolde)")
    )
  )



sort(abs(pca$rotation[,1]), decreasing = TRUE)
pca$rotation[,3]
pca$rotation


#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
#skud og afleveringer
super_div <- shots_med_xy %>% filter(afstand_til_mål<35, PRIMARYTYPE=="shot")
mål <- super_div %>% filter(str_detect(SHOTBODYPART, '"goal"') | SHOTISGOAL == 1)
mål <- cbind(mål, skud=as.factor(rep("mål", nrow(mål))))
ikkemål <- anti_join(super_div, mål, by="EVENT_WYID")
ikkemål <- cbind(ikkemål, skud=as.factor(rep("ikke-mål", nrow(ikkemål))))
super_div <- rbind(mål, ikkemål)

spiller_statistik_shots <- super_div %>%
  group_by(PLAYER_WYID) %>%
  summarise(
    # Gennemsnitlig position
    gns_afstand_mål = mean(afstand_til_mål, na.rm = TRUE),
    gns_vinkel_mål = mean(vinkel_mellem_stolper, na.rm = TRUE),
    
    # Antal forsøg (HELE)
    antal_forsøg_skud = n(),
    
    # Antal succesfulde (DELEN)
    antal_mål = sum(skud == "mål", na.rm = TRUE),
    
    # Udregning: (del / hel) * 100
    succes_procent_mål = (antal_mål / antal_forsøg_skud) * 100) %>%
  filter(antal_forsøg_skud > 2) %>%
  arrange(desc(succes_procent_mål))

#merge af shots og passes
spiller_statistik_shots_passes <- left_join(spiller_statistik_passes, spiller_statistik_shots, by = "PLAYER_WYID")
spiller_statistik_shots_passes[is.na(spiller_statistik_shots_passes)] <- 0

#kommer fra 1div ja/nej
div1 <- left_join(spiller_statistik_shots_passes, players[,c("PLAYER_WYID", "COMPETITION_WYID")] %>% filter(COMPETITION_WYID==328) %>% distinct(PLAYER_WYID, .keep_all = TRUE), by="PLAYER_WYID")
div1[is.na(div1)] <- 335
colnames(div1)[18] <- "liga spillet før (328 er 1. div)"

spiller_statistik_shots_passes <- left_join(spiller_statistik_shots_passes, div1[,c("PLAYER_WYID", "liga spillet før (328 er 1. div)")], by="PLAYER_WYID")



# kmeans
vars <- spiller_statistik_shots_passes[, c(
  "gns_X",
  "succes_procent",
  "gns_længde_succes",
  "gns_afstand_mål",
  "gns_vinkel_mål",
  "succes_procent_mål"
)]


vars$original_row <- as.numeric(rownames(vars))
vars_scaled_df <-as.data.frame(scale(vars[, !colnames(vars) %in% "original_row"]))
vars_scaled <-as.data.frame(scale(vars[, !colnames(vars) %in% "original_row"]))
wss <- list()





for (k in 1:10) {
  wss[k] <- sum(kmeans(vars_scaled, centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type="b")
kmodel <- kmeans(vars_scaled, centers = 5, nstart = 25)
kval=kmodel$betweenss / kmodel$totss
kval

# ADD CLUSTER TO DATA
spiller_statistik_shots_passes$cluster <- as.factor(kmodel$cluster)
###

nucol=colnames(vars_scaled_df)
nucol

#### SUMMARY PR CLUSTER
profilesup <- spiller_statistik_shots_passes %>% group_by(cluster) %>% 
  summarize(
    gns_X=mean(gns_X),
    succes_procent=mean(succes_procent),
    gns_længde_succes=mean(gns_længde_succes),
    gns_afstand_mål=mean(gns_afstand_mål),
    gns_vinkel_mål=mean(gns_vinkel_mål),
    succes_procent_mål=mean(succes_procent_mål),
    size=n()
  )


# FOR VIZ - PCA or ORIGINAL DATA?
fviz_cluster(kmodel,
             data = vars_scaled,
             geom = "point",
             stand = FALSE,
             ellipse.type = "norm",
             ggtheme = theme_minimal())


pca <- prcomp(vars_scaled)
summary(pca)

# dominans
pca$rotation
pca$x[,1]

pca_df <- data.frame(
  # PCA koordinater (de nye akser)
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  PC3 = pca$x[,3],
  
  # Metadata fra din aggregering
  cluster = factor(spiller_statistik_shots_passes$cluster),
  PLAYER_WYID = spiller_statistik_shots_passes$PLAYER_WYID,
  SHORTNAME = spiller_statistik_shots_passes$SHORTNAME,
  ROLENAME = spiller_statistik_shots_passes$ROLENAME,
  TEAMNAME = spiller_statistik_shots_passes$TEAMNAME,
  LIGA_FØR = spiller_statistik_shots_passes$`liga spillet før (328 er 1. div)`,
  
  
  # Dine originale cluster-variable (så du kan se dem i hover-info)
  gns_X = spiller_statistik_shots_passes$gns_X,
  gns_Y = spiller_statistik_shots_passes$gns_Y,
  succes_procent = spiller_statistik_shots_passes$succes_procent,
  gns_længde_succes = spiller_statistik_shots_passes$gns_længde_succes,
  gns_afstand_mål = spiller_statistik_shots_passes$gns_afstand_mål,
  gns_vinkel_mål = spiller_statistik_shots_passes$gns_vinkel_mål,
  succes_procent_mål=spiller_statistik_shots_passes$succes_procent_mål
  
)

ggplot(pca_df, aes(x=PC1, y=PC2, colour =cluster))+
  geom_point()+
  geom_text(aes(label=SHORTNAME),size=1.5 )


plot_ly(pca_df,
        x = ~PC1,
        y = ~PC2,
        z = ~PC3,
        color = ~cluster,
        colors = "Set1",
        type = "scatter3d",
        mode = "markers",  # Fjernet '+text' for at undgå rod på banen, navne ses ved hover
        marker = list(size = 5, opacity = 0.8),
        hovertext = ~paste(
          "<b>Navn:</b>", SHORTNAME,
          "<b>Hold:</b>", TEAMNAME,
          "<br><b>Rolle:</b>", ROLENAME,
          "<br><b>Liga spillet før:</b>", LIGA_FØR,
          
          "<br>---------------------------",
          "<br><b>Gns. Position (X):</b>", round(gns_X, 1),
          "<br><b>Succesrate_passes i procent:</b>", round(succes_procent, 1), "%",
          "<br><b>gns_længde afleveringer:</b>", round(gns_længde_succes, 1), "m",
          "<br><b>gns_afstand_mål:</b>", gns_afstand_mål,
          "<br><b>gns_vinkel_mål:</b>", gns_vinkel_mål,
          "<br><b>succes_procent_mål:</b>", succes_procent_mål
        ),
        hoverinfo = "text"
) %>% layout(
  title = " \n3D Clustering over afleveringer og skud i Superligaen \nog 1. division fordelt på spillere",
  scene = list(
    xaxis = list(title = "PC1 (Gns. længde på succes.afl. vs. gns. x-position ved afl.)"),
    yaxis = list(title = "       PC2 (Scoringspct. (mål/skud) vs. gns. x-position ved afl.)"),
    zaxis = list(title = "PC3 (Scoringspct. (mål/skud) vs. succesfuld afleveringspct.)")
  )
)



sort(abs(pca$rotation[,1]), decreasing = TRUE)
pca$rotation[,3]
pca$rotation
pca$rotation[,1:3]

#################################################################################
#################################################################################
#################################################################################

#kun 1. div

spiller_statistik_shots_passes <- spiller_statistik_shots_passes %>% filter(SEASON_WYID %in% c(189933,191620))
  
# kmeans
vars <- spiller_statistik_shots_passes[, c(
  "gns_X",
  "succes_procent",
  "gns_længde_succes",
  "gns_afstand_mål",
  "gns_vinkel_mål",
  "succes_procent_mål"
)]


vars$original_row <- as.numeric(rownames(vars))
vars_scaled_df <-as.data.frame(scale(vars[, !colnames(vars) %in% "original_row"]))
vars_scaled <-as.data.frame(scale(vars[, !colnames(vars) %in% "original_row"]))
wss <- list()





for (k in 1:10) {
  wss[k] <- sum(kmeans(vars_scaled, centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type="b")
kmodel <- kmeans(vars_scaled, centers = 6, nstart = 25)
kval=kmodel$betweenss / kmodel$totss
kval

# ADD CLUSTER TO DATA
spiller_statistik_shots_passes$cluster <- as.factor(kmodel$cluster)
###

nucol=colnames(vars_scaled_df)
nucol

#### SUMMARY PR CLUSTER
profilesup <- spiller_statistik_shots_passes %>% group_by(cluster) %>% 
  summarize(
    gns_X=mean(gns_X),
    succes_procent=mean(succes_procent),
    gns_længde_succes=mean(gns_længde_succes),
    gns_afstand_mål=mean(gns_afstand_mål),
    gns_vinkel_mål=mean(gns_vinkel_mål),
    succes_procent_mål=mean(succes_procent_mål),
    size=n()
  )


# FOR VIZ - PCA or ORIGINAL DATA?
fviz_cluster(kmodel,
             data = vars_scaled,
             geom = "point",
             stand = FALSE,
             ellipse.type = "norm",
             ggtheme = theme_minimal())


pca <- prcomp(vars_scaled)
summary(pca)

# dominans
pca$rotation
pca$x[,1]

pca_df <- data.frame(
  # PCA koordinater (de nye akser)
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  PC3 = pca$x[,3],
  
  # Metadata fra din aggregering
  cluster = factor(spiller_statistik_shots_passes$cluster),
  PLAYER_WYID = spiller_statistik_shots_passes$PLAYER_WYID,
  SHORTNAME = spiller_statistik_shots_passes$SHORTNAME,
  ROLENAME = spiller_statistik_shots_passes$ROLENAME,
  TEAMNAME = spiller_statistik_shots_passes$TEAMNAME,
  LIGA_FØR = spiller_statistik_shots_passes$`liga spillet før (328 er 1. div)`,
  
  
  # Dine originale cluster-variable (så du kan se dem i hover-info)
  gns_X = spiller_statistik_shots_passes$gns_X,
  gns_Y = spiller_statistik_shots_passes$gns_Y,
  succes_procent = spiller_statistik_shots_passes$succes_procent,
  gns_længde_succes = spiller_statistik_shots_passes$gns_længde_succes,
  gns_afstand_mål = spiller_statistik_shots_passes$gns_afstand_mål,
  gns_vinkel_mål = spiller_statistik_shots_passes$gns_vinkel_mål,
  succes_procent_mål=spiller_statistik_shots_passes$succes_procent_mål
  
)

ggplot(pca_df, aes(x=PC1, y=PC2, colour =cluster))+
  geom_point()+
  geom_text(aes(label=SHORTNAME),size=1.5 )


plot_ly(pca_df,
        x = ~PC1,
        y = ~PC2,
        z = ~PC3,
        color = ~cluster,
        colors = "Set1",
        type = "scatter3d",
        mode = "markers",  # Fjernet '+text' for at undgå rod på banen, navne ses ved hover
        marker = list(size = 5, opacity = 0.8),
        hovertext = ~paste(
          "<b>Navn:</b>", SHORTNAME,
          "<b>Hold:</b>", TEAMNAME,
          "<br><b>Rolle:</b>", ROLENAME,
          "<br><b>Liga spillet før:</b>", LIGA_FØR,
          
          "<br>---------------------------",
          "<br><b>Gns. Position (X):</b>", round(gns_X, 1),
          "<br><b>Succesrate_passes i procent:</b>", round(succes_procent, 1), "%",
          "<br><b>gns_længde afleveringer:</b>", round(gns_længde_succes, 1), "m",
          "<br><b>gns_afstand_mål:</b>", gns_afstand_mål,
          "<br><b>gns_vinkel_mål:</b>", gns_vinkel_mål,
          "<br><b>succes_procent_mål:</b>", succes_procent_mål
        ),
        hoverinfo = "text"
) %>% layout(
  title = " \n3D Clustering over afleveringer og skud i Superligaen \nog 1. division fordelt på spillere",
  scene = list(
    xaxis = list(title = "PC1 (Gns. længde på succes.afl. vs. gns. x-position ved afl.)"),
    yaxis = list(title = "       PC2 (Scoringspct. (mål/skud) vs. gns. x-position ved afl.)"),
    zaxis = list(title = "PC3 (Scoringspct. (mål/skud) vs. succesfuld afleveringspct.)")
  )
)



sort(abs(pca$rotation[,1]), decreasing = TRUE)
pca$rotation[,3]
pca$rotation
pca$rotation[,1:3]

pca_df1 <- pca_df
