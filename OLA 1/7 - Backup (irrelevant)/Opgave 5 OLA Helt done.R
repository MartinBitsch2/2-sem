##########################################################################################################################################
# Opg. 5
##########################################################################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(randomForest)


passes_med_xy <- readRDS("passes_med_xy.rds")
shots <- readRDS("shots.rds")
matches301 <- readRDS("matches301.rds")


##########################################################################################################################################
# SHOTS + koordinater
##########################################################################################################################################

# shots for 24/25 og 25/26 fra matchevents_shots. Både Superliga og 1. div
shots_season <- left_join(matches301, shots, by = "MATCH_WYID")

shots_med_xy <- left_join(
  shots_season,
  common[, c(2,3,5,6,14,16,12,13)] %>% distinct(EVENT_WYID, .keep_all = TRUE),
  by = "EVENT_WYID"
)

# koordinater fra procent til meter
afstand_vinkel <- shots_med_xy %>%
  mutate(
    x_meter = LOCATIONX * 0.01 * 105,
    y_meter = LOCATIONY * 0.01 * 68
  )

# afstand til mål (105, 34)
afstand_vinkel <- afstand_vinkel %>%
  mutate(
    afstand_til_mål = sqrt((105 - x_meter)^2 + (34 - y_meter)^2)
  )

# vinkel mellem stolper (cosinusrelation)
left_post  <- c(105, 34 + (7.32/2))
right_post <- c(105, 34 - (7.32/2))

afstand_vinkel <- afstand_vinkel %>%
  mutate(
    afstand_left  = sqrt((left_post[1]  - x_meter)^2 + (left_post[2]  - y_meter)^2),
    afstand_right = sqrt((right_post[1] - x_meter)^2 + (right_post[2] - y_meter)^2),
    vinkel_mellem_stolper = acos((afstand_left^2 + afstand_right^2 - 7.32^2) / (2 * afstand_left * afstand_right)) * 180/pi
  )

# behold alle kolonner (men fjern dubletter per event)
shots_med_xy <- afstand_vinkel %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

##########################################################################################################################################
# shots_2ligaer (kun de to ligaer) + join player info (SHORTNAME, ROLENAME)
##########################################################################################################################################

# robust player-key (undgår mange-til-mange joins)
players_key <- players %>%
  select(PLAYER_WYID, SHORTNAME, ROLENAME) %>%
  distinct(PLAYER_WYID, .keep_all = TRUE)

shots_2ligaer <- shots_med_xy %>%
  filter(
    COMPETITION_WYID %in% c(335, 328),
    afstand_til_mål < 35
  ) %>%
  left_join(players_key, by = "PLAYER_WYID") %>%
  mutate(
    competition = factor(
      COMPETITION_WYID,
      levels = c(335, 328),
      labels = c("Superliga", "1.div")
    )
  )

##########################################################################################################################################
# Super (model-df): outcome + hovedstød
##########################################################################################################################################

super <- shots_2ligaer %>%
  filter(
    afstand_til_mål < 35
  ) %>%
  mutate(
    skud = factor(
      ifelse(SHOTISGOAL == 1 | str_detect(SHOTBODYPART, "goal"), "mål", "ikke-mål"),
      levels = c("ikke-mål", "mål")
    ),
    goal = ifelse(skud == "mål", 1, 0),
    hovedstød = ifelse(SHOTBODYPART == "head_or_other", 1, 0),
    MINUTE = as.numeric(MINUTE),
    ROLENAME = factor(ROLENAME),
    hovedstød = as.integer(hovedstød)
  )

# rens til modeller (undgå NA-problemer)
super_clean <- super %>%
  drop_na(goal, skud, afstand_til_mål, vinkel_mellem_stolper, hovedstød, MINUTE, ROLENAME)

##########################################################################################################################################
# Train/test split (én gang)
##########################################################################################################################################

set.seed(123)
train_index <- createDataPartition(super_clean$skud, p = 0.8, list = FALSE)
train_data <- super_clean[train_index, ]
test_data  <- super_clean[-train_index, ]

# sikre levels i test
test_data$ROLENAME <- factor(test_data$ROLENAME, levels = levels(train_data$ROLENAME))

##########################################################################################################################################
# BESLUTNINGSTRÆ: fuldt træ + læsbart træ (target_splits)
##########################################################################################################################################

tree_full <- rpart(
  skud ~ afstand_til_mål + vinkel_mellem_stolper + hovedstød + MINUTE + ROLENAME,
  data = train_data,
  method = "class",
  control = rpart.control(minsplit = 20, cp = 0.0001, xval = 10)
)

printcp(tree_full)
print(sort(tree_full$variable.importance, decreasing = TRUE))

# lav læsbart træ til figur
target_splits <- 4
cp_table <- tree_full$cptable
idx <- which.min(abs(cp_table[, "nsplit"] - target_splits))
cp_vis <- cp_table[idx, "CP"]

tree_vis <- prune(tree_full, cp = cp_vis)

rpart.plot(
  tree_vis,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  tweak = 1.2,
  branch.lty = 3,
  main = paste0(
    "Beslutningstræ (", target_splits, " splits)\n",
    "Vinkel og afstand til mål er de vigtigste indikatorer i xG-modellen"
  )
)

pred_tree <- predict(tree_vis, test_data, type = "class")
confusionMatrix(pred_tree, test_data$skud)

##########################################################################################################################################
# xG-model (logistisk regression) + Brier + AUC + xG_pred
##########################################################################################################################################

xg_glm <- glm(
  goal ~ afstand_til_mål + vinkel_mellem_stolper + hovedstød + MINUTE + ROLENAME,
  data = train_data,
  family = binomial()
)

test_data$xG <- predict(xg_glm, newdata = test_data, type = "response")
brier <- mean((test_data$goal - test_data$xG)^2)
auc_val <- auc(roc(test_data$goal, test_data$xG))

brier
auc_val

# xG for alle skud i super_clean
super_clean$xG_pred <- predict(xg_glm, newdata = super_clean, type = "response")

#### Beregn sd på vores og Wyscout

# Filtrér til skud hvor Wyscout har xG > 0
xg_compare <- super_clean %>%
  filter(SHOTXG > 0) %>%
  select(SHOTXG, xG_pred)

# Antal observationer
nrow(xg_compare)

# Standardafvigelser
sd_wyscout <- sd(xg_compare$SHOTXG)
sd_ours    <- sd(xg_compare$xG_pred)

sd_wyscout
sd_ours

##########################################################################################################################################
# Random Forest xG + importance
##########################################################################################################################################

rf_xg <- randomForest(
  factor(goal) ~ afstand_til_mål + vinkel_mellem_stolper + hovedstød + MINUTE + ROLENAME,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

test_data$xG_rf <- predict(rf_xg, newdata = test_data, type = "prob")[, "1"]
importance(rf_xg)

# OPGAVE 5.3

# Antag: train_data og test_data findes og indeholder:
# skud (factor med levels c("ikke-mål","mål")) og goal (0/1)
# + afstand_til_mål, vinkel_mellem_stolper, hovedstød, MINUTE, ROLENAME

# Sikkerhed: ens factor-levels
test_data$ROLENAME <- factor(test_data$ROLENAME, levels = levels(train_data$ROLENAME))

############################################################
# HOVEDMODEL: Logistisk regression (GLM)
############################################################

glm_cls <- glm(
  goal ~ afstand_til_mål + vinkel_mellem_stolper + hovedstød + MINUTE + ROLENAME,
  data = train_data,
  family = binomial()
)

# Sandsynligheder på testdata
p_glm <- predict(glm_cls, newdata = test_data, type = "response")

# ROC og AUC
roc_glm  <- roc(test_data$goal, p_glm)
auc_glm  <- auc(roc_glm)

roc_glm
auc_glm

# Find optimal threshold på træningsdata
p_glm_train <- predict(glm_cls, newdata = train_data, type = "response")
roc_glm_train <- roc(train_data$goal, p_glm_train)

thr_glm <- coords(roc_glm_train, 
                  x = "best", 
                  best.method = "youden", 
                  transpose = FALSE)$threshold

thr_glm
#Threshold 0,12855 - passer godt med andelen af skud som bliver til mål

pred_glm_class <- factor(ifelse(p_glm >= thr_glm, "mål", "ikke-mål"),
                         levels = c("ikke-mål","mål"))

cm_glm <- confusionMatrix(pred_glm_class, test_data$skud)

cm_glm

############################################################
# Beslutningstræ (sekundær model)
############################################################

tree_cls <- rpart(
  skud ~ afstand_til_mål + vinkel_mellem_stolper + hovedstød + MINUTE + ROLENAME,
  data = train_data,
  method = "class",
  control = rpart.control(minsplit = 20, cp = 0.0001, xval = 10)
)

best_cp <- tree_cls$cptable[which.min(tree_cls$cptable[, "xerror"]), "CP"]
tree_pruned <- prune(tree_cls, cp = best_cp)

p_tree <- predict(tree_pruned, newdata = test_data, type = "prob")[, "mål"]
auc_tree <- auc(roc(test_data$goal, p_tree))


############################################################
# Random Forest (benchmark)
############################################################

set.seed(123)
rf_cls <- randomForest(
  skud ~ afstand_til_mål + vinkel_mellem_stolper + hovedstød + MINUTE + ROLENAME,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

p_rf <- predict(rf_cls, newdata = test_data, type = "prob")[, "mål"]
auc_rf <- auc(roc(test_data$goal, p_rf))

results <- data.frame(
  model = c("Logistisk regression (valgt model)",
            "Random Forest",
            "Beslutningstræ"),
  AUC = c(as.numeric(auc_glm),
          as.numeric(auc_rf),
          as.numeric(auc_tree))
)

results

plot(
  roc_glm,
  col = "darkblue",
  lwd = 3,
  legacy.axes = TRUE,
  main = "ROC-kurver – Logistisk regression som valgt model"
)

plot(
  roc(test_data$goal, p_rf),
  col = "darkgreen",
  lwd = 2,
  add = TRUE
)

plot(
  roc(test_data$goal, p_tree),
  col = "darkred",
  lwd = 2,
  add = TRUE
)

legend(
  "bottomright",
  legend = c(
    paste0("Logistisk regression (AUC = ", round(auc_glm, 3), ")"),
    paste0("Random Forest (AUC = ", round(auc_rf, 3), ")"),
    paste0("Beslutningstræ (AUC = ", round(auc_tree, 3), ")")
  ),
  col = c("darkblue", "darkgreen", "darkred"),
  lwd = c(3,2,2),
  bty = "n"
)

brier_glm <- mean((test_data$goal - p_glm)^2)
brier_glm

# --- GLM ---
p_glm_train <- predict(glm_cls, newdata = train_data, type = "response")
roc_glm_train <- roc(train_data$goal, p_glm_train)
thr_glm <- coords(roc_glm_train, x = "best", best.method = "youden", transpose = FALSE)$threshold

# --- Decision Tree ---
p_tree_train <- predict(tree_pruned, newdata = train_data, type = "prob")[, "mål"]
roc_tree_train <- roc(train_data$goal, p_tree_train)
thr_tree <- coords(roc_tree_train, x = "best", best.method = "youden", transpose = FALSE)$threshold

# --- Random Forest ---
p_rf_train <- predict(rf_cls, newdata = train_data, type = "prob")[, "mål"]
roc_rf_train <- roc(train_data$goal, p_rf_train)
thr_rf <- coords(roc_rf_train, x = "best", best.method = "youden", transpose = FALSE)$threshold

# --- GLM ---
pred_glm <- factor(ifelse(p_glm >= thr_glm, "mål", "ikke-mål"),
                   levels = c("ikke-mål","mål"))
cm_glm <- confusionMatrix(pred_glm, test_data$skud)

# --- Tree ---
p_tree <- predict(tree_pruned, newdata = test_data, type = "prob")[, "mål"]
pred_tree <- factor(ifelse(p_tree >= thr_tree, "mål", "ikke-mål"),
                    levels = c("ikke-mål","mål"))
cm_tree <- confusionMatrix(pred_tree, test_data$skud)

# --- RF ---
p_rf <- predict(rf_cls, newdata = test_data, type = "prob")[, "mål"]
pred_rf <- factor(ifelse(p_rf >= thr_rf, "mål", "ikke-mål"),
                  levels = c("ikke-mål","mål"))
cm_rf <- confusionMatrix(pred_rf, test_data$skud)

cm_compare <- data.frame(
  Model = c("Logistisk regression",
            "Random Forest",
            "Beslutningstræ"),
  
  Sensitivity = c(cm_glm$byClass["Sensitivity"],
                  cm_rf$byClass["Sensitivity"],
                  cm_tree$byClass["Sensitivity"]),
  
  Specificity = c(cm_glm$byClass["Specificity"],
                  cm_rf$byClass["Specificity"],
                  cm_tree$byClass["Specificity"]),
  
  Balanced_Accuracy = c(cm_glm$byClass["Balanced Accuracy"],
                        cm_rf$byClass["Balanced Accuracy"],
                        cm_tree$byClass["Balanced Accuracy"])
)

cm_compare

plot_cm <- function(cm_obj, title = "") {
  m <- cm_obj$table  # 2x2 matrix
  # Vend rækker for korrekt visning (som i jeres image() kode)
  image(t(m)[, nrow(m):1], axes = FALSE, main = title)
  axis(1, at = seq(0, 1, length.out = ncol(m)), labels = colnames(m))
  axis(2, at = seq(0, 1, length.out = nrow(m)), labels = rev(rownames(m)))
  text(expand.grid(x = seq(0, 1, length.out = ncol(m)),
                   y = seq(0, 1, length.out = nrow(m))),
       labels = as.vector(m), cex = 1.2)
}

par(mfrow = c(1, 3), mar = c(4, 4, 4, 1))

plot_cm(cm_glm,  "GLM (valgt)\nConfusion matrix")
plot_cm(cm_rf,   "Random Forest\nConfusion matrix")
plot_cm(cm_tree, "Beslutningstræ\nConfusion matrix")

par(mfrow = c(1, 1))



### Mulighed 2
metrics <- data.frame(
  Model = c("GLM", "RF", "Tree"),
  Sensitivity = c(cm_glm$byClass["Sensitivity"],
                  cm_rf$byClass["Sensitivity"],
                  cm_tree$byClass["Sensitivity"]),
  Specificity = c(cm_glm$byClass["Specificity"],
                  cm_rf$byClass["Specificity"],
                  cm_tree$byClass["Specificity"]),
  BalancedAcc = c(cm_glm$byClass["Balanced Accuracy"],
                  cm_rf$byClass["Balanced Accuracy"],
                  cm_tree$byClass["Balanced Accuracy"])
)

# omform til matrix til barplot
mat <- t(as.matrix(metrics[, c("Sensitivity","Specificity","BalancedAcc")]))
colnames(mat) <- metrics$Model

colnames(super_clean)

barplot(mat, beside = TRUE,
        legend.text = rownames(mat),
        main = "Model-sammenligning (testdata)",
        ylab = "Score",
        ylim = c(0, 1))

saveRDS(super_clean, file = "super_clean.rds")
