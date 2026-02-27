# Opga. 2.2 - xG på skud og mål

library(dplyr)
library(ggplot2)

super_clean <- readRDS("super_clean.rds")
passes_med_xy <- readRDS("passes_med_xy.rds")

data_2425 <- super_clean %>%
  filter(SEASON_WYID %in% c(189933, 189918))

########## Finder xG før og efter 80. minut

data_2425 <- super_clean %>%
  filter(
    SEASON_WYID %in% c(189933, 189918),
    PRIMARYTYPE == "shot"
  ) %>%
  mutate(
    late_game = ifelse(MINUTE > 80, "Efter minut 80", "Minut 0–79")
  )

xg_summary <- data_2425 %>%
  group_by(competition, late_game) %>%
  summarise(
    mean_xG = mean(xG_pred, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(xg_summary, aes(x = late_game, y = mean_xG, fill = competition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(mean_xG, 3)),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "Gennemsnitlig xG pr. skud før og efter minut 80 (2024/2025)",
    x = "",
    y = "Gennemsnitlig xG pr. skud",
    fill = "Liga"
  ) +
  ylim(0, max(xg_summary$mean_xG) * 1.15) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )

table(data_2425$competition)

# Finder sammenhæng mellem top 10 målscorer og deres xG

top10_superliga <- data_2425 %>%
  filter(competition == "Superliga") %>%
  group_by(PLAYER_WYID, SHORTNAME) %>%
  summarise(
    goals = sum(goal),
    xG = sum(xG_pred, na.rm = TRUE),
    shots = n(),
    diff = goals - xG,
    .groups = "drop"
  ) %>%
  arrange(desc(goals)) %>%
  slice_head(n = 10)

top10_1div <- data_2425 %>%
  filter(competition == "1.div") %>%
  group_by(PLAYER_WYID, SHORTNAME) %>%
  summarise(
    goals = sum(goal),
    xG = sum(xG_pred, na.rm = TRUE),
    shots = n(),
    diff = goals - xG,
    .groups = "drop"
  ) %>%
  arrange(desc(goals)) %>%
  slice_head(n = 10)

top10_1div

ggplot(top10_superliga, aes(x = reorder(SHORTNAME, goals), y = goals)) +
  geom_col(fill = "steelblue") +
  geom_point(aes(y = xG), color = "red", size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 målscorere – Superligaen (2024/2025)",
    x = "",
    y = "Antal mål",
    caption = "Blå søjler = faktiske mål, røde punkter = samlet xG"
  ) +
  theme_minimal()

ggplot(top10_1div, aes(x = reorder(SHORTNAME, goals), y = goals)) +
  geom_col(fill = "darkgreen") +
  geom_point(aes(y = xG), color = "red", size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 målscorere – 1. division (2024/2025)",
    x = "",
    y = "Antal mål",
    caption = "Blå/grønne søjler = faktiske mål, røde punkter = samlet xG"
  ) +
  theme_minimal()
