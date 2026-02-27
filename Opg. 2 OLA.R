# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(scales)
library(RMariaDB)

super_clean <- readRDS("super_clean.rds")
passes_med_xy <- readRDS("passes_med_xy.rds")
passes_succes_super <- readRDS("passes_succes_super.rds")
players <- readRDS("players.rds")
matches301 <- readRDS("matches301.rds")
assist <- readRDS("assist.rds")

#Kun succesfulde afleveringer
teamz_sod <- unique(na.omit(passes_med_xy$TEAM_WYID[passes_med_xy$SEASON_WYID %in% c(191611,189918,191620,189933)]))
filtered_df <- c()
for(i in 1:length(teamz_sod)){
  filtered_df2_sod <- passes_med_xy %>% filter(TEAM_WYID==teamz_sod[i])
  filtered_df1_sod <- filtered_df2_sod %>% filter(RECIPIENT_WYID %in% unique(filtered_df2_sod$PLAYER_WYID))
  
  filtered_df <- rbind(filtered_df, filtered_df1_sod)
}

# Laver succesfulde afleveringer for Superliga og 1. Division
passes_succes_sod <- filtered_df %>% filter(SEASON_WYID %in% c(191611,189918,191620,189933), PRIMARYTYPE=="pass", PLAYER_WYID != 0, RECIPIENT_WYID != 0)

#odd ones
season <- common %>% filter(SEASON_WYID %in% c(189918,189933, 191611, 191620))
odd_ones <- anti_join(matches301, season, by = "MATCH_WYID")

# Tjek af succesfulde afleveringer
table(passes_succes_sod$COMPETITION_WYID)

# 218549 afleveringer i 1. Division
# 235174 afleveringer i Superliga

############### Vi finder top og bund af Superliga og 1. Divison for 24/25 sæsonen
#Vi kører med top- og bund 6 da der er 12 hold i hver division

top_sup_2425 <- passes_med_xy %>%
  filter(
    SEASON_WYID == 189918,
    TEAM_WYID %in% c(7452, 7455, 7453, 7462, 7458, 7457),
    tolower(PRIMARYTYPE) == "pass"
  )

# 93020 aflveringer i alt
mean(top_sup_2425$LENGTH)
# 18,9m i gns
table(top_sup_2425$HEIGHT)
#13107 høje afleveringer

bund_sup_2425 <- passes_med_xy %>%
  filter(
    SEASON_WYID == 189918,
    TEAM_WYID %in% c(7461, 7456, 7499, 7473, 7484, 7454),
    tolower(PRIMARYTYPE) == "pass"
  )

# 82778 aflveringer i alt
mean(bund_sup_2425$LENGTH)
# 19,5m i gns
table(bund_sup_2425$HEIGHT)
#12901 høje afleveringer

top_div_2425 <- passes_med_xy %>%
  filter(
    SEASON_WYID == 189933,
    TEAM_WYID %in% c(7460, 7469, 7465, 7622, 7490, 7451),
    tolower(PRIMARYTYPE) == "pass"
  )

# 83376 aflveringer i alt
mean(top_div_2425$LENGTH)
# 19,7m i gns
table(top_div_2425$HEIGHT)
#15095 høje afleveringer

bund_div_2425 <- passes_med_xy %>%
  filter(
    SEASON_WYID == 189933,
    TEAM_WYID %in% c(7699, 7510, 7470, 7615, 7488, 7497),
    tolower(PRIMARYTYPE) == "pass"
  )

# 83710 aflveringer i alt
mean(bund_div_2425$LENGTH)
# 20,4m i gns
table(bund_div_2425$HEIGHT)
#16058 høje afleveringer

############ Succesfulde afleveringer #########################

top_sup_2425_suc <- passes_succes_sod %>%
  filter(
    SEASON_WYID == 189918,
    TEAM_WYID %in% c(7452, 7455, 7453, 7462, 7458, 7457),
    tolower(PRIMARYTYPE) == "pass"
  )

# 79864 succesfulde aflveringer i alt
mean(top_sup_2425_suc$LENGTH)
# 17,8m i gns længde for succesfulde
table(top_sup_2425_suc$HEIGHT)
#7393 succesfulde høje afleveringer

bund_sup_2425_suc <- passes_succes_sod %>%
  filter(
    SEASON_WYID == 189918,
    TEAM_WYID %in% c(7461, 7456, 7499, 7473, 7484, 7454),
    tolower(PRIMARYTYPE) == "pass"
  )

# 69643 succesfulde aflveringer i alt
mean(bund_sup_2425_suc$LENGTH)
# 18,4m i gns
table(bund_sup_2425_suc$HEIGHT)
#7018 succesfulde høje afleveringer

top_div_2425_suc <- passes_succes_sod %>%
  filter(
    SEASON_WYID == 189933,
    TEAM_WYID %in% c(7460, 7469, 7465, 7622, 7490, 7451),
    tolower(PRIMARYTYPE) == "pass"
  )

# 68849 aflveringer i alt
mean(top_div_2425_suc$LENGTH)
# 18,7m i gns
table(top_div_2425_suc$HEIGHT)
#8432 høje afleveringer

bund_div_2425_suc <- passes_succes_sod %>%
  filter(
    SEASON_WYID == 189933,
    TEAM_WYID %in% c(7699, 7510, 7470, 7615, 7488, 7497),
    tolower(PRIMARYTYPE) == "pass"
  )

# 83710 aflveringer i alt
mean(bund_div_2425_suc$LENGTH)
# 19,2m i gns
table(bund_div_2425_suc$HEIGHT)
#8781 høje afleveringer

################ Visualisering af afleveringsmønstre #############################

library(dplyr)

passes_all_compare <- bind_rows(
  top_sup_2425  %>% mutate(group = "Top Superliga"),
  bund_sup_2425 %>% mutate(group = "Bund Superliga"),
  top_div_2425  %>% mutate(group = "Top 1. division"),
  bund_div_2425 %>% mutate(group = "Bund 1. division")
)

passes_suc_compare <- bind_rows(
  top_sup_2425_suc  %>% mutate(group = "Top Superliga"),
  bund_sup_2425_suc %>% mutate(group = "Bund Superliga"),
  top_div_2425_suc  %>% mutate(group = "Top 1. division"),
  bund_div_2425_suc %>% mutate(group = "Bund 1. division")
)

success_share <- passes_all_compare %>%
  summarise(
    n_all = n(),
    .by = group
  ) %>%
  left_join(
    passes_suc_compare %>%
      summarise(n_suc = n(), .by = group),
    by = "group"
  ) %>%
  mutate(
    success_rate = n_suc / n_all
  )

library(dplyr)
library(ggplot2)
library(scales)

plot_df <- success_share %>%
  mutate(
    group = factor(group, levels = group[order(success_rate)])  # sortér lav→høj
  )

ggplot(plot_df, aes(x = group, y = success_rate, fill = group)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = percent(success_rate, accuracy = 0.1)),
    vjust = -0.4,
    fontface = "bold",
    size = 4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0.7, 0.90, by = 0.03),
    expand = expansion(mult = c(0, 0.08))
  ) +
  coord_cartesian(ylim = c(0.7, 0.90)) +
  labs(
    title = "Andel succesfulde afleveringer",
    subtitle = "Toppen af Superligaen har den højeste succesrate",
    x = NULL, y = "Succesrate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(size = 11),
    panel.grid.major.x = element_blank()
  )
############### Gns længde på succesfulde afleveringer

length_summary <- passes_suc_compare %>%
  summarise(
    mean_length = mean(LENGTH, na.rm = TRUE),
    .by = group
  ) %>%
  mutate(
    group = factor(group, levels = group[order(mean_length)])
  )

ggplot(length_summary, aes(x = group, y = mean_length, fill = group)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(mean_length, 1), " m")),
    vjust = -0.4,
    fontface = "bold",
    size = 4
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Gennemsnitlig længde på succesfulde afleveringer",
    subtitle = "Den gns længde på succesfulde afleveringer er længst i bunden af 1. division",
    x = NULL,
    y = "Meter"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(size = 11),
    panel.grid.major.x = element_blank()
  )


############# Afleveringer efter det 80. minut

library(dplyr)
library(ggplot2)
library(scales)

passes_all_80 <- passes_all_compare %>%
  filter(MINUTE >= 80)

passes_suc_80 <- passes_suc_compare %>%
  filter(MINUTE >= 80)

success_share_80 <- passes_all_80 %>%
  summarise(n_all = n(), .by = group) %>%
  left_join(
    passes_suc_80 %>% summarise(n_suc = n(), .by = group),
    by = "group"
  ) %>%
  mutate(
    n_suc = coalesce(n_suc, 0L),
    success_rate = n_suc / n_all,
    group = factor(group, levels = group[order(success_rate)])
  )

ggplot(success_share_80, aes(x = group, y = success_rate, fill = group)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = percent(success_rate, accuracy = 0.1)),
            vjust = -0.4, fontface = "bold", size = 4) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.10))) +
  coord_cartesian(ylim = c(0.70, 0.90)) +  # justér efter jeres niveau
  labs(
    title = "Andel succesfulde afleveringer efter 80. minut",
    subtitle = "Afleveringer efter 80. minut er mere succesfulde i toppen af Superligaen",
    x = NULL, y = "Succesrate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(size = 11),
    panel.grid.major.x = element_blank()
  )

##### Gns længde på aflevering efter det 80. minut

length_summary_80 <- passes_suc_80 %>%
  summarise(mean_length = mean(LENGTH, na.rm = TRUE), .by = group) %>%
  mutate(group = factor(group, levels = group[order(mean_length)]))

ggplot(length_summary_80, aes(x = group, y = mean_length, fill = group)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(mean_length, 1), " m")),
            vjust = -0.4, fontface = "bold", size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(
    title = "Gennemsnitlig pasningslængde (succesfulde) efter 80. minut",
    subtitle = "Mere længde på succesfulde afleveringer i bunden af 1. Division",
    x = NULL, y = "Meter"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(size = 11),
    panel.grid.major.x = element_blank()
  )
######################## Samenl. af succesrate på afleveringer før og efter 80, minut
library(dplyr)
library(ggplot2)
library(scales)

# Sikkerhed: MINUTE som tal
passes_all_compare <- passes_all_compare %>%
  mutate(MINUTE = as.numeric(MINUTE))

passes_suc_compare <- passes_suc_compare %>%
  mutate(MINUTE = as.numeric(MINUTE))

# Lav tidsbin: 0-79 vs 80+
all_bin <- passes_all_compare %>%
  mutate(period = ifelse(MINUTE >= 80, "80+", "0-79")) %>%
  count(group, period, name = "n_all")

suc_bin <- passes_suc_compare %>%
  mutate(period = ifelse(MINUTE >= 80, "80+", "0-79")) %>%
  count(group, period, name = "n_suc")

rate_80_vs_rest <- all_bin %>%
  left_join(suc_bin, by = c("group", "period")) %>%
  mutate(
    n_suc = coalesce(n_suc, 0L),
    success_rate = n_suc / n_all,
    period = factor(period, levels = c("0-79", "80+"))
  )

ggplot(rate_80_vs_rest, aes(x = group, y = success_rate, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = percent(success_rate, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    fontface = "bold",
    size = 4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Succesrate på afleveringer: 80+ vs resten af kampen",
    subtitle = "Succesraten falder efter 80. minut",
    x = NULL,
    y = "Succesrate",
    fill = "Periode"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank()
  )

####################################

library(dplyr)
library(tidyr)

matches_in_seasons <- passes_med_xy %>%
  filter(SEASON_WYID %in% c(189918, 189933, 191611, 191620)) %>%
  distinct(MATCH_WYID)

assist_seasons <- matches_in_seasons %>%
  semi_join(matches_in_seasons, by = "MATCH_WYID")


all_assist <- assist %>%
  mutate(across(starts_with("SECONDARYTYPE"), as.character)) %>%
  pivot_longer(
    cols = starts_with("SECONDARYTYPE"),
    names_to = "secondary_col",
    values_to = "secondary_value"
  ) %>%
  filter(tolower(secondary_value) == "assist") %>%
  distinct()

### Merge med SEASON_WYID

match_season_lookup <- passes_med_xy %>%
  select(MATCH_WYID, SEASON_WYID) %>%
  distinct() %>%
  group_by(MATCH_WYID) %>%
  summarise(SEASON_WYID = dplyr::first(SEASON_WYID), .groups = "drop")

assist_with_season <- assist %>%
  left_join(match_season_lookup, by = "MATCH_WYID")

all_assist <- assist_with_season %>%
  filter(SEASON_WYID %in% c(189918, 189933, 191611, 191620)) %>%
  mutate(across(starts_with("SECONDARYTYPE"), as.character)) %>%
  filter(if_any(starts_with("SECONDARYTYPE"), ~ tolower(.) == "assist"))

table(all_assist$SEASON_WYID)
#352 assist 24/25 sæsonen - 411 irl - 3 som er "odd ones"

all_assist <- all_assist %>%
  left_join(
    passes_med_xy %>%
      select(EVENT_WYID, LENGTH, PLAYER_WYID, TEAM_WYID),
    by = "EVENT_WYID"
  )

players <- dbGetQuery(con, "SELECT * FROM superliga2.wyscout_players;")

all_assist <- all_assist %>%
  left_join(
    players %>% select(PLAYER_WYID, SHORTNAME, ROLENAME),
    by = "PLAYER_WYID"
  )

all_assist <- all_assist %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

#### Nu laver vi sammenligning af længde på assists mellem 1. div og suppen

library(dplyr)
library(ggplot2)

colnames(all_assist)

assist_length <- all_assist %>%
  filter(
    PRIMARYTYPE == "pass",
    SEASON_WYID %in% c(189918, 189933)
  ) %>%
  mutate(liga = case_when(
    SEASON_WYID == 189918 ~ "Superliga",
    SEASON_WYID == 189933 ~ "1. division"
  )) %>%
  group_by(liga) %>%
  summarise(
    mean_length = mean(LENGTH, na.rm = TRUE),
    .groups = "drop"
  )

y_min <- min(assist_length$mean_length) * 0.95
y_max <- max(assist_length$mean_length) * 1.05

ggplot(assist_length, aes(x = liga, y = mean_length, fill = liga)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(mean_length, 1), " m")),
    vjust = -0.4,
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "Superliga" = "#1F4AFF",
      "1. division" = "#D62828"
    )
  ) +
  coord_cartesian(ylim = c(y_min, y_max)) +
  labs(
    title = "Gennemsnitlig længde på assists",
    subtitle = "Mere længde på assists i Superligaen",
    x = "",
    y = "Gennemsnitlig længde (meter)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

####### Oversigt over top assist folk

pass_assists <- all_assist %>%
  filter(
    PRIMARYTYPE == "pass",
    SEASON_WYID %in% c(189918, 189933)
  ) %>%
  group_by(SEASON_WYID, PLAYER_WYID, SHORTNAME) %>%
  summarise(
    assists = n(),
    .groups = "drop"
  )

pass_assists %>%
  filter(SEASON_WYID == 189918) %>%
  slice_max(order_by = assists, n = 10, with_ties = FALSE) %>%  # <-- præcis 10
  ggplot(aes(x = reorder(SHORTNAME, assists), y = assists)) +
  geom_col(fill = "#1F4AFF") +
  coord_flip() +
  labs(
    title = "Top 10 spillere med flest assists",
    subtitle = "Simen Nordli med flest assists i 1. Div i 24/25 sæsonen",
    x = "",
    y = "Antal assists"
  ) +
  theme_minimal()

pass_assists %>%
  filter(SEASON_WYID == 189933) %>%
  slice_max(assists, n = 10, with_ties = FALSE) %>%
  ggplot(aes(x = reorder(SHORTNAME, assists), y = assists)) +
  geom_col(fill = "#D62828") +
  coord_flip() +
  labs(
    title = "Top 10 spillere med flest assists",
    subtitle = "Tobias Arndal med flest assists i 1. Div i 24/25 sæsonen",
    x = "",
    y = "Antal assists"
  ) +
  theme_minimal()

################## Fordeling af assist fra positioner

pos_share <- all_assist %>%
  filter(
    PRIMARYTYPE == "pass",
    SEASON_WYID %in% c(189918, 189933),
    !is.na(ROLENAME)
  ) %>%
  mutate(liga = case_when(
    SEASON_WYID == 189918 ~ "Superliga",
    SEASON_WYID == 189933 ~ "1. division"
  )) %>%
  group_by(liga, ROLENAME) %>%
  summarise(assists = n_distinct(EVENT_WYID), .groups = "drop") %>%  # robust mod dubletter
  group_by(liga) %>%
  mutate(share = assists / sum(assists)) %>%
  ungroup()

ggplot(pos_share, aes(x = reorder(ROLENAME, share), y = share, fill = liga)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Superliga" = "#1F4AFF", "1. division" = "#D62828")) +
  labs(
    title = "Andel af 'pass'-assists fordelt på position",
    subtitle = "Andel af samlede assists er størst for midtbanespillere",
    x = "",
    y = "Andel af pass-assists",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

########## Mulig tilkobling af spillerværdier

# Spillerværdier
# install.packages("worldfootballR")
library(worldfootballR)
library(dplyr)

# Eksempel: hent markedsværdier for en liga/sæson via Transfermarkt URL / country_name
# Se dokumentationen for de præcise parametre for jeres liga og år.
mv <- tm_player_market_values(country_name = "Denmark", start_year = 2024)
