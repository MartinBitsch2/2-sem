# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(scales)
library(RMariaDB)

#forbindelse
con <- dbConnect(MariaDB(),
                 host="talmedos.com",
                 port="3306",
                 db="superliga2",
                 user="dalremote",
                 password="OttoRehagel123456789Long2026!"
)


players <- dbGetQuery(con,"SELECT * FROM superliga2.wyscout_players;")
saveRDS(players, file = "players.rds")
##########################################################################################################################################

#primarytypes for 24/25 og 25/26 fra matchevents_passes. Både Superliga og 1. div
passes_season <- left_join(matches301, passes, by="MATCH_WYID")
passes_med_xy <- left_join(passes_season, common[, c(3,5,6,14,16,12,13)] %>% distinct(EVENT_WYID, .keep_all = TRUE),by = "EVENT_WYID")
passes_med_xy <- passes_med_xy[,c(1,2,4,3,13,14,5,6,7,8,9,10,15,16,11,12)]

saveRDS(passes_med_xy, file = "passes_med_xy.rds")

#Kun succesfulde afleveringer
teamz <- unique(na.omit(passes_med_xy$TEAM_WYID[passes_med_xy$SEASON_WYID %in% c(191611,189918)]))
filtered_df <- c()
for(i in 1:length(teamz)){
  filtered_df2 <- passes_med_xy %>% filter(TEAM_WYID==teamz[i])
  filtered_df1 <- filtered_df2 %>% filter(RECIPIENT_WYID %in% unique(filtered_df2$PLAYER_WYID))
  
  filtered_df <- rbind(filtered_df, filtered_df1)
}
passes_succes_super <- filtered_df %>% filter(SEASON_WYID %in% c(191611,189918), PRIMARYTYPE=="pass", PLAYER_WYID != 0, RECIPIENT_WYID != 0)
saveRDS(passes_succes_super, file = "passes_succes_super.rds")

#odd ones
season <- common %>% filter(SEASON_WYID %in% c(189918,189933, 191611, 191620))
odd_ones <- anti_join(matches301, season, by = "MATCH_WYID")

passes_vff <- passes_med_xy %>%
  filter(TEAM_WYID == 7456,
         tolower(PRIMARYTYPE) == "pass")

passes_fcn <- passes_med_xy %>%
  filter(TEAM_WYID == 7458,
         tolower(PRIMARYTYPE) == "pass")

add_recipient_flags <- function(df_team, team_wyid) {
  
  # Egne spillere = alle PLAYER_WYID der optræder for holdet
  player_ids_team <- df_team %>%
    pull(PLAYER_WYID) %>%
    unique()
  
  df_out <- df_team %>%
    mutate(
      recipient_is_own = case_when(
        RECIPIENT_WYID == 0                  ~ FALSE,
        PLAYER_WYID == 0                     ~ FALSE,
        RECIPIENT_WYID %in% player_ids_team ~ TRUE,
        TRUE                                 ~ FALSE
      ),
      accurate_to_own = (ACCURATE == 1) & recipient_is_own
    )
  
  summary_tbl <- df_out %>%
    summarise(
      n_passes = n(),
      accurate_rate_raw = mean(ACCURATE == 1, na.rm = TRUE),
      accurate_rate_to_own = mean(accurate_to_own, na.rm = TRUE),
      n_recipient_zero = sum(RECIPIENT_WYID == 0, na.rm = TRUE),
      n_sender_zero    = sum(PLAYER_WYID == 0, na.rm = TRUE),
      n_recipient_not_own = sum(!recipient_is_own, na.rm = TRUE)
    )
  
  list(data = df_out, summary = summary_tbl)
}

vff <- add_recipient_flags(passes_vff, team_wyid = 7456)
fcn <- add_recipient_flags(passes_fcn, team_wyid = 7458)

passes_vff <- vff$data
passes_fcn <- fcn$data

vff$summary
fcn$summary

passes_vff %>%
  filter(!recipient_is_own) %>%
  count(RECIPIENT_WYID, sort = TRUE)

passes_fcn %>%
  filter(!recipient_is_own) %>%
  count(RECIPIENT_WYID, sort = TRUE)

passes_sup <- passes_med_xy %>% 
  filter(COMPETITION_WYID == 335,
         PRIMARYTYPE == "pass")

passes_vff_success <- passes_vff %>% filter(recipient_is_own == TRUE)
mean(passes_vff_success$LENGTH) 
passes_fcn_success <- passes_fcn %>% filter(recipient_is_own == TRUE)
mean(passes_fcn_success$LENGTH)

#### VISUALISERING AF SUCCESFULDE AFLEVERINGER FOR VFF OG FCN SAMMENHOLDT MED LIGAEN

# VFF Succesrate
table(passes_vff$recipient_is_own)
3122/21682
# 86% succesrate

# FCN succesrate
table(passes_fcn$recipient_is_own)
3248/31330
# 90% succesrate

# Superliga succesrate
# 235174 succesfulde afleveringer i Suppen 276393 afleveringer i alt
235174/276393
# 85% succesrate

### PLOT

library(dplyr)
library(ggplot2)

# -------------------------
# Samlet succesrate
# -------------------------
pass_summary <- tibble(
  team = c("VFF", "FCN", "Superligaen"),
  success = c(
    nrow(passes_vff_success),
    nrow(passes_fcn_success),
    nrow(passes_succes_super)
  ),
  total = c(
    nrow(passes_vff),
    nrow(passes_fcn),
    nrow(passes_sup)
  )
) %>%
  mutate(success_rate = success / total)

pass_summary

ggplot(pass_summary, aes(x = team, y = success_rate, fill = team)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = percent(success_rate, accuracy = 0.1)),
    vjust = -0.5,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0.7, 1)) +
  scale_fill_manual(values = c(
    "FCN" = "#d7191c",
    "VFF" = "#2ca02c",
    "Superligaen" = "grey70"
  )) +
  labs(
    title = "Succesrate på afleveringer",
    subtitle = "Y-akse zoomet (70–100%) for at fremhæve forskelle",
    x = NULL,
    y = "Succesrate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

#########################################

# Regner gennemsnit af længde på succesfulde afleveringer

library(dplyr)
library(ggplot2)
library(scales)

len_summary <- tibble(
  team = c("VFF", "FCN", "Superligaen"),
  mean_len = c(
    mean(passes_vff_success$LENGTH, na.rm = TRUE),
    mean(passes_fcn_success$LENGTH, na.rm = TRUE),
    mean(passes_succes_super$LENGTH, na.rm = TRUE)
  )
)

ggplot(len_summary, aes(x = team, y = mean_len, fill = team)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f m", mean_len)),
            vjust = -0.4, fontface = "bold") +
  scale_fill_manual(values = c("FCN"="#d7191c","VFF"="#2ca02c","Superligaen"="grey70")) +
  scale_y_continuous(labels = label_number(decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Gennemsnitlig pasningslængde (succesfulde afleveringer)",
    x = NULL,
    y = "Meter"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

################# NYT PLOT GNS LÆNGDE
library(dplyr)
library(ggplot2)
library(scales)

bins <- tibble::tibble(
  low   = c(0, 10, 20, 30),
  high  = c(10, 20, 30, Inf),
  label = c("0–10 m", "10–20 m", "20–30 m", "30+ m")
)

# Saml alle afleveringer + markér succes
all_len <- bind_rows(
  passes_vff        %>% transmute(team = "VFF", length = as.numeric(LENGTH), success = 0L),
  passes_fcn        %>% transmute(team = "FCN", length = as.numeric(LENGTH), success = 0L),
  passes_sup        %>% transmute(team = "Superligaen", length = as.numeric(LENGTH), success = 0L),
  
  passes_vff_success %>% transmute(team = "VFF", length = as.numeric(LENGTH), success = 1L),
  passes_fcn_success %>% transmute(team = "FCN", length = as.numeric(LENGTH), success = 1L),
  passes_succes_super %>% transmute(team = "Superligaen", length = as.numeric(LENGTH), success = 1L)
) %>%
  filter(is.finite(length), length >= 0)

# Funktion til at bin'e og tælle
bin_counts <- function(df, team_name) {
  df %>%
    transmute(team = team_name, length = as.numeric(LENGTH)) %>%
    filter(is.finite(length), length >= 0) %>%
    mutate(
      length_bin = cut(
        length,
        breaks = c(0, 10, 20, 30, Inf),
        right = FALSE,
        labels = c("0–10 m", "10–20 m", "20–30 m", "30+ m")
      )
    ) %>%
    filter(!is.na(length_bin)) %>%
    count(team, length_bin, name = "n")
}

# Total pr bin
totals <- bind_rows(
  bin_counts(passes_vff, "VFF"),
  bin_counts(passes_fcn, "FCN"),
  bin_counts(passes_sup, "Superligaen")
) %>% rename(n_total = n)

# Succes pr bin
successes <- bind_rows(
  bin_counts(passes_vff_success, "VFF"),
  bin_counts(passes_fcn_success, "FCN"),
  bin_counts(passes_succes_super, "Superligaen")
) %>% rename(n_success = n)

# Join + succesrate pr bin
rate_by_bin <- totals %>%
  left_join(successes, by = c("team", "length_bin")) %>%
  mutate(
    n_success = coalesce(n_success, 0L),
    success_rate = n_success / n_total
  )

ggplot(rate_by_bin, aes(x = team, y = success_rate, fill = team)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(success_rate, accuracy = 0.1)),
            vjust = -0.4, fontface = "bold") +
  facet_wrap(~length_bin, nrow = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = c("FCN"="#d7191c","VFF"="#2ca02c","Superligaen"="grey70")) +
  labs(
    title = "Succesrate pr. pasningslængde-interval",
    subtitle = "Succesrate = succesfulde / alle afleveringer i intervallet",
    x = NULL, y = "Succesrate"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# ------------------------------------------------------------
# 4) Samlet DF til plotting (begge hold)
# ------------------------------------------------------------
passes_df <- bind_rows(
  passes_vff_success %>% mutate(team = "VFF"),
  passes_fcn_success %>% mutate(team = "FCN")
)

# Range-check (viser at coords er ~0-100)
range(as.numeric(passes_df$ENDLOCATIONX), na.rm = TRUE)
range(as.numeric(passes_df$ENDLOCATIONY), na.rm = TRUE)

# ------------------------------------------------------------
# 5) Plot: 9 zoner (3x3) på modstanderens halvdel (x = 50..100)
#    Røde zoner + procentlabel i hver zone
# ------------------------------------------------------------
plot_9_zones_opponent_half_pct_red <- function(passes_df, title) {
  
  df <- passes_df %>%
    mutate(
      ENDLOCATIONX = as.numeric(ENDLOCATIONX),
      ENDLOCATIONY = as.numeric(ENDLOCATIONY)
    ) %>%
    filter(!is.na(ENDLOCATIONX), !is.na(ENDLOCATIONY)) %>%
    filter(between(ENDLOCATIONX, 50, 100),
           between(ENDLOCATIONY, 0, 100))
  
  # 9 zoner = 3 x 3 på modstanderens halvdel
  x_breaks <- seq(50, 100, length.out = 4)   # 50–66.7–83.3–100
  y_breaks <- seq(0, 100, length.out = 4)    # 0–33.3–66.7–100
  
  df_z <- df %>%
    mutate(
      x_bin = cut(ENDLOCATIONX, breaks = x_breaks, include.lowest = TRUE, right = FALSE),
      y_bin = cut(ENDLOCATIONY, breaks = y_breaks, include.lowest = TRUE, right = FALSE)
    ) %>%
    count(x_bin, y_bin, name = "n") %>%
    mutate(
      pct = n / sum(n),
      label = sprintf("%.1f%%", 100 * pct),
      x_idx = as.integer(x_bin),
      y_idx = as.integer(y_bin),
      x_center = (x_breaks[x_idx] + x_breaks[x_idx + 1]) / 2,
      y_center = (y_breaks[y_idx] + y_breaks[y_idx + 1]) / 2,
      tile_w = diff(x_breaks)[1],
      tile_h = diff(y_breaks)[1]
    )
  
  ggplot(df_z, aes(x = x_center, y = y_center, fill = pct)) +
    annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
    geom_tile(aes(width = tile_w, height = tile_h),
              color = "white", linewidth = 0.6, alpha = 0.9) +
    geom_text(aes(label = label), size = 5, fontface = "bold") +
    coord_fixed(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
    theme_pitch() +
    scale_fill_gradient(
      low = "white",
      high = "red",
      labels = percent_format(accuracy = 1)
    ) +
    labs(title = title, fill = "Andel") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# ------------------------------------------------------------
# 6) Kør plots (et for hver hold)
# ------------------------------------------------------------
print(plot_9_zones_opponent_half_pct_red(
  passes_df %>% filter(team == "FCN"),
  "FCN – 9 modtagezoner (modstanderens halvdel)"
))

print(plot_9_zones_opponent_half_pct_red(
  passes_df %>% filter(team == "VFF"),
  "VFF – 9 modtagezoner (modstanderens halvdel)"
))


# ------------------------------------------------------------
# 7) Andel "høje" afleveringer blandt succesfulde afleveringer
#    (Her antages HEIGHT = 1 betyder høj aflevering)
# ------------------------------------------------------------
table(passes_vff$HEIGHT)
3001/21682
#14% af VFF afleveringer er høje
table(passes_vff_success$HEIGHT)
1706/3001
#57% af høje afleveringer er succesfulde

table(passes_fcn$HEIGHT)
2962/31330
#9% af FCN afleveringer er høje
table(passes_fcn_success$HEIGHT)
1641/2962
#55% af høje afleveringer er succesfulde

table(passes_sup$HEIGHT)
40284/276393
#14% af afleveringer i ligaen er høje
table(passes_succes_super$HEIGHT)
22101/40284
#55% af høje afleveringer er succesfulde