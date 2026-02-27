#Vi laver et dataframe, hvor vi kun har skud fra 24/25 sæsonen
shots_2425 <- shots_med_xy %>%
  filter(SEASON_WYID %in% c(189918, 189933))

## Nu splitter vi de to ligaer fra hinanden
super_2425 <- shots_2425 %>% filter(COMPETITION_WYID == 335)
div1_2425  <- shots_2425 %>% filter(COMPETITION_WYID == 328)

## Nu definerer vi top 6 og bund 6
# SUPERLIGA
super_top6 <- c(7452,7455,7453,7462,7458,7457)
super_bot6 <- c(7461,7456,7499,7473,7484,7454)

# 1. DIVISION
div1_top6 <- c(7460,7469,7465,7622,7490,7451)
div1_bot6 <- c(7699,7510,7470,7615,7488,7497)

### Vi putter label på skudene
#Superligaen
super_2425 <- super_2425 %>%
  mutate(gruppe = case_when(
    TEAM_WYID %in% super_top6 ~ "Top 6",
    TEAM_WYID %in% super_bot6 ~ "Bund 6",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(gruppe))

#1. Division
div1_2425 <- div1_2425 %>%
  mutate(gruppe = case_when(
    TEAM_WYID %in% div1_top6 ~ "Top 6",
    TEAM_WYID %in% div1_bot6 ~ "Bund 6",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(gruppe))

####### Nu kigger vi på hvor mange skud henholdsvis Superligaen og 1.divisionen tager. 
#Superliga
super_skud_antal <- super_2425 %>%
  count(gruppe)
super_skud_antal

#Top 6 tager 2426 skud
#Bund 6 tager 2067 skud

#1.Division
div1_skud_antal <- div1_2425 %>%
  count(gruppe)
div1_skud_antal

#Top 6 tager 2526 skud
#Bund 6 tager 2120


##### Nu laver vi lige et plot over andelen af skud fra hver af de 4 grupper.
#Først samler vi de to ligaer
plot_skud <- bind_rows(
  super_2425 %>% mutate(liga = "Superliga"),
  div1_2425  %>% mutate(liga = "1. Division")
)

#Så tæller vi skudene i hver gruppe
plot_skud_count <- plot_skud %>%
  count(liga, gruppe)

### Nu prøver vi at lave det om til procent, så det bliver lettere sammenlignligt
plot_skud_pct <- plot_skud_count %>%
  group_by(liga) %>%
  mutate(pct = n / sum(n))

##Plotter det
ggplot(plot_skud_pct, aes(x = liga, y = pct, fill = gruppe)) +
  
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_dodge(width = 0.6),
            vjust = -0.4,
            fontface = "bold",
            size = 4.5) +
  
  scale_fill_manual(values = c(
    "Top 6" = "#2C7BB6",
    "Bund 6" = "#D7191C"
  )) +
  
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  
  labs(
    title = "Top 6 hold i Superligaen og 1.Divisionen står for største del af skudene i 24/25 sæsonen",
    subtitle = "Fordeling af skud mellem Top 6 og Bund 6 i sæsonen 2024/2025",
    x = NULL,
    y = "Andel af ligaens samlede skud",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    axis.text.x = element_text(face = "bold")
  )
####################################################################################################

##### Nu kigger vi så på afstanden til mål.
#Først beregner vi gennemsnittet
afstand_mean <- plot_skud %>%
  group_by(liga, gruppe) %>%
  summarise(
    mean_afstand = mean(afstand_til_mål, na.rm = TRUE),
    .groups = "drop"
  )

## Lad os prøve at plotte det
ggplot(afstand_mean, aes(x = liga, y = mean_afstand, fill = gruppe)) +
  
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  
  geom_text(aes(label = round(mean_afstand, 1)),
            position = position_dodge(width = 0.6),
            vjust = -0.4,
            fontface = "bold",
            size = 4.5) +
  
  scale_fill_manual(values = c(
    "Top 6" = "#2C7BB6",
    "Bund 6" = "#D7191C"
  )) +
  
  labs(
    title = "Tophold afslutter generelt tættere på mål end bundhold",
    subtitle = "Gennemsnitlig afstand til mål ved afslutninger (2024/2025)",
    x = NULL,
    y = "Afstand til mål (meter)",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

#################################################################
#Nu prøver vi at kigge på hvor skudene kommer fra på banen
library(dplyr)
library(ggplot2)
library(hexbin)

# ---- 1) Data til map (kun <35m hvis I vil holde det) ----
shots_map <- plot_skud %>%
  filter(!is.na(x_meter), !is.na(y_meter)) %>%
  filter(afstand_til_mål < 35)

# ---- 2) Konstanter til bane i meter ----
L <- 105
W <- 68

# Zoom (sidste 35m)
x_min <- 70
x_max <- 105

# Straffesparksfelt (16.5m dybt, 40.3m bredt)
pa_xmin <- L - 16.5          # 88.5
pa_xmax <- L
pa_ymin <- (W - 40.3) / 2    # 13.85
pa_ymax <- W - pa_ymin       # 54.15

# 5-meter felt (5.5m dybt, 18.3m bredt)
six_xmin <- L - 5.5          # 99.5
six_xmax <- L
six_ymin <- (W - 18.3) / 2   # 24.85
six_ymax <- W - six_ymin     # 43.15

# Straffesparksplet (11m fra mållinje)
pen_x <- L - 11              # 94
pen_y <- W / 2               # 34

# ---- 3) Plot ----
plt <- ggplot(shots_map, aes(x = x_meter, y = y_meter)) +
  
  # Heatmap (hexbin)
  stat_bin_hex(bins = 18, alpha = 0.95) +
  
  # Bane-omrids (kun den zoomede del)
  geom_rect(aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = W),
            fill = NA, colour = "grey70", linewidth = 0.6) +
  
  # Straffesparksfelt + 5-meter felt
  geom_rect(aes(xmin = pa_xmin, xmax = pa_xmax, ymin = pa_ymin, ymax = pa_ymax),
            fill = NA, colour = "grey70", linewidth = 0.6) +
  geom_rect(aes(xmin = six_xmin, xmax = six_xmax, ymin = six_ymin, ymax = six_ymax),
            fill = NA, colour = "grey70", linewidth = 0.6) +
  
  # Straffesparksplet
  geom_point(aes(x = pen_x, y = pen_y), colour = "grey70", size = 1.5) +
  
  coord_fixed(xlim = c(x_min, x_max), ylim = c(0, W), expand = FALSE) +
  
  facet_grid(gruppe ~ liga) +
  
  labs(
    title = "Hvorfra afsluttes der? Shot map for Top 6 vs Bund 6 (2024/2025)",
    subtitle = "Kun skud indenfor 35 meter — tæthed vist som hexbin (flere skud = varmere felter)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )

plt


##### Hygge plots
plt2 <- ggplot(shots_map, aes(x = x_meter, y = y_meter)) +
  
  # Smooth density (blødere end hex)
  stat_density_2d_filled(contour_var = "ndensity", alpha = 0.9, bins = 12) +
  
  # Bane-omrids (samme som før)
  geom_rect(aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = W),
            fill = NA, colour = "grey70", linewidth = 0.6) +
  geom_rect(aes(xmin = pa_xmin, xmax = pa_xmax, ymin = pa_ymin, ymax = pa_ymax),
            fill = NA, colour = "grey70", linewidth = 0.6) +
  geom_rect(aes(xmin = six_xmin, xmax = six_xmax, ymin = six_ymin, ymax = six_ymax),
            fill = NA, colour = "grey70", linewidth = 0.6) +
  geom_point(aes(x = pen_x, y = pen_y), colour = "grey70", size = 1.5) +
  
  coord_fixed(xlim = c(x_min, x_max), ylim = c(0, W), expand = FALSE) +
  facet_grid(gruppe ~ liga) +
  
  labs(
    title = "Afslutninger i Superligaen tages generelt tættere på mål end i 1. Division (2024/2025)",
    subtitle = "Kun skud indenfor 35 meter — tæthed vist som smooth density (blødere heatmap)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )

plt2
################## Vi cookede ovenover, så nu kører vi videre. 
###Vi har ikke XG pt, så den hopper vi lige over som at vi var højdespringere.

### Vi får lige defineret vores mål i en kolonne
plot_skud <- plot_skud %>%
  mutate(
    is_goal = ifelse(
      SHOTISGOAL == 1 |
        stringr::str_detect(SHOTBODYPART, "goal"),
      1, 0
    )
  )
table(plot_skud$is_goal)

### Nu vil vi sammenligne afstanden
#Først laver vi en læsbar tabel
plot_skud <- plot_skud %>%
  mutate(goal_label = ifelse(is_goal == 1, "Mål", "Ikke mål"))

##Gjorde vi åbenbart ovenover.
### Så håber vi at chatten kan cooke et plot
##Cooker endnu et nyt plot

#Først laver vi nogen zoner
plot_skud <- plot_skud %>%
  mutate(
    afstand_zone = case_when(
      afstand_til_mål <= 10 ~ "0–10m",
      afstand_til_mål <= 20 ~ "10–20m",
      TRUE ~ "20–35m"
    )
  )

#Så en procentfordeling
zone_pct <- plot_skud %>%
  group_by(liga, goal_label, afstand_zone) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(liga, goal_label) %>%
  mutate(pct = n / sum(n))

zone_pct <- zone_pct %>%
  group_by(liga, goal_label) %>%
  arrange(afstand_zone) %>%
  mutate(
    pct_label = round(pct * 100, 1),
    pct_label = ifelse(
      row_number() == n(),
      100 - sum(pct_label[-n()]),
      pct_label
    )
  )

#Det lune tydelige plot
ggplot(zone_pct, aes(x = afstand_zone, y = pct, fill = goal_label)) +
  
  geom_col(position = "dodge", width = 0.65) +
  
  geom_text(aes(label = paste0(pct_label, "%")),
            position = position_dodge(width = 0.65),
            vjust = -0.4,
            fontface = "bold",
            size = 4) +
  
  facet_wrap(~ liga) +
  
  scale_fill_manual(values = c(
    "Mål" = "#2C7BB6",
    "Ikke mål" = "#D7191C"
  )) +
  
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  
  labs(
    title = "Mål kommer oftere fra korte afstande end øvrige afslutninger",
    subtitle = "Fordeling af skudafstand i zoner (2024/2025)",
    x = "Afstand til mål",
    y = "Andel af skud",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )
##########
### Næste opgave
#Hvem scorer målene, forsvar, midtbane eller angreb.
### Nu har vi så fået lagt alt sammen ind i "plot_skud"
plot_skud <- plot_skud %>%
  left_join(
    players %>% select(PLAYER_WYID, SHORTNAME, ROLENAME),
    by = "PLAYER_WYID"
  )
plot_skud <- plot_skud %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

##### Vi filtrere på mål
mål_position <- plot_skud %>%
  filter(is_goal == 1, !is.na(ROLENAME))

#### Fordeler målene på liga
mål_pos_pct <- mål_position %>%
  group_by(liga, ROLENAME) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(liga) %>%
  mutate(pct = n / sum(n))

####SÅ SKAL DET PLOTTES
mål_pos_pct <- mål_pos_pct %>%
  mutate(
    ROLENAME = factor(ROLENAME,
                      levels = c("Goalkeeper",
                                 "Defender",
                                 "Midfielder",
                                 "Forward"))
  )

ggplot(mål_pos_pct, aes(x = ROLENAME, y = pct, fill = ROLENAME)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            vjust = -0.4,
            fontface = "bold",
            size = 4) +
  
  facet_wrap(~ liga) +
  
  scale_fill_manual(values = c(
    "Goalkeeper" = "#B07AA1",
    "Defender" = "#E15759",
    "Midfielder" = "#4E79A7",
    "Forward" = "#59A14F"
  )) +
  
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  
  labs(
    title = "Den største andel af mål i Superligaen og 1.divsionen bliver scoret af angribere(2024/2025)",
    subtitle = "Andel af mål scoret af forsvarere, midtbanespillere og angribere",
    x = NULL,
    y = "Andel af mål",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )
##########

##Så kører vi den sidste, top 10 spillere med flest mål
#Tager først kun mål
mål_spillere <- plot_skud %>%
  filter(is_goal == 1)

###Tæller mål pr. spiller
topscorere <- mål_spillere %>%
  group_by(liga, PLAYER_WYID, SHORTNAME) %>%
  summarise(mål = n(), .groups = "drop")

### Top 10 pr. liga
top10 <- topscorere %>%
  group_by(liga) %>%
  arrange(desc(mål)) %>%
  slice_head(n = 10)

#Vi plotter de gode målscorere
ggplot(top10, aes(x = reorder(SHORTNAME, mål), y = mål, fill = liga)) +
  
  geom_col() +
  
  coord_flip() +
  
  facet_wrap(~ liga, scales = "free_y") +
  
  labs(
    title = "Top 10 målscorere i Superligaen og 1. Division (2024/2025)",
    x = NULL,
    y = "Antal mål",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )




