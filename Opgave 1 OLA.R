library(dplyr)

write.csv(passes_med_xy,
          file = "passes_med_xy",
          row.names = FALSE)

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
        RECIPIENT_WYID == 0                 ~ FALSE,
        RECIPIENT_WYID %in% player_ids_team ~ TRUE,
        TRUE                                ~ FALSE
      ),
      accurate_to_own = (ACCURATE == 1) & recipient_is_own
    )
  
  summary_tbl <- df_out %>%
    summarise(
      n_passes = n(),
      accurate_rate_raw = mean(ACCURATE == 1, na.rm = TRUE),
      accurate_rate_to_own = mean(accurate_to_own, na.rm = TRUE),
      n_recipient_zero = sum(RECIPIENT_WYID == 0, na.rm = TRUE),
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

# VFF Succesrate
3097/21682
# 86% succesrate

# FCN succesrate
3222/31330
# 90% succesrate

# Regner gennemsnit af længde på succesfulde afleveringer

passes_vff_success <- passes_vff %>% 
  filter(recipient_is_own == TRUE)

mean(passes_vff_success$LENGTH)

passes_fcn_success <- passes_fcn %>% 
  filter(recipient_is_own == TRUE)

mean(passes_fcn_success$LENGTH)

# Heatmap over zoner med flest modtagede afleveringer

library(dplyr)
library(ggplot2)
library(ggsoccer)

range(as.numeric(passes_fcn_success$ENDLOCATIONX), na.rm = TRUE)
range(as.numeric(passes_fcn_success$ENDLOCATIONY), na.rm = TRUE)

range(as.numeric(passes_vff_success$ENDLOCATIONX), na.rm = TRUE)
range(as.numeric(passes_vff_success$ENDLOCATIONY), na.rm = TRUE)

plot_12_zones_100 <- function(passes_df,
                              title = "12 modtagezoner (100x100)",
                              opponent_half_only = TRUE,
                              flip_y = FALSE) {
  
  x_min <- 0; x_max <- 100
  y_min <- 0; y_max <- 100
  
  df <- passes_df %>%
    filter(tolower(PRIMARYTYPE) == "pass") %>%
    mutate(
      ENDLOCATIONX = as.numeric(ENDLOCATIONX),
      ENDLOCATIONY = as.numeric(ENDLOCATIONY)
    ) %>%
    filter(!is.na(ENDLOCATIONX), !is.na(ENDLOCATIONY)) %>%
    filter(between(ENDLOCATIONX, x_min, x_max),
           between(ENDLOCATIONY, y_min, y_max))
  
  # kun succesfulde afleveringer til eget hold (hvis kolonnerne findes)
  if ("recipient_is_own" %in% names(df)) df <- df %>% filter(recipient_is_own == TRUE)
  if ("ACCURATE" %in% names(df)) df <- df %>% filter(ACCURATE == 1)
  
  # modstanderens halvdel (100x100 -> midterlinje x=50)
  if (opponent_half_only) df <- df %>% filter(ENDLOCATIONX >= 50)
  
  # flip y hvis I vil (kun hvis det ser “op/ned” forkert ud)
  if (flip_y) df <- df %>% mutate(ENDLOCATIONY = y_max - ENDLOCATIONY)
  
  # 12 zoner = 4 x 3 på 0–100
  x_breaks <- seq(x_min, x_max, length.out = 5)  # 0,25,50,75,100
  y_breaks <- seq(y_min, y_max, length.out = 4)  # 0,33.33,66.66,100
  

plot_9_zones_opponent_half_pct_red <- function(passes_df, title) {
  
  df <- passes_df %>%
    filter(tolower(PRIMARYTYPE) == "pass") %>%
    mutate(
      ENDLOCATIONX = as.numeric(ENDLOCATIONX),
      ENDLOCATIONY = as.numeric(ENDLOCATIONY)
    ) %>%
    filter(!is.na(ENDLOCATIONX), !is.na(ENDLOCATIONY)) %>%
    filter(between(ENDLOCATIONX, 50, 100),   # modstanderens halvdel
           between(ENDLOCATIONY, 0, 100)) %>%
    # kun succesfulde afleveringer til eget hold
    { if ("recipient_is_own" %in% names(.)) filter(., recipient_is_own == TRUE) else . } %>%
    { if ("ACCURATE" %in% names(.)) filter(., ACCURATE == 1) else . }
  
  # 9 zoner = 3 x 3 på modstanderens halvdel
  x_breaks <- seq(50, 100, length.out = 4)   # 50–66.7–83.3–100
  y_breaks <- seq(0, 100, length.out = 4)    # 0–33.3–66.7–100
  
  df_z <- df %>%
    mutate(
      x_bin = cut(ENDLOCATIONX, breaks = x_breaks,
                  include.lowest = TRUE, right = FALSE),
      y_bin = cut(ENDLOCATIONY, breaks = y_breaks,
                  include.lowest = TRUE, right = FALSE)
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
    annotate_pitch(dimensions = pitch_opta,
                   colour = "black", fill = "white") +
    geom_tile(aes(width = tile_w, height = tile_h),
              color = "white", linewidth = 0.6, alpha = 0.9) +
    geom_text(aes(label = label),
              size = 5, fontface = "bold") +
    coord_fixed(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
    theme_pitch() +
    scale_fill_gradient(
      low = "white",
      high = "red",
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(title = title, fill = "Andel") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

plot_9_zones_opponent_half_pct_red(
  passes_fcn_success,
  "FCN – 9 modtagezoner (modstanderens halvdel)"
)

plot_9_zones_opponent_half_pct_red(
  passes_vff_success,
  "VFF – 9 modtagezoner (modstanderens halvdel)"
)

table(passes_vff_success$HEIGHT)
1706/11519
#15% af VFF succesfulde afleveringer er høje

table(passes_fcn_success$HEIGHT)
1641/16643
#9% af FCN succesfulde afleveringer er høje