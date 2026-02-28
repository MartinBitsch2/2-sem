library(dplyr)
library(ggplot2)
library(ggsoccer)
library(scales)
library(tidyverse)
library(string


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
#######################################################################################################
#Den position uden for feltet hvor der bliver skudt mest Viborg FF og FC Nordsjælland
#######################################################################################################


# ---------- 1) Klargør skuddata til 0-100 (Opta pitch) ----------
prep_shots_0_100 <- function(shots_df, x_col = "x_meter", y_col = "y_meter") {
  
  df <- shots_df %>%
    mutate(
      x_raw = suppressWarnings(as.numeric(.data[[x_col]])),
      y_raw = suppressWarnings(as.numeric(.data[[y_col]]))
    ) %>%
    filter(!is.na(x_raw), !is.na(y_raw))
  
  x_rng <- range(df$x_raw, na.rm = TRUE)
  y_rng <- range(df$y_raw, na.rm = TRUE)
  
  # Hvis det ligner meter, skaler til 0-100
  if (x_rng[2] > 101 | y_rng[2] > 101) {
    df <- df %>%
      mutate(
        x = x_raw / 105 * 100,
        y = y_raw / 68  * 100
      )
  } else {
    df <- df %>%
      mutate(x = x_raw, y = y_raw)
  }
  
  df %>% filter(between(x, 0, 100), between(y, 0, 100))
}

# ---------- 2) Straffesparksfeltet (i 0-100) ----------
is_inside_box <- function(x, y) {
  box_x_min <- 100 - (16.5/105) * 100               # ca. 84.2857
  box_y_min <- ((68 - 40.32)/2 / 68) * 100          # ca. 20.3529
  box_y_max <- 100 - box_y_min                       # ca. 79.6471
  x >= box_x_min & y >= box_y_min & y <= box_y_max
}

# ---------- 3) Heatmap (9 zoner) + top zone ----------
plot_9_zones_shots_outside_box <- function(shots_df,
                                           title = "Skud – 9 zoner (uden for feltet)",
                                           opponent_half_only = TRUE,
                                           x_col = "x_meter",
                                           y_col = "y_meter") {
  
  df <- prep_shots_0_100(shots_df, x_col = x_col, y_col = y_col) %>%
    { if ("PRIMARYTYPE" %in% names(.)) filter(., tolower(PRIMARYTYPE) == "shot") else . } %>%
    { if (opponent_half_only) filter(., x >= 50) else . } %>%
    filter(!is_inside_box(x, y))
  
  x_start <- if (opponent_half_only) 50 else 0
  x_breaks <- seq(x_start, 100, length.out = 4)  # 3 bins
  y_breaks <- seq(0, 100, length.out = 4)        # 3 bins
  
  df_z <- df %>%
    mutate(
      x_bin = cut(x, breaks = x_breaks, include.lowest = TRUE, right = FALSE),
      y_bin = cut(y, breaks = y_breaks, include.lowest = TRUE, right = FALSE)
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
  
  p <- ggplot(df_z, aes(x = x_center, y = y_center, fill = pct)) +
    annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
    geom_tile(aes(width = tile_w, height = tile_h),
              color = "white", linewidth = 0.6, alpha = 0.9) +
    geom_text(aes(label = label), size = 5, fontface = "bold") +
    coord_fixed(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
    theme_pitch() +
    scale_fill_gradient(
      low = "white",
      high = "red",
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(title = title, fill = "Andel") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  top_zone <- df_z %>% arrange(desc(n)) %>% slice(1) %>%
    select(x_bin, y_bin, n, pct, x_center, y_center)
  
  list(plot = p, top_zone = top_zone, zone_table = df_z)
}

# ---------- 4) Finder "mest skudte position" ----------
find_most_common_shot_position <- function(shots_df,
                                           bins = 25,
                                           opponent_half_only = TRUE,
                                           x_col = "x_meter",
                                           y_col = "y_meter") {
  
  df <- prep_shots_0_100(shots_df, x_col = x_col, y_col = y_col) %>%
    { if ("PRIMARYTYPE" %in% names(.)) filter(., tolower(PRIMARYTYPE) == "shot") else . } %>%
    { if (opponent_half_only) filter(., x >= 50) else . } %>%
    filter(!is_inside_box(x, y))
  
  x_start <- if (opponent_half_only) 50 else 0
  x_breaks <- seq(x_start, 100, length.out = bins + 1)
  y_breaks <- seq(0, 100, length.out = bins + 1)
  
  df %>%
    mutate(
      x_bin = cut(x, breaks = x_breaks, include.lowest = TRUE, right = FALSE),
      y_bin = cut(y, breaks = y_breaks, include.lowest = TRUE, right = FALSE)
    ) %>%
    count(x_bin, y_bin, name = "n") %>%
    mutate(
      x_idx = as.integer(x_bin),
      y_idx = as.integer(y_bin),
      x_center = (x_breaks[x_idx] + x_breaks[x_idx + 1]) / 2,
      y_center = (y_breaks[y_idx] + y_breaks[y_idx + 1]) / 2
    ) %>%
    arrange(desc(n)) %>%
    slice(1) %>%
    select(n, x_center, y_center, x_bin, y_bin)
}

# =========================
#  KØR FOR HVER HOLD
# =========================

teams <- tibble::tribble(
  ~TEAM_WYID, ~team_name,
  7456, "Viborg FF",
  7458, "FC Nordsjælland"
)

results <- list()

for (i in seq_len(nrow(teams))) {
  
  team_id   <- teams$TEAM_WYID[i]
  team_name <- teams$team_name[i]
  
  shots_team <- shots_med_xy %>%
    filter(TEAM_WYID == team_id)
  
  # Heatmap (9 zoner)
  res <- plot_9_zones_shots_outside_box(
    shots_team,
    title = paste0(team_name, " – skudzoner (uden for feltet, modstanderens halvdel)"),
    opponent_half_only = TRUE,
    x_col = "x_meter",
    y_col = "y_meter"
  )
  
  # Mest skudte position (finere grid)
  top_pos <- find_most_common_shot_position(
    shots_team,
    bins = 25,
    opponent_half_only = TRUE,
    x_col = "x_meter",
    y_col = "y_meter"
  )
  
  # Gem alt
  results[[team_name]] <- list(
    heatmap = res,
    top_zone = res$top_zone,
    top_position = top_pos
  )
  
  # Print outputs
  print(res$plot)
  cat("\n========================\n")
  cat(team_name, "\n")
  cat("Top-zone (3x3) uden for feltet:\n")
  print(res$top_zone)
  cat("\nMest skudte position (grid):\n")
  print(top_pos)
  cat("========================\n\n")
}



#######################################################################################################
#Mål fra dødbolde (hjørnespark, frispark, indkast)
#######################################################################################################
dødbolde <- left_join(matches301, secondarytype, by="MATCH_WYID")
dødbolde <- left_join(dødbolde, common[, c(3,14,16,12,13)] %>% distinct(EVENT_WYID, .keep_all = TRUE),by = "EVENT_WYID")
dødbolde_div_super <- dødbolde %>% filter(PRIMARYTYPE %in% c("corner", "free_kick", "penalty", "throw_in"))

dødbolde_super <- dødbolde %>% filter(PRIMARYTYPE %in% c("corner", "free_kick", "penalty", "throw_in"), SEASON_WYID %in% c(191611,189918))

#viborg
vff_død_ass <- dødbolde_super %>% filter(str_detect(SECONDARYTYPE2, "assist"), TEAM_WYID==7456)
vff_død_mål <- dødbolde_super %>% filter(str_detect(SECONDARYTYPE2, "goal"), TEAM_WYID==7456)
vff_død <- dødbolde_super %>% filter(TEAM_WYID==7456)
table(vff_død_ass$PRIMARYTYPE)
table(vff_død_mål$PRIMARYTYPE)
table(vff_død$PRIMARYTYPE)

#nordsjælland 7458
fcn_død_ass <- dødbolde_super %>% filter(str_detect(SECONDARYTYPE2, "assist"), TEAM_WYID==7458)
fcn_død_mål <- dødbolde_super %>% filter(str_detect(SECONDARYTYPE2, "goal"), TEAM_WYID==7458)
fcn_død <- dødbolde_super %>% filter(TEAM_WYID==7458)
table(fcn_død_ass$PRIMARYTYPE)
table(fcn_død_mål$PRIMARYTYPE)
table(fcn_død$PRIMARYTYPE)

#superliga
sup_død_ass <- dødbolde_super %>% filter(str_detect(SECONDARYTYPE2, "assist"))
sup_død_mål <- dødbolde_super %>% filter(str_detect(SECONDARYTYPE2, "goal"))
sup_død <- dødbolde_super
table(sup_død_ass$PRIMARYTYPE)
table(sup_død_mål$PRIMARYTYPE)
table(sup_død$PRIMARYTYPE)

