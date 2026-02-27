library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)

super_clean <- readRDS("super_clean.rds")
passes_med_xy <- readRDS("passes_med_xy.rds")
passes_succes_super <- readRDS("passes_succes_super.rds")
players <- readRDS("players.rds")

# ----------------------------
# Hjælper: find kolonne ud fra mulige navne
# ----------------------------
pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) stop("Kan ikke finde nogen af disse kolonner: ", paste(candidates, collapse = ", "))
  hit[[1]]
}

# ----------------------------
# Konstanter
# ----------------------------
TEAM_MAP <- c(`7456` = "Viborg", `7458` = "FC Nordsjælland")
TARGET_TEAMS <- as.integer(names(TEAM_MAP))

MATCH_COL <- "MATCH_WYID"
TEAM_COL  <- "TEAM_WYID"

# Skud-filter: i dine data har du tidligere PRIMARYTYPE == "shot"
TYPE_COL <- pick_col(super_clean, c("PRIMARYTYPE", "primarytype", "TYPE", "type"))

# Goal-kolonne (typisk "goal" i dit super_clean eksempel)
GOAL_COL <- pick_col(super_clean, c("goal", "GOAL", "isGoal", "IS_GOAL"))

# Koordinatkolonner (shot location)
# Tilpas kandidatlisten hvis dine hedder noget helt andet
X_COL <- pick_col(super_clean, c("LOCATIONX", "locationx", "X", "x", "STARTLOCATIONX", "startlocationx"))
Y_COL <- pick_col(super_clean, c("LOCATIONY", "locationy", "Y", "y", "STARTLOCATIONY", "startlocationy"))

# ----------------------------
# 1) Base shots: kun skud fra de to hold
# ----------------------------
shots_base <- super_clean %>%
  filter(.data[[TYPE_COL]] == "shot") %>%
  mutate(
    match_id = .data[[MATCH_COL]],
    team_id  = as.integer(.data[[TEAM_COL]]),
    is_goal  = as.integer(.data[[GOAL_COL]] %in% c(TRUE, 1, "1", "TRUE", "T")),
    x_raw    = as.numeric(.data[[X_COL]]),
    y_raw    = as.numeric(.data[[Y_COL]])
  ) %>%
  filter(team_id %in% TARGET_TEAMS) %>%
  mutate(team_name = unname(TEAM_MAP[as.character(team_id)]))

# ----------------------------
# 2) Find kun matches hvor BEGGE hold findes
# ----------------------------
eligible_matches <- shots_base %>%
  distinct(match_id, team_id) %>%
  count(match_id, name = "n_teams") %>%
  filter(n_teams == length(TARGET_TEAMS)) %>%
  arrange(desc(match_id)) %>%        # "seneste" proxy: højeste MATCH_WYID først
  pull(match_id)

matches_tbl <- tibble(match_id = eligible_matches) %>%
  mutate(label = paste0("Match ", match_id, " — Viborg vs FC Nordsjælland"))

default_match <- if (any(matches_tbl$match_id == 5709466)) 5709466 else matches_tbl$match_id[1]

# ----------------------------
# 3) Koordinat-normalisering til ggsoccer (StatsBomb 120x80)
#    Hvis dine data er 0-100 skala, skalerer vi op til 120x80.
# ----------------------------
# Heuristik: hvis max(x_raw) <= 100 og max(y_raw) <= 100 → antag 0-100 skala.
xmax <- suppressWarnings(max(shots_base$x_raw, na.rm = TRUE))
ymax <- suppressWarnings(max(shots_base$y_raw, na.rm = TRUE))
scale_100 <- is.finite(xmax) && is.finite(ymax) && xmax <= 100.5 && ymax <= 100.5

shots_plot_ready <- shots_base %>%
  filter(match_id %in% eligible_matches) %>%
  mutate(
    x = if (scale_100) x_raw * 1.2 else x_raw,
    y = if (scale_100) y_raw * 0.8 else y_raw
  )

# ----------------------------
# UI
# ----------------------------
# app.R — Opgave 4.3 (Skudkort + YouTube highlights)

library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(tibble)

# ----------------------------
# YouTube videoer pr. kamp
# ----------------------------
video_map <- tibble::tibble(
  MATCH_WYID = c(5709466, 5585739),
  youtube_id = c("UYeknzpinn0", "eD1xR1rZ9yc"),
  title = c("Highlights", "Highlights")
)

# ----------------------------
# Highlights pr. kamp (start i sekunder)
# 5709466: dine tidligere highlights
# 5585739: alle mål (fra din liste)
# ----------------------------
highlights_map <- tibble::tibble(
  MATCH_WYID = c(
    5709466, 5709466, 5709466, 5709466,
    5585739, 5585739, 5585739, 5585739, 5585739
  ),
  highlight = c(
    "1-0 Sindre Egeli (1:15)",
    "Stor chance FCN (1:35)",
    "Stor chance Viborg (2:55)",
    "Stor chance Viborg (3:22)",
    
    "0-1 Sindre Egeli (0:34)",
    "1-1 Stipe Radic (1:28)",
    "2-1 Renato Júnior (2:18)",
    "3-1 Isak Jensen (2:56)",
    "3-2 Lucas Hey (4:45)"
  ),
  start_sec = c(
    75, 95, 175, 202,
    34, 88, 138, 176, 285
  )
)

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  titlePanel("Opgave 4.3 – Skud i en kamp (ggsoccer) + YouTube highlights"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "match_id",
        "Vælg kamp:",
        choices = setNames(video_map$MATCH_WYID, paste0("Match ", video_map$MATCH_WYID)),
        selected = video_map$MATCH_WYID[1]
      ),
      selectInput(
        "highlight_start",
        "Highlights:",
        choices = c("Start af video" = 0),
        selected = 0
      ),
      helpText("Vælg kamp og derefter highlight for at hoppe i videoen.")
    ),
    mainPanel(
      plotOutput("shot_map", height = "650px"),
      hr(),
      uiOutput("youtube_embed")
    )
  )
)

# ----------------------------
# SERVER
# ----------------------------
server <- function(input, output, session) {
  
  # Opdater highlights-dropdown når kamp ændres
  observeEvent(input$match_id, {
    mid <- as.integer(input$match_id)
    
    hl <- highlights_map %>%
      filter(MATCH_WYID == mid) %>%
      arrange(start_sec)
    
    choices_vals <- c(0, hl$start_sec)
    choices_labs <- c("Start af video", hl$highlight)
    
    updateSelectInput(
      session,
      "highlight_start",
      choices = setNames(choices_vals, choices_labs),
      selected = 0
    )
  }, ignoreInit = FALSE)
  
  # Skuddata for den valgte kamp (forudsætter shots_plot_ready findes)
  match_shots <- reactive({
    req(input$match_id)
    mid <- as.integer(input$match_id)
    
    shots_plot_ready %>%
      filter(match_id == mid) %>%
      mutate(
        shot_class = case_when(
          team_name == "Viborg" & is_goal == 1L ~ "Viborg – Mål",
          team_name == "Viborg" & is_goal == 0L ~ "Viborg – Ikke mål",
          team_name == "FC Nordsjælland" & is_goal == 1L ~ "FC Nordsjælland – Mål",
          TRUE ~ "FC Nordsjælland – Ikke mål"
        )
      )
  })
  
  output$shot_map <- renderPlot({
    df <- match_shots()
    validate(need(nrow(df) > 0, "Ingen skuddata for den valgte kamp. Tjek shots_plot_ready."))
    
    ggplot(df, aes(x = x, y = y)) +
      annotate_pitch(dimensions = pitch_statsbomb) +
      theme_pitch() +
      geom_point(aes(color = shot_class), size = 3, alpha = 0.9) +
      scale_color_manual(values = c(
        "Viborg – Ikke mål" = "#7BC67B",
        "Viborg – Mål"      = "#009A44",
        "FC Nordsjælland – Ikke mål" = "#F28E8E",
        "FC Nordsjælland – Mål"      = "#D71920"
      )) +
      labs(
        title = paste0("Skudkort — Match ", input$match_id),
        subtitle = "Farve viser hold + om skuddet blev mål",
        color = ""
      ) +
      theme(legend.position = "bottom")
  })
  
  # YouTube embed, starter ved valgt highlight
  output$youtube_embed <- renderUI({
    req(input$match_id)
    mid <- as.integer(input$match_id)
    
    vid <- video_map %>% filter(MATCH_WYID == mid)
    
    if (nrow(vid) == 0) {
      return(tags$div(
        tags$h4("Video"),
        tags$p("Ingen YouTube-video tilknyttet denne kamp.")
      ))
    }
    
    start_sec <- 0L
    if (!is.null(input$highlight_start)) {
      start_sec <- as.integer(input$highlight_start)
    }
    
    tags$div(
      tags$h4(paste("Video:", vid$title[1])),
      tags$iframe(
        width = "100%",
        height = "450",
        src = paste0(
          "https://www.youtube.com/embed/", vid$youtube_id[1],
          "?start=", start_sec,
          "&autoplay=1"
        ),
        frameborder = "0",
        allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
        allowfullscreen = NA
      )
    )
  })
}

shinyApp(ui, server)
