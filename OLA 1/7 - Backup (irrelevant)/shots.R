library(RMariaDB)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)

#forbindelse
con <- dbConnect(MariaDB(),
                 host="talmedos.com",
                 port="3306",
                 db="superliga2",
                 user="dalremote",
                 password="OttoRehagel123456789Long2026!"
)


wyscout_shots <- dbGetQuery(con, "SELECT
    s.EVENT_WYID,
    s.MATCH_WYID,
    s.PRIMARYTYPE,
    s.SHOTBODYPART,
    s.SHOTISGOAL,
    s.SHOTONTARGET,
    s.SHOTGOALZONE,
    s.SHOTXG,
    s.SHOTPOSTSHOTXG,

    -- Shooter info
    CONCAT(sh.FIRSTNAME, ' ', sh.LASTNAME) AS shooter_name,
    sh.ROLENAME AS shooter_role,
    sh.FOOT AS shooter_foot,

    -- Goalkeeper info
    CONCAT(gk.FIRSTNAME, ' ', gk.LASTNAME) AS goalkeeper_name,

    -- Event context
    e.MATCHPERIOD,
    e.MINUTE,
    e.SECOND,
    e.LOCATIONX,
    e.LOCATIONY,
    e.TEAM_WYID,
    t.TEAMNAME AS team_name,
    ot.TEAMNAME AS opponent_name,

    -- Match info (score, side)
    m.SIDE AS team_side,
    m.SCORE AS team_score,
    m.SCOREHT AS team_score_ht

FROM wyscout_matchevents_shots s

JOIN wyscout_matchevents_common e
    ON s.EVENT_WYID = e.EVENT_WYID

LEFT JOIN wyscout_players sh
    ON e.PLAYER_WYID = sh.PLAYER_WYID
    AND sh.SEASON_WYID = e.SEASON_WYID

LEFT JOIN wyscout_players gk
    ON s.SHOTGOALKEEPER_WYID = gk.PLAYER_WYID
    AND gk.SEASON_WYID = e.SEASON_WYID

LEFT JOIN wyscout_teams t
    ON e.TEAM_WYID = t.TEAM_WYID
    AND e.SEASON_WYID = t.SEASON_WYID
    

LEFT JOIN wyscout_teams ot
    ON e.OPPONENTTEAM_WYID = ot.TEAM_WYID
    AND e.SEASON_WYID = ot.SEASON_WYID

LEFT JOIN wyscout_matchdetail_base m
    ON s.MATCH_WYID = m.MATCH_WYID
   AND e.TEAM_WYID = m.TEAM_WYID

WHERE s.PRIMARYTYPE = 'Shot';")

# Filtrer kamp (kør efter wyscout_shots er hentet)
match_fck <- wyscout_shots %>%
  filter(MATCH_WYID == 5585726)

# Rens + standardisér mål-flag og lav tydelig type til farver/legend
match_fck_ <- match_fck %>%
  distinct(EVENT_WYID, .keep_all = TRUE) %>%   # sikker mod gentagelser
  mutate(
    x = LOCATIONX,
    y = LOCATIONY,
    # robust: fungerer for 1/"1"/TRUE/"true" osv.
    is_goal = tolower(as.character(SHOTISGOAL)) %in% c("1", "true"),
    type = ifelse(is_goal, "Mål", "Skud"),
    type = factor(type, levels = c("Skud", "Mål"))
  )

# Dropdown med antal skud pr. hold
shots_by_team <- match_fck_ %>%
  count(team_name, name = "n_shots")

team_choices <- setNames(
  shots_by_team$team_name,
  paste0(shots_by_team$team_name, " (", shots_by_team$n_shots, " skud)")
)

video_id <- "Dfj8fvTiKfg"

goals_bif <- c(
  "Brøndby mål (3:50)" = 230
)

goals_fck <- c(
  "FCK mål 1 (0:39)" = 39,
  "FCK mål 2 (3:10)" = 190,
  "FCK mål 3 (4:48)" = 288
)


ui <- fluidPage(
  fluidRow(
    column(
      4,
      selectInput(
        "team",
        "Vælg hold",
        choices = c("Alle" = "Alle", team_choices),
        selected = "Alle"
      )
    )
  ),
  
  fluidRow(
    column(
      6,
      selectInput(
        "goal_bif",
        "Vælg Brøndby-mål",
        choices = c("— vælg —" = "", goals_bif),
        selected = ""
      )
    ),
    column(
      6,
      selectInput(
        "goal_fck",
        "Vælg FCK-mål",
        choices = c("— vælg —" = "", goals_fck),
        selected = ""
      )
    )
  ),
  
  uiOutput("yt_video"),
  
  plotOutput("soccerplot", height = "650px")
)


server <- function(input, output, session) {
  
  # --- 1) DATAFILTER TIL PLOT ---
  filtered_df <- reactive({
    df <- match_fck_
    if (input$team != "Alle") {
      df <- df %>% filter(team_name == input$team)
    }
    df
  })
  
  # --- 2) SOCCERPLOT ---
  output$soccerplot <- renderPlot({
    df <- filtered_df()
    req(nrow(df) > 0)
    
    n_goals <- sum(df$is_goal, na.rm = TRUE)
    n_shots <- nrow(df)
    n_shots_no_goal <- n_shots - n_goals
    
    ggplot(df, aes(x = x, y = y)) +
      annotate_pitch(
        colour = "white",
        dimensions = pitch_wyscout,
        fill   = "springgreen4",
        limits = FALSE
      ) +
      geom_point(aes(color = type), size = 4, alpha = 0.9) +
      scale_color_manual(
        values = c("Skud" = "yellow", "Mål" = "red"),
        breaks = c("Skud", "Mål"),
        labels = c(
          "Skud" = paste0("Skud (", n_shots_no_goal, ")"),
          "Mål"  = paste0("Mål (", n_goals, ")")
        )
      ) +
      theme_pitch() +
      theme(
        panel.background = element_rect(fill = "springgreen4", colour = NA),
        legend.position = "right"
      ) +
      scale_y_reverse() +
      labs(
        title = "Skud i kampen (opdelt på hold)",
        subtitle = ifelse(input$team == "Alle", "Begge hold", input$team),
        color = ""
      )
  })
  
  # --- 3) VIDEO-LOGIK (NYT) ---
  
  # Nulstil FCK-dropdown hvis Brøndby vælges
  observeEvent(input$goal_bif, {
    if (nzchar(input$goal_bif)) {
      updateSelectInput(session, "goal_fck", selected = "")
    }
  })
  
  # Nulstil Brøndby-dropdown hvis FCK vælges
  observeEvent(input$goal_fck, {
    if (nzchar(input$goal_fck)) {
      updateSelectInput(session, "goal_bif", selected = "")
    }
  })
  
  # Find aktiv starttid
  active_start <- reactive({
    if (nzchar(input$goal_bif)) return(as.integer(input$goal_bif))
    if (nzchar(input$goal_fck)) return(as.integer(input$goal_fck))
    return(0L)
  })
  
  # Render YouTube-video
  output$yt_video <- renderUI({
    start_t <- active_start()
    
    tags$div(
      style = "margin: 10px 0;",
      tags$h4("Mål-video"),
      tags$iframe(
        width = "100%",
        height = "360",
        src = sprintf(
          "https://www.youtube.com/embed/%s?start=%d",
          video_id, start_t
        ),
        frameborder = "0",
        allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
        allowfullscreen = NA
      )
    )
  })
}

shinyApp(ui, server)
