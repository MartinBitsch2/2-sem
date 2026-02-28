library(RMariaDB)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)

super_clean <- readRDS("super_clean.rds")
passes_med_xy <- readRDS("passes_med_xy.rds")
passes_succes_super <- readRDS("passes_succes_super.rds")
players <- readRDS("players.rds")

# Filtrer kamp (kør efter wyscout_shots er hentet)
vff_fcn_shot <- super_clean %>%
  filter(MATCH_WYID == 5709466)

# Rens + standardisér mål-flag og lav tydelig type til farver/legend
vff_fcn_shot <- vff_fcn_shot %>%
  distinct(EVENT_WYID, .keep_all = TRUE) %>%   # sikker mod gentagelser
  mutate(
    x = LOCATIONX,
    y = LOCATIONY,
    # robust: fungerer for 1/"1"/TRUE/"true" osv.
    is_goal = tolower(as.character(SHOTISGOAL)) %in% c("1", "true"),
    type = ifelse(is_goal, "Mål", "Skud"),
    type = factor(type, levels = c("Skud", "Mål"))
  )

library(dplyr)

match_id <- 5709466

passes_all <- passes_med_xy %>%
  filter(
    MATCH_WYID == match_id,
    PRIMARYTYPE == "pass"
  )

passes_success <- passes_succes_super %>%
  filter(
    MATCH_WYID == match_id,
    PRIMARYTYPE == "pass"
  )

vff_fcn_pass <- passes_all %>%
  left_join(
    passes_success %>%
      distinct(MATCH_WYID, EVENT_WYID) %>%   # sikrer ingen dubletter
      mutate(successful_pass = 1L),
    by = c("MATCH_WYID", "EVENT_WYID")
  ) %>%
  mutate(successful_pass = ifelse(is.na(successful_pass), 0L, successful_pass))

vff_fcn_pass <- vff_fcn_pass %>%
  left_join(
    players %>%
      select(PLAYER_WYID, SHORTNAME) %>%
      distinct(),      # sikrer ingen dubletter
    by = "PLAYER_WYID"
  )

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

to_success_flag <- function(x) {
  # Konverter alt til character (og trim), så vi kan matche sikkert
  x_chr <- trimws(tolower(as.character(x)))
  
  as.integer(x_chr %in% c("true", "t", "1", "yes", "y"))
}

ui <- fluidPage(
  titlePanel("Opgave 4.2 – Afleveringer + spiller-successrate"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "match",
        "Kamp:",
        choices = setNames(match_id, paste0("Match_WYID ", match_id)),
        selected = match_id
      ),
      checkboxInput("show_pct", "Vis hold-plot som procentfordeling (100%)", value = FALSE),
      hr(),
      uiOutput("player_dropdown")
    ),
    mainPanel(
      h4("Holdenes afleveringer (ramte vs fejl)"),
      plotOutput("barplot", height = "420px"),
      br(),
      h4("Spillerens succesrate på afleveringer"),
      tableOutput("player_table"),
      br(),
      tableOutput("team_table")
    )
  )
)

# ----------------------------
# SERVER
# ----------------------------
server <- function(input, output, session) {
  
  # Data for kampen (kun 'pass')
  passes_match_raw <- reactive({
    vff_fcn_pass %>%
      filter(MATCH_WYID == input$match, PRIMARYTYPE == "pass") %>%
      mutate(
        successful_pass = {
          x <- successful_pass
          x_chr <- trimws(tolower(as.character(x)))
          as.integer(x_chr %in% c("1", "true", "t", "yes", "y"))
        }
      )
  })
  
  # Dynamisk dropdown med spillere (kun spillere i kampen)
  output$player_dropdown <- renderUI({
    df <- passes_match_raw()
    validate(need(nrow(df) > 0, "Ingen afleveringsdata for kampen."))
    
    players_in_match <- df %>%
      mutate(SHORTNAME = ifelse(is.na(SHORTNAME) | SHORTNAME == "", as.character(PLAYER_WYID), SHORTNAME)) %>%
      distinct(PLAYER_WYID, SHORTNAME) %>%
      arrange(SHORTNAME)
    
    selectInput(
      "player_id",
      "Vælg spiller:",
      choices = setNames(players_in_match$PLAYER_WYID, players_in_match$SHORTNAME),
      selected = players_in_match$PLAYER_WYID[1]
    )
  })
  
  # Hold-opsummering til plot
  team_summary <- reactive({
    df <- passes_match_raw()
    
    df %>%
      mutate(
        pass_type = ifelse(successful_pass == 1L, "Ramt aflevering", "Fejlaflevering"),
        team_name = case_when(
          TEAM_WYID == 7456 ~ "Viborg",
          TEAM_WYID == 7458 ~ "FC Nordsjælland",
          TRUE ~ as.character(TEAM_WYID)
        )
      ) %>%
      count(team_name, pass_type, name = "n")
  })
  
  output$barplot <- renderPlot({
    df <- team_summary()
    validate(need(nrow(df) > 0, "Ingen data at plotte."))
    
    pos <- if (isTRUE(input$show_pct)) "fill" else "stack"
    
    ggplot(df, aes(x = team_name, y = n, fill = team_name)) +
      geom_col(position = pos) +
      coord_flip() +
      scale_fill_manual(
        values = c(
          "Viborg" = "#009A44",
          "FC Nordsjælland" = "#D71920"
        )
      ) +
      labs(
        title = paste("Afleveringsfordeling – Match", input$match),
        x = "",
        y = if (isTRUE(input$show_pct)) "Andel" else "Antal afleveringer",
        fill = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  # Spiller-successrate tabel
  output$player_table <- renderTable({
    req(input$player_id)
    
    df <- passes_match_raw() %>%
      filter(PLAYER_WYID == input$player_id) %>%
      mutate(SHORTNAME = ifelse(is.na(SHORTNAME) | SHORTNAME == "", as.character(PLAYER_WYID), SHORTNAME))
    
    validate(need(nrow(df) > 0, "Ingen afleveringer for den valgte spiller."))
    
    out <- df %>%
      summarise(
        Spiller = first(SHORTNAME),
        Afleveringer = n(),
        Ramte = sum(successful_pass, na.rm = TRUE),
        Fejl = Afleveringer - Ramte,
        Succesrate_pct = round(100 * Ramte / pmax(Afleveringer, 1), 1),
        .groups = "drop"
      )
    
    out
  })
  
  # Hold-tabel (nice at have)
  output$team_table <- renderTable({
    df <- team_summary()
    validate(need(nrow(df) > 0, "Ingen data."))
    
    df %>%
      pivot_wider(names_from = pass_type, values_from = n, values_fill = 0) %>%
      mutate(
        Total = `Ramt aflevering` + `Fejlaflevering`,
        Accuracy_pct = round(100 * `Ramt aflevering` / pmax(Total, 1), 1)
      )
  })
}

shinyApp(ui, server)

names(vff_fcn_pass)
