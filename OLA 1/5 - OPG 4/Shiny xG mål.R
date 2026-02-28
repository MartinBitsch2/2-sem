library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

super_clean <- readRDS("super_clean.rds")
passes_med_xy <- readRDS("passes_med_xy.rds")

# Ryd op i typer + fjern tomt level
super_clean <- super_clean %>%
  mutate(
    ROLENAME = as.character(ROLENAME),
    PRIMARYTYPE = as.character(PRIMARYTYPE),
    SHORTNAME = as.character(SHORTNAME)
  ) %>%
  filter(!is.na(ROLENAME), ROLENAME != "")

ui <- fluidPage(
  titlePanel("Skud og spillere: Mål vs. xG"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("top_n", "Antal spillere (Top N)", min = 5, max = 50, value = 10, step = 1),
      
      selectInput(
        "role",
        "Spillerposition",
        choices = c("Alle" = "Alle",
                    setNames(sort(unique(super_clean$ROLENAME)),
                             sort(unique(super_clean$ROLENAME)))),
        selected = "Alle"
      ),
      
      checkboxInput("only_shots", "Kun afslutninger (PRIMARYTYPE == 'shot')", value = TRUE)
    ),
    mainPanel(
      plotOutput("barplot", height = "600px"),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  player_summary <- reactive({
    df <- super_clean
    
    if (input$only_shots) {
      df <- df %>% filter(PRIMARYTYPE == "shot")
    }
    
    if (input$role != "Alle") {
      df <- df %>% filter(ROLENAME == input$role)
    }
    
    df %>%
      group_by(SHORTNAME) %>%
      summarise(
        goals = sum(goal, na.rm = TRUE),
        xg_sum = sum(xG_pred, na.rm = TRUE),
        shots = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(goals), desc(xg_sum)) %>%
      slice_head(n = input$top_n)
  })
  
  output$barplot <- renderPlot({
    df <- player_summary()
    
    validate(need(nrow(df) > 0, "Ingen data for de valgte filtre."))
    
    df_long <- df %>%
      select(SHORTNAME, goals, xg_sum) %>%
      pivot_longer(cols = c(goals, xg_sum), names_to = "metric", values_to = "value") %>%
      mutate(metric = recode(metric, goals = "Mål", xg_sum = "xG-sum"))
    
    ggplot(df_long, aes(x = reorder(SHORTNAME, value), y = value, fill = metric)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(
        title = paste0("Top ", input$top_n, " spillere – ",
                       ifelse(input$role == "Alle", "alle positioner", input$role)),
        x = "",
        y = "Sum",
        fill = ""
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  output$table <- renderTable({
    df <- player_summary()
    validate(need(nrow(df) > 0, "Ingen data for de valgte filtre."))
    
    df %>%
      mutate(
        goals = as.integer(goals),
        xg_sum = round(xg_sum, 3)
      )
  })
}

shinyApp(ui, server)
