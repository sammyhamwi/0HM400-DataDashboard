library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(tidyr)

# Always show this user ID
user_id_value <- "woGWqjdPF+nFiSLpkbm8vQ=="

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
    tags$style(HTML("
    body { background-color: #f8f9fa; font-family: system-ui; }
    .card { border-radius: 1rem; box-shadow: 0 1px 6px rgba(0,0,0,0.05); margin-bottom: 1rem; }
    .section-title { font-weight: 600; font-size: 1.2rem; margin-bottom: 0.75rem; }
    .irs-grid-text, .irs-min, .irs-max {
      display: none !important;
    }
    .irs--shiny .irs-grid {
      display: none !important;
    }
  "))
  ),
  
  div(class = "container-fluid mt-4",
      div(class = "row",
          div(class = "col-md-3",
              div(class = "card p-3",
                  h5("My Courses"),
                  radioButtons("cId", label = NULL,
                               choices = list("0HV30-2024" = 27886, "0HV60-2024" = 28301, "0HV90-2024" = 27893, "0HV100-2024" = 27974),
                               selected = 28301)
              )
          ),
          
          div(class = "col-md-9",
              div(class = "card p-3",
                  h4("Course A Self-Regulated Learning Reflection Dashboard"),
                  textOutput("selectedUserId")
              ),
              
              div(class = "row",
                  div(class = "col-md-6",
                      div(class = "card p-3",
                          div(class = "section-title", "Activity Tracker"),
                          uiOutput("activityTracker")
                      ),
                      div(class = "card p-3",
                          div(class = "section-title", "Reflections"),
                          selectInput("selected_week", "Select Week", choices = 1:10, selected = 1),
                          uiOutput("satisfaction_ui"),
                          
                          uiOutput("expectation_ui"),
                          
                          uiOutput("save_button_ui"),
                          
                          br(),
                          # Feedback message
                          div(
                            class = "text-muted mb-3",
                            textOutput("satisfaction_feedback"),
                          ),
                          
                          uiOutput("streak_badge")
                      )
                  ),
                  div(class = "col-md-6",
                      div(class = "card p-3",
                          div(class = "section-title", "Daily Activity Breakdown"),
                          plotlyOutput("stackedBarChart")
                      ),
                      div(class = "card p-3",
                          div(class = "section-title", "Badge Tab"),
                          uiOutput("badges")
                      )
                  )
              )
          )
      )
  )
)

# Server
server <- function(input, output, session) {
  course_id <- reactive({ input$cId })
  user_id <- reactive({ user_id_value })
  
  # Storage for satisfaction ratings by week
  satisfaction_ratings <- reactiveValues(data = list())
  expectation_ratings <- reactiveValues(data = list())
  
  # Text feedback message
  satisfaction_feedback_store <- reactiveValues(data = list())
  
  # Reflection count
  save_count <- reactiveValues(count = 0)
  
  # Simulate current week
  current_week <- reactive({ as.numeric(input$selected_week) })
  
  # Track saved weeks
  reflection_saved <- reactiveValues(data = list())
  
  observeEvent(input$save_rating, {
    save_count$count <- save_count$count + 1
    course <- as.character(input$cId)
    this_week <- as.character(input$selected_week)
    last_week <- as.character(as.numeric(input$selected_week) - 1)
    
    # Initialize if first time this course
    if (is.null(satisfaction_ratings$data[[course]])) satisfaction_ratings$data[[course]] <- list()
    if (is.null(expectation_ratings$data[[course]])) expectation_ratings$data[[course]] <- list()
    
    # Save current week's ratings
    satisfaction_ratings$data[[course]][[this_week]] <- input$satisfaction
    expectation_ratings$data[[course]][[this_week]] <- input$expectation
    
    # Mark this week as saved
    if (is.null(reflection_saved$data[[course]])) reflection_saved$data[[course]] <- list()
    reflection_saved$data[[course]][[this_week]] <- TRUE
    feedback <- ""
    
    # Last week's expectation
    last_week_expectation <- expectation_ratings$data[[course]][[last_week]]
    
    if (!is.null(last_week_expectation)) {
      if (input$satisfaction > last_week_expectation) {
        feedback <- "You exceeded your expectations! Great job. What do you think contributed to this?"
      } else if (input$satisfaction < last_week_expectation) {
        feedback <- "You didn’t quite meet your expectations. Think about what barriers might have affected your progress."
      } else {
        feedback <- "Your satisfaction aligned with your expectations. This suggests a good sense of self-awareness."
      }
    }
    
    if (input$expectation > input$satisfaction) {
      feedback <- paste(feedback, "You’re aiming higher for next week. What’s your plan to get there?")
    } else if (input$expectation < input$satisfaction) {
      feedback <- paste(feedback, "It seems you’re not expecting to maintain this level. Is something changing next week?")
    } else {
      feedback <- paste(feedback, "You’re expecting similar satisfaction next week. Do you feel you’ve found a stable rhythm?")
    }
    
    if (is.null(satisfaction_feedback_store$data[[course]])) {
      satisfaction_feedback_store$data[[course]] <- list()
    }
    satisfaction_feedback_store$data[[course]][[this_week]] <- feedback
    output$satisfaction_feedback <- renderText({ feedback })
    
  })
  observeEvent(input$selected_week, {
    course <- as.character(input$cId)
    week <- as.character(input$selected_week)
    
    # Show saved feedback if it exists for this course/week
    if (!is.null(satisfaction_feedback_store$data[[course]]) &&
        !is.null(satisfaction_feedback_store$data[[course]][[week]])) {
      
      feedback <- satisfaction_feedback_store$data[[course]][[week]]
      output$satisfaction_feedback <- renderText({ feedback })
      
    } else {
      # No saved reflection/feedback for this week → clear output
      output$satisfaction_feedback <- renderText({ "" })
    }
  })
  
  
  output$selectedUserId <- renderText({
    paste("Welcome", user_id(), "!")
  })
  
  output$activityTracker <- renderUI({
    # Simulate data — replace with real logic from DB
    activities <- list(
      list(name = "Submit Week 1 Quiz", done = TRUE),
      list(name = "Reflect on Week 1", done = TRUE),
      list(name = "Complete Reading", done = FALSE),
      list(name = "Submit Week 2 Quiz", done = FALSE),
      list(name = "Reflect on Week 2", done = FALSE)
    )
    
    total <- length(activities)
    completed <- sum(sapply(activities, function(x) x$done))
    percent <- round((completed / total) * 100)
    
    list_items <- paste0(
      "<ul class='list-group mb-3'>",
      paste(sapply(activities, function(x) {
        if (x$done) {
          sprintf("<li class='list-group-item d-flex justify-content-between align-items-center text-success'>
                  ✅ %s
                </li>", x$name)
        } else {
          sprintf("<li class='list-group-item d-flex justify-content-between align-items-center text-muted'>
                  ⬜ %s
                </li>", x$name)
        }
      }), collapse = ""),
      "</ul>"
    )
    
    progress_bar <- sprintf("
    <div class='progress'>
      <div class='progress-bar bg-success' role='progressbar' style='width: %d%%;' aria-valuenow='%d' aria-valuemin='0' aria-valuemax='100'>
        %d%%
      </div>
    </div>", percent, percent, percent)
    
    HTML(paste0(list_items, progress_bar))
  })
  
  output$badges <- renderUI({
    HTML("
      <p><strong>You are a reflection expert</strong></p>
      <span class='badge bg-primary me-1'>🌟 Reflection Master</span>
      <span class='badge bg-success'>💡 Insight Champion</span>
    ")
  })
  
  output$stackedBarChart <- renderPlotly({
    # Static sample data
    df <- data.frame(
      date = rep(seq.Date(Sys.Date() - 6, Sys.Date(), by = "day"), times = 3),
      item = rep(c("quizzes", "files", "discussions"), each = 7),
      count = c(2, 3, 4, 3, 5, 4, 6, 1, 2, 2, 3, 2, 3, 4, 0, 1, 1, 0, 2, 1, 1)
    )
    
    # Convert `date` to Date class
    df$date <- as.Date(df$date)
    
    # Plot line graph (1 line per item)
    plot_ly(df, x = ~date, y = ~count, color = ~item, type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Daily Activity Breakdown",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Activity Count"),
        hovermode = "x unified"
      )
  })
  
  output$satisfaction_feedback <- renderText({
    satisfaction_feedback()
  })
  
  output$satisfaction_ui <- renderUI({
    course <- as.character(input$cId)
    week <- as.character(input$selected_week)
    saved <- !is.null(reflection_saved$data[[course]][[week]]) && reflection_saved$data[[course]][[week]]
    
    if (saved) {
      value <- satisfaction_ratings$data[[course]][[week]]
      div(class = "form-control-plaintext", paste("Self-Reported Satisfaction Score:", value))
    } else {
      sliderInput("satisfaction", "How satisfied are you with your learning progress this week?", min = 1, max = 10, value = 5)
    }
  })
  
  output$expectation_ui <- renderUI({
    course <- as.character(input$cId)
    week <- as.character(input$selected_week)
    saved <- !is.null(reflection_saved$data[[course]][[week]]) && reflection_saved$data[[course]][[week]]
    
    if (saved) {
      value <- expectation_ratings$data[[course]][[week]]
      div(class = "form-control-plaintext", paste("Expected Progress Satisfaction:", value))
    } else {
      sliderInput("expectation", "How satisfied do you expect to be with your learning progress next week?", min = 1, max = 10, value = 5)
    }
  })
  
  output$save_button_ui <- renderUI({
    course <- as.character(input$cId)
    week <- as.character(input$selected_week)
    saved <- !is.null(reflection_saved$data[[course]][[week]]) && reflection_saved$data[[course]][[week]]
    
    if (!saved) {
      actionButton("save_rating", "Save Reflection", class = "btn btn-primary mt-2")
    } else {
      NULL  # No button if already saved
    }
  })
  
  
  
  output$streak_badge <- renderUI({
    course <- as.character(input$cId)
    week <- as.numeric(input$selected_week)
    
    # Don't show badge unless this week has a saved reflection
    if (is.null(reflection_saved$data[[course]]) || is.null(reflection_saved$data[[course]][[as.character(week)]]) || !reflection_saved$data[[course]][[as.character(week)]]) {
      return(NULL)
    }
    
    # Find all submitted weeks up to the selected week
    submitted_weeks <- sort(as.numeric(names(satisfaction_ratings$data[[course]])))
    submitted_weeks <- submitted_weeks[submitted_weeks <= week]
    
    if (length(submitted_weeks) == 0) return(NULL)
    
    # Walk backward from selected week to calculate streak ending at that week
    streak <- 0
    for (w in week:1) {
      if (w %in% submitted_weeks) {
        streak <- streak + 1
      } else {
        break
      }
    }
    
    if (streak >= 2) {
      HTML(sprintf("
      <div class='alert alert-warning mt-3'>
        🏅 <strong>%d Week Streak!</strong> Reflection badge for %d consecutive submissions.
      </div>
    ", streak, streak))
    } else {
      return(NULL)
    }
  })
  
}

shinyApp(ui, server)
