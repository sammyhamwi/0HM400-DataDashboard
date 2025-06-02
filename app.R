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
    .irs-grid-text, .irs-min, .irs-max { display: none !important; }
    .irs--shiny .irs-grid { display: none !important; }
    
    .badge-container {
      display: flex;
      flex-direction: row-reverse;
      justify-content: flex-end;
      align-items: center;
      gap: 30px;
      flex-wrap: nowrap;
      transition: all 0.3s ease;
      min-width: 300px;
    }

    .badge-item {
      position: relative;
      cursor: pointer;
    }

    .badge-item:hover::after {
      content: attr(data-tooltip);
      position: absolute;
      bottom: 110%;
      left: 50%;
      transform: translateX(-50%);
      background-color: rgba(0,0,0,0.75);
      color: #fff;
      padding: 6px 10px;
      border-radius: 6px;
      white-space: nowrap;
      font-size: 14px;
      pointer-events: none;
      opacity: 1;
      transition: opacity 0.3s ease;
      z-index: 1000;
    }

    .badge-item::after {
      content: '';
      opacity: 0;
      pointer-events: none;
      transition: opacity 0.3s ease;
    }
    .card {
  overflow-x: auto;  /* allows horizontal scrolling if badges exceed width */
  /* or use overflow: hidden; if you want to just clip overflow */
  padding: 1rem;
  position: relative; /* helps with positioned children */
    }
.card, .col-md-6, .col-md-9, .row {
    overflow: visible !important;
    position: relative;
}

  "))
  )
  ,
  
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
                          sliderInput("satisfaction", "How satisfied are you with your learning progress this week?", min = 1, max = 10, value = 5),
                          
                          # Feedback message
                          div(
                            class = "text-muted mb-3",
                            textOutput("satisfaction_feedback")
                          ),
                          sliderInput("expectation", "How satisfied do you expect to be with your learning progress next week?", min = 1, max = 10, value = 5),
                          
                          actionButton("save_rating", "Save Reflection", class = "btn btn-primary mt-2"),
                          
                          uiOutput("streak_badge")
                      )
                  ),
                  div(class = "col-md-6",
                      div(class = "card p-3",
                          div(class = "section-title", "Daily Activity Breakdown"),
                          plotlyOutput("stackedBarChart")
                      ),
                      div(class = "card p-3",
                          div(class = "section-title", "Badges"),
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
  satisfaction_feedback <- reactiveVal("")
  
  # Reflection count
  save_count <- reactiveValues(count = 0)
  
  # Simulate current week
  current_week <- reactive({ as.numeric(input$selected_week) })
  
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
    
    # Last week's expectation
    last_week_expectation <- expectation_ratings$data[[course]][[last_week]]
    
    if (!is.null(last_week_expectation)) {
      if (input$satisfaction > last_week_expectation) {
        satisfaction_feedback("O I see the value is higher than last week")
      } else if (input$satisfaction < last_week_expectation) {
        satisfaction_feedback("O I see the value is lower than last week")
      } else {
        satisfaction_feedback("O the value is the same as last week")
      }
    } else {
      satisfaction_feedback("No rating available for last week to compare")
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
    # Retrieve the save_count (assuming it’s reactive and available)
    count <- save_count$count
    
    # Prepare badge info as a list of lists, each with unlock threshold, unlocked image + tooltip, and locked tooltip
    badges <- list(
      list(
        threshold = 1,
        unlocked_img = "ROOKIE.png",
        unlocked_tooltip = "Your first reflection - Nice start!",
        locked_tooltip = "Reflect for the first time."
      ),
      list(
        threshold = 4,
        unlocked_img = "CONSISTENT.png",
        unlocked_tooltip = "4 week of reflections - Keep reflecting!",
        locked_tooltip = "Reflect for 4 weeks."
      ),
      list(
        threshold = 8,
        unlocked_img = "MASTER.png",
        unlocked_tooltip = "8 Weeks of reflecting?! - Good job!",
        locked_tooltip = "Reflect for 8 weeks."
      )
    )
    
    # Start building the HTML
    badge_html <- "<p><strong>Your Reflection Badges</strong></p><div class='badge-container'>"
    
    # Loop through each badge, check if unlocked, show correct image and tooltip
    for (b in badges) {
      if (count >= b$threshold) {
        # Unlocked badge
        badge_html <- paste0(
          badge_html,
          "<span class='badge-item' data-tooltip='", b$unlocked_tooltip, "'>",
          "<img src='", b$unlocked_img, "' height='90px'/>",
          "</span>"
        )
      } else {
        # Locked badge with QUESTION.png and hint tooltip
        badge_html <- paste0(
          badge_html,
          "<span class='badge-item' data-tooltip='", b$locked_tooltip, "'>",
          "<img src='QUESTION.png' height='90px'/>",
          "</span>"
        )
      }
    }
    
    badge_html <- paste0(badge_html, "</div>")
    
    HTML(badge_html)
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
  
  output$streak_badge <- renderUI({
    course <- as.character(input$cId)
    submitted_weeks <- sort(as.numeric(names(satisfaction_ratings$data[[course]])))
    
    if (length(submitted_weeks) == 0) {
      streak <- 0
    } else if (length(submitted_weeks) == 1) {
      streak <- 1
    } else {
      # Calculate streak: walk from latest back to oldest, stop at gap
      reversed_weeks <- rev(submitted_weeks)
      streak <- 1
      for (i in 2: length(reversed_weeks)) {
        if (reversed_weeks[i - 1] - reversed_weeks[i] == 1) {
          streak <- streak + 1
        } else {
          break
        }
      }
    }
    # Display the streak in a styled badge
    HTML(sprintf("
      <div class='alert alert-warning mt-3'>
        🏅 <strong>%d Week Streak!</strong> Reflection badge for %d consecutive submission%s.
      </div>
    ", streak, streak, ifelse(streak == 1, "", "s")))
  })
}

shinyApp(ui, server)
