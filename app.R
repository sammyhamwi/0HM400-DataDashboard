library(jsonlite)
library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(tidyr)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
    tags$style(HTML("
    body { background-color: #f8f9fa; font-family: system-ui; }
    .card { border-radius: 1rem; box-shadow: 0 1px 6px rgba(0,0,0,0.05); margin-bottom: 1rem; }
    .section-title { font-weight: 600; font-size: 1.2rem; margin-bottom: 0.75rem; }
    .sidebar { background-color: #ffffff; height: 100vh; padding: 1.5rem; border-right: 1px solid #ddd; }
    .sidebar h5 { margin-bottom: 1rem; }
    .main-content { padding: 2rem; }
    .header-banner { background-color: #007bff; color: white; padding: 1rem 2rem; margin-bottom: 1rem; display: flex; justify-content: space-between; align-items: center; }
    .header-banner h3 { margin: 0; }
    .login-card { max-width: 400px; margin: 100px auto; padding: 2rem; }
    .irs-grid-text, .irs-min, .irs-max { display: none !important; }
    .irs--shiny .irs-grid { display: none !important; }
    
    .badge-container {
      display: flex;
      flex-direction: row;
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
  ),
  uiOutput("mainUI")  # dynamically switch between login and app UI
)

server <- function(input, output, session) {
  credentials <- reactiveValues(logged_in = FALSE, user_id = NULL, date = NULL)
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  observeEvent(input$login_button, {
    req(input$user_id_input, input$date_input)
    
    credentials$logged_in <- TRUE
    credentials$user_id <- input$user_id_input
    credentials$date <- input$date_input
    
    # Clear any old user's data
    satisfaction_ratings$data <- list()
    expectation_ratings$data <- list()
    satisfaction_feedback_store$data <- list()
    reflection_saved$data <- list()
  })
  
  observe({
    req(credentials$logged_in)
    req(credentials$user_id)
    req(input$cId)  # Now it's safe — UI is rendered
    
    user_id_input <- credentials$user_id
    course <- as.character(input$cId)
    
    user_data <- loadUserData(user_id_input)
    
    if (is.list(user_data) && !is.null(user_data[[course]])) {
      weeks_data <- user_data[[course]]
      
      for (week in names(weeks_data)) {
        week_data <- weeks_data[[week]]
        if (!is.null(week_data)) {
          satisfaction_ratings$data[[course]][[week]] <- week_data$satisfaction %||% NULL
          expectation_ratings$data[[course]][[week]] <- week_data$expectation %||% NULL
          satisfaction_feedback_store$data[[course]][[week]] <- week_data$feedback %||% NULL
          reflection_saved$data[[course]][[week]] <- TRUE
        }
      }
    } 
  })
  
  observeEvent(input$logout_button, {
    credentials$logged_in <- FALSE
    credentials$user_id <- NULL
  })
  
  output$mainUI <- renderUI({
    if (!credentials$logged_in) {
      # Login UI
      fluidPage(
        div(class = "card login-card",
            h4("Login"),
            textInput("user_id_input", "Enter your User ID"),
            dateInput("date_input", "Select a Date", value = Sys.Date(), format = "yyyy-mm-dd"),
            actionButton("login_button", "Login", class = "btn btn-primary mt-3")
        )
      )
    } else {
      # Main App UI
      fluidPage(
        fluidRow(
          column(
            width = 12,
            div(class = "header-banner",
                h3("Self-Regulated Learning Dashboard"),
                actionButton("logout_button", "Logout", class = "btn btn-light")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 3,
            div(class = "sidebar",
                h5("Select Course"),
                radioButtons("cId", label = NULL,
                             choices = list("0HV30-2024" = 27886, "0HV60-2024" = 28301, "0HV90-2024" = 27893, "0HV100-2024" = 27974),
                             selected = 28301)
            )
          ),
          column(
            width = 9,
            div(class = "main-content",
                div(class = "card p-3 mb-4",
                    h4("Course A Self-Regulated Learning Reflection Dashboard"),
                    textOutput("selectedUserId")
                ),
                fluidRow(
                  column(
                    width = 6,
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
                  column(
                    width = 6,
                    div(class = "card p-3",
                        div(class = "section-title", "Daily Activity Breakdown"),
                        tabsetPanel(
                          id = "activityTabs",
                          tabPanel("Files", plotlyOutput("filesPlot")),
                          tabPanel("Quizzes", plotlyOutput("quizzesPlot")),
                          tabPanel("Discussions", plotlyOutput("discussionsPlot"))
                        )
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
    }
  })
  
  course_id <- reactive({ input$cId })
  user_id <- reactive({ credentials$user_id })
  
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
    saveAllUserData(
      user_id = user_id(),
      course_id = input$cId,
      week = input$selected_week,
      satisfaction = input$satisfaction,
      expectation = input$expectation,
      feedback = feedback
    )
  })
  
  observeEvent(input$refresh_rating, ignoreInit = TRUE, {
    save_count$count <- save_count$count - 1
    course <- as.character(input$cId)
    week <- as.character(input$selected_week)
    user <- credentials$user_id
    
    # Safely remove in isolate block
    isolate({
      satisfaction_ratings$data[[course]][[week]] <- NULL
      expectation_ratings$data[[course]][[week]] <- NULL
      satisfaction_feedback_store$data[[course]][[week]] <- NULL
      reflection_saved$data[[course]][[week]] <- NULL
    })
    
    # Safely update JSON
    file_path <- "all_users_data.json"
    if (file.exists(file_path)) {
      data <- jsonlite::read_json(file_path, simplifyVector = FALSE)
      
      if (!is.null(data[[user]][[course]][[week]])) {
        data[[user]][[course]][[week]] <- NULL
        
        # Optional cleanup of empty branches
        if (length(data[[user]][[course]]) == 0) data[[user]][[course]] <- NULL
        if (length(data[[user]]) == 0) data[[user]] <- NULL
        
        jsonlite::write_json(data, file_path, pretty = TRUE, auto_unbox = TRUE)
      }
    }
    
    output$satisfaction_feedback <- renderText({ "" })
  })
  
  saveAllUserData <- function(user_id, course_id, week, satisfaction, expectation, feedback) {
    file_path <- "all_users_data.json"
    data <- list()
    
    if (file.exists(file_path)) {
      data <- jsonlite::read_json(file_path, simplifyVector = FALSE)
    }
    
    user_id <- as.character(user_id)
    course_id <- as.character(course_id)
    week <- as.character(week)
    
    if (is.null(data[[user_id]])) data[[user_id]] <- list()
    if (is.null(data[[user_id]][[course_id]])) data[[user_id]][[course_id]] <- list()
    
    data[[user_id]][[course_id]][[week]] <- list(
      satisfaction = satisfaction,
      expectation = expectation,
      feedback = feedback,
      reflection_saved = TRUE
    )
    
    jsonlite::write_json(data, file_path, pretty = TRUE, auto_unbox = TRUE)
  }
  
  loadUserData <- function(user_id) {
    file_path <- "all_users_data.json"
    if (file.exists(file_path)) {
      data <- jsonlite::read_json(file_path, simplifyVector = FALSE)
      return(data[[user_id]] %||% list())
    }
    return(list())
  }
  
  observeEvent(input$selected_week, {
    course <- as.character(input$cId)
    week <- as.character(input$selected_week)
    
    # Show saved feedback if it exists for this course/week
    if (!is.null(satisfaction_feedback_store$data[[course]]) &&
        !is.null(satisfaction_feedback_store$data[[course]][[week]])) {
      
      feedback <- satisfaction_feedback_store$data[[course]][[week]]
      output$satisfaction_feedback <- renderText({
        satisfaction_feedback_store$data[[course]][[week]] %||% ""
      })
      
      
    } else {
      # No saved reflection/feedback for this week → clear output
      output$satisfaction_feedback <- renderText({ "" })
    }
  })
  
  observeEvent(input$cId, {
    course <- as.character(input$cId)
    week <- as.character(input$selected_week)
    
    # If feedback exists for new course and selected week, show it
    if (!is.null(satisfaction_feedback_store$data[[course]]) &&
        !is.null(satisfaction_feedback_store$data[[course]][[week]])) {
      
      output$satisfaction_feedback <- renderText({
        satisfaction_feedback_store$data[[course]][[week]]
      })
    } else {
      # Otherwise, clear the feedback
      output$satisfaction_feedback <- renderText({ "" })
    }
  })
  
  
  output$selectedUserId <- renderText({
    course_id_value <- course_id()
    user_id_value <- user_id()
    date_value <- credentials$date
    
    totat_sec_result <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_enrollments")) %>%
      filter(course_id == course_id_value, user_id == user_id_value) %>%
      select(total_active_seconds) %>%
      head(1) %>%
      collect()
    
    seconds <- if (nrow(totat_sec_result) > 0) {
      totat_sec_result$total_active_seconds[1]
    } else {
      0
    }
    
    paste("Welcome", user_id_value, "| Date:", date_value, "| Total Active Time:", seconds, "seconds")
  })
  
  output$activityTracker <- renderUI({
    req(user_id())
    course_id <- course_id()
    
    week1_reflected <- isTRUE(reflection_saved$data[[course_id]][["1"]])
    week2_reflected <- isTRUE(reflection_saved$data[[course_id]][["2"]])
    
    query <- sprintf("
      SELECT COUNT(*) > 0 AS quiz_submitted_in_week
      FROM sandbox_la_conijn_cbl.silver_canvas_quiz_submissions
      WHERE user_id = '%s'
        AND workflow_state IN ('pending_review', 'complete')
        AND finished_at_anonymous >= '2024-11-27 08:00:00.000'
        AND finished_at_anonymous < '2024-11-27 09:00:00.000';
    ", user_id())

    result <- dbGetQuery(sc, query)
    week1_quiz_done <- result$quiz_submitted_in_week[1]
    
    activities <- list(
      list(name = "Submit Week 1 Quiz", done = week1_quiz_done),
      list(name = "Reflect on Week 1", done = week1_reflected),
      list(name = "Complete Reading", done = FALSE),
      list(name = "Submit Week 2 Quiz", done = FALSE),
      list(name = "Reflect on Week 2", done = week2_reflected)
    )
    
    total <- length(activities)
    completed <- sum(sapply(activities, function(x) x$done))
    percent <- 20 * sum(sapply(activities, function(x) x$done))
    
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
  
  df_files <- data.frame(
    date = seq.Date(Sys.Date() - 6, Sys.Date(), by = "day"),
    count = c(2, 3, 4, 3, 5, 4, 6)
  )
  
  df_quizzes <- data.frame(
    date = seq.Date(Sys.Date() - 6, Sys.Date(), by = "day"),
    time_spent = c(15, 20, 10, 25, 30, 20, 18)
  )
  
  df_discussions <- data.frame(
    date = seq.Date(Sys.Date() - 6, Sys.Date(), by = "day"),
    time_spent = c(5, 10, 8, 12, 9, 7, 6)
  )
  
  output$filesPlot <- renderPlotly({
    plot_ly(df_files, x = ~date, y = ~count, type = 'scatter', mode = 'lines+markers', name = 'Files') %>%
      layout(
        title = "Files Accessed per Day",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Activity Count"),
        hovermode = "x unified"
      )
  })
  
  output$quizzesPlot <- renderPlotly({
    plot_ly(df_quizzes, x = ~date, y = ~time_spent, type = 'scatter', mode = 'lines+markers', name = 'Quizzes') %>%
      layout(
        title = "Time Spent on Quizzes",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Time Spent (min)"),
        hovermode = "x unified"
      )
  })
  
  output$discussionsPlot <- renderPlotly({
    plot_ly(df_discussions, x = ~date, y = ~time_spent, type = 'scatter', mode = 'lines+markers', name = 'Discussions') %>%
      layout(
        title = "Time Spent on Discussions",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Time Spent (min)"),
        hovermode = "x unified"
      )
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
      actionButton("refresh_rating", "Update Reflection", class = "btn btn-primary mt-2")
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
  
  output$badges <- renderUI({
    req(user_id(), course_id())
    
    course <- as.character(course_id())
    reflections <- reflection_saved$data[[course]]
    
    reflection_count <- if (!is.null(reflections)) {
      sum(unlist(reflections), na.rm = TRUE)
      } else {
      0
      }
    
    # Optional debug line to Console
    print(paste("Reflection count for course", course, ":", reflection_count))
    
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
      if (reflection_count >= b$threshold) {
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
}

shinyApp(ui, server)