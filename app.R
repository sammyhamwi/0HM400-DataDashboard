# app.R

library(jsonlite)
library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(tidyr)

ui <- fluidPage(
  tags$head(
    # Bootstrap CSS
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
    # Custom CSS with new dashboard palette:
    #   - #F0F2F5 (light gray background)
    #   - #2C3E50 (dark slate for sidebar)
    #   - #2980B9 (blue accent)
    #   - #E67E22 (orange highlight)
    #   - #2ECC71 (green for progress)
    tags$style(HTML("
      /* Page background and font */
      body {
        background-color: #F0F2F5;
        font-family: system-ui;
      }

      /* Header banner: gradient from dark slate to blue */
      .header-banner {
        background: linear-gradient(90deg, #2C3E50, #2980B9);
        color: white;
        padding: 1rem 2rem;
        margin-bottom: 0;
        display: flex;
        justify-content: space-between;
        align-items: center;
        border-radius: 0;  /* remove bottom-left rounding so sidebar meets flush */
      }
      .header-banner h3 {
        margin: 0;
        font-size: 1.75rem;
        font-weight: 600;
      }
      .header-banner .btn-logout {
        background-color: rgba(255,255,255,0.85);
        color: #2980B9;
        font-weight: 500;
        border: 1px solid #2980B9;
      }
      .header-banner .btn-logout:hover {
        background-color: white;
        color: #2C3E50;
        border-color: #2C3E50;
      }

      /* Sidebar styling as vertical nav */
      .sidebar {
        background-color: #2C3E50;
        height: calc(100vh - 3.5rem); /* subtract header height (~3.5rem) */
        padding: 2rem 1rem;
        border-right: 1px solid #1A252F;
        position: sticky;
        top: 3.5rem; /* stick just below header */
      }
      .sidebar h5 {
        margin-bottom: 1.5rem;
        font-size: 1.25rem;
        font-weight: 600;
        color: #ECF0F1;
        text-align: center;
      }
      .sidebar .list-group-item {
        border-radius: 0.5rem;
        margin-bottom: 1rem;
        font-weight: 500;
        color: #ECF0F1;
        background-color: #34495E;
        border: 1px solid #1A252F;
      }
      .sidebar .list-group-item.active {
        background-color: #E67E22;
        color: white;
        border-color: #E67E22;
      }
      .sidebar .list-group-item:hover {
        background-color: #2980B9;
        color: white;
        cursor: pointer;
        border-color: #2980B9;
      }
      .sidebar .list-group-item.active:hover {
        background-color: #E67E22 !important;
        color: white !important;
      }

      /* Main content area */
      .main-content {
        padding: 1.5rem 2rem 1rem 2rem;
      }

      /* Card styling */
      .card {
        background-color: white;
        border-radius: 1rem;
        box-shadow: 0 1px 6px rgba(0, 0, 0, 0.05);
        margin-bottom: 1.5rem;
        overflow: visible !important;
        position: relative;
        border-left: 5px solid #2980B9;  /* blue accent on left */
      }
      .section-title {
        font-weight: 600;
        font-size: 1.2rem;
        margin-bottom: 0.75rem;
        color: #E67E22;  /* orange accent */
      }

      /* Login card centered */
      .login-card {
        max-width: 400px;
        margin: 10% auto;
        padding: 2rem;
        border-radius: 1rem;
        box-shadow: 0 1px 6px rgba(0, 0, 0, 0.1);
        background-color: white;
        display: flex;
        flex-direction: column;
        align-items: center;
      }
      .login-card .shiny-input-container {
        width: 100%;
        max-width: 300px;
        margin-bottom: 1rem;
      }
      .login-card .btn {
        width: 100%;
        max-width: 300px;
        margin-top: 1rem;
        background-color: #E67E22;
        border-color: #E67E22;
      }
      .login-card .btn:hover {
        background-color: #2980B9;
        border-color: #2980B9;
      }

      /* Hide Shiny slider ticks */
      .irs-grid-text,
      .irs-min, .irs-max {
        display: none !important;
      }
      .irs--shiny .irs-grid {
        display: none !important;
      }

      /* Badge container styling */
      .badge-container {
        display: flex;
        flex-direction: row;
        justify-content: center;
        align-items: center;
        gap: 30px;
        flex-wrap: wrap;
        transition: all 0.3s ease;
        padding: 1rem 0;
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
        background-color: rgba(0, 0, 0, 0.75);
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
      /* Ensure cards don’t clip badge tooltips */
      .card,
      .col-md-6,
      .col-md-9,
      .row {
        overflow: visible !important;
        position: relative;
      }

      /* Button overrides for Save/Update and Logout */
      .btn-primary {
        background-color: #E67E22;
        border-color: #E67E22;
      }
      .btn-primary:hover {
        background-color: #2980B9;
        border-color: #2980B9;
      }
      .btn-outline-primary {
        color: #E67E22;
        border-color: #E67E22;
      }
      .btn-outline-primary:hover {
        background-color: #E67E22;
        color: white;
      }

      /* Progress bar accent */
      .progress-bar {
        background-color: #2ECC71 !important;
      }
      /* Activity Tracker list items */
      .list-group-item.text-success {
        background-color: #DFF0D8 !important;  /* light green */
        color: #2C3E50 !important;             /* dark slate */
      }
      .list-group-item.text-muted {
        background-color: #FDEDEC !important;  /* light red */
        color: #C0392B !important;             /* dark red */
      }
    "))
  ),
  uiOutput("mainUI")
)

server <- function(input, output, session) {
  credentials <- reactiveValues(logged_in = FALSE, user_id = NULL, date = NULL)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Track selected course manually (initial: 28301)
  selectedCourse <- reactiveVal("28301")
  
  # Handle login
  observeEvent(input$login_button, {
    req(input$user_id_input, input$date_input)
    credentials$logged_in <- TRUE
    credentials$user_id <- input$user_id_input
    credentials$date <- input$date_input
    # Clear old user data
    satisfaction_ratings$data <- list()
    expectation_ratings$data <- list()
    satisfaction_feedback_store$data <- list()
    reflection_saved$data <- list()
  })
  
  # Update selectedCourse when sidebar links clicked
  observeEvent(input$course_27886, { selectedCourse("27886") })
  observeEvent(input$course_28301, { selectedCourse("28301") })
  observeEvent(input$course_27893, { selectedCourse("27893") })
  observeEvent(input$course_27974, { selectedCourse("27974") })
  
  # Load user data on login or course change
  observe({
    req(credentials$logged_in, credentials$user_id)
    course <- selectedCourse()
    user_data <- loadUserData(credentials$user_id)
    
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
  
  # Handle logout
  observeEvent(input$logout_button, {
    credentials$logged_in <- FALSE
    credentials$user_id <- NULL
  })
  
  # Main UI rendering
  output$mainUI <- renderUI({
    if (!credentials$logged_in) {
      # Login UI
      fluidPage(
        div(class = "card login-card",
            h4("Login", class = "text-center mb-5"),
            textInput("user_id_input", "User ID", placeholder = "Enter your User ID"),
            dateInput("date_input", "Select Date", value = Sys.Date(), format = "yyyy-mm-dd"),
            actionButton("login_button", "Login", class = "btn btn-primary mt-3")
        )
      )
    } else {
      # Main App UI
      fluidPage(
        # Header banner
        fluidRow(
          column(
            width = 12,
            div(class = "header-banner",
                h3("Self-Regulated Learning Dashboard"),
                actionButton("logout_button", "Logout", class = "btn btn-logout")
            )
          )
        ),
        # Sidebar + Main content
        fluidRow(
          # Sidebar as vertical nav
          column(
            width = 3,
            div(class = "sidebar",
                h5("Select Course"),
                uiOutput("sidebar")  # will render list-group links
            )
          ),
          # Main content area
          column(
            width = 9,
            div(class = "main-content",
                # Welcome card
                div(class = "card p-3 mb-5",
                    fluidRow(
                      column(
                        width = 8,
                        h4(textOutput("dashboardTitle", inline = TRUE), class = "mb-2")
                      ),
                      column(
                        width = 4,
                        div(class = "text-end text-muted", textOutput("selectedUserId"))
                      )
                    )
                ),
                # Two columns for tracker/reflections and plots/badges
                fluidRow(
                  # Left column
                  column(
                    width = 6,
                    # Activity Tracker
                    div(class = "card p-3 mb-5",
                        div(class = "section-title", "Activity Tracker"),
                        uiOutput("activityTracker")
                    ),
                    # Reflections
                    div(class = "card p-3 mb-5",
                        div(class = "section-title", "Reflections"),
                        selectInput("selected_week", "Select Week", choices = 1:10, selected = 1),
                        uiOutput("satisfaction_ui"),
                        uiOutput("expectation_ui"),
                        uiOutput("save_button_ui"),
                        br(),
                        div(class = "text-muted mb-3", textOutput("satisfaction_feedback")),
                        uiOutput("streak_badge")
                    )
                  ),
                  # Right column
                  column(
                    width = 6,
                    # Daily Activity Breakdown
                    div(class = "card p-3 mb-5",
                        div(class = "section-title", "Daily Activity Breakdown"),
                        tabsetPanel(
                          id = "activityTabs",
                          tabPanel("Files", plotlyOutput("filesPlot", height = "250px")),
                          tabPanel("Quizzes", plotlyOutput("quizzesPlot", height = "250px")),
                          tabPanel("Discussions", plotlyOutput("discussionsPlot", height = "250px"))
                        )
                    ),
                    # Badges
                    div(class = "card p-3 mb-5",
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
  
  #########################
  # Render sidebar links
  #########################
  output$sidebar <- renderUI({
    current <- selectedCourse()
    tags$div(class = "list-group",
             actionLink(
               inputId = "course_27886",
               label   = "0HV30-2024",
               class   = paste0("list-group-item list-group-item-action",
                                if (current == "27886") " active" else "")
             ),
             actionLink(
               inputId = "course_28301",
               label   = "0HV60-2024",
               class   = paste0("list-group-item list-group-item-action",
                                if (current == "28301") " active" else "")
             ),
             actionLink(
               inputId = "course_27893",
               label   = "0HV90-2024",
               class   = paste0("list-group-item list-group-item-action",
                                if (current == "27893") " active" else "")
             ),
             actionLink(
               inputId = "course_27974",
               label   = "0HV100-2024",
               class   = paste0("list-group-item list-group-item-action",
                                if (current == "27974") " active" else "")
             )
    )
  })
  
  #########################
  # Reactive storage
  #########################
  course_id <- reactive({ selectedCourse() })
  user_id <- reactive({ credentials$user_id })
  
  satisfaction_ratings <- reactiveValues(data = list())
  expectation_ratings <- reactiveValues(data = list())
  satisfaction_feedback_store <- reactiveValues(data = list())
  reflection_saved <- reactiveValues(data = list())
  save_count <- reactiveValues(count = 0)
  current_week <- reactive({ as.numeric(input$selected_week) })
  
  #########################
  # Save / Update Reflection logic
  #########################
  observeEvent(input$save_rating, {
    save_count$count <- save_count$count + 1
    course <- selectedCourse()
    this_week <- as.character(input$selected_week)
    last_week <- as.character(as.numeric(input$selected_week) - 1)
    
    if (is.null(satisfaction_ratings$data[[course]])) satisfaction_ratings$data[[course]] <- list()
    if (is.null(expectation_ratings$data[[course]])) expectation_ratings$data[[course]] <- list()
    
    satisfaction_ratings$data[[course]][[this_week]] <- input$satisfaction
    expectation_ratings$data[[course]][[this_week]] <- input$expectation
    
    if (is.null(reflection_saved$data[[course]])) reflection_saved$data[[course]] <- list()
    reflection_saved$data[[course]][[this_week]] <- TRUE
    
    feedback <- ""
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
      course_id = course,
      week = input$selected_week,
      satisfaction = input$satisfaction,
      expectation = input$expectation,
      feedback = feedback
    )
  })
  
  observeEvent(input$refresh_rating, ignoreInit = TRUE, {
    save_count$count <- save_count$count - 1
    course <- selectedCourse()
    week <- as.character(input$selected_week)
    user <- credentials$user_id
    
    isolate({
      satisfaction_ratings$data[[course]][[week]] <- NULL
      expectation_ratings$data[[course]][[week]] <- NULL
      satisfaction_feedback_store$data[[course]][[week]] <- NULL
      reflection_saved$data[[course]][[week]] <- NULL
    })
    
    file_path <- "all_users_data.json"
    if (file.exists(file_path)) {
      data <- jsonlite::read_json(file_path, simplifyVector = FALSE)
      if (!is.null(data[[user]][[course]][[week]])) {
        data[[user]][[course]][[week]] <- NULL
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
  
  # Show saved feedback when week changes
  observeEvent(input$selected_week, {
    course <- selectedCourse()
    week <- as.character(input$selected_week)
    if (!is.null(satisfaction_feedback_store$data[[course]]) &&
        !is.null(satisfaction_feedback_store$data[[course]][[week]])) {
      output$satisfaction_feedback <- renderText({
        satisfaction_feedback_store$data[[course]][[week]] %||% ""
      })
    } else {
      output$satisfaction_feedback <- renderText({ "" })
    }
  })
  
  observeEvent(selectedCourse(), {
    course <- selectedCourse()
    week <- as.character(input$selected_week)
    if (!is.null(satisfaction_feedback_store$data[[course]]) &&
        !is.null(satisfaction_feedback_store$data[[course]][[week]])) {
      output$satisfaction_feedback <- renderText({
        satisfaction_feedback_store$data[[course]][[week]]
      })
    } else {
      output$satisfaction_feedback <- renderText({ "" })
    }
  })
  
  #########################
  # Dynamic Outputs
  #########################
  
  # Dynamic user info
  output$selectedUserId <- renderText({
    course_id_value <- course_id()
    user_id_value <- user_id()
    date_value <- credentials$date
    
    # Example query - replace with actual DB connection details
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
  
  # Activity Tracker
  output$activityTracker <- renderUI({
    req(user_id()) # Ensures user_id is available
    current_course_id_char <- as.character(course_id())
    
    # Existing reflection checks
    week1_reflected <- isTRUE(reflection_saved$data[[current_course_id_char]][["1"]])
    week2_reflected <- isTRUE(reflection_saved$data[[current_course_id_char]][["2"]])
    
    # --- Query for Week 1 Quiz (Existing) ---
    start_time_w1_quiz <- '2024-11-27 08:00:00.000'
    end_time_w1_quiz <- '2024-11-27 09:00:00.000'
    
    query_w1_quiz <- sprintf("
    SELECT COUNT(*) > 0 AS quiz_submitted_in_week
    FROM sandbox_la_conijn_cbl.silver_canvas_quiz_submissions
    WHERE user_id = '%s'
      AND workflow_state IN ('pending_review', 'complete')
      AND finished_at_anonymous >= '%s'
      AND finished_at_anonymous < '%s';
  ", user_id(), start_time_w1_quiz, end_time_w1_quiz)
    
    result_w1_quiz <- tryCatch({
      dbGetQuery(sc, query_w1_quiz)
    }, error = function(e) {
      cat("Error querying quiz submissions for Week 1:", e$message, "\n")
      data.frame(quiz_submitted_in_week = FALSE)
    })
    week1_quiz_done <- if (nrow(result_w1_quiz) > 0) result_w1_quiz$quiz_submitted_in_week[1] else FALSE
    
    # --- START: New Code for Week 1 Assignment ---
    start_time_w1_assignment <- '2025-02-01 00:00:00.000'
    end_time_w1_assignment   <- '2025-02-08 00:00:00.000'
    
    query_w1_assignment <- sprintf("
    SELECT COUNT(*) > 0 AS assignment_submitted_this_week
    FROM sandbox_la_conijn_cbl.silver_canvas_submissions
    WHERE user_id = '%s'
      AND workflow_state IN ('pending_review', 'graded', 'submitted')
      AND submitted_at_anonymous >= '%s'
      AND submitted_at_anonymous < '%s';
  ", user_id(), start_time_w1_assignment, end_time_w1_assignment)
    
    result_w1_assignment <- tryCatch({
      dbGetQuery(sc, query_w1_assignment)
    }, error = function(e) {
      cat("Error querying assignment submissions for Week 1:", e$message, "\n")
      data.frame(assignment_submitted_this_week = FALSE)
    })
    week1_assignment_done <- if (nrow(result_w1_assignment) > 0) result_w1_assignment$assignment_submitted_this_week[1] else FALSE
    # --- END: New code for Week 1 Assignment ---
    
    # Define the list of activities including the new assignment task
    activities <- list(
      list(name = "Submit Week 1 Quiz", done = week1_quiz_done),
      list(name = "Submit Week 1 Assignment", done = week1_assignment_done),
      list(name = "Reflect on Week 1", done = week1_reflected),
      list(name = "Complete Reading", done = FALSE),
      list(name = "Submit Week 2 Quiz", done = FALSE),
      list(name = "Reflect on Week 2", done = week2_reflected)
    )
    
    total <- length(activities)
    completed <- sum(sapply(activities, function(x) x$done))
    percent <- if (total > 0) round((completed / total) * 100) else 0
    
    # =============================================================
    # START: New Feedback Generation Logic
    # =============================================================
    
    # Initialize an empty feedback message
    activity_feedback_text <- ""
    
    # Generate feedback based on the number of completed tasks
    if (completed == total && total > 0) {
      # Case 1: All tasks are completed
      activity_feedback_text <- "Great job! You've completed all activities. 🎉"
    } else if (completed > 0) {
      # Case 2: At least one task is completed
      activity_feedback_text <- "Excellent! You've checked another activity off the list. Keep the momentum going! 💪"
    } else {
      # Case 3: No tasks are completed yet
      activity_feedback_text <- "It looks like you're just getting started! For a good overview, perhaps look at the activity breakdown to see where you can best invest your time. 🗺"
    }
    
    # Create the HTML for the feedback message. Using Bootstrap's 'alert' class for nice styling.
    feedback_html <- sprintf(
      "<div class='alert alert-info mt-3' role='alert'>%s</div>", 
      activity_feedback_text
    )
    
    # =============================================================
    # END: New Feedback Generation Logic
    # =============================================================
    
    # HTML for the list items (Unchanged)
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
    
    # HTML for the progress bar (Unchanged)
    progress_bar <- sprintf("
  <div class='progress'>
    <div class='progress-bar bg-success' role='progressbar' style='width: %d%%;' aria-valuenow='%d' aria-valuemin='0' aria-valuemax='100'>
      %d%%
    </div>
  </div>", percent, percent, percent)
    
    # Combine all HTML elements, with the new feedback message at the end
    HTML(paste0(list_items, progress_bar, feedback_html))
  })
  
  # -------------------------------------------------------------------
  # REAL DATA FOR PLOTS (last 7 days up to selected login date, per user_id)
  # -------------------------------------------------------------------
  
  ## FILES PLOT: count of “files” actions in web_logs
  output$filesPlot <- renderPlotly({
    req(credentials$logged_in, credentials$user_id, credentials$date)
    
    # Build an 8-day window: [selected_date - 7, selected_date]
    end_date   <- as.Date(credentials$date)
    start_date <- end_date - 7
    
    # Use timestamp range instead of CAST(… AS DATE)
    # We include all of end_date by using < (end_date + 1)
    start_ts <- paste0(start_date, " 00:00:00")
    end_ts   <- paste0(end_date + 1, " 00:00:00")
    
    query_files <- sprintf("
      SELECT
        CAST(timestamp AS DATE) AS activity_date,
        COUNT(*)                AS file_count
      FROM sandbox_la_conijn_cbl.silver_canvas_web_logs
      WHERE user_id = '%s'
        AND web_application_controller = 'files'
        AND timestamp BETWEEN '%s' AND '%s'
        AND course_id = %s
      GROUP BY CAST(timestamp AS DATE)
      ORDER BY activity_date;
    ",
                           credentials$user_id,
                           start_ts,
                           end_ts,
                           course_id()
    )
    
    df_files <- tryCatch({
      dbGetQuery(sc, query_files)
    }, error = function(e) {
      data.frame(
        activity_date = seq.Date(start_date, end_date, by = "day"),
        file_count    = rep(0L, 8)
      )
    })
    
    all_days <- data.frame(activity_date = seq.Date(start_date, end_date, by = "day"))
    df_files <- merge(all_days, df_files, by = "activity_date", all.x = TRUE)
    df_files$file_count[is.na(df_files$file_count)] <- 0
    
    plot_ly(
      df_files,
      x = ~activity_date,
      y = ~file_count,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Files'
    ) %>%
      layout(
        title = "Files Accessed per Day",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Activity Count"),
        hovermode = "x unified"
      )
  })
  
  ## QUIZZES PLOT: sum of DurationInMinutes per day from quiz_submissions
  output$quizzesPlot <- renderPlotly({
    req(credentials$logged_in, credentials$user_id, credentials$date)
    
    end_date   <- as.Date(credentials$date)
    start_date <- end_date - 7
    
    start_ts <- paste0(start_date, " 00:00:00")
    end_ts   <- paste0(end_date + 1, " 00:00:00")
    
    query_quizzes <- sprintf("
      SELECT
        CAST(finished_at_anonymous AS DATE) AS attempt_date,
        COALESCE(SUM(DurationInMinutes), 0)  AS total_time_spent
      FROM sandbox_la_conijn_cbl.silver_canvas_quiz_submissions
      WHERE user_id = '%s'
        AND finished_at_anonymous BETWEEN '%s' AND '%s'
        AND course_id = %s
      GROUP BY CAST(finished_at_anonymous AS DATE)
      ORDER BY attempt_date;
    ",
                             credentials$user_id,
                             start_ts,
                             end_ts,
                             course_id()
    )
    
    df_quizzes <- tryCatch({
      dbGetQuery(sc, query_quizzes)
    }, error = function(e) {
      data.frame(
        attempt_date     = seq.Date(start_date, end_date, by = "day"),
        total_time_spent = rep(0L, 8)
      )
    })
    
    all_days <- data.frame(attempt_date = seq.Date(start_date, end_date, by = "day"))
    df_quizzes <- merge(all_days, df_quizzes, by = "attempt_date", all.x = TRUE)
    df_quizzes$total_time_spent[is.na(df_quizzes$total_time_spent)] <- 0
    
    plot_ly(
      df_quizzes,
      x = ~attempt_date,
      y = ~total_time_spent,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Quizzes'
    ) %>%
      layout(
        title = "Time Spent on Quizzes",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Time Spent (min)"),
        hovermode = "x unified"
      )
  })
  
  ## DISCUSSIONS PLOT: count of discussion entries per day
  output$discussionsPlot <- renderPlotly({
    course <- selectedCourse()
    req(credentials$logged_in, credentials$user_id, credentials$date)
    
    end_date   <- as.Date(credentials$date)
    start_date <- end_date - 7
    
    start_ts <- paste0(start_date, " 00:00:00")
    end_ts   <- paste0(end_date + 1, " 00:00:00")
    
    query_discussions <- sprintf("
      SELECT
        CAST(created_at_anonymous AS DATE) AS post_date,
        COUNT(*)                        AS posts_count
      FROM sandbox_la_conijn_cbl.silver_canvas_discussion_entries
      WHERE user_id = '%s'
        AND created_at_anonymous BETWEEN '%s' AND '%s'
        AND course_id = %s
      GROUP BY CAST(created_at_anonymous AS DATE)
      ORDER BY post_date;
    ",
                                 credentials$user_id,
                                 start_ts,
                                 end_ts,
                                 course_id()
    )
    
    df_discussions <- tryCatch({
      dbGetQuery(sc, query_discussions)
    }, error = function(e) {
      data.frame(
        post_date   = seq.Date(start_date, end_date, by = "day"),
        posts_count = rep(0L, 8)
      )
    })
    
    all_days <- data.frame(post_date = seq.Date(start_date, end_date, by = "day"))
    df_discussions <- merge(all_days, df_discussions, by = "post_date", all.x = TRUE)
    df_discussions$posts_count[is.na(df_discussions$posts_count)] <- 0
    
    plot_ly(
      df_discussions,
      x = ~post_date,
      y = ~posts_count,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Discussions'
    ) %>%
      layout(
        title = "Discussion Posts per Day",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Posts Count"),
        hovermode = "x unified"
      )
  })
  
  # Satisfaction input or plain text if saved
  output$satisfaction_ui <- renderUI({
    course <- selectedCourse()
    week <- as.character(input$selected_week)
    saved <- !is.null(reflection_saved$data[[course]][[week]]) && reflection_saved$data[[course]][[week]]
    
    if (saved) {
      value <- satisfaction_ratings$data[[course]][[week]]
      div(class = "form-control-plaintext", paste("Self-Reported Satisfaction Score:", value))
    } else {
      sliderInput(
        "satisfaction",
        "How satisfied are you with your learning progress this week?",
        min = 1, max = 10, value = 5,
        width = "100%"
      )
    }
  })
  
  # Expectation input or plain text if saved
  output$expectation_ui <- renderUI({
    course <- selectedCourse()
    week <- as.character(input$selected_week)
    saved <- !is.null(reflection_saved$data[[course]][[week]]) && reflection_saved$data[[course]][[week]]
    
    if (saved) {
      value <- expectation_ratings$data[[course]][[week]]
      div(class = "form-control-plaintext", paste("Expected Progress Satisfaction:", value))
    } else {
      sliderInput(
        "expectation",
        "How satisfied do you expect to be with your learning progress next week?",
        min = 1, max = 10, value = 5,
        width = "100%"
      )
    }
  })
  
  # Save or Update button
  output$save_button_ui <- renderUI({
    course <- selectedCourse()
    week <- as.character(input$selected_week)
    saved <- !is.null(reflection_saved$data[[course]][[week]]) && reflection_saved$data[[course]][[week]]
    
    if (!saved) {
      actionButton("save_rating", "Save Reflection", class = "btn btn-primary w-100 mt-2")
    } else {
      actionButton("refresh_rating", "Update Reflection", class = "btn btn-outline-primary w-100 mt-2")
    }
  })
  
  # Streak Badge
  output$streak_badge <- renderUI({
    course <- selectedCourse()
    week <- as.numeric(input$selected_week)
    
    if (is.null(reflection_saved$data[[course]]) ||
        is.null(reflection_saved$data[[course]][[as.character(week)]]) ||
        !reflection_saved$data[[course]][[as.character(week)]]) {
      return(NULL)
    }
    
    submitted_weeks <- sort(as.numeric(names(satisfaction_ratings$data[[course]])))
    submitted_weeks <- submitted_weeks[submitted_weeks <= week]
    
    if (length(submitted_weeks) == 0) return(NULL)
    
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
          🏅 <strong>%d-Week Streak!</strong> Reflection badge for %d consecutive submissions.
        </div>
      ", streak, streak))
    } else {
      return(NULL)
    }
  })
  
  # Badges display
  output$badges <- renderUI({
    req(user_id(), course_id())
    
    course <- selectedCourse()
    reflections <- reflection_saved$data[[course]]
    
    reflection_count <- if (!is.null(reflections)) {
      sum(unlist(reflections), na.rm = TRUE)
    } else {
      0
    }
    
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
        unlocked_tooltip = "4 weeks of reflections - Keep reflecting!",
        locked_tooltip = "Reflect for 4 weeks."
      ),
      list(
        threshold = 8,
        unlocked_img = "MASTER.png",
        unlocked_tooltip = "8 weeks of reflecting?! – Good job!",
        locked_tooltip = "Reflect for 8 weeks."
      )
    )
    
    badge_html <- "<p><strong>Your Reflection Badges</strong></p><div class='badge-container'>"
    for (b in badges) {
      if (reflection_count >= b$threshold) {
        badge_html <- paste0(
          badge_html,
          "<span class='badge-item' data-tooltip='", b$unlocked_tooltip, "'>",
          "<img src='", b$unlocked_img, "' height='90px'/>",
          "</span>"
        )
      } else {
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
  
  output$dashboardTitle <- renderText({
    # Map the selectedCourse() ID to its human-readable label:
    switch(
      selectedCourse(),
      "27886" = "0HV30-2024",
      "28301" = "0HV60-2024",
      "27893" = "0HV90-2024",
      "27974" = "0HV100-2024",
      "Unknown Course"
    ) %>% paste("Self-Regulated Learning Reflection Dashboard")
  })
}

shinyApp(ui, server)
