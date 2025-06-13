# app.R

library(jsonlite)
library(shiny)
library(shinyjs)
library(dplyr)
library(dbplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(tidyr)
library(shinycssloaders)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
    includeCSS("www/css/custom.css")
  ),
  
  div(id = "app-content",
      
    # ——————————————————————————————————
    # Login Panel (shown when not logged in)
    # ——————————————————————————————————
    div(
      id    = "login_panel",
      class = "card login-card",
      h4("Login", class = "text-center mb-5"),
      textInput("user_id_input", "User ID", placeholder = "Enter your User ID"),
      dateInput("date_input",   "Select Date",
                value = Sys.Date(), format = "yyyy-mm-dd"),
      actionButton("login_button", "Login",
                   class = "btn btn-primary mt-3")
    ),
    
    # ——————————————————————————————————
    # Main App Panel (hidden until login)
    # ——————————————————————————————————
    hidden(
      div(
        id = "main_panel",
        
        # Header banner
        fluidRow(
          column(
            width = 12,
            div(class = "header-banner d-flex align-items-center",
                actionButton("toggle_sidebar", label = NULL, icon = icon("bars"),
                             class = "btn btn-outline-primary me-3"),
                h3("Self-Regulated Learning Dashboard", class = "mb-0 flex-grow-1"),
                actionButton("logout_button", "Logout",
                             class = "btn btn-logout")
            )
          )
        ),
        
        # Sidebar + Main content
        fluidRow(
          # Sidebar
          column(
            width = 3, id = "sidebar_col",
            div(class = "sidebar",
                h5("Select Course"),
                uiOutput("sidebar")
            )
          ),
          # Main area
          column(
            width = 9, id = "main_col",
            div(class = "main-content",
                
                # Welcome card
                div(class = "card p-3 mb-5",
                    fluidRow(
                      column(
                        width = 8,
                        h4(textOutput("dashboardTitle", inline = TRUE),
                           class = "mb-2")
                      ),
                      column(width = 2, div()),
                      column(
                        width = 2,
                        div(class = "p", textOutput("selectedUserId"))
                      )
                    )
                ),
                
                # Two columns: Tracker/Reflections & Graph/Badges
                fluidRow(
                  # Left column
                  column(
                    width = 6,
                    # Activity Tracker
                    div(class = "card p-3 mb-5",
                        div(class = "section-title","Activity Tracker"),
                        withSpinner(
                          uiOutput("activityTracker"),
                          type = 6,
                          color = "#2980B9"
                        )
                    ),
                    # Reflections
                    div(class = "card p-3 mb-5",
                        div(class = "section-title","Reflections"),
                        withSpinner(
                          uiOutput("reflections_card"),
                          type = 6,
                          color = "#2980B9"
                        )
                    )
                  ),
                  # Right column
                  column(
                    width = 6,
                    # Graph Builder
                    div(class = "card p-3 mb-5",
                        div(class = "section-title","Weekly Activity Overview"),
                        fluidRow(
                          column(4,
                                 selectInput("data_type","Select Activity",
                                             choices = c("Quizzes","Files","Discussions","Assignments"),
                                             selected = "Quizzes")
                          ),
                          column(4, 
                                 selectInput("selected_week", "Select Week", 
                                             choices = setNames(1:10, paste("Week", 1:10)), 
                                             selected = 1, 
                                             width = "100%") 
                          ),
                          column(4,
                                 uiOutput("metric_ui")
                          )
                        ),
                        br(),
                        uiOutput("activity_view")
                    ),
                    # Badges
                    div(class = "card p-3 mb-5",
                        div(class = "section-title","Badges"),
                        withSpinner(
                          uiOutput("badges"),
                          type = 6,
                          color = "#2980B9"
                        )
                    )
                  )
                ) # end fluidRow
            ) # end main-content
          ) # end col-9
        ) # end fluidRow
      ) # end main_panel
    ) # end hidden()
  ) # app-content div
)

server <- function(input, output, session) {
  show_optional_text <- reactiveVal(FALSE)
  sidebar_collapsed <- reactiveVal(FALSE)
  activity_completion <- reactiveValues(data = list())
  credentials <- reactiveValues(logged_in = FALSE, user_id = NULL, date = NULL)
  userCourses <- reactiveVal(NULL)
  
  credentials$theme <- "default"
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Track selected course manually (initial: 28301)
  selectedCourse <- reactiveVal("28301")
  
  #course starting date
  COURSE_START_DATE <- as.Date("2024-11-11")
  
  # Handle login
  observeEvent(input$login_button, {
    req(input$user_id_input, input$date_input)
    
    shinyjs::hide("login_panel")
    shinyjs::show("main_panel")
    
    credentials$logged_in <- TRUE
    credentials$user_id <- input$user_id_input
    credentials$date <- input$date_input
    
    # load saved theme (if any)
    prefs_file <- "all_users_data.json"
    if (file.exists(prefs_file)) {
      all <- jsonlite::read_json(prefs_file, simplifyVector = FALSE)
      saved <- all[[credentials$user_id]]$theme %||% "default"
      credentials$theme <- saved
    }
    # apply theme
    shinyjs::removeClass(
      class    = c("theme-default","theme-light","theme-dark","theme-ocean","theme-solarized"),
      selector = "html"
    )
    # add the saved theme class to the <html> tag
    shinyjs::addClass(
      class    = paste0("theme-", credentials$theme),
      selector = "html"
    )
    
    # Fetch and cache this user’s course_ids
    df <- dbGetQuery(
      sc,
      sprintf("
       SELECT DISTINCT course_id
       FROM sandbox_la_conijn_cbl.silver_canvas_enrollments
       WHERE user_id = '%s'
     ", credentials$user_id)
    )
    userCourses(as.character(df$course_id))
    
    # auto starts activity graph to assignments by submission for last 7 days
    updateSelectInput(session, "data_type",
                      selected = "Assignments")
    updateDateRangeInput(session, "date_range",
                         start = as.Date(credentials$date) - 7,
                         end   = as.Date(credentials$date))
    updateSelectInput(session, "metric",
                      selected = "Number of Submissions")
    shinyjs::click("submit_query")
    
    updateSelectInput(session, "selected_week",
                      selected = weekNumber())
    
    # Clear old user data
    satisfaction_ratings$data <- list()
    expectation_ratings$data <- list()
    satisfaction_feedback_store$data <- list()
    reflection_saved$data <- list()
  })
  
  observeEvent(input$date_input, {
    req(credentials$logged_in)
    credentials$date <- input$date_input
    
    # auto-start activity graph exactly as on login:
    updateSelectInput(session, "data_type",
                      selected = "Assignments")
    # set the date_range to the last 7 days *ending* on the new banner date
    updateSelectInput(session, "selected_week",
                      selected = weekNumber())
    updateSelectInput(session, "metric",
                      selected = "Number of Submissions")
    # trigger the plot to redraw
    shinyjs::click("submit_query")
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
    week <- as.character(weekNumber())
    user_data <- loadUserData(credentials$user_id)
    
    if (is.list(user_data) && !is.null(user_data[[course]])) {
      weeks_data <- user_data[[course]]
      for (w in names(weeks_data)) {
        week_data <- weeks_data[[w]]
        if (!is.null(week_data)) {
          satisfaction_ratings$data[[course]][[w]] <- week_data$satisfaction %||% NULL
          expectation_ratings$data[[course]][[w]] <- week_data$expectation %||% NULL
          satisfaction_feedback_store$data[[course]][[w]] <- week_data$feedback %||% NULL
          reflection_saved$data[[course]][[w]] <- TRUE
        }
      }
    }
    
    # Only show feedback if reflection_saved is TRUE for this week
    if (!is.null(reflection_saved$data[[course]]) &&
        isTRUE(reflection_saved$data[[course]][[week]]) &&
        !is.null(satisfaction_feedback_store$data[[course]]) &&
        !is.null(satisfaction_feedback_store$data[[course]][[week]])) {
      output$satisfaction_feedback <- renderText({
        satisfaction_feedback_store$data[[course]][[week]] %||% ""
      })
      show_optional_text(TRUE)
    } else {
      output$satisfaction_feedback <- renderText({ "" })
      show_optional_text(FALSE)
    }
  })
  
  # Handle logout
  observeEvent(input$logout_button, {
    # Reset UI theme back to default
    shinyjs::removeClass(
      class = c("theme-default","theme-light","theme-dark","theme-ocean","theme-solarized"),
      selector = "html")
    shinyjs::addClass(class = "theme-default", selector = "html")

    credentials$logged_in <- FALSE
    credentials$user_id   <- NULL

    shinyjs::show("login_panel")
    shinyjs::hide("main_panel")
  })
  
  observeEvent(input$toggle_sidebar, {
    collapsed <- !sidebar_collapsed()
    sidebar_collapsed(collapsed)
    
    # sidebar collapse animations
    if (collapsed) {
      shinyjs::runjs("
      $('#sidebar_col').css('overflow','hidden')
                      .animate({ width: '0%' }, 300, function(){ $(this).hide(); });
      $('#main_col').animate({ width: '100%' }, 300);
    ")
    } else {
      shinyjs::runjs("
      $('#sidebar_col').show()
                      .css({ overflow: 'hidden', width: '0%' })
                      .animate({ width: '25%' }, 300);
      $('#main_col').animate({ width: '75%' }, 300);
    ")
    }
    
    # give the CSS animation 300ms to finish, then fire a resize
    shinyjs::delay(100, shinyjs::runjs("window.dispatchEvent(new Event('resize'));"))
  })
  
  #########################
  # Render sidebar links
  #########################
  output$sidebar <- renderUI({
    req(credentials$logged_in)
    
    # Master map of all possible courses
    course_map <- list(
      "27886" = "0HV30-2024",
      "28301" = "0HV60-2024",
      "27893" = "0HV90-2024",
      "27974" = "0HV100-2024"
    )
    
    # Only keep those the user actually has
    available <- intersect(names(course_map), userCourses())
    
    # Build one actionLink per available course
    items <- lapply(available, function(cid) {
      actionLink(
        inputId = paste0("course_", cid),
        label   = course_map[[cid]],
        class   = paste0(
          "list-group-item list-group-item-action",
          if (selectedCourse() == cid) " active" else ""
        )
      )
    })
    
    theme_picker <- div(
      class = "list-group-item theme-selector mt-5",
      style = "margin-top: 100px !important;",
      selectInput(
        "theme_choice", tags$strong("🎨 Theme Picker"),
        choices = c(
          "Default"   = "default",
          "Light"     = "light",
          "Dark"      = "dark",
          "Ocean"     = "ocean",
          "Solarized" = "solarized"
        ),
        selected = credentials$theme,
        width = "100%"
      ),
      actionButton(
        "apply_theme", "Apply",
        class = "btn btn-outline-primary btn-sm mt-2"
      )
    )
    
    # Wrap up
    tags$div(
      class = "list-group",
      items,
      theme_picker
    )
  })
  
  #########################
  # Reactive storage
  #########################
  course_id <- reactive({ selectedCourse() })
  user_id <- reactive({ credentials$user_id })
  
  weekNumber <- reactive({
    course_start_date <- as.Date("2024-11-11")
    login_date <- as.Date(credentials$date)
    num <- as.integer(floor(as.numeric(difftime(login_date, course_start_date, units = "days")) / 7)) + 1
    min(max(1, num), 10)
  })
  
  activity_completion <- reactiveValues(data = list())
  
  satisfaction_ratings <- reactiveValues(data = list())
  expectation_ratings <- reactiveValues(data = list())
  satisfaction_feedback_store <- reactiveValues(data = list())
  reflection_saved <- reactiveValues(data = list())
  
  current_week <- reactive({ as.numeric(input$selected_week) })
  
  #########################
  # Save / Update Reflection logic
  #########################
  saveActivityCompletion <- function(user_id, course_id, block, completion) {
    path <- "activity_completion_data.json"
    data <- if (file.exists(path)) jsonlite::read_json(path, simplifyVector = FALSE) else list()
    uid <- as.character(user_id); cid <- as.character(course_id); blk <- as.character(block)
    data[[uid]] <- data[[uid]] %||% list()
    data[[uid]][[cid]] <- data[[uid]][[cid]] %||% list()
    data[[uid]][[cid]][[blk]] <- completion
    jsonlite::write_json(data, path, pretty = TRUE, auto_unbox = TRUE)
  }
  
  loadActivityCompletion <- function(user_id) {
    path <- "activity_completion_data.json"
    if (!file.exists(path)) return(list())
    data <- jsonlite::read_json(path, simplifyVector = FALSE)
    data[[as.character(user_id)]] %||% list()
  }
  
  observe({
    req(credentials$logged_in)
    activity_completion$data <- loadActivityCompletion(credentials$user_id)
  })
  
  observeEvent(input$save_rating, {
    course <- selectedCourse()
    this_week <- as.character(weekNumber())
    last_week <- as.character(as.numeric(weekNumber()) - 1)
    show_optional_text(TRUE)
    
    if (is.null(satisfaction_ratings$data[[course]])) satisfaction_ratings$data[[course]] <- list()
    if (is.null(expectation_ratings$data[[course]])) expectation_ratings$data[[course]] <- list()
    
    satisfaction_ratings$data[[course]][[this_week]] <- input$satisfaction
    expectation_ratings$data[[course]][[this_week]] <- input$expectation
    
    if (is.null(reflection_saved$data[[course]])) reflection_saved$data[[course]] <- list()
    reflection_saved$data[[course]][[this_week]] <- TRUE
    
    feedback <- ""
    last_week_expectation <- expectation_ratings$data[[course]][[last_week]]
    
    # --- Main logic based on satisfaction and expectation ---
    # 1. 🎯 Categorized Personalized Feedback Based on Satisfaction + Expectation
    if (input$satisfaction <= 4 && input$expectation <= 4) {
      feedback <- "🚨 It seems you're struggling and not feeling hopeful. What small change could help you feel more in control next week?"
    } else if (input$satisfaction <= 4 && input$expectation >= 7) {
      feedback <- "😟 You're not satisfied, but you're hopeful things can improve. That's a strong mindset! What will you try differently?"
    } else if (input$satisfaction >= 5 && input$satisfaction <= 6 && input$expectation <= input$satisfaction) {
      feedback <- "😐 You're maintaining a steady pace, but not seeing much progress. What’s holding you back, and how can you break through?"
    } else if (input$satisfaction <= 6 && input$expectation > input$satisfaction) {
      feedback <- "🌱 You're looking to grow! Think about what support, habits, or focus can help you improve next week."
    } else if (input$satisfaction >= 7 && input$satisfaction <= 8 && input$expectation == input$satisfaction) {
      feedback <- "👍 You're on a steady path and feeling good about it. Keep doing what works!"
    } else if (input$satisfaction >= 9 && input$expectation >= 9) {
      feedback <- "🚀 You're flying high! Excellent work and strong confidence. Reflect on what’s driving your success so you can repeat it."
    } else {
      feedback <- "🧭 You're somewhere in between. Take a moment to reflect on what's working and where you can grow."
    }
    
    # 2. ⏮️ Compare to Last Week’s Expectation
    if (!is.null(last_week_expectation)) {
      diff <- input$satisfaction - last_week_expectation
      if (diff > 0 && input$satisfaction < 6) {
        feedback <- paste(feedback, "<br><br>📈 You exceeded your expectations, even if satisfaction is still low, that's a step forward. What helped this week?")
      } else if (diff > 0 && input$satisfaction >= 6) {
        feedback <- paste(feedback, "<br><br>🎉 You surpassed your expectations! Great progress, reflect on what contributed to that.")
      } else if (diff == 0 && input$satisfaction < 6) {
        feedback <- paste(feedback, "<br><br>⚖️ You matched your expectations, but satisfaction remains low. What could lift your experience next week?")
      } else if (diff == 0 && input$satisfaction >= 6) {
        feedback <- paste(feedback, "<br><br>🧠 You met your expectations, a great sign of self-awareness. Stay consistent!")
      } else if (diff < 0 && input$satisfaction < 6) {
        feedback <- paste(feedback, "<br><br>🚧 You fell short of your expectations and aren't feeling great. What barriers came up? How can you respond differently next time?")
      } else if (diff < 0 && input$satisfaction >= 6) {
        feedback <- paste(feedback, "<br><br>🤔 You didn’t meet your goals, but you're still satisfied. Maybe you set your expectations too high? Adjusting goals is also growth.")
      }
    }
    
    # 3. 🔮 Future Outlook Based on Expectation
    if (input$expectation > input$satisfaction) {
      feedback <- paste(feedback, "<br><br>📈 You're aiming higher next week, awesome! What’s your action plan to reach that goal?")
    } else if (input$expectation < input$satisfaction) {
      feedback <- paste(feedback, "<br><br>📉 You’re expecting a dip next week, is something changing? Reflect on how to adapt while keeping your momentum.")
    } else {
      feedback <- paste(feedback, "<br><br>🔁 You’re aiming for consistency, sounds like you're building a rhythm. What will help you maintain that?")
    }
    
    if (is.null(satisfaction_feedback_store$data[[course]])) {
      satisfaction_feedback_store$data[[course]] <- list()
    }
    satisfaction_feedback_store$data[[course]][[this_week]] <- feedback
    output$satisfaction_feedback <- renderUI({HTML(as.character(feedback))})
    
    saveAllUserData(
      user_id = user_id(),
      course_id = course,
      week = this_week,
      satisfaction = input$satisfaction,
      expectation = input$expectation,
      feedback = feedback
    )
  })
  
  observeEvent(input$refresh_rating, ignoreInit = TRUE, {
    show_optional_text(FALSE)
    course <- selectedCourse()
    week <- as.character(weekNumber())
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
  
  observeEvent(selectedCourse(), {
    course <- selectedCourse()
    week <- weekNumber()
    if (!is.null(satisfaction_feedback_store$data[[course]]) &&
        !is.null(satisfaction_feedback_store$data[[course]][[week]])) {
      output$satisfaction_feedback <- renderText({
        satisfaction_feedback_store$data[[course]][[week]]
      })
    } else {
      output$satisfaction_feedback <- renderText({ "" })
    }
  })
  
  # helper to write theme into the same JSON
  saveUserTheme <- function(user, theme) {
    path <- "all_users_data.json"
    data <- list()
    if (file.exists(path)) data <- jsonlite::read_json(path, simplifyVector = FALSE)
    if (is.null(data[[user]])) data[[user]] <- list()
    data[[user]]$theme <- theme
    jsonlite::write_json(data, path, pretty = TRUE, auto_unbox = TRUE)
  }
  
  # when "Apply" clicked, swap the CSS class and persist
  observeEvent(input$apply_theme, {
    req(credentials$logged_in, input$theme_choice)
    newt <- input$theme_choice
    newt_cap <- paste0(toupper(substr(newt, 1, 1)), substr(newt, 2, nchar(newt)))
    showNotification(paste("Applying theme:", newt_cap), type = "message")
    credentials$theme <- newt
    saveUserTheme(credentials$user_id, newt)
    
    # write to your JSON prefs
    shinyjs::removeClass(
      class    = c("theme-default","theme-light","theme-dark","theme-ocean","theme-solarized"),
      selector = "html"
    )
    # then apply the newly chosen theme
    shinyjs::addClass(
      class    = paste0("theme-", newt),
      selector = "html"
    )
  })
  
  #########################
  # Dynamic Outputs
  #########################
  
  # Dynamic user info
  output$selectedUserId <- renderText({
    user_id_value <- user_id()
    paste("Welcome", user_id_value, " !")
  })
  
  output$activityTracker <- renderUI({
    req(user_id(), credentials$date)
    
    # 1. Compute block & timestamps
    course      <- as.character(course_id())
    user        <- user_id()
    week        <- weekNumber()
    block_start <- (((week - 1) %/% 2) * 2) + 1
    block_end   <- block_start + 1
    block_str   <- as.character(block_start)
    
    start_block <- COURSE_START_DATE + (block_start - 1) * 7
    end_block   <- COURSE_START_DATE +   block_end   * 7
    start_ts    <- paste0(start_block, " 00:00:00.000")
    end_ts      <- paste0(end_block,   " 00:00:00.000")
    
    # 2. One query for quiz weeks in this block
    quiz_sql <- sprintf("
    SELECT floor(extract(day 
      from finished_at_anonymous - DATE '%s') / 7) + 1 AS week
    FROM sandbox_la_conijn_cbl.silver_canvas_quiz_submissions
    WHERE user_id = '%s'
      AND workflow_state IN ('pending_review','complete')
      AND finished_at_anonymous >= '%s'
      AND finished_at_anonymous <  '%s'
    GROUP BY week;",
                        COURSE_START_DATE, user, start_ts, end_ts
    )
    quiz_weeks <- tryCatch(dbGetQuery(sc, quiz_sql)$week, error = function(e) integer())
    
    # 3. One query for assignment weeks in this block
    assign_sql <- sprintf("
    SELECT floor(extract(day 
      from submitted_at_anonymous - DATE '%s') / 7) + 1 AS week
    FROM sandbox_la_conijn_cbl.silver_canvas_submissions
    WHERE user_id = '%s'
      AND workflow_state IN ('pending_review','graded','submitted')
      AND submitted_at_anonymous >= '%s'
      AND submitted_at_anonymous <  '%s'
    GROUP BY week;",
                          COURSE_START_DATE, user, start_ts, end_ts
    )
    assign_weeks <- tryCatch(dbGetQuery(sc, assign_sql)$week, error = function(e) integer())
    
    # 4. Build a flat list of six activities (quiz, assignment, reflect) for each week
    weeks <- c(block_start, block_end)
    all_activities <- lapply(weeks, function(w) {
      list(
        list(name = sprintf("Submit Week %d Quiz",      w), done = w %in% quiz_weeks),
        list(name = sprintf("Submit Week %d Assignment", w), done = w %in% assign_weeks),
        list(name = sprintf("Reflect on Week %d",        w), done = isTRUE(reflection_saved$data[[course]][[as.character(w)]]))
      )
    }) %>% unlist(recursive = FALSE)
    
    total     <- length(all_activities)
    completed <- sum(vapply(all_activities, `[[`, logical(1), "done"))
    percent   <- if (total > 0) round(completed / total * 100) else 0
    
    # 5. Only write JSON when the “done” flag for this block actually flips
    prev_done <- activity_completion$data[[course]][[block_str]] %||% FALSE
    if (percent == 100 && !prev_done) {
      activity_completion$data[[course]][[block_str]] <- TRUE
      saveActivityCompletion(user, course, block_str, TRUE)
    } else if (percent < 100 && prev_done) {
      activity_completion$data[[course]][[block_str]] <- FALSE
      # Remove it from disk, just like your original code
      fp <- "activity_completion_data.json"
      if (file.exists(fp)) {
        dat <- jsonlite::read_json(fp, simplifyVector = FALSE)
        if (!is.null(dat[[user]][[course]][[block_str]])) {
          dat[[user]][[course]][[block_str]] <- NULL
          if (length(dat[[user]][[course]]) == 0) dat[[user]][[course]] <- NULL
          if (length(dat[[user]])          == 0) dat[[user]]          <- NULL
          jsonlite::write_json(dat, fp, pretty = TRUE, auto_unbox = TRUE)
        }
      }
    }
    
    # 6. Choose the feedback message
    feedback_text <- if (completed == total && total > 0) {
      "Great job! You've completed all activities for this block. 🎉"
    } else if (completed > 0) {
      "Excellent! You've checked another activity off the list. Keep the momentum going! 💪"
    } else {
      "It looks like you're just getting started! For a good overview, perhaps look at the activity breakdown to see where you can best invest your time. 🗺️"
    }
    
    # 7. Render the HTML via tags (faster & safer than paste0)
    tagList(
      tags$ul(class = "list-group mb-3",
              lapply(all_activities, function(act) {
                cls  <- if (act$done) "text-success" else "text-muted"
                icon <- if (act$done) "✅" else "⬜"
                tags$li(class = paste("list-group-item d-flex justify-content-between align-items-center", cls),
                        HTML(paste(icon, act$name)))
              })
      ),
      tags$div(class = "progress",
               tags$div(class = "progress-bar bg-success", role = "progressbar",
                        style               = sprintf("width: %d%%;", percent),
                        `aria-valuenow`     = percent,
                        `aria-valuemin`     = 0,
                        `aria-valuemax`     = 100,
                        sprintf("%d%%", percent)
               )
      ),
      tags$div(class = "alert alert-info mt-3", role = "alert", feedback_text)
    )
  })
  
  # ----------------------------
  # Dynamic Metric Selector UI
  # ----------------------------
  output$metric_ui <- renderUI({
    req(input$data_type)
    choices <- switch(input$data_type,
                      "Quizzes" = c(
                        "Submissions"        = "Submissions",
                        "Average Score"  = "Avg Quiz Score",
                        "Min & Max Scores"      = "Max Quiz Score"
                      ),
                      "Files" = c(
                        "File Accesses"         = "File Accesses",
                        "Unique Files Accessed" = "Unique Files Accessed"
                      ),
                      "Discussions" = c(
                        "Number of Posts" = "Number of Posts",
                        "Replies Made"    = "Replies Made"
                      ),
                      "Assignments" = c(
                        "Submissions"            = "Submissions",
                        "Average Score"      = "Avg Assignment Score",
                        "Min & Max Scores"          = "Max Assignment Score",
                        "Submission Attempts"    = "Submission Attempts"
                      )
    )
    selectInput("metric", "Select Metric",
                choices = choices,
                selected = names(choices)[1]
    )
  })
  
  # ----------------------------------------
  # Reactive Data Fetcher for Activity Data
  # ----------------------------------------
  activityData <- eventReactive(
    list(input$data_type, input$metric, input$selected_week, selectedCourse()),
    {
      req(input$data_type, input$metric, input$selected_week)
      
      # compute 7-day window
      week_idx   <- as.integer(input$selected_week)
      start_date <- COURSE_START_DATE + (week_idx - 1) * 7
      end_date   <- start_date + 6L
      
      start_ts <- paste0(start_date, " 00:00:00")
      end_ts   <- paste0(end_date + 1,  " 00:00:00")
      
      user   <- credentials$user_id
      course <- course_id()
      
      # simple‐count metrics config
      config <- switch(input$data_type,
                       "Quizzes" = {
                         if (input$metric == "Submissions") {
                           list(tbl="silver_canvas_quiz_submissions",
                                date_col="finished_at_anonymous",
                                expr="COUNT(*) AS value",
                                ylab="Submissions")
                         } else NULL
                       },
                       "Files" = {
                         if (input$metric == "File Accesses") {
                           list(tbl="silver_canvas_web_logs",
                                date_col="timestamp",
                                expr="COUNT(*) AS value",
                                ylab="File Accesses")
                         } else {
                           list(tbl="silver_canvas_web_logs",
                                date_col="timestamp",
                                expr="COUNT(DISTINCT attachment_id) AS value",
                                ylab="Unique Files Accessed")
                         }
                       },
                       "Discussions" = {
                         if (input$metric == "Number of Posts") {
                           list(tbl="silver_canvas_discussion_entries",
                                date_col="created_at_anonymous",
                                expr="COUNT(*) AS value",
                                ylab="Posts")
                         } else {
                           list(tbl="silver_canvas_discussion_entries",
                                date_col="created_at_anonymous",
                                expr="COUNT(*) FILTER (WHERE parent_entry_id IS NOT NULL) AS value",
                                ylab="Replies Made")
                         }
                       },
                       "Assignments" = {
                         if (input$metric == "Submissions") {
                           list(tbl="silver_canvas_submissions",
                                date_col="submitted_at_anonymous",
                                expr="COUNT(*) AS value",
                                ylab="Submissions")
                         } else if (input$metric == "Submission Attempts") {
                           list(tbl="silver_canvas_submissions",
                                date_col="submitted_at_anonymous",
                                expr="COALESCE(SUM(attempt),0) AS value",
                                ylab="Submission Attempts")
                         } else NULL
                       }
      )
      
      # Unified raw-score query for Avg/Min&Max branches (Assignments & Quizzes)
      if (input$data_type == "Quizzes" &&
          input$metric %in% c("Avg Quiz Score", "Max Quiz Score")) {
        
        # Always fetch raw scores per submission for this week
        query <- sprintf(
          "SELECT CAST(s.finished_at_anonymous AS DATE) AS day,
                (s.score_anonymous / q.points_possible * 100) AS value
         FROM sandbox_la_conijn_cbl.silver_canvas_quiz_submissions AS s
         LEFT JOIN sandbox_la_conijn_cbl.silver_canvas_quizzes AS q
           ON s.quiz_id = q.id
         WHERE s.user_id = '%s'
           AND s.finished_at_anonymous BETWEEN '%s' AND '%s'
           AND s.course_id = %s;",
          user, start_ts, end_ts, course
        )
        df_raw <- tryCatch(dbGetQuery(sc, query),
                           error = function(e)
                             data.frame(day=as.Date(character(0)), value=numeric(0)))
        
        # Aggregate to daily mean for plot and details
        all_days <- data.frame(day=seq.Date(start_date, end_date, by="day"))
        # For each day: average score (if any), 0 otherwise
        daily_means <- tapply(df_raw$value, df_raw$day, mean)
        df <- merge(all_days, data.frame(day=as.Date(names(daily_means)), value=as.numeric(daily_means)), by="day", all.x=TRUE)
        df$value[is.na(df$value)] <- 0
        
        ylab <- if (input$metric == "Avg Quiz Score")
          "Average Quiz Score (%)" else "Max Quiz Score (%)"
        # Attach the raw scores (for correct min/max display)
        attr(df, "raw_values") <- df_raw
        
      } else if (input$data_type == "Assignments" &&
                 input$metric %in% c("Avg Assignment Score", "Max Assignment Score")) {
        
        # Always fetch raw scores per submission for this week
        query <- sprintf(
          "SELECT CAST(s.submitted_at_anonymous AS DATE) AS day,
                (s.score_anonymous / a.points_possible * 100) AS value
         FROM sandbox_la_conijn_cbl.silver_canvas_submissions AS s
         LEFT JOIN sandbox_la_conijn_cbl.silver_canvas_assignments AS a
           ON s.assignment_id = a.id
         WHERE s.user_id = '%s'
           AND s.submitted_at_anonymous BETWEEN '%s' AND '%s'
           AND s.course_id = %s;",
          user, start_ts, end_ts, course
        )
        df_raw <- tryCatch(dbGetQuery(sc, query),
                           error = function(e)
                             data.frame(day=as.Date(character(0)), value=numeric(0)))
        
        # Aggregate to daily mean for plot and details
        all_days <- data.frame(day=seq.Date(start_date, end_date, by="day"))
        daily_means <- tapply(df_raw$value, df_raw$day, mean)
        df <- merge(all_days, data.frame(day=as.Date(names(daily_means)), value=as.numeric(daily_means)), by="day", all.x=TRUE)
        df$value[is.na(df$value)] <- 0
        
        ylab <- if (input$metric == "Avg Assignment Score")
          "Average Assignment Score (%)" else "Max Assignment Score (%)"
        attr(df, "raw_values") <- df_raw
        
      } else {
        # default single‐table metrics (counts, etc.)
        df_config <- config
        df <- tryCatch(
          dbGetQuery(sc, sprintf(
            "SELECT CAST(%s AS DATE) AS day, %s
           FROM sandbox_la_conijn_cbl.%s
           WHERE user_id = '%s'
             AND %s BETWEEN '%s' AND '%s'
             AND course_id = %s
           GROUP BY day
           ORDER BY day;",
            df_config$date_col, df_config$expr, df_config$tbl,
            user, df_config$date_col, start_ts, end_ts, course
          )),
          error = function(e)
            data.frame(day=seq.Date(start_date, end_date, by="day"), value=0)
        )
        ylab <- df_config$ylab
        attr(df, "raw_values") <- NULL
      }
      
      # ensure full week (already handled above for Avgs)
      # Merge for others just in case
      all_days <- data.frame(day=seq.Date(start_date, end_date, by="day"))
      df       <- merge(all_days, df, by="day", all.x=TRUE)
      df$value[is.na(df$value)] <- 0
      
      list(df = df, ylab = ylab, raw = attr(df, "raw_values"))
    }
  )
  
  # ———————————————————————————————————————
  # 1) Plotly renderer (only when not avg/max)
  # ———————————————————————————————————————
  output$activityPlot <- renderPlotly({
    req(input$data_type, input$metric, input$selected_week)
    if (input$metric %in% c("Avg Quiz Score","Avg Assignment Score",
                            "Max Quiz Score","Max Assignment Score")) {
      return(plotly_empty(type = "scatter"))
    }
    
    dat  <- activityData()
    df   <- dat$df
    ylab <- dat$ylab
    wk   <- as.integer(input$selected_week)
    
    # Theme colors (unchanged) …
    th <- credentials$theme
    theme_colors <- switch(th,
                           light     = list(bg="#FFFFFF", txt="#2C3E50", accent="#2980B9"),
                           dark      = list(bg="#1E1E1E", txt="#FFFFFF", accent="#E67E22"),
                           ocean     = list(bg="#E0F7FA", txt="#004D40", accent="#00796B"),
                           solarized = list(bg="#FDF6E3", txt="#073642", accent="#B58900"),
                           list(bg="#F0F2F5", txt="#2C3E50", accent="#2980B9")
    )
    
    if (all(df$value == 0)) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(list(
              text = "No activity during this week",
              showarrow = FALSE,
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5,
              font = list(family = "system-ui", size = 16, color = theme_colors$txt)
            )),
            paper_bgcolor = theme_colors$bg,
            plot_bgcolor  = theme_colors$bg,
            margin = list(t = 120, b = 80, l = 80, r = 40),
            font   = list(family = "system-ui", color = theme_colors$txt)
          )
      )
    }
    
    plot_ly(
      df, x = ~day, y = ~value, type = "bar",
      marker = list(color = theme_colors$accent),
      hovertemplate = paste0("%{x|%b %d, %Y}<br>%{y:,} ", ylab, "<extra></extra>")
    ) %>%
      layout(
        title = list(
          text    = sprintf("Week %d<br>%s per Day", wk, ylab),
          x       = 0.5,
          xanchor = "center",
          pad     = list(b = 15),
          font    = list(family = "system-ui", size = 20, color = theme_colors$txt)
        ),
        margin = list(t = 120, b = 80, l = 80, r = 40),
        xaxis  = list(
          title      = list(text = "Date", font = list(family = "system-ui", size = 14, color = theme_colors$txt)),
          tickformat = "%b %d", dtick = "D1",
          tickfont   = list(family = "system-ui", size = 12, color = theme_colors$txt)
        ),
        yaxis = list(
          title     = list(text = ylab, font = list(family = "system-ui", size = 14, color = theme_colors$txt)),
          tickfont  = list(family = "system-ui", size = 12, color = theme_colors$txt),
          rangemode = "tozero"
          
        ),
        plot_bgcolor  = theme_colors$bg,
        paper_bgcolor = theme_colors$bg,
        font          = list(family = "system-ui", color = theme_colors$txt)
      ) %>%
      config(responsive = TRUE)
  })
  
  # ———————————————————————————————————————
  # Unified UI: either the chart or a one‐line summary
  # ———————————————————————————————————————
  output$activity_view <- renderUI({
    req(input$data_type, input$metric, input$selected_week)
    
    is_avg <- input$metric %in% c("Avg Quiz Score","Avg Assignment Score")
    is_max <- input$metric %in% c("Max Quiz Score","Max Assignment Score")
    
    if (is_avg || is_max) {
      df  <- activityData()$df
      df_nz <- df[df$value > 0, ]
      
      if (nrow(df_nz) == 0) {
        txt <- "No submissions this week."
        details <- NULL
      } else {
        if (is_avg) {
          avg <- round(mean(df_nz$value), 1)
          avg_cls <- if (avg < 60) "bg-danger" else if (avg < 80) "bg-warning text-dark" else "bg-success"
          txt <- HTML(paste0("<span class='badge ", avg_cls, " rounded-pill fs-5'>", avg, "%</span>"))
        } else {
          maxv <- round(max(df_nz$value), 1)
          minv <- round(min(df_nz$value), 1)
          max_cls <- if (maxv < 60) "bg-danger" else if (maxv < 80) "bg-warning text-dark" else "bg-success"
          min_cls <- if (minv < 60) "bg-danger" else if (minv < 80) "bg-warning text-dark" else "bg-success"
          txt <- HTML(paste0(
            "<br/>Minimum: ",
            "<span class='badge ", min_cls, " rounded-pill fs-5'>", minv, "%</span>",
            "<br/>Maximum: ",
            "<span class='badge ", max_cls, " rounded-pill fs-5'>", maxv, "%</span>"
          ))
        }
        
        details <- div(class = "mt-4",
                       div("Daily scores:", class = "mb-3 fw-bold fs-5"),
                       div(class = "list-group",
                           lapply(seq_len(nrow(df_nz)), function(i) {
                             score <- round(df_nz$value[i], 1)
                             score_cls <- if (score < 60) "bg-danger"
                             else if (score < 80) "bg-warning text-dark"
                             else "bg-success"
                             div(class = "list-group-item d-flex justify-content-between align-items-center",
                                 span(format(df_nz$day[i], "%b %d"), class = "fs-5"),
                                 span(paste0(score, "%"), class = paste("badge", score_cls, "rounded-pill fs-5"))
                             )
                           })
                       )
        )
      }
      
      title_text <- if (is_avg) {
        h4(HTML(paste0("Week ", input$selected_week, "<br/>Average Score"))) 
      } else { 
        h4(HTML(paste0("Week ", input$selected_week, "<br/>Min & Max Scores")))
      }
      
      tagList(
        div(class = "weekly-summary text-center p-4",
            title_text,
            span(style = "font-size:2rem; font-weight:600;", HTML(txt))
        ),
        details
      )
    } else {
      withSpinner(
        plotlyOutput("activityPlot", height="400px", width="100%"),
        type = 6, color = "#2980B9"
      )
    }
  })
  
  # Single renderUI for satisfaction reflection card
  output$reflections_card <- renderUI({
    req(credentials$logged_in)
    
    course <- selectedCourse()
    wk     <- as.character(weekNumber())
    saved  <- isTRUE(reflection_saved$data[[course]][[wk]])
    
    tags$script(HTML("
    $(function() {
      ['satisfaction', 'expectation'].forEach(function(id) {
        var $inp = $('#' + id);

        function updateLabel() {
          var single = $inp.closest('.slider-with-value').find('.irs-single');
          if (single.length) {
            single.text($inp.val());
          }
        }

        $inp.on('input change', updateLabel);
        setTimeout(updateLabel, 100); // wait for ion.rangeSlider to init
      });
    });
  "))
    
    # Motivation Reflection
    motivation_reflection <- div(class = "mb-3 pb-2",
                                 div(class = "fw-semibold",
                                     "Fill out your reflection for this week to better understand your progress and earn your badges.")
    )
    
    # Satisfaction
    sat_ui <- if (saved) {
      div(class = "mb-3 pb-2",
          div(class = "fw-bold mb-1", "Self-Reported Satisfaction Score:"),
          div(class = "text-muted", satisfaction_ratings$data[[course]][[wk]])
      )
    } else {
      div(class = "mb-3 pb-2 slider-with-value",
          sliderInput(
            "satisfaction",
            "How satisfied are you with your learning progress this week?",
            min = 1, max = 10, value = 5, width = "100%", ticks = FALSE
          )
      )
    }
    
    # Expectation
    exp_ui <- if (saved) {
      div(class = "mb-3 pb-2",
          div(class = "fw-bold mb-1", "Expected Progress Satisfaction:"),
          div(class = "text-muted", expectation_ratings$data[[course]][[wk]])
      )
    } else {
      div(class = "mb-3 pb-2 slider-with-value",
          sliderInput(
            "expectation",
            "How satisfied do you expect to be next week?",
            min = 1, max = 10, value = 5, width = "100%", ticks = FALSE
          )
      )
    }
    
    # Save/Update button
    btn_ui <- div(class = "d-grid gap-2 mb-3 pb-2",
                  if (!saved) {
                    actionButton("save_rating", "Save Reflection", class = "btn btn-primary")
                  } else {
                    actionButton("refresh_rating", "Update Reflection", class = "btn btn-outline-primary")
                  }
    )
    
    # Feedback
    fb_ui <- div(class = "mb-3 pb-2", uiOutput("satisfaction_feedback"))
    
    # Streak badge
    streak_ui <- {
      submitted <- sort(as.numeric(names(satisfaction_ratings$data[[course]])))
      submitted <- submitted[submitted <= weekNumber()]
      streak <- 0L
      for (w in weekNumber():1) {
        if (w %in% submitted) streak <- streak + 1L else break
      }
      if (streak >= 2L) {
        div(class = "alert alert-warning mb-3 pb-2",
            HTML(sprintf("🏅 <strong>%d-Week Streak!</strong> Reflection badge for %d weeks.", streak, streak))
        )
      } else NULL
    }
    
    # Optional free-text reflection
    optional_text_ui <- {
      if (show_optional_text()) {
        div(class = "mb-3",
            tags$label("Optional: You can elaborate on your reflection here if you'd like."),
            textAreaInput("optional_reflection_text", label = NULL, rows = 4, width = "100%")
        )
      }
    }
    
    tagList(
      motivation_reflection,
      sat_ui,
      exp_ui,
      btn_ui,
      fb_ui,
      streak_ui,
      optional_text_ui
    )
  })
  
  # Badges display
  output$badges <- renderUI({
    req(user_id(), course_id())
    
    course <- selectedCourse()
    reflections <- reflection_saved$data[[course]]
    activity_blocks <- activity_completion$data[[course]]
    
    reflection_count <- if (!is.null(reflections)) {
      sum(unlist(reflections), na.rm = TRUE)
    } else { 0 }
    
    completed_block_count <- if (!is.null(activity_blocks)) {
      sum(unlist(activity_blocks), na.rm = TRUE)
    } else { 0 }
    
    badges <- list(
      list(threshold = 1, unlocked_img = "img/ROOKIE.png", unlocked_tooltip = "Your first reflection - Nice start!", locked_tooltip = "Reflect for the first time."),
      list(threshold = 4, unlocked_img = "img/CONSISTENT.png", unlocked_tooltip = "4 weeks of reflections - Keep reflecting!", locked_tooltip = "Reflect for 4 weeks."),
      list(threshold = 8, unlocked_img = "img/MASTER.png", unlocked_tooltip = "8 weeks of reflecting?! – Good job!", locked_tooltip = "Reflect for 8 weeks."),
      list(id = "selfaware", threshold = 1, unlocked_img = "img/SELF-AWARE.png", unlocked_tooltip = "You have completed all your activities - Nice start!", locked_tooltip = "Complete the activity tracker for the first time."),
      list(id = "insightchampion", threshold = 2, unlocked_img = "img/INSIGHT.png", unlocked_tooltip = "You have finished your second block of activities - Very nice!", locked_tooltip = "Complete the activity tracker for the second time."),
      list(id = "kingofwisdom", threshold = 4, unlocked_img = "img/KING.png", unlocked_tooltip = "You have complete all the activities for this course - Amazing!", locked_tooltip = "Complete all the activities for the entire course.")
    )
    
    badge_html <- "<p><strong>Your Reflection Badges, hover over them to see how you can earn them</strong></p><div class='badge-container'>"
    for (b in badges) {
      unlocked <- if (!is.null(b$id)) {
        completed_block_count >= b$threshold
      } else {
        reflection_count >= b$threshold
      }
      
      if (unlocked) {
        badge_html <- paste0(badge_html, "<span class='badge-item' data-tooltip='", b$unlocked_tooltip, "'><img src='", b$unlocked_img, "' height='90px'/></span>")
      } else {
        badge_html <- paste0(badge_html, "<span class='badge-item' data-tooltip='", b$locked_tooltip, "'><img src='img/QUESTION.png' height='90px'/></span>")
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
    ) %>% paste("- Self-Regulated Learning Reflection Dashboard")
  })
  
  output$current_week_display <- renderText({
    paste("Week", weekNumber())
  })
  
  output$motivation_reflection <- renderText({
    paste("Fill out your reflection for this week to better understand your progress and earn your badges.")
  })
}

shinyApp(ui, server)
