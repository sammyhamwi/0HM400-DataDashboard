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
      selectInput(
        "week_input", "Select Week",
        choices = setNames(1:10, paste("Week", 1:10)),
        selected = 1
      ),
      actionButton("login_button", "Login",
                   class = "btn btn-primary mt-3")
    ),
    
    # Data Caching Footer (shown when not logged in)
    div(
      id = "cache_footer",
      class = "cache-footer mt-4",
      div(class = "card",
          div(class = "card-body p-3",
              # Toggle button for cache interface
              div(class = "text-center",
                  actionButton("toggle_cache_interface", 
                              HTML("🛠️ Developer Tools <i class='fas fa-chevron-down' id='cache_chevron'></i>"),
                              class = "btn btn-link btn-sm text-muted p-0",
                              style = "text-decoration: none; border: none;")
              ),
              # Collapsible cache interface content (hidden by default)
              hidden(
                div(id = "cache_interface_content",
                    div(class = "text-center mb-3 mt-3",
                        h6("Speed Up Loading", class = "text-muted mb-1"),
                        p("Cache user data locally for faster performance", class = "small text-muted mb-0")
                    ),
                    div(class = "row",
                        column(8,
                               textInput("cache_user_id", NULL, 
                                        placeholder = "User ID to cache",
                                        width = "100%")
                        ),
                        column(4,
                               actionButton("download_data", "Cache Data",
                                           class = "btn btn-outline-success btn-sm w-100")
                        )
                    ),
                    div(id = "cache_status", class = "mt-2"),
                    div(id = "cache_progress", class = "mt-2", style = "display: none;",
                        div(class = "progress", style = "height: 20px;",
                            div(class = "progress-bar progress-bar-striped progress-bar-animated",
                                role = "progressbar", style = "width: 0%", id = "progress_bar", "0%")
                        ),
                        div(id = "progress_message", class = "text-center text-muted small mt-2", "Starting...")
                    ),
                    div(class = "mt-3 pt-2 border-top",
                        div(class = "d-flex justify-content-between align-items-center mb-2",
                            h6("📁 Cached Users", class = "mb-0 text-muted"),
                            actionButton("refresh_cache_list", "Refresh", 
                                        class = "btn btn-outline-secondary btn-sm")
                        ),
                        uiOutput("cached_users_list")
                    )
                )
              )
          )
      )
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
            width = 9, id = "main_col", class = "sidebar-expanded",
            div(class = "main-content",
                
                # Welcome card with badges
                div(class = "card p-3 mb-4",
                    fluidRow(
                      column(
                        width = 8,
                        h4(textOutput("dashboardTitle", inline = TRUE),
                           class = "mb-2")
                      ),
                      column(
                        width = 4,
                        div(class = "text-end", textOutput("selectedUserId"))
                      )
                    ),
                    # Badges section integrated
                    div(class = "badges-strip mt-3 pt-3",
                        div(class = "text-center mb-2",
                            h6("🏆 Your Badges", class = "mb-1 text-muted"),
                            span(class = "small text-muted", style = "font-style: italic;", "Hover over badges for details")
                        ),
                        withSpinner(
                          uiOutput("badges_horizontal"),
                          type = 6,
                          color = "#2980B9"
                        )
                    )
                ),
                
                # Two columns: Tracker/Reflections & Weekly Overview  
                fluidRow(
                  # Left column - Activity & Reflections
                  column(
                    width = 6,
                    # Activity Tracker
                    div(class = "card p-3 mb-3",
                        div(class = "section-title","Activity Tracker"),
                        withSpinner(
                          uiOutput("activityTracker"),
                          type = 6,
                          color = "#2980B9"
                        )
                    ),
                    # Reflections
                    div(class = "card p-3 mb-3",
                        div(class = "section-title","Reflections"),
                        withSpinner(
                          uiOutput("reflections_card"),
                          type = 6,
                          color = "#2980B9"
                        )
                    )
                  ),
                  # Right column - Weekly Overview (more compact)
                  column(
                    width = 6,
                    # Weekly Overview Module (streamlined)
                    div(class = "card p-3 mb-3",
                        div(class = "section-title","Weekly Activity Overview"),
                        
                        # Controls Section (more compact)
                        div(class = "controls-section mb-3",
                            fluidRow(
                              column(4,
                                     selectInput("selected_week", "Week", 
                                                 choices = setNames(1:10, paste("Week", 1:10)), 
                                                 selected = 1, 
                                                 width = "100%") 
                              ),
                              column(4,
                                     selectInput("data_type","Activity",
                                                 choices = c("Web Activities","Assignments","Quizzes","Files"),
                                                 selected = "Web Activities")
                              ),
                              column(4,
                                     uiOutput("metric_ui")
                              )
                            )
                        ),
                        
                        # Always-visible Weekly Summary (more compact)
                        div(class = "weekly-summary-section mb-3",
                            div(class = "d-flex align-items-center mb-2",
                                h6("📊 Weekly Summary", class = "mb-0 text-primary"),
                                span(class = "ms-auto badge bg-light text-dark", textOutput("current_week_display", inline = TRUE))
                            ),
                            withSpinner(
                              uiOutput("weekly_summary_card"),
                              type = 6,
                              color = "#2980B9"
                            )
                        ),
                        
                        # Dynamic Content Section (more compact)
                        div(class = "detailed-view-section",
                            div(class = "d-flex align-items-center mb-2",
                                h6("📈 Detailed View", class = "mb-0 text-secondary"),
                                span(class = "ms-auto small text-muted", textOutput("metric_description", inline = TRUE))
                            ),
                            withSpinner(
                              uiOutput("detailed_activity_view"),
                              type = 6,
                              color = "#2980B9"
                            )
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
  
  #########################
  # Data Caching Functions
  #########################
  
  # Function to get cache file path for a user
  getCacheFilePath <- function(user_id) {
    # Sanitize user_id for filename - replace invalid characters
    safe_user_id <- gsub("[^A-Za-z0-9._-]", "_", user_id)
    paste0("cached_data_", safe_user_id, ".json")
  }
  
  # Function to check if cached data exists and is recent (within 7 days)
  hasCachedData <- function(user_id) {
    cache_file <- getCacheFilePath(user_id)
    if (!file.exists(cache_file)) return(FALSE)
    
    # Check if file is recent (within 7 days)
    file_age <- difftime(Sys.time(), file.mtime(cache_file), units = "days")
    return(file_age < 7)
  }
  
  # Function to download all user data from database
  downloadUserData <- function(user_id, progress_callback = NULL) {
    tryCatch({
      if (!is.null(progress_callback)) progress_callback(5, "Checking write permissions...")
      
      # Test if we can write to the current directory
      test_file <- "test_write_permission.tmp"
      write_test <- tryCatch({
        writeLines("test", test_file)
        file.remove(test_file)
        TRUE
      }, error = function(e) {
        if (!is.null(progress_callback)) progress_callback(-1, paste("Cannot write to directory:", getwd(), "-", e$message))
        return(FALSE)
      })
      
      if (!write_test) {
        cat("Write test failed!\n")
        return(FALSE)
      }
      
      cat("Write test passed\n")
      if (!is.null(progress_callback)) progress_callback(7, "Checking database connection...")
      
      # Check if database connection exists
      cat("Checking if 'sc' exists...\n")
      if (!exists("sc") || is.null(sc)) {
        cat("Database connection 'sc' not found!\n")
        if (!is.null(progress_callback)) progress_callback(-1, "Database connection 'sc' not found")
        return(FALSE)
      }
      cat("Database connection 'sc' found\n")
      
      # Test database connection
      test_result <- tryCatch({
        dbGetQuery(sc, "SELECT 1 as test")
        TRUE
      }, error = function(e) {
        if (!is.null(progress_callback)) progress_callback(-1, paste("Database connection failed:", e$message))
        return(FALSE)
      })
      
      if (!test_result) return(FALSE)
      
      if (!is.null(progress_callback)) progress_callback(10, "Fetching user courses...")
      
      # 1. Get user courses
      courses_query <- sprintf("
        SELECT DISTINCT course_id
        FROM sandbox_la_conijn_cbl.silver_canvas_enrollments
        WHERE user_id = '%s'
      ", user_id)
      
      user_courses_result <- tryCatch({
        dbGetQuery(sc, courses_query)
      }, error = function(e) {
        if (!is.null(progress_callback)) progress_callback(-1, paste("Error fetching courses:", e$message))
        return(NULL)
      })
      
      if (is.null(user_courses_result)) return(FALSE)
      user_courses <- user_courses_result$course_id
      
      if (length(user_courses) == 0) {
        cat("No courses found for user:", user_id, "\n")
        if (!is.null(progress_callback)) progress_callback(-1, "No courses found for this user")
        return(FALSE)
      }
      cat("Found", length(user_courses), "courses for user\n")
      
      if (!is.null(progress_callback)) progress_callback(20, "Fetching quiz data...")
      
      # 2. Get all quiz submissions for this user
      quiz_query <- sprintf("
        SELECT s.course_id, s.quiz_id, s.finished_at_anonymous, s.score_anonymous, s.DurationInMinutes,
               s.workflow_state, q.points_possible,
               floor(extract(day from s.finished_at_anonymous - DATE '2024-11-11') / 7) + 1 AS week
        FROM sandbox_la_conijn_cbl.silver_canvas_quiz_submissions s
        LEFT JOIN sandbox_la_conijn_cbl.silver_canvas_quizzes q ON s.quiz_id = q.id
        WHERE s.user_id = '%s'
          AND s.finished_at_anonymous >= '2024-11-11 00:00:00'
          AND s.finished_at_anonymous < '2025-02-01 00:00:00'
      ", user_id)
      
      quiz_data <- tryCatch({
        dbGetQuery(sc, quiz_query)
      }, error = function(e) {
        if (!is.null(progress_callback)) progress_callback(-1, paste("Error fetching quiz data:", e$message))
        return(NULL)
      })
      
      if (is.null(quiz_data)) return(FALSE)
      
      if (!is.null(progress_callback)) progress_callback(40, "Fetching assignment data...")
      
      # 3. Get all assignment submissions
      assignment_query <- sprintf("
        SELECT s.course_id, s.assignment_id, s.submitted_at_anonymous, s.score_anonymous,
               s.workflow_state, a.points_possible,
               floor(extract(day from s.submitted_at_anonymous - DATE '2024-11-11') / 7) + 1 AS week
        FROM sandbox_la_conijn_cbl.silver_canvas_submissions s
        LEFT JOIN sandbox_la_conijn_cbl.silver_canvas_assignments a ON s.assignment_id = a.id
        WHERE s.user_id = '%s'
          AND s.submitted_at_anonymous >= '2024-11-11 00:00:00'
          AND s.submitted_at_anonymous < '2025-02-01 00:00:00'
      ", user_id)
      assignment_data <- dbGetQuery(sc, assignment_query)
      
      if (!is.null(progress_callback)) progress_callback(60, "Fetching web activity data...")
      
      # 4. Get web activity logs (this might be large, so we'll summarize by day)
      web_activity_query <- sprintf("
        SELECT course_id, CAST(timestamp AS DATE) AS day, COUNT(*) as interactions,
               MIN(timestamp) as first_activity, MAX(timestamp) as last_activity,
               floor(extract(day from CAST(timestamp AS DATE) - DATE '2024-11-11') / 7) + 1 AS week
        FROM sandbox_la_conijn_cbl.silver_canvas_web_logs
        WHERE user_id = '%s'
          AND timestamp >= '2024-11-11 00:00:00'
          AND timestamp < '2025-02-01 00:00:00'
        GROUP BY course_id, CAST(timestamp AS DATE)
      ", user_id)
      web_activity_data <- dbGetQuery(sc, web_activity_query)
      
      if (!is.null(progress_callback)) progress_callback(80, "Fetching file access data...")
      
      # 5. Get file access data (summarized)
      file_access_query <- sprintf("
        SELECT course_id, CAST(timestamp AS DATE) AS day, 
               COUNT(*) as file_accesses, COUNT(DISTINCT attachment_id) as unique_files,
               floor(extract(day from CAST(timestamp AS DATE) - DATE '2024-11-11') / 7) + 1 AS week
        FROM sandbox_la_conijn_cbl.silver_canvas_web_logs
        WHERE user_id = '%s'
          AND timestamp >= '2024-11-11 00:00:00'
          AND timestamp < '2025-02-01 00:00:00'
          AND attachment_id IS NOT NULL
        GROUP BY course_id, CAST(timestamp AS DATE)
      ", user_id)
      file_access_data <- dbGetQuery(sc, file_access_query)
      
      if (!is.null(progress_callback)) progress_callback(90, "Saving cached data...")
      
      # Combine all data
      cached_data <- list(
        user_id = user_id,
        cached_at = Sys.time(),
        courses = user_courses,
        quiz_data = quiz_data,
        assignment_data = assignment_data,
        web_activity_data = web_activity_data,
        file_access_data = file_access_data
      )
      
      # Save to file with better error handling
      cache_file <- getCacheFilePath(user_id)
      
      # Check if we can write to the directory
      test_file <- paste0(cache_file, ".test")
      tryCatch({
        writeLines("test", test_file)
        file.remove(test_file)
      }, error = function(e) {
        if (!is.null(progress_callback)) progress_callback(-1, paste("Cannot write to directory:", dirname(cache_file), "-", e$message))
        return(FALSE)
      })
      
      # Actually save the data
      save_result <- tryCatch({
        jsonlite::write_json(cached_data, cache_file, pretty = TRUE, auto_unbox = TRUE)
        TRUE
      }, error = function(e) {
        if (!is.null(progress_callback)) progress_callback(-1, paste("Error saving JSON:", e$message))
        return(FALSE)
      })
      
      if (!save_result) return(FALSE)
      
      # Verify the file was created and has content
      if (!file.exists(cache_file)) {
        if (!is.null(progress_callback)) progress_callback(-1, "Cache file was not created")
        return(FALSE)
      }
      
      file_size <- file.size(cache_file)
      if (file_size < 100) {  # File should be at least 100 bytes
        if (!is.null(progress_callback)) progress_callback(-1, paste("Cache file too small:", file_size, "bytes"))
        return(FALSE)
      }
      
      if (!is.null(progress_callback)) progress_callback(100, "Cache complete!")
      
      return(TRUE)
    }, error = function(e) {
      cat("ERROR in downloadUserData:", e$message, "\n")
      if (!is.null(progress_callback)) progress_callback(-1, paste("Error:", e$message))
      return(FALSE)
    })
  }
  
  # Function to load cached data
  loadCachedData <- function(user_id) {
    cache_file <- getCacheFilePath(user_id)
    if (!file.exists(cache_file)) return(NULL)
    
    tryCatch({
      jsonlite::read_json(cache_file, simplifyVector = TRUE)
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Function to get all cached user IDs
  getCachedUserIds <- function() {
    cache_files <- list.files(pattern = "^cached_data_.*\\.json$")
    if (length(cache_files) == 0) return(character(0))
    
    user_ids <- character(0)
    for (file in cache_files) {
      tryCatch({
        data <- jsonlite::read_json(file, simplifyVector = TRUE)
        if (!is.null(data$user_id)) {
          user_ids <- c(user_ids, data$user_id)
        }
      }, error = function(e) {
        # Skip invalid files
      })
    }
    
    return(user_ids)
  }
  
  # Function to get data (cached or from database)
  getUserData <- function(user_id, query_type, ...) {
    # First try to get from cache
    cached_data <- loadCachedData(user_id)
    
    if (!is.null(cached_data)) {
      # Use cached data
      switch(query_type,
        "courses" = return(as.character(cached_data$courses)),
        "quiz_weeks" = {
          args <- list(...)
          course_id <- args$course_id
          start_week <- args$start_week
          end_week <- args$end_week
          
          quiz_data <- cached_data$quiz_data
          if (nrow(quiz_data) > 0) {
            filtered <- quiz_data[quiz_data$course_id == course_id & 
                                 quiz_data$week >= start_week & 
                                 quiz_data$week <= end_week &
                                 quiz_data$workflow_state %in% c('pending_review','complete'), ]
            return(unique(filtered$week))
          }
          return(integer())
        },
        "assignment_weeks" = {
          args <- list(...)
          course_id <- args$course_id
          start_week <- args$start_week
          end_week <- args$end_week
          
          assignment_data <- cached_data$assignment_data
          if (nrow(assignment_data) > 0) {
            filtered <- assignment_data[assignment_data$course_id == course_id & 
                                       assignment_data$week >= start_week & 
                                       assignment_data$week <= end_week &
                                       assignment_data$workflow_state %in% c('pending_review','graded','submitted'), ]
            return(unique(filtered$week))
          }
          return(integer())
        },
        "quiz_scores" = {
          args <- list(...)
          course_id <- args$course_id
          start_date <- args$start_date
          end_date <- args$end_date
          
          quiz_data <- cached_data$quiz_data
          if (nrow(quiz_data) > 0) {
            # Convert finished_at_anonymous to date for filtering
            quiz_data$day <- as.Date(quiz_data$finished_at_anonymous)
            filtered <- quiz_data[quiz_data$course_id == course_id & 
                                 quiz_data$day >= start_date & 
                                 quiz_data$day <= end_date, ]
            if (nrow(filtered) > 0) {
              # Calculate percentage scores
              filtered$value <- (filtered$score_anonymous / filtered$points_possible) * 100
              filtered$value[is.na(filtered$value)] <- 0
              # Return all days in the range with proper aggregation
              all_days <- data.frame(day = seq.Date(start_date, end_date, by="day"))
              if (nrow(filtered) > 0) {
                daily_means <- aggregate(value ~ day, filtered, mean)
                result <- merge(all_days, daily_means, by="day", all.x=TRUE)
                result$value[is.na(result$value)] <- 0
                return(result)
              } else {
                result <- all_days
                result$value <- 0
                return(result)
              }
            }
          }
          # Return empty data frame with proper structure
          result <- data.frame(day = seq.Date(start_date, end_date, by="day"))
          result$value <- 0
          return(result)
        },
        "assignment_scores" = {
          args <- list(...)
          course_id <- args$course_id
          start_date <- args$start_date
          end_date <- args$end_date
          
          assignment_data <- cached_data$assignment_data
          if (nrow(assignment_data) > 0) {
            # Convert submitted_at_anonymous to date for filtering
            assignment_data$day <- as.Date(assignment_data$submitted_at_anonymous)
            filtered <- assignment_data[assignment_data$course_id == course_id & 
                                       assignment_data$day >= start_date & 
                                       assignment_data$day <= end_date, ]
            if (nrow(filtered) > 0) {
              # Calculate percentage scores
              filtered$value <- (filtered$score_anonymous / filtered$points_possible) * 100
              filtered$value[is.na(filtered$value)] <- 0
              # Return all days in the range with proper aggregation
              all_days <- data.frame(day = seq.Date(start_date, end_date, by="day"))
              if (nrow(filtered) > 0) {
                daily_means <- aggregate(value ~ day, filtered, mean)
                result <- merge(all_days, daily_means, by="day", all.x=TRUE)
                result$value[is.na(result$value)] <- 0
                return(result)
              } else {
                result <- all_days
                result$value <- 0
                return(result)
              }
            }
          }
          # Return empty data frame with proper structure
          result <- data.frame(day = seq.Date(start_date, end_date, by="day"))
          result$value <- 0
          return(result)
        },
        "web_activity" = {
          args <- list(...)
          course_id <- args$course_id
          start_date <- args$start_date
          end_date <- args$end_date
          
          web_data <- cached_data$web_activity_data
          if (nrow(web_data) > 0) {
            web_data$day <- as.Date(web_data$day)
            filtered <- web_data[web_data$course_id == course_id & 
                                web_data$day >= start_date & 
                                web_data$day <= end_date, ]
            if (nrow(filtered) > 0) {
              # Return all days in the range
              all_days <- data.frame(day = seq.Date(start_date, end_date, by="day"))
              result <- merge(all_days, filtered[, c("day", "interactions")], by="day", all.x=TRUE)
              result$interactions[is.na(result$interactions)] <- 0
              return(result)
            }
          }
          # Return empty data frame with proper structure
          result <- data.frame(day = seq.Date(start_date, end_date, by="day"))
          result$interactions <- 0
          return(result)
        },
        "file_access" = {
          args <- list(...)
          course_id <- args$course_id
          start_date <- args$start_date
          end_date <- args$end_date
          metric <- args$metric %||% "file_accesses"
          
          file_data <- cached_data$file_access_data
          if (nrow(file_data) > 0) {
            file_data$day <- as.Date(file_data$day)
            filtered <- file_data[file_data$course_id == course_id & 
                                 file_data$day >= start_date & 
                                 file_data$day <= end_date, ]
            if (nrow(filtered) > 0) {
              value_col <- if (metric == "unique_files") "unique_files" else "file_accesses"
              # Return all days in the range
              all_days <- data.frame(day = seq.Date(start_date, end_date, by="day"))
              result_data <- filtered[, c("day", value_col)]
              names(result_data) <- c("day", "value")
              result <- merge(all_days, result_data, by="day", all.x=TRUE)
              result$value[is.na(result$value)] <- 0
              return(result)
            }
          }
          # Return empty data frame with proper structure
          result <- data.frame(day = seq.Date(start_date, end_date, by="day"))
          result$value <- 0
          return(result)
        },
        "quiz_duration" = {
          args <- list(...)
          course_id <- args$course_id
          start_date <- args$start_date
          end_date <- args$end_date
          
          quiz_data <- cached_data$quiz_data
          if (nrow(quiz_data) > 0) {
            quiz_data$day <- as.Date(quiz_data$finished_at_anonymous)
            filtered <- quiz_data[quiz_data$course_id == course_id & 
                                 quiz_data$day >= start_date & 
                                 quiz_data$day <= end_date, ]
            if (nrow(filtered) > 0) {
              # Sum duration by day
              daily_duration <- aggregate(DurationInMinutes ~ day, filtered, sum, na.rm = TRUE)
              names(daily_duration) <- c("day", "value")
              daily_duration$value[is.na(daily_duration$value)] <- 0
              # Return all days in the range
              all_days <- data.frame(day = seq.Date(start_date, end_date, by="day"))
              result <- merge(all_days, daily_duration, by="day", all.x=TRUE)
              result$value[is.na(result$value)] <- 0
              return(result)
            }
          }
          # Return empty data frame with proper structure
          result <- data.frame(day = seq.Date(start_date, end_date, by="day"))
          result$value <- 0
          return(result)
        }
      )
    }
    
    # Fallback to database query
    return(NULL)
  }
  
  #course starting date
  COURSE_START_DATE <- as.Date("2024-11-11")
  
  # Handle data caching
  observeEvent(input$download_data, {
    req(input$cache_user_id)
    
    if (nchar(trimws(input$cache_user_id)) == 0) {
      output$cache_status <- renderUI({
        div(class = "alert alert-warning", "Please enter a User ID to cache.")
      })
      return()
    }
    
    user_to_cache <- trimws(input$cache_user_id)
    
    # Show progress bar
    shinyjs::show("cache_progress")
    shinyjs::runjs("
      $('#progress_bar').css('width', '0%').text('0%');
      $('#progress_message').text('Starting...');
    ")
    
    # Progress callback function
    updateProgress <- function(percent, message) {
      if (percent < 0) {
        # Error case
        shinyjs::hide("cache_progress")
        output$cache_status <- renderUI({
          div(class = "alert alert-danger", HTML(paste("❌", message)))
        })
      } else {
        shinyjs::runjs(sprintf(
          "$('#progress_bar').css('width', '%d%%').text('%d%%');
           $('#progress_message').text('%s');", 
          percent, percent, message
        ))
        
        if (percent == 100) {
          shinyjs::delay(1000, {
            shinyjs::hide("cache_progress")
            
            # Check if file actually exists
            cache_file <- getCacheFilePath(user_to_cache)
            file_exists <- file.exists(cache_file)
            file_size <- if(file_exists) file.size(cache_file) else 0
            
            if (file_exists && file_size > 0) {
              output$cache_status <- renderUI({
                div(class = "alert alert-success", 
                    HTML(paste("✅ Data cached successfully for user", user_to_cache, 
                              "<br><small>File:", cache_file, "(", round(file_size/1024, 1), "KB)</small>")))
              })
            } else {
              output$cache_status <- renderUI({
                div(class = "alert alert-warning", 
                    HTML(paste("⚠️ Cache completed but file not found:", cache_file, 
                              "<br><small>Working directory:", getwd(), "</small>")))
              })
            }
          })
        }
      }
    }
    
    # Also log some debug info
    cat("Cache attempt for user:", user_to_cache, "\n")
    cat("Working directory:", getwd(), "\n")
    cat("Cache file path:", getCacheFilePath(user_to_cache), "\n")
    cat("Starting download function...\n")
    
    # Start download
    result <- downloadUserData(user_to_cache, updateProgress)
    
    cat("Download function returned:", result, "\n")
    
    # Refresh the cached users list after download
    if (result) {
      updateCachedUsersList()
    }
  })
  
  # Refresh cached users list manually
  observeEvent(input$refresh_cache_list, {
    updateCachedUsersList()
  })
  
  # Function to update cached users list
  updateCachedUsersList <- function() {
    output$cached_users_list <- renderUI({
      cached_users <- getCachedUserIds()
      
      if (length(cached_users) == 0) {
        div(class = "text-center text-muted small py-2",
            "No users cached yet")
      } else {
        # Create badges for each cached user
        user_badges <- lapply(cached_users, function(user) {
          # Truncate long user IDs for display
          display_id <- if (nchar(user) > 20) {
            paste0(user)
          } else {
            user
          }
          
          cache_file <- getCacheFilePath(user)
          file_age <- if (file.exists(cache_file)) {
            age_days <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "days"))
            if (age_days < 1) {
              "today"
            } else if (age_days < 7) {
              paste(floor(age_days), "days ago")
            } else {
              "1+ week ago"
            }
          } else {
            "missing"
          }
          
          badge_class <- if (file_age == "missing") {
            "badge bg-danger"
          } else if (file_age == "today") {
            "badge bg-success"
          } else if (grepl("days ago", file_age)) {
            "badge bg-primary"
          } else {
            "badge bg-warning text-dark"
          }
          
          span(class = paste(badge_class, "me-1 mb-1"),
               style = "font-size: 0.75rem;",
               title = paste("Full ID:", user, "- Cached:", file_age),
               display_id)
        })
        
        div(
          div(class = "cached-users-container",
              user_badges
          ),
          div(class = "text-center small text-muted mt-2",
              paste("Total:", length(cached_users), "cached users"))
        )
      }
    })
  }
  
  # Initialize cached users list on startup
  updateCachedUsersList()
  
  # Track cache interface visibility state
  cache_interface_visible <- reactiveVal(FALSE)
  
  # Toggle cache interface visibility
  observeEvent(input$toggle_cache_interface, {
    if (cache_interface_visible()) {
      # Hide interface
      shinyjs::hide("cache_interface_content")
      shinyjs::runjs("$('#cache_chevron').removeClass('fa-chevron-up').addClass('fa-chevron-down');")
      cache_interface_visible(FALSE)
    } else {
      # Show interface
      shinyjs::show("cache_interface_content")
      shinyjs::runjs("$('#cache_chevron').removeClass('fa-chevron-down').addClass('fa-chevron-up');")
      cache_interface_visible(TRUE)
    }
  })
  
  # Handle login
  observeEvent(input$login_button, {
    req(input$user_id_input, input$week_input)
    
    shinyjs::hide("login_panel")
    shinyjs::hide("cache_footer")
    shinyjs::show("main_panel")
    
    credentials$logged_in <- TRUE
    credentials$user_id <- input$user_id_input
    credentials$date <- COURSE_START_DATE + (as.numeric(input$week_input) - 1) * 7
    
    # load saved theme (if any)
    credentials$theme <- loadUserTheme(credentials$user_id)
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
    
    # Fetch this user's course_ids (try cache first)
    courses_from_cache <- getUserData(credentials$user_id, "courses")
    if (!is.null(courses_from_cache)) {
      userCourses(courses_from_cache)
      showNotification("🚀 Using cached data for faster loading!", type = "message", duration = 3)
    } else {
      df <- dbGetQuery(
        sc,
        sprintf("
         SELECT DISTINCT course_id
         FROM sandbox_la_conijn_cbl.silver_canvas_enrollments
         WHERE user_id = '%s'
       ", credentials$user_id)
      )
      userCourses(as.character(df$course_id))
    }
    
    # auto starts activity graph to web activities weekly summary
    updateSelectInput(session, "data_type",
                      selected = "Web Activities")
    updateSelectInput(session, "metric",
                      selected = "Weekly Summary")
    
    updateSelectInput(session, "selected_week",
                      selected = weekNumber())
    
    # Clear old user data
    satisfaction_ratings$data <- list()
    expectation_ratings$data <- list()
    satisfaction_feedback_store$data <- list()
    reflection_saved$data <- list()
  })
  
  observeEvent(input$week_input, {
    req(credentials$logged_in)
    credentials$date <- COURSE_START_DATE + (as.numeric(input$week_input) - 1) * 7
    
    
    # auto-start activity graph exactly as on login:
    updateSelectInput(session, "data_type",
                      selected = "Web Activities")
    updateSelectInput(session, "selected_week",
                      selected = weekNumber())
    updateSelectInput(session, "metric",
                      selected = "Weekly Summary")
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
    
    # Updated structure: user_data is now organized by course directly
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
    shinyjs::show("cache_footer")
    shinyjs::hide("main_panel")
  })
  
  observeEvent(input$toggle_sidebar, {
    collapsed <- !sidebar_collapsed()
    sidebar_collapsed(collapsed)
    
    # sidebar collapse animations with fixed positioning
    if (collapsed) {
      shinyjs::runjs("
      $('#sidebar_col').animate({ 
        width: '0px',
        left: '-300px'
      }, 300, function(){ 
        $(this).hide(); 
      });
      $('#main_col').removeClass('sidebar-expanded').addClass('sidebar-collapsed');
    ")
    } else {
      shinyjs::runjs("
      $('#sidebar_col').show()
                      .css({ 
                        width: '0px',
                        left: '-300px'
                      })
                      .animate({ 
                        width: '25%',
                        left: '0px'
                      }, 300);
      $('#main_col').removeClass('sidebar-collapsed').addClass('sidebar-expanded');
    ")
    }
    
    # give the CSS animation 300ms to finish, then fire a resize
    shinyjs::delay(350, shinyjs::runjs("window.dispatchEvent(new Event('resize'));"))
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
      feedback <- "😐 You're maintaining a steady pace, but not seeing much progress. What's holding you back, and how can you break through?"
    } else if (input$satisfaction <= 6 && input$expectation > input$satisfaction) {
      feedback <- "🌱 You're looking to grow! Think about what support, habits, or focus can help you improve next week."
    } else if (input$satisfaction >= 7 && input$satisfaction <= 8 && input$expectation == input$satisfaction) {
      feedback <- "👍 You're on a steady path and feeling good about it. Keep doing what works!"
    } else if (input$satisfaction >= 9 && input$expectation >= 9) {
      feedback <- "🚀 You're flying high! Excellent work and strong confidence. Reflect on what's driving your success so you can repeat it."
    } else {
      feedback <- "🧭 You're somewhere in between. Take a moment to reflect on what's working and where you can grow."
    }
    
    # 2. ⏮️ Compare to Last Week's Expectation
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
        feedback <- paste(feedback, "<br><br>🤔 You didn't meet your goals, but you're still satisfied. Maybe you set your expectations too high? Adjusting goals is also growth.")
      }
    }
    
    # 3. 🔮 Future Outlook Based on Expectation
    if (input$expectation > input$satisfaction) {
      feedback <- paste(feedback, "<br><br>📈 You're aiming higher next week, awesome! What's your action plan to reach that goal?")
    } else if (input$expectation < input$satisfaction) {
      feedback <- paste(feedback, "<br><br>📉 You're expecting a dip next week, is something changing? Reflect on how to adapt while keeping your momentum.")
    } else {
      feedback <- paste(feedback, "<br><br>🔁 You're aiming for consistency, sounds like you're building a rhythm. What will help you maintain that?")
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
    
    # Fast deletion from individual reflection file
    reflection_file <- getReflectionFilePath(user)
    if (file.exists(reflection_file)) {
      data <- tryCatch({
        jsonlite::read_json(reflection_file, simplifyVector = FALSE)
      }, error = function(e) list())
      
      if (!is.null(data[[course]][[week]])) {
        data[[course]][[week]] <- NULL
        if (length(data[[course]]) == 0) data[[course]] <- NULL
        
        if (length(data) == 0) {
          # Delete entire file if empty
          file.remove(reflection_file)
        } else {
          # Save updated file
          tryCatch({
            jsonlite::write_json(data, reflection_file, pretty = TRUE, auto_unbox = TRUE)
          }, error = function(e) {
            cat("Error updating reflection file:", e$message, "\n")
          })
        }
      }
    }
    
    output$satisfaction_feedback <- renderText({ "" })
  })
  
  # New efficient reflection file system
  getReflectionFilePath <- function(user_id) {
    safe_user_id <- gsub("[^A-Za-z0-9._-]", "_", user_id)
    paste0("reflections_", safe_user_id, ".json")
  }
  
  saveAllUserData <- function(user_id, course_id, week, satisfaction, expectation, feedback) {
    # Use individual reflection files instead of one big file
    reflection_file <- getReflectionFilePath(user_id)
    data <- list()
    if (file.exists(reflection_file)) {
      data <- tryCatch({
        jsonlite::read_json(reflection_file, simplifyVector = FALSE)
      }, error = function(e) list())
    }
    
    user_id <- as.character(user_id)
    course_id <- as.character(course_id)
    week <- as.character(week)
    
    if (is.null(data[[course_id]])) data[[course_id]] <- list()
    
    data[[course_id]][[week]] <- list(
      satisfaction = satisfaction,
      expectation = expectation,
      feedback = feedback,
      reflection_saved = TRUE,
      saved_at = Sys.time()
    )
    
    tryCatch({
      jsonlite::write_json(data, reflection_file, pretty = TRUE, auto_unbox = TRUE)
    }, error = function(e) {
      cat("Error saving reflection data:", e$message, "\n")
    })
  }
  
  loadUserData <- function(user_id) {
    # Load from individual reflection file
    reflection_file <- getReflectionFilePath(user_id)
    if (file.exists(reflection_file)) {
      data <- tryCatch({
        jsonlite::read_json(reflection_file, simplifyVector = FALSE)
      }, error = function(e) {
        cat("Error loading reflection data:", e$message, "\n")
        return(list())
      })
      return(data)
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
  
  # helper to write theme using separate theme file
  saveUserTheme <- function(user, theme) {
    theme_file <- paste0("theme_", gsub("[^A-Za-z0-9._-]", "_", user), ".txt")
    tryCatch({
      writeLines(theme, theme_file)
    }, error = function(e) {
      cat("Error saving theme:", e$message, "\n")
    })
  }
  
  loadUserTheme <- function(user) {
    theme_file <- paste0("theme_", gsub("[^A-Za-z0-9._-]", "_", user), ".txt")
    if (file.exists(theme_file)) {
      tryCatch({
        readLines(theme_file, n = 1)
      }, error = function(e) {
        "default"
      })
    } else {
      "default"
    }
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

    # --- Previous block calculation ---
    prev_block_start <- block_start - 2
    prev_block_end   <- prev_block_start + 1
    prev_block_str   <- as.character(prev_block_start)
    prev_activities <- list()
    prev_total <- 0
    prev_completed <- 0
    prev_percent <- 0
    prev_feedback <- NULL
    if (prev_block_start >= 1) {
      # Weeks in previous block
      prev_weeks <- c(prev_block_start, prev_block_end)
      # Quizzes
      prev_quiz_sql <- sprintf("
        SELECT floor(extract(day 
          from finished_at_anonymous - DATE '%s') / 7) + 1 AS week
        FROM sandbox_la_conijn_cbl.silver_canvas_quiz_submissions
        WHERE user_id = '%s'
          AND workflow_state IN ('pending_review','complete')
          AND finished_at_anonymous >= '%s'
          AND finished_at_anonymous <  '%s'
        GROUP BY week;",
        COURSE_START_DATE, user, 
        paste0(COURSE_START_DATE + (prev_block_start - 1) * 7, " 00:00:00.000"),
        paste0(COURSE_START_DATE + (prev_block_end) * 7, " 00:00:00.000")
      )
      prev_quiz_weeks <- tryCatch(dbGetQuery(sc, prev_quiz_sql)$week, error = function(e) integer())
      # Assignments
      prev_assign_sql <- sprintf("
        SELECT floor(extract(day 
          from submitted_at_anonymous - DATE '%s') / 7) + 1 AS week
        FROM sandbox_la_conijn_cbl.silver_canvas_submissions
        WHERE user_id = '%s'
          AND workflow_state IN ('pending_review','graded','submitted')
          AND submitted_at_anonymous >= '%s'
          AND submitted_at_anonymous <  '%s'
        GROUP BY week;",
        COURSE_START_DATE, user, 
        paste0(COURSE_START_DATE + (prev_block_start - 1) * 7, " 00:00:00.000"),
        paste0(COURSE_START_DATE + (prev_block_end) * 7, " 00:00:00.000")
      )
      prev_assign_weeks <- tryCatch(dbGetQuery(sc, prev_assign_sql)$week, error = function(e) integer())
      # Reflections
      prev_reflections <- lapply(prev_weeks, function(w) isTRUE(reflection_saved$data[[course]][[as.character(w)]]))
      # Build activity list
      prev_activities <- lapply(prev_weeks, function(w) {
        list(
          list(name = sprintf("Submit Week %d Quiz",      w), done = w %in% prev_quiz_weeks),
          list(name = sprintf("Submit Week %d Assignment", w), done = w %in% prev_assign_weeks),
          list(name = sprintf("Reflect on Week %d",        w), done = isTRUE(reflection_saved$data[[course]][[as.character(w)]]))
        )
      }) %>% unlist(recursive = FALSE)
      prev_total     <- length(prev_activities)
      prev_completed <- sum(vapply(prev_activities, `[[`, logical(1), "done"))
      prev_percent   <- if (prev_total > 0) round(prev_completed / prev_total * 100) else 0
      # Dynamic feedback message
      if (prev_completed == 0) {
        prev_feedback <- "You didn't complete any activities last block. Let's aim for more this time!"
      } else if (prev_completed == prev_total && prev_total > 0) {
        prev_feedback <- "You completed all activities last block! Keep up the great work!"
      } else {
        prev_feedback <- sprintf(
          "You completed %d out of %d activities last block. Every step counts—can you do even better this week?",
          prev_completed, prev_total
        )
      }
    } else {
      prev_feedback <- "No previous block to report yet."
    }
    # --- End previous block calculation ---

    # 2. Get quiz weeks (try cache first)
    quiz_weeks_cached <- getUserData(user, "quiz_weeks", 
                                   course_id = course, 
                                   start_week = block_start, 
                                   end_week = block_end)
    if (!is.null(quiz_weeks_cached)) {
      quiz_weeks <- quiz_weeks_cached
    } else {
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
    }
    
    # 3. Get assignment weeks (try cache first)
    assign_weeks_cached <- getUserData(user, "assignment_weeks", 
                                     course_id = course, 
                                     start_week = block_start, 
                                     end_week = block_end)
    if (!is.null(assign_weeks_cached)) {
      assign_weeks <- assign_weeks_cached
    } else {
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
    }
    
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
    
    # 5. Only write JSON when the "done" flag for this block actually flips
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
                icon <- if (act$done) "✅" else "🕒"
                tags$li(class = paste("list-group-item d-flex justify-content-between align-items-center", cls),
                        HTML(paste(icon, act$name)))
              })
      ),
      tags$div(class = "alert alert-info mb-3", role = "alert", 
               HTML(paste0("<strong>Last Block Progress:</strong> ", prev_feedback))),
      tags$div(class = "alert alert-warning mt-3", role = "alert", feedback_text)
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
                      "Assignments" = c(
                        "Submissions"            = "Submissions",
                        "Average Score"      = "Avg Assignment Score",
                        "Min & Max Scores"          = "Max Assignment Score"
                      ),
                      "Web Activities" = c(
                        "Weekly Summary"         = "Weekly Time Summary",
                        "Daily Web Activity"     = "Daily Web Activity",
                        "Active Study Time"      = "Active Study Time",
                        "Quiz Time Duration"     = "Quiz Time Duration"
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
      
      # Unified raw-score query for Avg/Min&Max branches (Assignments & Quizzes)
      if (input$data_type == "Quizzes" &&
          input$metric %in% c("Avg Quiz Score", "Max Quiz Score")) {
        
        # Try cache first - now returns fully processed data
        df_cached <- getUserData(user, "quiz_scores", 
                               course_id = course, 
                               start_date = start_date, 
                               end_date = end_date)
        
        if (!is.null(df_cached)) {
          df <- df_cached
          # For raw values (min/max), we need the original data
          cached_data <- loadCachedData(user)
          if (!is.null(cached_data) && nrow(cached_data$quiz_data) > 0) {
            quiz_data <- cached_data$quiz_data
            quiz_data$day <- as.Date(quiz_data$finished_at_anonymous)
            filtered <- quiz_data[quiz_data$course_id == course & 
                                 quiz_data$day >= start_date & 
                                 quiz_data$day <= end_date, ]
            if (nrow(filtered) > 0) {
              filtered$value <- (filtered$score_anonymous / filtered$points_possible) * 100
              filtered$value[is.na(filtered$value)] <- 0
              attr(df, "raw_values") <- filtered[, c("day", "value")]
            }
          }
        } else {
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
          attr(df, "raw_values") <- df_raw
        }
        
        ylab <- if (input$metric == "Avg Quiz Score")
          "Average Quiz Score (%)" else "Max Quiz Score (%)"
        
      } else if (input$data_type == "Assignments" &&
                 input$metric %in% c("Avg Assignment Score", "Max Assignment Score")) {
        
        # Try cache first - now returns fully processed data
        df_cached <- getUserData(user, "assignment_scores", 
                               course_id = course, 
                               start_date = start_date, 
                               end_date = end_date)
        
        if (!is.null(df_cached)) {
          df <- df_cached
          # For raw values (min/max), we need the original data
          cached_data <- loadCachedData(user)
          if (!is.null(cached_data) && nrow(cached_data$assignment_data) > 0) {
            assignment_data <- cached_data$assignment_data
            assignment_data$day <- as.Date(assignment_data$submitted_at_anonymous)
            filtered <- assignment_data[assignment_data$course_id == course & 
                                       assignment_data$day >= start_date & 
                                       assignment_data$day <= end_date, ]
            if (nrow(filtered) > 0) {
              filtered$value <- (filtered$score_anonymous / filtered$points_possible) * 100
              filtered$value[is.na(filtered$value)] <- 0
              attr(df, "raw_values") <- filtered[, c("day", "value")]
            }
          }
        } else {
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
          attr(df, "raw_values") <- df_raw
        }
        
        ylab <- if (input$metric == "Avg Assignment Score")
          "Average Assignment Score (%)" else "Max Assignment Score (%)"
        
      } else if (input$data_type == "Web Activities" && input$metric == "Active Study Time") {
        # Try cache first for web activity data - now returns fully processed data
        web_cached <- getUserData(user, "web_activity", 
                                course_id = course, 
                                start_date = start_date, 
                                end_date = end_date)
        
        if (!is.null(web_cached)) {
          # Use cached data and estimate active time - data already has all days
          df <- data.frame(
            day = web_cached$day,
            value = web_cached$interactions * 2  # 2 minutes per interaction
          )
        } else {
          # Calculate active study time using session gaps (more realistic than total span)
          query <- sprintf(
            "SELECT CAST(timestamp AS DATE) AS day,
                  timestamp,
                  ROW_NUMBER() OVER (PARTITION BY CAST(timestamp AS DATE) ORDER BY timestamp) as seq_num
           FROM sandbox_la_conijn_cbl.silver_canvas_web_logs
           WHERE user_id = '%s'
             AND timestamp BETWEEN '%s' AND '%s'
             AND course_id = %s
           ORDER BY day, timestamp;",
            user, start_ts, end_ts, course
          )
          
          df_raw <- tryCatch(
            dbGetQuery(sc, query),
            error = function(e) {
              showNotification("Error fetching activity data", type = "error")
              data.frame(day=character(), timestamp=character(), seq_num=numeric())
            }
          )
          
          # Calculate active study time by counting interactions * average time per interaction
          if (nrow(df_raw) > 0) {
            df_raw$day <- as.Date(df_raw$day)
            # Estimate 2 minutes per interaction as active study time (more realistic)
            daily_active_time <- aggregate(seq_num ~ day, df_raw, FUN = function(x) length(x) * 2)
            names(daily_active_time) <- c("day", "value")
          } else {
            daily_active_time <- data.frame(day=seq.Date(start_date, end_date, by="day"), value=0)
          }
          
          # Ensure we have data for the full week
          all_days <- data.frame(day=seq.Date(start_date, end_date, by="day"))
          df <- merge(all_days, daily_active_time, by="day", all.x=TRUE)
          df$value[is.na(df$value)] <- 0
        }
        
        ylab <- "Active Study Time (Minutes)"
        attr(df, "raw_values") <- NULL
        
      } else if (input$data_type == "Web Activities" && input$metric == "Weekly Time Summary") {
        # Custom handling for weekly summary - will be handled in the UI section
        df <- data.frame(day=seq.Date(start_date, end_date, by="day"), value=0)
        ylab <- "Weekly Summary"
        attr(df, "raw_values") <- NULL
        
      } else {
        # Handle single-table metrics (counts, etc.) - Try cache first for all
        df_cached <- NULL
        
        if (input$data_type == "Web Activities" && input$metric == "Daily Web Activity") {
          df_cached <- getUserData(user, "web_activity", 
                                 course_id = course, 
                                 start_date = start_date, 
                                 end_date = end_date)
          if (!is.null(df_cached)) {
            names(df_cached) <- c("day", "value")
          }
          ylab <- "Daily Interactions"
        } else if (input$data_type == "Web Activities" && input$metric == "Quiz Time Duration") {
          df_cached <- getUserData(user, "quiz_duration", 
                                 course_id = course, 
                                 start_date = start_date, 
                                 end_date = end_date)
          ylab <- "Minutes Spent on Quizzes"
        } else if (input$data_type == "Files" && input$metric == "File Accesses") {
          df_cached <- getUserData(user, "file_access", 
                                 course_id = course, 
                                 start_date = start_date, 
                                 end_date = end_date,
                                 metric = "file_accesses")
          ylab <- "File Accesses"
        } else if (input$data_type == "Files" && input$metric == "Unique Files Accessed") {
          df_cached <- getUserData(user, "file_access", 
                                 course_id = course, 
                                 start_date = start_date, 
                                 end_date = end_date,
                                 metric = "unique_files")
          ylab <- "Unique Files Accessed"
        } else if (input$data_type == "Quizzes" && input$metric == "Submissions") {
          # For quiz submissions, we can derive from cached quiz_data
          cached_data <- loadCachedData(user)
          if (!is.null(cached_data) && nrow(cached_data$quiz_data) > 0) {
            quiz_data <- cached_data$quiz_data
            quiz_data$day <- as.Date(quiz_data$finished_at_anonymous)
            filtered <- quiz_data[quiz_data$course_id == course & 
                                 quiz_data$day >= start_date & 
                                 quiz_data$day <= end_date, ]
            if (nrow(filtered) > 0) {
              daily_counts <- aggregate(quiz_id ~ day, filtered, length)
              names(daily_counts) <- c("day", "value")
              # Ensure all days are included
              all_days <- data.frame(day=seq.Date(start_date, end_date, by="day"))
              df_cached <- merge(all_days, daily_counts, by="day", all.x=TRUE)
              df_cached$value[is.na(df_cached$value)] <- 0
            } else {
              df_cached <- data.frame(day=seq.Date(start_date, end_date, by="day"), value=0)
            }
          }
          ylab <- "Submissions"
        } else if (input$data_type == "Assignments" && input$metric == "Submissions") {
          # For assignment submissions, we can derive from cached assignment_data
          cached_data <- loadCachedData(user)
          if (!is.null(cached_data) && nrow(cached_data$assignment_data) > 0) {
            assignment_data <- cached_data$assignment_data
            assignment_data$day <- as.Date(assignment_data$submitted_at_anonymous)
            filtered <- assignment_data[assignment_data$course_id == course & 
                                       assignment_data$day >= start_date & 
                                       assignment_data$day <= end_date, ]
            if (nrow(filtered) > 0) {
              daily_counts <- aggregate(assignment_id ~ day, filtered, length)
              names(daily_counts) <- c("day", "value")
              # Ensure all days are included
              all_days <- data.frame(day=seq.Date(start_date, end_date, by="day"))
              df_cached <- merge(all_days, daily_counts, by="day", all.x=TRUE)
              df_cached$value[is.na(df_cached$value)] <- 0
            } else {
              df_cached <- data.frame(day=seq.Date(start_date, end_date, by="day"), value=0)
            }
          }
          ylab <- "Submissions"
        }
        
        if (!is.null(df_cached)) {
          df <- df_cached
        } else {
          # Fallback to database query
          config <- switch(input$data_type,
                          "Quizzes" = list(tbl="silver_canvas_quiz_submissions",
                                          date_col="finished_at_anonymous",
                                          expr="COUNT(*) AS value",
                                          ylab="Submissions"),
                          "Files" = {
                            if (input$metric == "File Accesses") {
                              list(tbl="silver_canvas_web_logs",
                                   date_col="timestamp",
                                   expr="COUNT(*) AS value",
                                   ylab="File Accesses",
                                   extra_filter="AND attachment_id IS NOT NULL")
                            } else {
                              list(tbl="silver_canvas_web_logs",
                                   date_col="timestamp",
                                   expr="COUNT(DISTINCT attachment_id) AS value",
                                   ylab="Unique Files Accessed",
                                   extra_filter="AND attachment_id IS NOT NULL")
                            }
                          },
                          "Assignments" = list(tbl="silver_canvas_submissions",
                                              date_col="submitted_at_anonymous",
                                              expr="COUNT(*) AS value",
                                              ylab="Submissions"),
                          "Web Activities" = {
                            if (input$metric == "Daily Web Activity") {
                              list(tbl="silver_canvas_web_logs",
                                   date_col="timestamp",
                                   expr="COUNT(*) AS value",
                                   ylab="Daily Interactions")
                            } else {
                              list(tbl="silver_canvas_quiz_submissions",
                                   date_col="finished_at_anonymous",
                                   expr="COALESCE(SUM(DurationInMinutes), 0) AS value",
                                   ylab="Minutes Spent on Quizzes")
                            }
                          }
          )
          
          extra_filter <- config$extra_filter %||% ""
          
          df <- tryCatch(
            dbGetQuery(sc, sprintf(
              "SELECT CAST(%s AS DATE) AS day, %s
             FROM sandbox_la_conijn_cbl.%s
             WHERE user_id = '%s'
               AND %s BETWEEN '%s' AND '%s'
               AND course_id = %s
               %s
             GROUP BY day
             ORDER BY day;",
              config$date_col, config$expr, config$tbl,
              user, config$date_col, start_ts, end_ts, course, extra_filter
            )),
            error = function(e)
              data.frame(day=seq.Date(start_date, end_date, by="day"), value=0)
          )
          
          # Ensure full week coverage for database queries
          all_days <- data.frame(day=seq.Date(start_date, end_date, by="day"))
          df <- merge(all_days, df, by="day", all.x=TRUE)
          df$value[is.na(df$value)] <- 0
          
          ylab <- config$ylab
        }
        attr(df, "raw_values") <- NULL
      }
      
      # Data should already be complete with all days - no additional merging needed
      list(df = df, ylab = ylab, raw = attr(df, "raw_values"))
    }
  )
  
  # ———————————————————————————————————————
  # 1) Plotly renderer (only when not avg/max)
  # ———————————————————————————————————————
  output$activityPlot <- renderPlotly({
    req(input$data_type, input$metric, input$selected_week)
    if (input$metric %in% c("Avg Quiz Score","Avg Assignment Score",
                            "Max Quiz Score","Max Assignment Score",
                            "Weekly Time Summary")) {
      return(plotly_empty(type = "scatter"))
    }
    
    dat  <- activityData()
    df   <- dat$df
    ylab <- dat$ylab
    wk   <- as.integer(input$selected_week)
    
    # No special handling needed for removed activity breakdown metric
    
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
            annotations = list(
              list(
                text        = "<b>No activity this week</b>",
                showarrow   = FALSE,
                xref        = "paper", 
                yref        = "paper",
                x           = 0.5, 
                y           = 0.5,
                font        = list(
                  family = "system-ui",
                  size   = 20,
                  color  = theme_colors$accent
                ),
                align       = "center",
                bgcolor     = "rgba(255, 255, 255, 0.8)",
                bordercolor = theme_colors$accent,
                borderwidth = 2,
                borderpad   = 6
              )
            ),
            paper_bgcolor = theme_colors$bg,
            plot_bgcolor  = theme_colors$bg,
            margin = list(t = 120, b = 80, l = 80, r = 40),
            font   = list(family = "system-ui", color = theme_colors$txt)
          )
      )
    }
    
    plot_ly(
      df, x = ~day, y = ~value, type = "scatter", mode = "lines+markers",
      line = list(color = theme_colors$accent, width = 3),
      marker = list(color = theme_colors$accent, size = 8),
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
  # Unified UI: either the chart or a one-line summary
  # ———————————————————————————————————————
  # New metric description output
  output$metric_description <- renderText({
    req(input$data_type, input$metric)
    paste(input$data_type, "→", input$metric)
  })
  
  # Always-visible weekly summary
  output$weekly_summary_card <- renderUI({
    req(input$selected_week, credentials$logged_in)
    
    # Custom weekly time summary view
    week_idx <- as.integer(input$selected_week)
    start_date <- COURSE_START_DATE + (week_idx - 1) * 7
    end_date <- start_date + 6L
    start_ts <- paste0(start_date, " 00:00:00")
    end_ts <- paste0(end_date + 1, " 00:00:00")
    user <- credentials$user_id
    course <- course_id()
    
    # Get weekly summary data (try cache first)
    web_data_cached <- getUserData(user, "web_activity", 
                                 course_id = course, 
                                 start_date = start_date, 
                                 end_date = end_date)
    
    if (!is.null(web_data_cached)) {
      # Use cached web data - need to reshape it
      web_data <- data.frame(
        day = web_data_cached$day,
        interactions = web_data_cached$interactions,
        first_activity = paste(web_data_cached$day, "09:00:00"),  # approximate
        last_activity = paste(web_data_cached$day, "17:00:00")    # approximate
      )
    } else {
      # 1. Get daily web activity from database
      web_query <- sprintf(
        "SELECT CAST(timestamp AS DATE) AS day,
               COUNT(*) as interactions,
               MIN(timestamp) as first_activity,
               MAX(timestamp) as last_activity
        FROM sandbox_la_conijn_cbl.silver_canvas_web_logs
        WHERE user_id = '%s' AND course_id = %s
          AND timestamp BETWEEN '%s' AND '%s'
        GROUP BY CAST(timestamp AS DATE);",
        user, course, start_ts, end_ts
      )
      
      web_data <- tryCatch(
        dbGetQuery(sc, web_query),
        error = function(e) {
          showNotification("Error fetching web activity data", type = "error")
          data.frame(day=character(), interactions=numeric(), first_activity=character(), last_activity=character())
        }
      )
    }
    
    # Get quiz duration (try cache first)
    quiz_data_cached <- getUserData(user, "quiz_duration", 
                                  course_id = course, 
                                  start_date = start_date, 
                                  end_date = end_date)
    
    if (!is.null(quiz_data_cached)) {
      quiz_data <- data.frame(total_quiz_minutes = sum(quiz_data_cached$value, na.rm = TRUE))
    } else {
      # 2. Get quiz time from database
      quiz_query <- sprintf(
        "SELECT COALESCE(SUM(DurationInMinutes), 0) as total_quiz_minutes
        FROM sandbox_la_conijn_cbl.silver_canvas_quiz_submissions
        WHERE user_id = '%s' AND course_id = %s
          AND finished_at_anonymous BETWEEN '%s' AND '%s';",
        user, course, start_ts, end_ts
      )
      
      quiz_data <- tryCatch(
        dbGetQuery(sc, quiz_query),
        error = function(e) {
          showNotification("Error fetching quiz data", type = "error")
          data.frame(total_quiz_minutes = 0)
        }
      )
    }
    
    # Calculate summary statistics in R using realistic time estimates
    if (nrow(web_data) > 0) {
      web_data$day <- as.Date(web_data$day)
      
      # Use realistic time estimate: 2 minutes per interaction (active engagement time)
      web_data$session_minutes <- web_data$interactions * 2
      
      # Count only days with actual activity (interactions > 0)
      active_days_count <- sum(web_data$interactions > 0, na.rm = TRUE)
      
      summary_data <- data.frame(
        total_interactions = sum(web_data$interactions, na.rm = TRUE),
        total_session_minutes = sum(web_data$session_minutes, na.rm = TRUE),
        active_days = active_days_count,
        avg_daily_interactions = round(mean(web_data$interactions, na.rm = TRUE), 1),
        quiz_minutes = ifelse(is.na(quiz_data$total_quiz_minutes[1]), 0, quiz_data$total_quiz_minutes[1])
      )
    } else {
      summary_data <- data.frame(
        total_interactions = 0, total_session_minutes = 0, active_days = 0,
        avg_daily_interactions = 0, quiz_minutes = ifelse(is.na(quiz_data$total_quiz_minutes[1]), 0, quiz_data$total_quiz_minutes[1])
      )
    }
    
    # Format the summary
    total_hours <- round(summary_data$total_session_minutes / 60, 1)
    quiz_hours <- round(summary_data$quiz_minutes / 60, 1)
    
    engagement_level <- if (summary_data$active_days >= 5) {
      list(text = "Excellent", class = "bg-success")
    } else if (summary_data$active_days >= 3) {
      list(text = "Good", class = "bg-warning text-dark")
    } else if (summary_data$active_days > 0) {
      list(text = "Limited", class = "bg-warning text-dark")
    } else {
      list(text = "No Activity", class = "bg-danger")
    }
    
    tagList(
      div(class = "weekly-summary text-center p-3",
          div(class = "row",
              div(class = "col-md-4",
                  div(class = "card bg-light border-0",
                      div(class = "card-body text-center py-3",
                          h6("Study Hours", class = "card-title text-muted mb-1"),
                          h4(paste0(total_hours, "h"), class = "text-primary mb-0")
                      )
                  )
              ),
              div(class = "col-md-4",
                  div(class = "card bg-light border-0",
                      div(class = "card-body text-center py-3",
                          h6("Quiz Time", class = "card-title text-muted mb-1"),
                          h4(paste0(quiz_hours, "h"), class = "text-info mb-0")
                      )
                  )
              ),
              div(class = "col-md-4",
                  div(class = "card bg-light border-0",
                      div(class = "card-body text-center py-3",
                          h6("Active Days", class = "card-title text-muted mb-1"),
                          h4(paste0(summary_data$active_days, "/7"), class = "text-secondary mb-0")
                      )
                  )
              )
          )
      ),
      div(class = "mt-3",
          div(class = paste("alert alert-sm py-2", engagement_level$class), 
              HTML(paste0("<strong>", engagement_level$text, " Engagement</strong> • ",
                         summary_data$total_interactions, " interactions • ",
                         "Avg: ", round(summary_data$avg_daily_interactions), "/day"))
          )
      )
    )
  })
  
  # Detailed activity view (charts and specific metrics)
  output$detailed_activity_view <- renderUI({
    req(input$data_type, input$metric, input$selected_week)
    
    # Skip if this is weekly summary (already shown above)
    if (input$metric == "Weekly Time Summary") {
      return(
        div(class = "alert alert-info text-center py-4",
            icon("info-circle"), " ",
            "Weekly Summary is shown above. Select a different metric to see detailed charts and breakdowns."
        )
      )
    }
    
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
        
        details <- div(
          class = "small-daily-scores mt-4",
          div("Daily scores:", class = "mb-3 fw-bold"),
          div(
            class = "list-group",
            lapply(seq_len(nrow(df_nz)), function(i) {
              score <- round(df_nz$value[i], 1)
              score_cls <- if (score < 60) {
                "bg-danger"
              } else if (score < 80) {
                "bg-warning text-dark"
              } else {
                "bg-success"
              }
              div(
                class = "list-group-item d-flex justify-content-between align-items-center",
                span(format(df_nz$day[i], "%b %d")),
                span(
                  paste0(score, "%"),
                  class = paste("badge", score_cls, "rounded-pill")
                )
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
        tags$div(class = "alert alert-warning mb-3 pb-2", role = "alert",
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
  
  # Horizontal badges display (for top area)
  output$badges_horizontal <- renderUI({
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
    
    # Create horizontal badge layout
    badge_html <- "<div class='horizontal-badge-container'>"
    for (b in badges) {
      unlocked <- if (!is.null(b$id)) {
        completed_block_count >= b$threshold
      } else {
        reflection_count >= b$threshold
      }
      
      if (unlocked) {
        badge_html <- paste0(badge_html, "<span class='badge-item-horizontal' data-tooltip='", b$unlocked_tooltip, "'><img src='", b$unlocked_img, "' height='90px'/></span>")
      } else {
        badge_html <- paste0(badge_html, "<span class='badge-item-horizontal' data-tooltip='", b$locked_tooltip, "'><img src='img/QUESTION.png' height='90px'/></span>")
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
    paste("Week", input$selected_week)
  })
  
  output$motivation_reflection <- renderText({
    paste("Fill out your reflection for this week to better understand your progress and earn your badges.")
  })
}

shinyApp(ui, server)
