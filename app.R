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
    "))
  ),
  uiOutput("mainUI")  # dynamically switch between login and app UI
)

server <- function(input, output, session) {
  # Track login state
  credentials <- reactiveValues(logged_in = FALSE, user_id = NULL, date = NULL)
  
  observeEvent(input$login_button, {
    req(input$user_id_input, input$date_input)
    credentials$logged_in <- TRUE
    credentials$user_id <- input$user_id_input
    credentials$date <- input$date_input
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
                        sliderInput("satisfaction", "How satisfied were you this week?", min = 1, max = 10, value = 7),
                        sliderInput("expectation", "What do you expect next week?", min = 1, max = 10, value = 8),
                        HTML("
                          <div class='alert alert-warning mt-3'>
                            🏅 <strong>5 Week Streak!</strong> Reflection badge for 5 submissions in a row.
                          </div>
                        ")
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
                        div(class = "section-title", "Badge Tab"),
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
}

shinyApp(ui, server)