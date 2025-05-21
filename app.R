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
                          sliderInput("satisfaction", "How satisfied were you this week?", min = 1, max = 10, value = 7),
                          sliderInput("expectation", "What do you expect next week?", min = 1, max = 10, value = 8),
                          HTML("
                              <div class='alert alert-warning mt-3'>
                                🏅 <strong>5 Week Streak!</strong> Reflection badge for 5 submissions in a row.
                              </div>
                            ")
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
  
}

shinyApp(ui, server)
