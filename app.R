# app.R

library(jsonlite)
library(shiny)
library(shinyjs)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(tidyr)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    # Bootstrap CSS
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
    tags$style(HTML("
      /* Page background and font */
      body {
        background-color: #F0F2F5;
        font-family: system-ui;
      }
      
      /*–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––*/
      /* Smooth theme‐color transitions on background, text & borders */
      /*–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––*/
      body,
      html,
      .header-banner,
      .sidebar,
      .card,
      .section-title,
      .list-group-item,
      .btn,
      .btn-outline-primary,
      input,
      select,
      .input-group-text {
        transition: 
          background-color 0.4s ease,
          color            0.4s ease,
          border-color     0.4s ease;
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
        border-radius: 0;
      }
      .header-banner h3 {
        margin: 0;
        font-size: 1.75rem;
        font-weight: 600;
      }
      .header-banner .btn-logout {
        background-color: #FFC7C7 !important;
        border-color:     #FFA3A3 !important;
        color:            #800000 !important;
      }
      .header-banner .btn-logout:hover {
        background-color: #FFA3A3 !important;
        border-color:     #FF8A8A !important;
        color:            #660000 !important;
      }
      .header-banner .shiny-date-input {
        margin: 0 !important;
        display: flex !important;
        align-items: center !important;
      }
      .header-banner .btn-logout {
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
      }

      /* Sidebar styling as vertical nav */
      .sidebar {
        background-color: #2C3E50;
        height: calc(100vh - 3.5rem); /* subtract header height (~3.5rem) */
        padding: 2rem 1rem;
        position: sticky;
        top: 3.5rem; /* stick just below header */
      }
      .sidebar .theme-selector {
        margin-top: 10rem !important;
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
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 30px;
        justify-items: center;
        padding: 1rem 0;
        transition: all 0.3s ease;
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
      
      /*–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––*/
      /* Theme palettes — each prefix will be added as html.theme-<name> */
      /*–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––*/
      
      /* DEFAULT (no changes) */
      html.theme-default {}
      
      /* LIGHT: very light backgrounds, blue accents */
      html.theme-light {
        background-color: #FFFFFF !important;
        color:            #2C3E50  !important;
      }
      html.theme-light body {
        background-color: #FFFFFF !important;
      }
      html.theme-light .sidebar {
        background-color: #F0F0F0 !important;
      }
      html.theme-light .card {
        border-left-color: #2980B9 !important;
      }
      html.theme-light .header-banner {
        background: linear-gradient(90deg, #F0F0F0, #F0F0F1) !important;
        color:      #2C3E50 !important;
      }
      html.theme-light .section-title {
        color: #2980B9 !important;
      }
      html.theme-light .sidebar .list-group-item {
        background-color: #F8F9FA !important;
        color:            #2C3E50  !important;
        border-color:     #D0D0D0  !important;
      }
      html.theme-light .sidebar .list-group-item.active {
        background-color: #2980B9 !important;
        color:            #FFFFFF !important;
        border-color:     #2980B9 !important;
      }
      html.theme-light .sidebar .list-group-item:hover {
        background-color: #E0E0E0 !important;
        color:            #2C3E50  !important;
      }
      /* ========== BUTTONS ========== */
      html.theme-light .btn-primary {
        background-color: #2980B9 !important;
        border-color:     #2980B9 !important;
        color:            #FFFFFF !important;
      }
      html.theme-light .btn-primary:hover {
        background-color: #2C3E50 !important;
        border-color:     #2C3E50 !important;
      }
      html.theme-light .btn-outline-primary {
        color:         #2980B9 !important;
        border-color:  #2980B9 !important;
      }
      html.theme-light .btn-outline-primary:hover {
        background-color: #2980B9 !important;
        color:            #FFFFFF !important;
      }
      /* ========== INPUTS & DROPDOWNS ========== */
      html.theme-light input,
      html.theme-light select,
      html.theme-light textarea {
        background-color: #FFFFFF !important;
        color:            #2C3E50  !important;
        border-color:     #D0D0D0  !important;
      }
      html.theme-light .selectize-input,
      html.theme-light .selectize-dropdown {
        background-color: #FFFFFF !important;
        color:            #2C3E50  !important;
        border-color:     #D0D0D0  !important;
      }
      /* ========== DATE-RANGE “to” ADDON ========== */
      html.theme-light .input-group-addon,
      html.theme-light .input-group-text {
        background-color: #FFFFFF !important;
        color:            #2C3E50  !important;
        border-color:     #D0D0D0  !important;
      }
      
      
      /* DARK: charcoal backgrounds, orange accents */
      html.theme-dark {
        background-color: #2C2C2C !important;
        color:            #EEEEEE !important;
      }
      html.theme-dark body {
        background-color: #2C2C2C !important;
      }
      html.theme-dark .sidebar {
        background-color: #3A3A3A !important;
      }
      html.theme-dark .card {
        background-color:  #3A3A3A !important;
        border-left-color:  #E67E22 !important;
        color:             #EEEEEE  !important;
      }
      html.theme-dark .header-banner {
        background: linear-gradient(90deg, #1A1A1A, #333333) !important;
        color:      #E67E22 !important;
      }
      html.theme-dark .section-title {
        color: #E67E22 !important;
      }
      html.theme-dark .sidebar .list-group-item {
        background-color: #3A3A3A !important;
        color:            #EEEEEE  !important;
        border-color:     #444444  !important;
      }
      html.theme-dark .sidebar .list-group-item.active {
        background-color: #E67E22 !important;
        color:            #2C2C2C  !important;
        border-color:     #E67E22  !important;
      }
      html.theme-dark .sidebar .list-group-item:hover {
        background-color: #4A4A4A !important;
        color:            #EEEEEE  !important;
      }
      /* ========== BUTTONS ========== */
      html.theme-dark .btn-primary {
        background-color: #E67E22 !important;
        border-color:     #E67E22 !important;
        color:            #FFFFFF !important;
      }
      html.theme-dark .btn-primary:hover {
        background-color: #D35400 !important;
        border-color:     #D35400 !important;
      }
      html.theme-dark .btn-outline-primary {
        color:         #E67E22 !important;
        border-color:  #E67E22 !important;
      }
      html.theme-dark .btn-outline-primary:hover {
        background-color: #E67E22 !important;
        color:            #FFFFFF !important;
      }
      /* ========== INPUTS & DROPDOWNS ========== */
      html.theme-dark input,
      html.theme-dark select,
      html.theme-dark textarea {
        background-color: #3A3A3A !important;
        color:            #EEEEEE  !important;
        border-color:     #444444  !important;
      }
      html.theme-dark .selectize-input,
      html.theme-dark .selectize-dropdown {
        background-color: #3A3A3A !important;
        color:            #EEEEEE  !important;
        border-color:     #444444  !important;
      }
      /* ========== DATE-RANGE “to” ADDON ========== */
      html.theme-dark .input-group-addon,
      html.theme-dark .input-group-text {
        background-color: #3A3A3A !important;
        color:            #EEEEEE  !important;
        border-color:     #444444  !important;
      }
      
      
      /* OCEAN: teal backgrounds, sea-green accents */
      html.theme-ocean {
        background-color: #E0F7FA !important;
      }
      html.theme-ocean body {
        background-color: #E0F7FA !important;
      }
      html.theme-ocean .sidebar {
        background-color: #004D40 !important;
      }
      html.theme-ocean .card {
        border-left-color: #00796B !important;
      }
      html.theme-ocean .header-banner {
        background: linear-gradient(90deg, #004D40, #00796B) !important;
        color:      #E0F7FA !important;
      }
      html.theme-ocean .section-title {
        color: #004D40 !important;
      }
      html.theme-ocean .sidebar .list-group-item {
        background-color: #004D40 !important;
        color:            #E0F7FA !important;
        border-color:     #00695C !important;
      }
      html.theme-ocean .sidebar .list-group-item.active {
        background-color: #00796B !important;
        color:            #FFFFFF !important;
        border-color:     #00796B !important;
      }
      html.theme-ocean .sidebar .list-group-item:hover {
        background-color: #00695C !important;
        color:            #E0F7FA !important;
      }
      /* ========== BUTTONS ========== */
      html.theme-ocean .btn-primary {
        background-color: #00796B !important;
        border-color:     #00796B !important;
        color:            #FFFFFF !important;
      }
      html.theme-ocean .btn-primary:hover {
        background-color: #004D40 !important;
        border-color:     #004D40 !important;
      }
      html.theme-ocean .btn-outline-primary {
        color:         #00796B !important;
        border-color:  #00796B !important;
      }
      html.theme-ocean .btn-outline-primary:hover {
        background-color: #00796B !important;
        color:            #FFFFFF !important;
      }
      /* ========== INPUTS & DROPDOWNS ========== */
      html.theme-ocean input,
      html.theme-ocean select,
      html.theme-ocean textarea {
        background-color: #E0F7FA !important;
        color:            #004D40  !important;
        border-color:     #00695C  !important;
      }
      html.theme-ocean .selectize-input,
      html.theme-ocean .selectize-dropdown {
        background-color: #E0F7FA !important;
        color:            #004D40  !important;
        border-color:     #00695C  !important;
      }
      /* ========== DATE-RANGE “to” ADDON ========== */
      html.theme-ocean .input-group-addon,
      html.theme-ocean .input-group-text {
        background-color: #E0F7FA !important;
        color:            #004D40  !important;
        border-color:     #00695C  !important;
      }
      
      
      /* SOLARIZED: cream backgrounds, golden accents */
      html.theme-solarized {
        background-color: #FDF6E3 !important;
      }
      html.theme-solarized body {
        background-color: #FDF6E3 !important;
      }
      html.theme-solarized .sidebar {
        background-color: #073642 !important;
      }
      html.theme-solarized .card {
        border-left-color: #268BD2 !important;
      }
      html.theme-solarized .header-banner {
        background: linear-gradient(90deg, #073642, #586E75) !important;
        color:      #EEE8D5 !important;
      }
      html.theme-solarized .section-title {
        color: #B58900 !important;
      }
      html.theme-solarized .sidebar .list-group-item {
        background-color: #073642 !important;
        color:            #EEE8D5 !important;
        border-color:     #586E75 !important;
      }
      html.theme-solarized .sidebar .list-group-item.active {
        background-color: #B58900 !important;
        color:            #073642 !important;
        border-color:     #B58900 !important;
      }
      html.theme-solarized .sidebar .list-group-item:hover {
        background-color: #586E75 !important;
        color:            #EEE8D5 !important;
      }
      /* ========== BUTTONS ========== */
      html.theme-solarized .btn-primary {
        background-color: #B58900 !important;
        border-color:     #B58900 !important;
        color:            #FFFFFF !important;
      }
      html.theme-solarized .btn-primary:hover {
        background-color: #9C7400 !important;
        border-color:     #9C7400 !important;
      }
      html.theme-solarized .btn-outline-primary {
        color:         #B58900 !important;
        border-color:  #B58900 !important;
      }
      html.theme-solarized .btn-outline-primary:hover {
        background-color: #B58900 !important;
        color:            #FFFFFF !important;
      }
      /* ========== INPUTS & DROPDOWNS ========== */
      html.theme-solarized input,
      html.theme-solarized select,
      html.theme-solarized textarea {
        background-color: #EEE8D5 !important;
        color:            #073642 !important;
        border-color:     #586E75 !important;
      }
      html.theme-solarized .selectize-input,
      html.theme-solarized .selectize-dropdown {
        background-color: #EEE8D5 !important;
        color:            #073642 !important;
        border-color:     #586E75 !important;
      }
      /* ========== DATE-RANGE “to” ADDON ========== */
      html.theme-solarized .input-group-addon,
      html.theme-solarized .input-group-text {
        background-color: #EEE8D5 !important;
        color:            #073642  !important;
        border-color:     #586E75  !important;
      }
      
      /*–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––*/
      /* Styling shiny-date-range input */
      /*–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––*/
      
      .input-group-text {
        display:         flex   !important;
        align-items:     center !important;
        margin-right: 0.57rem !important;
        font-size:       0      !important;
      }
      .input-group-text::before {
        content:    '↔️'    !important;  /* en-dash */
        font-size:  1rem  !important;
        color:      currentColor !important;
      }
      .shiny-date-range .input-group {
        align-items: center !important;
      }
      .shiny-date-range .input-group-text {
        align-self:   center !important;
      }
      
      .shiny-date-range .input-group-text {
        /* strip away Bootstrap’s box so we only see our dash */
        background:   transparent !important;
        border:       none        !important;
        font-size:    0           !important;
        position:     relative    !important;
      }
      
      .shiny-date-range .input-group-text::before {
        content:      '<–>'         !important;
        font-size:    1rem        !important;
        color:        currentColor !important;
        position:     absolute    !important;
        top:          50%         !important;
        left:         25%         !important;
        transform:    translate(-50%, -50%) !important;
      }
    "))
  ),
  uiOutput("mainUI")
)

server <- function(input, output, session) {
  activity_completion <- reactiveValues(data = list())
  credentials <- reactiveValues(logged_in = FALSE, user_id = NULL, date = NULL)
  credentials$theme <- "default"
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Track selected course manually (initial: 28301)
  selectedCourse <- reactiveVal("28301")
  
  #course starting date
  COURSE_START_DATE <- as.Date("2024-11-11")
  
  # Handle login
  observeEvent(input$login_button, {
    req(input$user_id_input, input$date_input)
    credentials$logged_in <- TRUE
    credentials$user_id <- input$user_id_input
    credentials$date <- input$date_input
    
    # auto starts activity graph to assignments by submission for last 7 days
    updateSelectInput(session, "data_type",
                      selected = "Assignments")
    updateDateRangeInput(session, "date_range",
                         start = as.Date(credentials$date) - 7,
                         end   = as.Date(credentials$date))
    updateSelectInput(session, "metric",
                      selected = "Number of Submissions")
    shinyjs::click("submit_query")
    
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
    updateDateRangeInput(session, "date_range",
                         start = as.Date(input$date_input) - 7,
                         end   = as.Date(input$date_input))
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
    } else {
      output$satisfaction_feedback <- renderText({ "" })
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
            div(class = "header-banner d-flex justify-content-between align-items-center",
                h3("Self-Regulated Learning Dashboard"),
                # right‐side controls
                div(class = "d-flex align-items-stretch", style = "gap:0.5rem;",
                    # date picker: make its wrapper a flex box
                    #div(class = "shiny-date-input d-flex align-items-center", 
                    #    dateInput("date_input hidde", NULL,
                    #              value  = credentials$date,
                    #              format = "yyyy-mm-dd",
                    #              width  = "120px")
                    #),
                    # logout button: also flex + center
                    actionButton("logout_button", "Logout",
                                 class = "btn btn-logout d-flex align-items-center justify-content-center")
                )
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
                        div(class = "p", textOutput("selectedUserId"))
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
                        textOutput("current_week_display"),
                        br(),
                        uiOutput("satisfaction_ui"),
                        uiOutput("expectation_ui"),
                        uiOutput("save_button_ui"),
                        br(),
                        div(class = "text-muted mb-3", uiOutput("satisfaction_feedback")),
                        uiOutput("streak_badge")
                    )
                  ),
                  # Right column
                  column(
                    width = 6,
                    # ————————————————————————————————————————————————
                    # Activity Tracker Graph Builder
                    # ————————————————————————————————————————————————
                    div(class = "card p-3 mb-5",
                        div(class = "section-title", "Activity Tracker Graph Builder"),
                        fluidRow(
                          column(4,
                                 selectInput("data_type", "Select Data Type",
                                             choices = c("Quizzes", "Files", "Discussions", "Assignments"),
                                             selected = "Quizzes")
                          ),
                          column(4,
                                 dateRangeInput("date_range", "Date Range",
                                                start  = Sys.Date() - 7,
                                                end    = Sys.Date(),
                                                format = "yyyy-mm-dd")
                          ),
                          column(4,
                                 uiOutput("metric_ui")
                          )
                        ),
                        br(),
                        plotlyOutput("activityPlot", height = "400px")
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
             ),
             # Theme picker inline
             div(class = "list-group-item theme-selector",
                 selectInput("theme_choice", tags$strong("🎨 Theme Picker"),
                             choices = c(
                               "Default"    = "default",
                               "Light"      = "light",
                               "Dark"       = "dark",
                               "Ocean"      = "ocean",
                               "Solarized"  = "solarized"
                             ),
                             selected = credentials$theme,
                             width = "100%"),
                 actionButton("apply_theme", "Apply", class = "btn btn-outline-primary btn-sm mt-2")
             )
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
    course_id_value <- course_id()
    user_id_value <- user_id()
    date_value <- credentials$date
    week <- weekNumber()
    
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
    
    paste("Welcome", user_id_value, "| Date:", date_value, "| Currently in week:", week, "| Total Active Time:", seconds, "seconds")
  })
  
  output$activityTracker <- renderUI({
    req(user_id(), credentials$date)
    
    course <- as.character(course_id())
    user <- user_id()
    week <- weekNumber()
    
    # Two-week block logic (1–2, 3–4, etc.)
    block_start <- (((week - 1) %/% 2) * 2) + 1
    block_end <- block_start + 1
    block <- as.character(block_start)
    
    # Timestamp helper
    get_week_timestamps <- function(week_num, course_start_date) {
      start_of_week <- course_start_date + (7 * (week_num - 1))
      end_of_week <- start_of_week + 7
      list(
        start_time = paste0(start_of_week, " 00:00:00.000"),
        end_time = paste0(end_of_week, " 00:00:00.000")
      )
    }
    
    # DB check helpers
    is_quiz_submitted <- function(user_id, start_time, end_time, db_con) {
      query <- sprintf("
      SELECT COUNT(*) > 0 AS submitted FROM sandbox_la_conijn_cbl.silver_canvas_quiz_submissions
      WHERE user_id = '%s' AND workflow_state IN ('pending_review', 'complete') AND 
      finished_at_anonymous >= '%s' AND finished_at_anonymous < '%s';",
                       user_id, start_time, end_time
      )
      result <- tryCatch({
        dbGetQuery(db_con, query)
      }, error = function(e) {
        cat("Error querying quiz subs:", e$message, "\n")
        data.frame(submitted = FALSE)
      })
      if (nrow(result) > 0) result$submitted[1] else FALSE
    }
    
    is_assignment_submitted <- function(user_id, start_time, end_time, db_con) {
      query <- sprintf("
      SELECT COUNT(*) > 0 AS submitted FROM sandbox_la_conijn_cbl.silver_canvas_submissions
      WHERE user_id = '%s' AND workflow_state IN ('pending_review', 'graded', 'submitted') AND 
      submitted_at_anonymous >= '%s' AND submitted_at_anonymous < '%s';",
                       user_id, start_time, end_time
      )
      result <- tryCatch({
        dbGetQuery(db_con, query)
      }, error = function(e) {
        cat("Error querying assignment subs:", e$message, "\n")
        data.frame(submitted = FALSE)
      })
      if (nrow(result) > 0) result$submitted[1] else FALSE
    }
    
    reflected <- function(week_num) {
      isTRUE(reflection_saved$data[[course]][[as.character(week_num)]])
    }
    
    make_week_activities <- function(week_num) {
      timestamps <- get_week_timestamps(week_num, COURSE_START_DATE)
      quiz_done <- is_quiz_submitted(user, timestamps$start_time, timestamps$end_time, sc)
      assignment_done <- is_assignment_submitted(user, timestamps$start_time, timestamps$end_time, sc)
      list(
        list(name = sprintf("Submit Week %d Quiz", week_num), done = quiz_done),
        list(name = sprintf("Submit Week %d Assignment", week_num), done = assignment_done),
        list(name = sprintf("Reflect on Week %d", week_num), done = reflected(week_num))
      )
    }
    
    # Build activity list
    all_activities <- c(make_week_activities(block_start), make_week_activities(block_end))
    total <- length(all_activities)
    completed <- sum(sapply(all_activities, function(x) x$done))
    percent <- if (total > 0) round((completed / total) * 100) else 0
    
    # Completion tracking logic
    completion <- 0
    if (percent == 100) {
      completion <- 1
      if (is.null(activity_completion$data[[course]])) {
        activity_completion$data[[course]] <- list()
      }
      activity_completion$data[[course]][[block]] <- TRUE
      saveActivityCompletion(user_id(), course, block, TRUE)
    } else if (completion == 1) {
      completion <- 0
      isolate({ activity_completion$data[[course]][[block]] <- NULL })
      file_path <- "activity_completion_data.json"
      if (file.exists(file_path)) {
        data <- jsonlite::read_json(file_path, simplifyVector = FALSE)
        if (!is.null(data[[user]][[course]][[block]])) {
          data[[user]][[course]][[block]] <- NULL
          if (length(data[[user]][[course]]) == 0) data[[user]][[course]] <- NULL
          if (length(data[[user]]) == 0) data[[user]] <- NULL
          jsonlite::write_json(data, file_path, pretty = TRUE, auto_unbox = TRUE)
        }
      }
    } else {
      activity_completion$data[[course]][[block]] <- FALSE
    }
    
    # Feedback text
    activity_feedback_text <- if (completed == total && total > 0) {
      "Great job! You've completed all activities for this block. 🎉"
    } else if (completed > 0) {
      "Excellent! You've checked another activity off the list. Keep the momentum going! 💪"
    } else {
      "It looks like you're just getting started! For a good overview, perhaps look at the activity breakdown to see where you can best invest your time. 🗺️"
    }
    feedback_html <- sprintf("<div class='alert alert-info mt-3' role='alert'>%s</div>", activity_feedback_text)
    
    # UI generation
    list_items <- paste0(
      "<ul class='list-group mb-3'>",
      paste(sapply(all_activities, function(x) {
        if (x$done) {
          sprintf("<li class='list-group-item d-flex justify-content-between align-items-center text-success'>✅ %s</li>", x$name)
        } else {
          sprintf("<li class='list-group-item d-flex justify-content-between align-items-center text-muted'>⬜ %s</li>", x$name)
        }
      }), collapse = ""), "</ul>"
    )
    progress_bar <- sprintf("<div class='progress'><div class='progress-bar bg-success' role='progressbar' style='width: %d%%;' aria-valuenow='%d' aria-valuemin='0' aria-valuemax='100'>%d%%</div></div>", percent, percent, percent)
    
    HTML(paste0(list_items, progress_bar, feedback_html))
  })
  
  
  # ----------------------------
  # Dynamic Metric Selector UI
  # ----------------------------
  output$metric_ui <- renderUI({
    req(input$data_type)
    switch(input$data_type,
           "Quizzes" = selectInput(
             "metric", "Metric",
             choices = c("Time Spent", "Number of Submissions"),
             selected = "Time Spent"
           ),
           "Files" = selectInput(
             "metric", "Metric",
             choices = c("Number of Submissions"),
             selected = "Number of Submissions"
           ),
           "Discussions" = selectInput(
             "metric", "Metric",
             choices = c("Number of Posts"),
             selected = "Number of Posts"
           ),
           "Assignments" = selectInput(
             "metric", "Metric",
             choices = c("Number of Submissions", "Average Score"),
             selected = "Number of Submissions"
           )
    )
  })
  
  # ----------------------------------------
  # Reactive Data Fetcher for ActivityData
  # ----------------------------------------
  activityData <- eventReactive(
    { 
      input$submit_query
      input$data_type
      input$date_range
      input$metric
      selectedCourse()
    },
    {
      req(input$data_type, input$date_range, input$metric)
      
      # build timestamps
      start_ts <- paste0(input$date_range[1], " 00:00:00")
      end_ts   <- paste0(input$date_range[2] + 1, " 00:00:00")
      user     <- credentials$user_id
      course   <- course_id()
      
      # pick table, date column, aggregation & y‐axis label
      if (input$data_type == "Quizzes") {
        tbl      <- "silver_canvas_quiz_submissions"
        date_col <- "finished_at_anonymous"
        if (input$metric == "Time Spent") {
          agg_expr <- "COALESCE(SUM(DurationInMinutes),0) AS value"
          ylab     <- "Time Spent (min)"
        } else {
          agg_expr <- "COUNT(*) AS value"
          ylab     <- "Submissions"
        }
      } else if (input$data_type == "Files") {
        tbl      <- "silver_canvas_web_logs"
        date_col <- "timestamp"
        agg_expr <- "COUNT(*) AS value"
        ylab     <- "File Accesses"
      } else if (input$data_type == "Discussions") {
        tbl      <- "silver_canvas_discussion_entries"
        date_col <- "created_at_anonymous"
        agg_expr <- "COUNT(*) AS value"
        ylab     <- "Posts"
      } else if (input$data_type == "Assignments") {
        tbl      <- "silver_canvas_submissions"
        date_col <- "submitted_at_anonymous"
        if (input$metric == "Number of Submissions") {
          agg_expr <- "COUNT(*) AS value"
          ylab     <- "Submissions"
        } else {
          agg_expr <- "COALESCE(AVG(score_anonymous),0) AS value"
          ylab     <- "Average Score"
        }
      }
      
      # construct and run query
      query <- sprintf("
    SELECT
      CAST(%s AS DATE) AS day,
      %s
    FROM sandbox_la_conijn_cbl.%s
    WHERE user_id = '%s'
      AND %s BETWEEN '%s' AND '%s'
      AND course_id = %s
    GROUP BY CAST(%s AS DATE)
    ORDER BY day;
  ", date_col, agg_expr, tbl, user, date_col, start_ts, end_ts, course, date_col)
      
      df <- tryCatch(
        dbGetQuery(sc, query),
        error = function(e) {
          # if error, return zeros for full range
          days <- seq.Date(input$date_range[1], input$date_range[2], by = "day")
          data.frame(day = days, value = 0)
        }
      )
      
      # ensure every day is present
      all_days <- data.frame(day = seq.Date(input$date_range[1], input$date_range[2], by = "day"))
      df       <- merge(all_days, df, by = "day", all.x = TRUE)
      df$value[is.na(df$value)] <- 0
      
      list(df = df, ylab = ylab)
    })
  
  # single unified plot
  output$activityPlot <- renderPlotly({
    suppressWarnings({
      dat <- activityData()
      req(dat)
      th     <- credentials$theme
      accent <- switch(th,
                       "light"     = "#2980B9",
                       "dark"      = "#E67E22",
                       "ocean"     = "#00796B",
                       "solarized" = "#B58900",
                       "#2980B9"   # default
      )
      bg     <- switch(th,
                       "light"     = "#FFFFFF",
                       "dark"      = "#2C2C2C",
                       "ocean"     = "#E0F7FA",
                       "solarized" = "#FDF6E3",
                       "#F0F2F5"
      )
      txt    <- switch(th,
                       "light"     = "#2C3E50",
                       "dark"      = "#EEEEEE",
                       "ocean"     = "#004D40",
                       "solarized" = "#073642",
                       "#2C3E50"
      )
      palette <- switch(th,
                        "light"     = c("#2980B9",  # blue accent
                                        "#27AE60",  # green
                                        "#E67E22",  # orange
                                        "#8E44AD"), # purple
                        "dark"      = c("#E67E22",  # orange accent
                                        "#3498DB",  # sky blue
                                        "#2ECC71",  # bright green
                                        "#9B59B6"), # magenta
                        "ocean"     = c("#00796B",  # teal accent
                                        "#0288D1",  # ocean blue
                                        "#FF5722",  # coral
                                        "#FFC107"), # amber
                        "solarized" = c("#B58900",  # gold accent
                                        "#268BD2",  # blue
                                        "#2AA198",  # cyan
                                        "#CB4B16"), # orange
                        c("#2980B9","#27AE60","#E67E22","#8E44AD")
      )
      df_full <- dat$df
      ylab    <- dat$ylab
      
      # For Average Score I already dropped zeros in activityData(),
      # but here I want the same rule for all metrics:
      df_active <- df_full[df_full$value > 0, ]
      
      # Compute summaries only on active days:
      if (nrow(df_active) > 0) {
        summary_avg <- round(mean(df_active$value), 2)
        summary_med <- round(median(df_active$value), 2)
        summary_min <- round(min(df_active$value), 2)
        summary_max <- round(max(df_active$value), 2)
      } else {
        summary_avg <- summary_med <- summary_min <- summary_max <- 0
      }
      
      # Base trace: lines+markers for counts/time; markers only for Average Score
      base <- if (input$metric == "Average Score") {
        plot_ly(
          df_active,
          x    = ~day, y = ~value,
          type = 'scatter', mode = 'markers',
          name = 'Daily Avg'
        )
      } else {
        plot_ly(
          df_full,
          x    = ~day, y = ~value,
          type = 'scatter', mode = 'lines+markers',
          line   = list(color = accent),
          marker = list(color = accent),
          name = input$metric
        )
      }
      
      # Overlay summaries across full date axis
      p <- base %>%
        add_trace(
          x = df_full$day,
          y = rep(summary_avg, nrow(df_full)),
          type = 'scatter',
          mode = 'lines+markers',
          name = sprintf("Avg (%.2f)", summary_avg),
          line = list(dash = 'dash', color = palette[1]),
          marker = list(color = palette[1]),
          opacity = 1
        ) %>%
        add_trace(
          x = df_full$day,
          y = rep(summary_med, nrow(df_full)),
          type = 'scatter',
          mode = 'lines+markers',
          name = sprintf("Med (%.2f)", summary_med),
          line = list(dash = 'dot', color = palette[2]),
          marker = list(color = palette[2]),
          opacity = 1
        ) %>%
        add_trace(
          x = df_full$day,
          y = rep(summary_min, nrow(df_full)),
          type = 'scatter',
          mode = 'lines+markers',
          name = sprintf("Min (%.2f)", summary_min),
          line = list(dash = 'dashdot', color = palette[3]),
          marker = list(color = palette[3]),
          opacity = 1
        ) %>%
        add_trace(
          x = df_full$day,
          y = rep(summary_max, nrow(df_full)),
          type = 'scatter',
          mode = 'lines+markers',
          name = sprintf("Max (%.2f)", summary_max),
          line = list(dash = 'longdash', color = palette[4]),
          marker = list(color = palette[4]),
          opacity = 1
        )
      
      
      # Count how many active days vs. total days
      total_days  <- nrow(df_full)
      active_days <- nrow(df_active)
      stats_text  <- sprintf(
        "Active Days: %d/%d | Avg=%.2f | Med=%.2f | Min=%.2f | Max=%.2f",
        active_days, total_days,
        summary_avg, summary_med, summary_min, summary_max
      )
      
      p %>% layout(
        # Plot title configuration
        title = list(
          text    = paste(input$data_type, input$metric, "over Time"),
          font    = list(size = 14),
          x       = 0.5,          # center the title
          xanchor = "center"
        ),
        
        # Margins around the plotting area (in pixels)
        margin = list(
          t = 100,  # top margin to give space above the plot (for toolbar/annotations)
          b = 80,   # bottom margin for x-axis title and legend
          l = 80,   # left margin for y-axis title
          r = 40    # right margin for any additional elements
        ),
        
        # Plotly theme matching recolor
        paper_bgcolor = bg,
        plot_bgcolor  = bg,
        font = list(color = txt),
        
        # Annotation for stats text
        annotations = list(
          list(
            x       = 0.47,        # center of the plotting area
            xref    = "paper",
            xanchor = "center",   # anchor annotation by its center
            y       = 1.12,       # just above the plot
            yref    = "paper",
            text    = stats_text,
            showarrow = FALSE,
            font    = list(size = 11)
          )
        ),
        
        # X-axis styling
        xaxis = list(
          title = list(
            text     = "Date",       # label for the x-axis
            font     = list(size = 12), # font size for x-axis label
            standoff = 10            # padding (in pixels) between tick labels and axis title
          ),
          tickfont   = list(size = 10),  # font size for tick labels
          automargin = TRUE              # let Plotly auto-adjust margins if labels get cut off
        ),
        
        # Y-axis styling
        yaxis = list(
          title = list(
            text     = ylab,         # dynamic y-axis label (e.g., "Submissions")
            font     = list(size = 12), # font size for y-axis label
            standoff = 30            # padding between tick labels and axis title
          ),
          tickfont   = list(size = 10),  # font size for tick labels
          automargin = TRUE              # auto-adjust margins if needed
        ),
        
        # Legend placement and styling
        legend = list(
          font        = list(size = 10), # font size for legend items
          orientation = "h",             # horizontal layout
          x           = 0.5,             # horizontal center of legend (in paper coords)
          xanchor     = "center",        # anchor legend at its center horizontally
          y           = -0.3             # vertical position into bottom margin (negative moves below plot)
        ),
        
        # Hover label styling
        hoverlabel = list(
          font = list(size = 11)         # font size for hover tooltips
        ),
        
        # Hover behavior
        hovermode  = "x unified"         # show a single vertical tooltip box for all traces at the same x
      )
    })
  })
  
  # Satisfaction input or plain text if saved
  output$satisfaction_ui <- renderUI({
    course <- selectedCourse()
    week <- as.character(weekNumber())
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
    week <- as.character(weekNumber())
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
    week <- as.character(weekNumber())
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
    week <- as.character(weekNumber())
    
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
      HTML(sprintf("<div class='alert alert-warning mt-3'>🏅 <strong>%d-Week Streak!</strong> Reflection badge for %d consecutive submissions.</div>", streak, streak))
    } else {
      return(NULL)
    }
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
      list(threshold = 1, unlocked_img = "ROOKIE.png", unlocked_tooltip = "Your first reflection - Nice start!", locked_tooltip = "Reflect for the first time."),
      list(threshold = 4, unlocked_img = "CONSISTENT.png", unlocked_tooltip = "4 weeks of reflections - Keep reflecting!", locked_tooltip = "Reflect for 4 weeks."),
      list(threshold = 8, unlocked_img = "MASTER.png", unlocked_tooltip = "8 weeks of reflecting?! – Good job!", locked_tooltip = "Reflect for 8 weeks."),
      list(id = "selfaware", threshold = 1, unlocked_img = "SELF-AWARE.png", unlocked_tooltip = "You have completed all your activities - Nice start!", locked_tooltip = "Complete the activity tracker for the first time."),
      list(id = "insightchampion", threshold = 2, unlocked_img = "INSIGHT.png", unlocked_tooltip = "You have finished your second block of activities - Very nice!", locked_tooltip = "Complete the activity tracker for the second time."),
      list(id = "kingofwisdom", threshold = 4, unlocked_img = "KING.png", unlocked_tooltip = "You have complete all the activities for this course - Amazing!", locked_tooltip = "Complete all the activities for the entire course.")
    )
    
    badge_html <- "<p><strong>Your Reflection Badges</strong></p><div class='badge-container'>"
    for (b in badges) {
      unlocked <- if (!is.null(b$id)) {
        completed_block_count >= b$threshold
      } else {
        reflection_count >= b$threshold
      }
      
      if (unlocked) {
        badge_html <- paste0(badge_html, "<span class='badge-item' data-tooltip='", b$unlocked_tooltip, "'><img src='", b$unlocked_img, "' height='90px'/></span>")
      } else {
        badge_html <- paste0(badge_html, "<span class='badge-item' data-tooltip='", b$locked_tooltip, "'><img src='QUESTION.png' height='90px'/></span>")
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
  
  output$current_week_display <- renderText({
    paste("Week", weekNumber())
  })
}

shinyApp(ui, server)
