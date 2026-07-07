# =============================================================
# Planora - Plan. Focus. Achieve.
# A production-quality R Shiny study planner application
# Author: Generated for user project
# =============================================================
# Features: Welcome page, Dashboard, Task CRUD, Search & Filters,
# Statistics, Charts, Deadlines, Achievements, Dark Glassmorphism UI
# =============================================================

# ---------------------- Libraries ----------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinyjs)

# ---------------------- Constants ------------------------------
TASKS_FILE <- "tasks.csv"
# Subjects are fully user-defined -- there is no predefined/starter list.
# The dropdown starts empty and the user types any subject name they like
# (via the "create new" option). Once a subject has been used on a task it
# is automatically remembered and offered again for future selection.
SUBJECTS   <- character(0)
CATEGORIES <- c("Homework", "Assignment", "Reading", "Project", "Writing",
                 "Exam Prep", "Other")
PRIORITIES <- c("High", "Medium", "Low")
DAILY_GOAL_HOURS <- 4   # default daily study goal

# ---------------------- Helper Functions -----------------------

# Load tasks safely from CSV, create file if missing
empty_tasks_df <- function() {
  data.frame(
    id = integer(0), task = character(0), subject = character(0),
    category = character(0), priority = character(0),
    deadline = character(0), study_hours = numeric(0),
    status = character(0), created_on = character(0),
    stringsAsFactors = FALSE
  )
}

load_tasks <- function() {
  if (!file.exists(TASKS_FILE)) {
    empty_df <- empty_tasks_df()
    write.csv(empty_df, TASKS_FILE, row.names = FALSE)
    return(empty_df)
  }
  df <- tryCatch(
    read.csv(TASKS_FILE, stringsAsFactors = FALSE),
    error = function(e) empty_tasks_df()
  )
  # Guard against a corrupt/empty CSV that reads back with no columns at all
  required_cols <- names(empty_tasks_df())
  if (nrow(df) == 0 || !all(required_cols %in% names(df))) {
    return(empty_tasks_df())
  }
  df$deadline <- as.character(df$deadline)
  df$study_hours <- as.numeric(df$study_hours)
  df$id <- as.integer(df$id)
  df
}

# Save tasks back to CSV
save_tasks <- function(df) {
  write.csv(df, TASKS_FILE, row.names = FALSE)
}

# Get the next available id
get_next_id <- function(df) {
  if (nrow(df) == 0) return(1)
  max(df$id, na.rm = TRUE) + 1
}

# Badge/achievement calculation based on completed task count
get_achievements <- function(completed_count, streak_hours) {
  badges <- list(
    list(name = "First Step", desc = "Complete your first task", threshold = 1, icon = "seedling"),
    list(name = "Getting Started", desc = "Complete 5 tasks", threshold = 5, icon = "walking"),
    list(name = "Consistent", desc = "Complete 10 tasks", threshold = 10, icon = "fire"),
    list(name = "Achiever", desc = "Complete 25 tasks", threshold = 25, icon = "trophy"),
    list(name = "Planora Master", desc = "Complete 50 tasks", threshold = 50, icon = "crown")
  )
  lapply(badges, function(b) {
    b$unlocked <- completed_count >= b$threshold
    b
  })
}

# ---------------------- UI --------------------------------------
ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$title("Planora - Plan. Focus. Achieve.")
  ),

  navbarPage(
    id = "main_nav",
    title = NULL,
    windowTitle = "Planora",
    header = div(
      class = "navbar-planora",
      div(
        span(class = "brand-title", "Planora"),
        br(),
        span(class = "brand-tagline", "Plan. Focus. Achieve.")
      ),
      div(
        materialSwitch(inputId = "dark_mode_toggle", label = "Dark Mode",
                        value = TRUE, status = "primary", right = TRUE)
      )
    ),
    collapsible = TRUE,

    # ---------------- Welcome Tab -----------------
    tabPanel(
      "Welcome",
      div(
        class = "welcome-wrap",
        icon("graduation-cap", class = "fa-4x", style = "color:#a78bfa; margin-bottom:20px;"),
        div(class = "welcome-title", "Planora"),
        div(class = "welcome-sub", "Plan. Focus. Achieve. Your personal study companion."),
        actionButton("go_dashboard", "Get Started", class = "btn-start", icon = icon("arrow-right")),
        br(), br(),
        fluidRow(
          style = "max-width: 900px; margin-top: 40px;",
          column(4, div(class = "glass-card", icon("tasks", class="fa-2x"), h4("Organize Tasks"),
                        p("Add, edit, and track your study tasks effortlessly."))),
          column(4, div(class = "glass-card", icon("chart-pie", class="fa-2x"), h4("Visual Insights"),
                        p("Understand your progress with charts and stats."))),
          column(4, div(class = "glass-card", icon("award", class="fa-2x"), h4("Stay Motivated"),
                        p("Earn badges and hit your daily study goals.")))
        )
      )
    ),

    # ---------------- Dashboard Tab -----------------
    tabPanel(
      "Dashboard",
      value = "dashboard_tab",
      fluidRow(
        column(3, div(class = "glass-card stat-card",
                      icon("list-check", class = "stat-icon"),
                      div(class = "stat-number", textOutput("stat_total")),
                      div(class = "stat-label", "Total Tasks"))),
        column(3, div(class = "glass-card stat-card",
                      icon("circle-check", class = "stat-icon"),
                      div(class = "stat-number", textOutput("stat_completed")),
                      div(class = "stat-label", "Completed"))),
        column(3, div(class = "glass-card stat-card",
                      icon("hourglass-half", class = "stat-icon"),
                      div(class = "stat-number", textOutput("stat_pending")),
                      div(class = "stat-label", "Pending"))),
        column(3, div(class = "glass-card stat-card",
                      icon("triangle-exclamation", class = "stat-icon"),
                      div(class = "stat-number", textOutput("stat_overdue")),
                      div(class = "stat-label", "Overdue")))
      ),

      fluidRow(
        column(
          8,
          div(
            class = "glass-card",
            div(class = "section-title", "Overall Progress"),
            progressBar(id = "overall_progress", value = 0, display_pct = TRUE,
                        status = "custom", title = ""),
            br(),
            div(class = "section-title", "Daily Study Goal"),
            progressBar(id = "daily_goal_progress", value = 0, display_pct = TRUE,
                        status = "custom", title = paste0("Goal: ", DAILY_GOAL_HOURS, " hrs/day"))
          ),
          div(
            class = "glass-card",
            div(class = "section-title", "Task Breakdown by Subject"),
            plotOutput("pie_chart", height = "300px")
          ),
          div(
            class = "glass-card",
            div(class = "section-title", "Study Hours by Category"),
            plotOutput("bar_chart", height = "300px")
          )
        ),
        column(
          4,
          div(
            class = "glass-card",
            div(class = "section-title", "Upcoming Deadlines"),
            uiOutput("upcoming_deadlines")
          ),
          div(
            class = "glass-card",
            div(class = "section-title", "Overdue Alerts"),
            uiOutput("overdue_alerts")
          ),
          div(
            class = "glass-card",
            div(class = "section-title", "Achievements"),
            uiOutput("achievement_badges")
          )
        )
      )
    ),

    # ---------------- Manage Tasks Tab -----------------
    tabPanel(
      "Manage Tasks",
      value = "manage_tab",
      fluidRow(
        column(
          4,
          div(
            class = "glass-card task-form-card",
            div(class = "task-form-header",
                icon("wand-magic-sparkles", class = "task-form-header-icon"),
                div(class = "section-title", "Add / Edit Task")),
            textInput("task_name", "Task Name", placeholder = "e.g. Revise Chapter 4"),
            selectizeInput("task_subject", "Subject", choices = SUBJECTS,
                          options = list(create = TRUE,
                                        createOnBlur = TRUE,
                                        dropdownParent = "body",
                                        placeholder = "Type a subject name (e.g. Mathematics)")),
            selectizeInput("task_category", "Category", choices = CATEGORIES,
                          options = list(create = TRUE,
                                        dropdownParent = "body",
                                        placeholder = "Select or type a new category")),
            div(style = "margin-top: 6px;",
              selectizeInput("task_priority", "Priority", choices = PRIORITIES,
                            options = list(dropdownParent = "body"))
            ),
            dateInput("task_deadline", "Deadline", value = Sys.Date() + 3),
            numericInput("task_hours", "Study Hours", value = 1, min = 0, max = 24, step = 0.5),
            fluidRow(
              column(6, actionButton("add_task_btn", "Add Task", icon = icon("plus"),
                                     class = "btn-planora", width = "100%")),
              column(6, actionButton("update_task_btn", "Update Task", icon = icon("pen"),
                                     class = "btn-planora", width = "100%"))
            ),
            br(),
            actionButton("clear_form_btn", "Clear Form", icon = icon("eraser"),
                        class = "btn-planora-ghost", width = "100%")
          )
        ),
        column(
          8,
          div(
            class = "glass-card",
            div(class = "section-title", "Search & Filter"),
            fluidRow(
              column(4, textInput("search_box", NULL, placeholder = "Search tasks...")),
              column(4, selectizeInput("filter_subject", NULL, choices = c("All Subjects", SUBJECTS),
                                       options = list(dropdownParent = "body"))),
              column(4, selectInput("filter_status", NULL,
                                    choices = c("All Status", "Pending", "Completed")))
            )
          ),
          div(
            class = "glass-card",
            div(class = "section-title", "Your Tasks"),
            DTOutput("task_table"),
            br(),
            fluidRow(
              column(4, actionButton("mark_complete_btn", "Mark Complete",
                                     icon = icon("check"), class = "btn-success-planora", width = "100%")),
              column(4, actionButton("edit_selected_btn", "Load for Edit",
                                     icon = icon("edit"), class = "btn-planora", width = "100%")),
              column(4, actionButton("delete_task_btn", "Delete Selected",
                                     icon = icon("trash"), class = "btn-danger-planora", width = "100%"))
            )
          )
        )
      )
    ),

    # ---------------- About Tab -----------------
    tabPanel(
      "About",
      div(
        style = "max-width: 800px; margin: 30px auto;",
        div(
          class = "glass-card",
          h3("About Planora"),
          p("Planora is a personal study planning application designed to help students
             organize tasks, track deadlines, and build consistent study habits."),
          p("Built with R Shiny, featuring a modern purple-black glassmorphism interface,
             this tool combines productivity features with motivating visual feedback."),
          tags$ul(
            tags$li("Track tasks by subject, category, and priority"),
            tags$li("Visualize progress with interactive charts"),
            tags$li("Stay on top of deadlines with smart alerts"),
            tags$li("Earn achievement badges as you complete tasks")
          )
        )
      )
    )
  ),
  div(class = "footer-planora", "Planora \u00A9 2026 - Plan. Focus. Achieve.")
)

# ---------------------- SERVER -----------------------------------
server <- function(input, output, session) {

  # Reactive store of tasks, initialized from CSV
  tasks_data <- reactiveVal(load_tasks())

  # Track which task id is currently loaded for editing (NULL = none)
  editing_id <- reactiveVal(NULL)

  # ---------- Navigation ----------
  observeEvent(input$go_dashboard, {
    updateNavbarPage(session, "main_nav", selected = "dashboard_tab")
  })

  # ---------- Dark mode toggle ----------
  observeEvent(input$dark_mode_toggle, {
    if (isTRUE(input$dark_mode_toggle)) {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  }, ignoreNULL = FALSE)

  # ---------- Keep Subject/Category choices in sync with custom entries ----------
  # Whenever a new subject or category gets typed in (via the "create new"
  # dropdown option), fold it into the choice lists so it shows up again
  # next time, instead of only existing as free text on that one task.
  observe({
    df <- tasks_data()
    # No predefined subjects -- the choice list is built entirely from
    # subjects the user has previously typed in.
    used_subjects <- if (nrow(df) > 0) sort(unique(df$subject)) else character(0)

    updateSelectizeInput(session, "task_subject",
                        choices = used_subjects,
                        selected = isolate(input$task_subject),
                        options = list(create = TRUE,
                                      createOnBlur = TRUE,
                                      dropdownParent = "body",
                                      placeholder = "Type a subject name (e.g. Mathematics)"))
    updateSelectizeInput(session, "filter_subject",
                        choices = c("All Subjects", used_subjects),
                        selected = isolate(input$filter_subject))

    used_categories <- if (nrow(df) > 0) unique(df$category) else character(0)
    all_categories <- sort(unique(c(CATEGORIES, used_categories)))
    updateSelectizeInput(session, "task_category",
                        choices = all_categories,
                        selected = isolate(input$task_category),
                        options = list(create = TRUE,
                                      placeholder = "Select or type a new category"))
  })

  # ---------- Filtered tasks (search + filters) ----------
  filtered_tasks <- reactive({
    df <- tasks_data()
    if (nrow(df) == 0) return(df)

    # Search filter
    if (!is.null(input$search_box) && nzchar(trimws(input$search_box))) {
      q <- tolower(trimws(input$search_box))
      df <- df[grepl(q, tolower(df$task)) |
                 grepl(q, tolower(df$subject)) |
                 grepl(q, tolower(df$category)), , drop = FALSE]
    }

    # Subject filter
    if (!is.null(input$filter_subject) && input$filter_subject != "All Subjects") {
      df <- df[df$subject == input$filter_subject, , drop = FALSE]
    }

    # Status filter
    if (!is.null(input$filter_status) && input$filter_status != "All Status") {
      df <- df[df$status == input$filter_status, , drop = FALSE]
    }

    df
  })

  # ---------- Task Table Render ----------
  output$task_table <- renderDT({
    df <- filtered_tasks()
    if (is.null(df) || nrow(df) == 0) {
      empty_msg <- data.frame(Message = "No tasks found. Add a new task to get started!",
                              stringsAsFactors = FALSE)
      return(datatable(empty_msg, options = list(dom = "t"), rownames = FALSE))
    }

    display_df <- df %>%
      select(id, task, subject, category, priority, deadline, study_hours, status) %>%
      rename(
        ID = id, Task = task, Subject = subject, Category = category,
        Priority = priority, Deadline = deadline, Hours = study_hours, Status = status
      )

    datatable(
      display_df,
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE,
                    order = list(list(5, "asc"))),
      class = "stripe hover"
    ) %>%
      formatStyle("Priority",
                 color = styleEqual(c("High", "Medium", "Low"),
                                    c("#f87171", "#fbbf24", "#34d399"))) %>%
      formatStyle("Status",
                 backgroundColor = styleEqual(c("Completed", "Pending"),
                                              c("rgba(52,211,153,0.15)", "rgba(251,191,36,0.1)")))
  })

  # The Manage Tasks tab isn't the tab shown on app load (Welcome is), so the
  # table would otherwise get created while hidden -- DT initializes it with
  # zero width in that case, which can leave row selection/clicks unreliable
  # even after switching tabs. Forcing it to render immediately avoids that.
  outputOptions(output, "task_table", suspendWhenHidden = FALSE)

  # Proxy used to clear/refresh selection after CRUD actions so a stale
  # selected row can't linger and point at the wrong task afterwards.
  task_table_proxy <- dataTableProxy("task_table")

  # ---------- Add Task ----------
  observeEvent(input$add_task_btn, {
    if (is.null(input$task_name) || !nzchar(trimws(input$task_name))) {
      showNotification("Please enter a task name.", type = "warning")
      return()
    }
    df <- tasks_data()
    new_row <- data.frame(
      id = get_next_id(df),
      task = trimws(input$task_name),
      subject = input$task_subject,
      category = input$task_category,
      priority = input$task_priority,
      deadline = as.character(input$task_deadline),
      study_hours = as.numeric(input$task_hours),
      status = "Pending",
      created_on = as.character(Sys.Date()),
      stringsAsFactors = FALSE
    )
    df <- rbind(df, new_row)
    tasks_data(df)
    save_tasks(df)
    showNotification("Task added successfully!", type = "message")
    reset_form()
  })

  # ---------- Load Task for Edit ----------
  observeEvent(input$edit_selected_btn, {
    sel <- input$task_table_rows_selected
    df <- filtered_tasks()
    if (is.null(sel) || length(sel) == 0 || nrow(df) == 0) {
      showNotification("Please select a task to edit.", type = "warning")
      return()
    }
    row <- df[sel, ]
    editing_id(row$id)
    updateTextInput(session, "task_name", value = row$task)
    updateSelectizeInput(session, "task_subject", selected = row$subject)
    updateSelectizeInput(session, "task_category", selected = row$category)
    updateSelectizeInput(session, "task_priority", selected = row$priority)
    updateDateInput(session, "task_deadline", value = as.Date(row$deadline))
    updateNumericInput(session, "task_hours", value = row$study_hours)
    showNotification("Task loaded into form for editing.", type = "message")
  })

  # ---------- Update Task ----------
  observeEvent(input$update_task_btn, {
    id_to_edit <- editing_id()
    # If nothing was explicitly loaded via "Load for Edit", fall back to
    # whatever row is currently selected in the table -- selecting a row and
    # clicking Update should just work, without a mandatory extra step.
    if (is.null(id_to_edit)) {
      sel <- input$task_table_rows_selected
      df_filtered <- filtered_tasks()
      if (!is.null(sel) && length(sel) > 0 && nrow(df_filtered) > 0) {
        id_to_edit <- df_filtered[sel, "id"]
      }
    }
    if (is.null(id_to_edit)) {
      showNotification("Select a task in the table, or use 'Load for Edit', before updating.", type = "warning")
      return()
    }
    if (is.null(input$task_name) || !nzchar(trimws(input$task_name))) {
      showNotification("Please enter a task name.", type = "warning")
      return()
    }
    df <- tasks_data()
    idx <- which(df$id == id_to_edit)
    if (length(idx) == 0) {
      showNotification("Task not found.", type = "error")
      return()
    }
    df$task[idx] <- trimws(input$task_name)
    df$subject[idx] <- input$task_subject
    df$category[idx] <- input$task_category
    df$priority[idx] <- input$task_priority
    df$deadline[idx] <- as.character(input$task_deadline)
    df$study_hours[idx] <- as.numeric(input$task_hours)
    tasks_data(df)
    save_tasks(df)
    showNotification("Task updated successfully!", type = "message")
    reset_form()
  })

  # ---------- Delete Task ----------
  observeEvent(input$delete_task_btn, {
    sel <- input$task_table_rows_selected
    df_filtered <- filtered_tasks()
    if (is.null(sel) || length(sel) == 0 || nrow(df_filtered) == 0) {
      showNotification("Please select a task to delete.", type = "warning")
      return()
    }
    id_to_delete <- df_filtered[sel, "id"]
    df <- tasks_data()
    df <- df[df$id != id_to_delete, , drop = FALSE]
    tasks_data(df)
    save_tasks(df)
    selectRows(task_table_proxy, NULL)
    showNotification("Task deleted.", type = "message")
  })

  # ---------- Mark Complete ----------
  observeEvent(input$mark_complete_btn, {
    sel <- input$task_table_rows_selected
    df_filtered <- filtered_tasks()
    if (is.null(sel) || length(sel) == 0 || nrow(df_filtered) == 0) {
      showNotification("Please select a task to mark complete.", type = "warning")
      return()
    }
    id_to_complete <- df_filtered[sel, "id"]
    df <- tasks_data()
    df$status[df$id == id_to_complete] <- "Completed"
    tasks_data(df)
    save_tasks(df)
    selectRows(task_table_proxy, NULL)
    showNotification("Task marked as completed! Great job!", type = "message")
  })

  # ---------- Clear Form ----------
  observeEvent(input$clear_form_btn, {
    reset_form()
  })

  reset_form <- function() {
    updateTextInput(session, "task_name", value = "")
    updateSelectizeInput(session, "task_subject", selected = "")
    updateSelectizeInput(session, "task_category", selected = CATEGORIES[1])
    updateSelectizeInput(session, "task_priority", selected = PRIORITIES[1])
    updateDateInput(session, "task_deadline", value = Sys.Date() + 3)
    updateNumericInput(session, "task_hours", value = 1)
    editing_id(NULL)
  }

  # ---------- Statistics Outputs ----------
  output$stat_total <- renderText({ nrow(tasks_data()) })

  output$stat_completed <- renderText({
    df <- tasks_data()
    if (nrow(df) == 0) return("0")
    sum(df$status == "Completed", na.rm = TRUE)
  })

  output$stat_pending <- renderText({
    df <- tasks_data()
    if (nrow(df) == 0) return("0")
    sum(df$status == "Pending", na.rm = TRUE)
  })

  output$stat_overdue <- renderText({
    df <- tasks_data()
    if (nrow(df) == 0) return("0")
    sum(df$status == "Pending" & as.Date(df$deadline) < Sys.Date(), na.rm = TRUE)
  })

  # ---------- Progress Bars ----------
  observe({
    df <- tasks_data()
    total <- nrow(df)
    completed <- if (total > 0) sum(df$status == "Completed", na.rm = TRUE) else 0
    pct <- if (total > 0) round((completed / total) * 100) else 0
    updateProgressBar(session, "overall_progress", value = pct,
                      title = paste0(completed, " of ", total, " tasks completed"))

    today_hours <- if (total > 0) {
      sum(df$study_hours[df$created_on == as.character(Sys.Date())], na.rm = TRUE)
    } else 0
    goal_pct <- min(100, round((today_hours / DAILY_GOAL_HOURS) * 100))
    updateProgressBar(session, "daily_goal_progress", value = goal_pct,
                      title = paste0(today_hours, " / ", DAILY_GOAL_HOURS, " hrs today"))
  })

  # ---------- Pie Chart: Tasks by Subject ----------
  output$pie_chart <- renderPlot({
    df <- tasks_data()
    if (nrow(df) == 0) {
      return(ggplot() + theme_void() +
              annotate("text", x = 0, y = 0, label = "No data yet",
                       color = "white", size = 6))
    }
    subj_counts <- df %>% count(subject)

    # Vibrant, distinct palette (not just shades of one hue) that still
    # sits comfortably on the dark glassmorphism background.
    planora_palette <- c("#a78bfa", "#f472b6", "#38bdf8", "#34d399",
                        "#fbbf24", "#fb923c", "#f87171", "#22d3ee",
                        "#c084fc", "#4ade80")
    n_needed <- length(unique(subj_counts$subject))
    pie_colors <- if (n_needed <= length(planora_palette)) {
      planora_palette[seq_len(n_needed)]
    } else {
      colorRampPalette(planora_palette)(n_needed)
    }

    ggplot(subj_counts, aes(x = "", y = n, fill = subject)) +
      geom_bar(stat = "identity", width = 1, color = "#0d0b1a") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = pie_colors) +
      theme_void() +
      theme(
        legend.text = element_text(color = "white", size = 11),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
  }, bg = "transparent")

  # ---------- Bar Chart: Study Hours by Category ----------
  output$bar_chart <- renderPlot({
    df <- tasks_data()
    if (nrow(df) == 0) {
      return(ggplot() + theme_void() +
              annotate("text", x = 0, y = 0, label = "No data yet",
                       color = "white", size = 6))
    }
    cat_hours <- df %>% group_by(category) %>% summarise(hours = sum(study_hours, na.rm = TRUE))
    ggplot(cat_hours, aes(x = reorder(category, hours), y = hours, fill = hours)) +
      geom_col(width = 0.6) +
      coord_flip() +
      scale_fill_gradient(low = "#8b5cf6", high = "#c4b5fd") +
      labs(x = NULL, y = "Study Hours") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid.major = element_line(color = rgb(1, 1, 1, alpha = 0.08)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
  }, bg = "transparent")

  # ---------- Upcoming Deadlines ----------
  output$upcoming_deadlines <- renderUI({
    df <- tasks_data()
    if (nrow(df) == 0) return(p("No upcoming deadlines.", style = "color:#b8b0d9;"))
    upcoming <- df %>%
      filter(status == "Pending",
            as.Date(deadline) >= Sys.Date(),
            as.Date(deadline) <= Sys.Date() + 7) %>%
      arrange(as.Date(deadline))
    if (nrow(upcoming) == 0) return(p("No upcoming deadlines in the next 7 days.", style = "color:#b8b0d9;"))
    tagList(lapply(seq_len(nrow(upcoming)), function(i) {
      r <- upcoming[i, ]
      div(class = "alert-upcoming",
         icon("clock"), " ", r$task, " - ", r$deadline,
         span(style = "float:right;", r$subject))
    }))
  })

  # ---------- Overdue Alerts ----------
  output$overdue_alerts <- renderUI({
    df <- tasks_data()
    if (nrow(df) == 0) return(p("No overdue tasks.", style = "color:#b8b0d9;"))
    overdue <- df %>%
      filter(status == "Pending", as.Date(deadline) < Sys.Date()) %>%
      arrange(as.Date(deadline))
    if (nrow(overdue) == 0) return(p("Great! No overdue tasks.", style = "color:#34d399;"))
    tagList(lapply(seq_len(nrow(overdue)), function(i) {
      r <- overdue[i, ]
      div(class = "alert-overdue",
         icon("triangle-exclamation"), " ", r$task, " - was due ", r$deadline,
         span(style = "float:right;", r$subject))
    }))
  })

  # ---------- Achievement Badges ----------
  output$achievement_badges <- renderUI({
    df <- tasks_data()
    completed_count <- if (nrow(df) > 0) sum(df$status == "Completed", na.rm = TRUE) else 0
    badges <- get_achievements(completed_count, 0)
    tagList(lapply(badges, function(b) {
      cls <- if (b$unlocked) "badge-planora" else "badge-planora badge-locked"
      span(class = cls, icon(b$icon), " ", b$name)
    }))
  })
}

# ---------------------- Run App -----------------------------------
shinyApp(ui = ui, server = server)
