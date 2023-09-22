library(dplyr)
library(timevis)
library(shiny)
library(shinyTime)
library(shinydashboard)
library(googlesheets4)
library(ggplot2)
library(rpivotTable)

### To use proxy for some popping errors
# library(httr)
# set_config(
#   use_proxy(url="ecoreproxy.sg.ap.jnj.com", port=8080, username="ychen209",password="Xdk@930611LYT")
# )
# gs4_auth_configure(api_key = "f449f931544a5ce90c3c2ac94860a08bbc443ddd")
# gs4_auth(
#   scopes = 'https://www.googleapis.com/auth/spreadsheets',
#   path = Sys.getenv('GOOGLE_DRIVE_KEY'))

gs4_auth(path = "strong-imagery-332905-f449f931544a.json")

gspath <- "https://docs.google.com/spreadsheets/d/122crpMUr-JhZwGDeEKgkZFSPKBCDyEXePliX6VVu_-g/edit#gid=0"

### Shiny begins
ui <- dashboardPage(
  dashboardHeader(title = "Project Planner"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Planner", tabName = "planner", icon = icon("tasks")),
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Plot", tabName = "plot", icon = icon("chart-line")),
      menuItem("Weekly Report", tabName = "report", icon = icon("flag"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "planner",
              fluidRow(
                box(
                  width = 4,
                  # file upload manager
                  fileInput("file", label = "File input", accept = c(".csv", ".CSV")),
                  hr(),
                  fluidRow(column(4, verbatimTextOutput("value"))),
                  dateInput(inputId = "date",
                            label = "Date"),
                  selectInput(inputId = "work",
                              label = "The work you will focus on today",
                              choices = ""),
                  checkboxInput("newitem", "Add New Item"),
                  conditionalPanel(
                    condition = "input.newitem == true",
                    textInput("item", label = "New Item"),
                    actionButton("additem", "Add Item")),
                  checkboxInput("newpro", "Add New Project"),
                  conditionalPanel(
                    condition = "input.newpro == true",
                    textInput("project", label = "New Project"),
                    dateRangeInput("daterange", label = "Date range"),
                    actionButton("addpro", "Add Project")),
                  br(),
                  timeInput("start", "Start time:", value = Sys.time(), seconds = FALSE),
                  timeInput("end", "End time:", value = Sys.time(), seconds = FALSE),
                  actionButton("add", "Add"),
                  actionButton("delete", "Delete"),
                  actionButton("save", "Save")
                ),
                box(
                  width = 8,
                  timevisOutput("schedule"),
                  actionButton("delete2", "Delete"),
                  actionButton("undo", "Undo"),
                  hr(),
                  checkboxGroupInput("finish", label = "To do list"),
                  textInput("note", "Note for today")
                )
              )),
      tabItem(tabName = "table",
              fluidRow(
                box(
                  width = 12,
                  dataTableOutput("table1")
                )
              )),
      tabItem(tabName = "plot",
              fluidRow(
                box(
                  width = 12,
                  div(style="display: inline-block;vertical-align:top; width: 150px;", dateInput(inputId = "stdate", label = "Start Date", value = Sys.Date() - 30)),
                  div(style="display: inline-block;vertical-align:top; width: 100px;", HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;", dateInput(inputId = "endate", label = "End Date", value = Sys.Date())),
                  plotOutput("bar1", height = 500)
                ),
                box(
                  width = 12,
                  plotOutput("bar2", height = 500)
                )
              )),
      tabItem(tabName = "report",
              fluidRow(
                box(
                  width = 12,
                  div(style="display: inline-block;vertical-align:top; width: 150px;", dateInput(inputId = "stdate1", label = "Start Date", value = Sys.Date() - 7)),
                  div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;", dateInput(inputId = "endate1", label = "End Date", value = Sys.Date())),
                  tags$head(tags$style( type = 'text/css',  '#pivot{ overflow-x: scroll; }')),
                  rpivotTableOutput("pivot")
                )
              ))
    )
  )
)

server <- function(input, output, session) {
  
  calendar0 <- read_sheet(gspath, sheet = "Sheet1")
  values <- reactiveValues(timedf = NULL, id_now = NULL, undo = list(), counter = 1)
  ### Add input onto the calendar dataframe
  observe({
    if (!is.null(input$file)){
      file <- input$file
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv"|ext == "CSV", "Please upload a csv file"))
      
      calendar <- read.csv(file$datapath)
      
      calendar1 <- calendar %>% 
        mutate(start_time = format(strptime(Start.Time, "%I:%M:%S %p"), "%H:%M:%S"),
               end_time = format(strptime(End.Time, "%I:%M:%S %p"), "%H:%M:%S"),
               start_date = as.character(as.Date.character(Start.Date, format = "%m/%d/%Y")),
               end_date = as.character(as.Date.character(End.Date, format = "%m/%d/%Y")),
               start = paste(start_date, trimws(start_time), " "),
               end = paste(end_date, trimws(end_time), " "),
               content = calendar[, grepl("Subject", colnames(calendar))],
               title = calendar[, grepl("Subject", colnames(calendar))],
               group = 1,
               editable = TRUE,
               style = NA,
               note = NA,
               complete = NA) %>% 
        bind_cols(id = paste0("item", 1:nrow(calendar))) %>% 
        select(id, content, title, start_date, start, end, group, editable, style, note, complete)
      
      values$timedf <- rbind(calendar1, filter(calendar0, group != 1))
    } 
    else {
      values$timedf <- calendar0 %>% 
        mutate(start_date = as.character(start_date))
      # all <<- read.csv("Yufan Chen Calendar1.csv")
    }
  })
  
  ### Add a new task
  observeEvent(input$add, {
    values$id_now <- as.character(as.numeric(Sys.time())*100000)
    start_time <- trimws(format(input$start, format = "%H:%M:%S"))
    end_time <- trimws(format(input$end, format = "%H:%M:%S"))
    start_date <-  trimws(as.character(input$date))
    df_temp <- data.frame(stringsAsFactors = F,
                          id = values$id_now,
                          content = input$work,
                          title = input$work,
                          start_date = start_date,
                          start = paste(start_date, trimws(start_time), " "),
                          end = paste(start_date, trimws(end_time), " "),
                          group = 2,
                          editable = TRUE,
                          style = NA,
                          note = NA,
                          complete = NA)
    values$timedf <- rbind(values$timedf, df_temp) 
    # df_temp <<- as.data.frame(p)
  })
  
  ### Add a new project
  observeEvent(input$addpro, {
    values$id_now <- as.character(as.numeric(Sys.time())*100000)
    start_date <-  trimws(as.character(input$daterange[1]))
    end_date <-  trimws(as.character(input$daterange[2]))
    df_temp <- data.frame(stringsAsFactors = F,
                          id = values$id_now,
                          content = input$project,
                          title = input$project,
                          start_date = start_date,
                          start = start_date,
                          end = end_date,
                          group = 3,
                          editable = TRUE,
                          style = NA,
                          note = NA,
                          complete = NA)
    values$timedf <- rbind(values$timedf, df_temp) 
  })

  ### Update the select input
  observe({
    if (2 %in% values$timedf$group) {
      choice_list <<- values$timedf %>%
        filter(group == 2) %>%
        pull(content) %>%
        unique()
    } else {
      choice_list <<- c("1003 QC", "R learning", "Seraphine NMPA")
    }
    
    # choice_list <<- unique(c(choice_list, input$item))
    updateSelectInput(session, "work", choices = choice_list[order(choice_list)])
    
    values$id_now <- input$schedule_selected
  })
  
  observeEvent(input$delete, {
    values$timedf <- values$timedf[-nrow(values$timedf), ]
  })
  
  ### Delete by clicking, add another reactiveValue origdf for undo functionality
  observeEvent(input$delete2, {
    if (!is.null(input$schedule_selected)){
      values$undo[[values$counter]] <- values$timedf
      values$counter <- values$counter + 1
      values$timedf <- values$timedf[values$timedf$id != input$schedule_selected, ]
    }
  })
  
  ### Undo for deleting by clicking
  observeEvent(input$undo, {
    if (values$counter > 1){
      values$timedf <- values$undo[[values$counter - 1]]
      values$counter <- values$counter - 1
    }
  })
  
  ### Check after finishing
  # observe({
  #   if (!is.null(input$schedule_selected)){
  #     if (input$schedule_selected %in% values$timedf$id){
  #       if (is.na(values$timedf$style[values$timedf$id == input$schedule_selected])){
  #         updateCheckboxInput(session, "finish", label = values$timedf$content[values$timedf$id == input$schedule_selected], value = FALSE)
  #       } else if (!is.na(values$timedf$style[values$timedf$id == input$schedule_selected])){
  #         updateCheckboxInput(session, "finish", label = values$timedf$content[values$timedf$id == input$schedule_selected], value = TRUE)
  #       }
  #     }
  #   }
  # })
  # Flag the finished tasks with red color
  # observeEvent(input$finish, {
  #   if (!is.null(input$schedule_selected)){
  #     if (input$schedule_selected %in% values$timedf$id){
  #       if (input$finish){
  #         values$timedf$style[values$timedf$id == input$schedule_selected] <- "color: red;"
  #       } else if (input$finish == F){
  #         values$timedf$style[values$timedf$id == input$schedule_selected] <- NA}
  #     }
  #   }
  # })
  
  ### Update checkboxgroup input
  # observe({
  #   start_date1 <-  trimws(as.character(input$date))
  #   if (2 %in% values$timedf$group & start_date1 %in% values$timedf$start_date) {
  #     today_list <- values$timedf %>%
  #       filter(group == 2 & start_date == start_date1) %>%
  #       pull(content) %>%
  #       unique()
  #   } else {
  #     today_list <- "No item"
  #   }
  #   today_frame <- data.frame(t(1:length(today_list)))
  #   names(today_frame)[1:length(today_list)] <- today_list
  #   updateCheckboxGroupInput(session, "finish", label = "To do list", choices = today_frame, selected = c(1,2))
  # })
  #, selected = values$timedf$complete
  ### Update checkboxgroup input again when checked
  # observeEvent(input$finish, {
  #   values$timedf$complete <- input$finish
  # })
  
  ## Update note and checkboxgroup input
  observe({
    
    start_date1 <-  trimws(as.character(input$date))
    
    ## Update note
    note_today <- values$timedf$note[values$timedf$start_date == start_date1]
    note_today <- unique(note_today[!is.na(note_today)])
    
    updateTextInput(session, "note", label = "Note for today", value = note_today)
    
    ### Update checkboxgroup input
    if (2 %in% values$timedf$group[values$timedf$start_date == start_date1] & start_date1 %in% values$timedf$start_date) {
      today_list <- values$timedf %>%
        filter(group == 2 & start_date == start_date1) %>%
        pull(content) %>%
        unique()
      ### Choices
      today_frame <- data.frame(t(1:length(today_list)))
      names(today_frame)[1:length(today_list)] <- today_list
      ### Selection
      if (sum(!is.na(values$timedf$complete[values$timedf$start_date == start_date1])) != 0) {
        finish_today <- values$timedf$complete[values$timedf$start_date == start_date1]
        finish_today1 <- unique(finish_today[!is.na(finish_today)])
        if (length(finish_today1) != 0){
          finish_today2 <- as.numeric(strsplit(finish_today1, ",")[[1]])
        } else {
          finish_today2 <- NULL
        }
        updateCheckboxGroupInput(session, "finish", label = "To do list", choices = today_frame, selected = finish_today2)
      } else{
        updateCheckboxGroupInput(session, "finish", label = "To do list", choices = today_frame, selected = NULL)
      }
    } else {
      updateCheckboxGroupInput(session, "finish", label = "To do list", choices = list("No item" = 1), selected = NULL)
    }
    # ### Choices
    # today_frame <- data.frame(t(1:length(today_list)))
    # names(today_frame)[1:length(today_list)] <- today_list
    # 
    # ### Selection
    # finish_today <- values$timedf$complete[values$timedf$start_date == start_date1]
    # 
    # aaa<<-finish_today
    # 
    # finish_today1 <- unique(finish_today[!is.na(finish_today)])
    # 
    # bbb<<-finish_today1
    # ### Change character to numeric
    # finish_today2 <- ifelse(length(finish_today1)==0, NULL, as.numeric(finish_today1))
    # 
    # updateCheckboxGroupInput(session, "finish", label = "To do list", choices = today_frame, selected = finish_today2)
  })
  
  ### Output
  output$schedule <- renderTimevis({
    timevis(data = values$timedf, groups = data.frame(id = 1:3, content = c("Meetings", "Tasks", "Projects")), fit = F) %>% 
      centerItem(values$id_now)
  })
  
  ### Update the timeinput
  observeEvent(input$add, {
    updateTimeInput(session, "start", value = input$end)
  })
  
  ### Update the select input
  observeEvent(input$additem, {
    choice_list <<- unique(c(choice_list, input$item))
    updateSelectInput(session, "work", choices = choice_list[order(choice_list)])
  })
  
  ### Save input data 
  observeEvent(input$save, {
    showNotification("New tasks have been saved",
                     action = a(href = "javascript:location.reload();", "Reload page"))
  
    start_date2 <-  trimws(as.character(input$date))
    
    ### Add today's note
    values$timedf$note[values$timedf$start_date == start_date2] <- input$note
    
    ### Add today's to do list
    if (!is.null(input$finish)){
      values$timedf$complete[values$timedf$start_date == start_date2] <- paste(input$finish, collapse = ",")
    }

    sheet_write(values$timedf, ss = gspath, sheet = "Sheet1")
    # write.csv(values$timedf, file = "Yufan Chen Calendar1.csv", row.names = FALSE, quote = TRUE)
  })
  
  ### table output
  output$table1 <- renderDataTable({
    values$timedf
  }) 
  
  ### plot output for the latest month
  output$bar1 <- renderPlot({
    data1 <- values$timedf %>% 
      filter(group == 2 & as.Date(start) >= input$stdate & as.Date(end) <= input$endate) %>% 
      mutate(timespan = as.numeric(difftime(end, start, units="hours")),
             Completeness = factor(style, levels = c(NA, "color: red;"), labels = c("Planned but not completed", "Completed"), exclude = NULL)) %>% 
      group_by(content, Completeness) %>% 
      summarise(timesum = sum(timespan)) %>% 
      ungroup() %>% 
      mutate(contentid = content)
    
    data2 <- data1 %>% 
      group_by(contentid, content) %>% 
      summarise(timesum1 = sum(timesum)) %>% 
      ungroup()
    
    data3 <- data2 %>% 
      mutate(content_fct = factor(content, levels = content[order(data2$timesum1, decreasing = TRUE)])) %>% 
      left_join(data1, by = "contentid")
    
    ggplot(data3, aes(x = content_fct, y = timesum, fill = Completeness)) +
      geom_bar(position = "stack", stat = "identity") +
      scale_fill_manual(values = c(colors()[621], colors()[611])) +
      labs(title = paste0("Hours Spent on Projects from ", input$stdate, " to ", input$endate), x = "Projects", y = "Hours") +
      theme(plot.title = element_text(hjust = 0.5))
  }) 
  
  ### plot output for all
  output$bar2 <- renderPlot({
    data1 <- values$timedf %>% 
      filter(group == 2) %>% 
      mutate(timespan = as.numeric(difftime(end, start, units="hours")),
             Completeness = factor(style, levels = c(NA, "color: red;"), labels = c("Planned but not completed", "Completed"), exclude = NULL)) %>% 
      group_by(content, Completeness) %>% 
      summarise(timesum = sum(timespan)) %>% 
      ungroup() %>% 
      mutate(contentid = content)
      
    data2 <- data1 %>% 
      group_by(contentid, content) %>% 
      summarise(timesum1 = sum(timesum)) %>% 
      ungroup()
    
    data3 <- data2 %>% 
      mutate(content_fct = factor(content, levels = content[order(data2$timesum1, decreasing = TRUE)])) %>% 
      left_join(data1, by = "contentid")
    
    ggplot(data3, aes(x = content_fct, y = timesum, fill = Completeness)) +
      geom_bar(position = "stack", stat = "identity") +
      scale_fill_manual(values = c(colors()[621], colors()[611])) +
      labs(title = "Hours Spent on Projects", x = "Projects", y = "Hours") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
  }) 
  
  ### Weekly Report
  output$pivot <- renderRpivotTable({
    data_pivot <- values$timedf %>% 
      filter(group == 2 & as.Date(start) >= input$stdate1 & as.Date(end) <= input$endate1) %>% 
      mutate(timespan = as.numeric(difftime(end, start, units="hours")),
             Completeness = factor(style, levels = c(NA, "color: red;"), labels = c("Planned but not completed", "Completed"), exclude = NULL),
             Date = as.Date(start)) %>%
      select(content, Completeness, timespan, Date)  
    
    rpivotTable(data_pivot, rows = "content", cols = "timespan", aggregatorName = "Sum", vals = "timespan")
  }) 
  
}

shinyApp(ui = ui, server = server)


