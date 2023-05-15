library(dplyr)
library(timevis)
library(shiny)
library(shinyTime)
library(shinydashboard)
library(googlesheets4)
library(ggplot2)

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
      menuItem("Plot", tabName = "plot", icon = icon("chart-line"))
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
                  br(),
                  checkboxInput("finish", label = "Selected Task", value = FALSE)
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
                  plotOutput("bar1", height = 500)
                ),
                box(
                  width = 12,
                  plotOutput("bar2", height = 500)
                )
              ))
    )
  )
)

server <- function(input, output, session) {
  
  all <- data.frame()
  calendar0 <- read_sheet(gspath, sheet = "Sheet1")
  values <- reactiveValues(timedf = NULL, origdf = NULL, undo = list(), counter = 1)
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
               start_date = as.Date.character(Start.Date, format = "%m/%d/%Y"),
               end_date = as.Date.character(End.Date, format = "%m/%d/%Y"),
               start = paste(start_date, trimws(start_time), " "),
               end = paste(end_date, trimws(end_time), " "),
               content = calendar[, grepl("Subject", colnames(calendar))],
               title = calendar[, grepl("Subject", colnames(calendar))],
               group = 1,
               editable = TRUE,
               style = NA) %>% 
        bind_cols(id = paste0("item", 1:nrow(calendar))) %>% 
        select(id, content, title, start, end, group, editable, style)
      
      all <<- rbind(calendar1, filter(calendar0, group == 2))
    } 
    else {
      all <<- calendar0
      # all <<- read.csv("Yufan Chen Calendar1.csv")
    }
    
    if (2 %in% all$group) {
      choice_list <<- all %>% 
        filter(group == 2) %>% 
        pull(content) %>% 
        unique()
    } else {
      choice_list <<- c("1003 QC", "R learning", "Seraphine NMPA")
    }
    
    # choice_list <<- unique(c(choice_list, input$item))
    updateSelectInput(session, "work", choices = choice_list[order(choice_list)])
    values$timedf <- all
  })
  
  observeEvent(input$add, {
    id_now <<- as.character(as.numeric(Sys.time())*100000)
    df_temp <- data.frame(stringsAsFactors = F,
                    id = id_now,
                    content = input$work,
                    title = input$work,
                    start = trimws(as.character(input$start)),
                    end = trimws(as.character(input$end)),
                    group = 2,
                    editable = TRUE,
                    style = NA)
    all <<- rbind(all, df_temp)
    values$timedf <- all
    # df_temp <<- as.data.frame(p)
  })
  
  # df <- eventReactive(input$add, {
  #   all <<- rbind(all, df_temp)
  # })

  ### Add and delete functionality  

  # observeEvent(input$add, {
  #   # values$timedf <- all
  #   # if (!is.null(input$add)){
  #   all <<- rbind(all, df_temp)
  #   values$timedf <- all
  #   # }
  # })
  
  observeEvent(input$delete, {
    all <<- all[-nrow(all), ]
    values$timedf <- all
  })
  
  ### Delete by clicking, add another reactiveValue origdf for undo functionality
  observeEvent(input$delete2, {
    values$undo[[values$counter]] <- all
    values$counter <- values$counter + 1
    all <<- all[all$id != input$schedule_selected, ]
    values$timedf <- all
  })
  
  ### Undo for deleting by clicking
  observeEvent(input$undo, {
    if (values$counter > 1){
      all <<- values$undo[[values$counter - 1]]
      values$counter <- values$counter - 1
      values$timedf <- all
    }
  })
  
  ### Check after finishing
  observe({
    if (!is.null(input$schedule_selected) && is.na(all$style[all$id == input$schedule_selected])){
      updateCheckboxInput(session, "finish", label = all$content[all$id == input$schedule_selected], value = FALSE)
    } else if (!is.null(input$schedule_selected) && !is.na(all$style[all$id == input$schedule_selected])){
      updateCheckboxInput(session, "finish", label = all$content[all$id == input$schedule_selected], value = TRUE)
    }
    id_now <<- input$schedule_selected
  })
    
  # observe({
  #   if (input$finish){
  #     all$style[all$id == input$schedule_selected] <- "color: red;"
  #   } else {
  #     all$style[all$id == input$schedule_selected] <- NA
  #   }
  #   all
  # })
  
  # values$timedf <- reactive({
  #   if (input$finish){
  #     all$style[all$id == input$schedule_selected] <- "color: red;"
  #   } else {
  #     all$style[all$id == input$schedule_selected] <- NA
  #   }
  #   all
  # })
  
  # selected_item <- eventReactive(input$finish, {
  #   input$schedule_selected
  #   id_now <<- input$schedule_selected
  # })
  
  # observe({
  #   if (!is.null(selected_item()) && is.na(all$style[all$id == selected_item()])){
  #     all$style[all$id == selected_item()] <<- "color: red;"
  #   }
  #   values$timedf <- all
  # })

  # observeEvent(input$finish, {
  #   
  #   if (!is.null(input$schedule_selected) && is.na(all$style[all$id == input$schedule_selected])){
  #     all$style[all$id == input$schedule_selected] <<- "color: red;"
  #   } else if (input$finish == F){
  #     all$style[all$id == input$schedule_selected] <<- NA}
  #   # } else {
  #   #   all$style[all$id == input$schedule_selected] <<- NA
  #   # }
  #   values$timedf <- all
  # })

  ### Output
  output$schedule <- renderTimevis({
    if (!is.null(input$schedule_selected) && is.na(all$style[all$id == input$schedule_selected]) && input$finish){
      all$style[all$id == input$schedule_selected] <<- "color: red;"
    } else if (input$finish == F){
      all$style[all$id == input$schedule_selected] <<- NA}
    if (input$finish == F){
      all$style[all$id == input$schedule_selected] <<- NA
    }
    values$timedf <- all
    
    timevis(data = values$timedf, groups = data.frame(id = 1:2, content = c("Meetings", "Tasks")), fit = F) %>% 
      centerItem(id_now)
  })

  ### Update the timeinput
  observeEvent(input$add, {
    updateTimeInput(session, "start", value = input$end)
  })
  
  ### Update the select input
  observeEvent(input$additem, {
    choice_list <<- unique(c(choice_list, input$item))
    updateSelectInput(session, "work", choices = choice_list[order(choice_list)], selected = input$item)
  })
  
  ### Save input data 
  observeEvent(input$save, {
    showNotification("New tasks have been saved",
                     action = a(href = "javascript:location.reload();", "Reload page"))
    
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
      filter(group == 2 & as.numeric(difftime(Sys.time(), start, units="days")) <= 30) %>% 
      mutate(timespan = as.numeric(difftime(end, start, units="hours")),
             Completeness = factor(style, levels = c(NA, "color: red;"), labels = c("Planned but not completed", "Completed"), exclude = NULL)) %>% 
      group_by(content, Completeness) %>% 
      summarise(timesum = sum(timespan)) %>% 
      ungroup()
    
    ggplot(data1, aes(x = content, y = timesum, fill = Completeness)) +
      geom_bar(position = "stack", stat = "identity") +
      scale_fill_manual(values = c(colors()[621], colors()[611])) +
      labs(title = "Hours Spent on Projects in the Latest 30 Days", x = "Projects", y = "Hours")
  }) 
  
  ### plot output for all
  output$bar2 <- renderPlot({
    data1 <- values$timedf %>% 
      filter(group == 2) %>% 
      mutate(timespan = as.numeric(difftime(end, start, units="hours")),
             Completeness = factor(style, levels = c(NA, "color: red;"), labels = c("Planned but not completed", "Completed"), exclude = NULL)) %>% 
      group_by(content, Completeness) %>% 
      summarise(timesum = sum(timespan)) %>% 
      ungroup()
    
    ggplot(data1, aes(x = content, y = timesum, fill = Completeness)) +
      geom_bar(position = "stack", stat = "identity") +
      scale_fill_manual(values = c(colors()[621], colors()[611])) +
      labs(title = "Hours Spent on Projects", x = "Projects", y = "Hours")
  }) 

}

shinyApp(ui = ui, server = server)


