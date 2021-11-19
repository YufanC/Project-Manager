library(dplyr)
library(timevis)
library(shiny)
library(shinyTime)
library(shinydashboard)


### Shiny begins
ui <- dashboardPage(
  dashboardHeader(title = "Project Planner"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Planner", tabName = "planner", icon = icon("tasks"))
      # menuItem("Table", tabName = "table", icon = icon("table")),
      # menuItem("Plot", tabName = "plot", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "planner",
              fluidRow(
                box(
                  width = 4,
                  # file upload manager
                  fileInput("file", label = "File input", accept = ".csv"),
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
                  timeInput("start", "Start Time:", value = Sys.time(), seconds = FALSE),
                  timeInput("end", "End Time:", value = Sys.time(), seconds = FALSE),
                  actionButton("add", "Add"),
                  actionButton("delete", "Delete"),
                  actionButton("save", "Save")
                ),
                box(
                  width = 8,
                  timevisOutput("schedule"),
                  actionButton("delete2", "Delete"),
                  br(),
                  checkboxInput("finish", label = "Selected Task")
                  # textOutput("items")
                )
              ))
    )
  )
)

server <- function(input, output, session) {
  
  all <- data.frame()
  values <- reactiveValues(timedf = NULL)
  ### Add input onto the calendar dataframe
  observe({
    if (!is.null(input$file)){
      file <- input$file
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      all <<- read.csv(file$datapath)
    } 
    else {
      all <<- read.csv("Yufan Chen Calendar1.csv")
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
    updateSelectInput(session, "work", choices = choice_list)
    values$timedf <- all
  })
  
  observeEvent(input$add, {
    id_now <<- as.character(as.numeric(Sys.time())*100000)
    df_temp <<- data.frame(stringsAsFactors = F,
                    id = id_now,
                    content = input$work,
                    title = input$work,
                    start = trimws(as.character(input$start)),
                    end = trimws(as.character(input$end)),
                    group = 2,
                    editable = TRUE,
                    style = NA)
    # df_temp <<- as.data.frame(p)
  }, ignoreNULL = FALSE)
  
  df <- eventReactive(input$add, {
    all <<- rbind(all, df_temp)
  })

  ### Add and delete functionality  

  observeEvent(input$add, {
    # values$timedf <- all
    # if (!is.null(input$add)){
      values$timedf <- df()
    # }
  })
  
  observeEvent(input$delete, {
    all <<- all[-nrow(all), ]
    values$timedf <- all
  })
  
  ### Delete by clicking
  observeEvent(input$delete2, {
    all <<- all[all$id != input$schedule_selected, ]
    values$timedf <- all
  })
  
  ### Check after finishing
  observe({
    if (!is.null(input$schedule_selected) && is.na(all$style[all$id == input$schedule_selected])){
      updateCheckboxInput(session, "finish", label = all$content[all$id == input$schedule_selected], value = FALSE)
    } else if (!is.null(input$schedule_selected) && !is.na(all$style[all$id == input$schedule_selected])){
      updateCheckboxInput(session, "finish", label = all$content[all$id == input$schedule_selected], value = TRUE)
    }
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

  observeEvent(input$finish, {
    id_now <<- input$schedule_selected
    if (!is.null(input$schedule_selected) && is.na(all$style[all$id == input$schedule_selected])){
      all$style[all$id == input$schedule_selected] <<- "color: red;"
    } else if (input$finish == F){
      all$style[all$id == input$schedule_selected] <<- NA}
    # } else {
    #   all$style[all$id == input$schedule_selected] <<- NA
    # }
    values$timedf <- all
  })

  ### Output
  output$schedule <- renderTimevis({
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
    updateSelectInput(session, "work", choices = choice_list, selected = input$item)
  })
  
  ### Save input data
  observeEvent(input$save, {
    write.csv(values$timedf, file = "Yufan Chen Calendar1.csv", row.names = FALSE, quote = TRUE)
  })
  
  observeEvent(input$save, {
    showNotification("New tasks have been saved",
                     action = a(href = "javascript:location.reload();", "Reload page"))
  })
}

shinyApp(ui = ui, server = server)


