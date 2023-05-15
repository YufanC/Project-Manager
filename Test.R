
library(dplyr)
library(timevis)
library(shiny)
library(shinyTime)

calendar <- read.csv("Yufan Chen Calendar.csv")

calendar1 <- calendar %>% 
  mutate(start_time = format(strptime(Start.Time, "%I:%M:%S %p"), "%H:%M:%S"),
         end_time = format(strptime(End.Time, "%I:%M:%S %p"), "%H:%M:%S"),
         start_date = as.Date.character(Start.Date, format = "%m/%d/%Y"),
         end_date = as.Date.character(End.Date, format = "%m/%d/%Y"),
         start = paste(start_date, trimws(start_time), " "),
         end = paste(end_date, trimws(end_time), " "),
         title = `ï..Subject`) %>% 
  rename(content = `ï..Subject`) %>% 
  bind_cols(id = paste0("item", 1:nrow(calendar))) %>% 
  select(id, content, title, start, end)


timevis(calendar1, fit = F)

style <- "
.vis-timeline {
  border-color: #269026;
  background-color: lightgreen;
  font-size: 15px;
  color: green;
}

.vis-item {
  border: 2px solid #5ace5a;
  font-size: 12pt;
  background: #d9ffd9;
  font-family: cursive;
  padding: 5px;
}
"

tv <- tagList(tags$style(style), tv)
htmltools::html_print(tv)


ui <- fluidPage(
  timevisOutput("mytime"),
  actionButton("btn", "Add item and center")
)

server <- function(input, output, session) {
  output$mytime <- renderTimevis(timevis())
  observeEvent(input$btn, {
    addItem("mytime", list(id = "item1", content = "one", start = "2016-08-01"))
    centerItem("mytime", "item1")
  })
}

shinyApp(ui = ui, server = server)


### New test


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
                  width = 6,
                  dateInput(inputId = "date",
                            label = "Date"),
                  selectInput(inputId = "work",
                              label = "The work you will focus on this week",
                              choices = c("1003 QC", "3018 Outputs Production", "R learning")),
                  timeInput("start", "Start Time:", minute.steps = 5),
                  timeInput("end", "End Time:", minute.steps = 5),
                  actionButton("add", "Add"),
                  actionButton("delete", "Delete"),
                  actionButton("save", "Save")
                ),
                box(
                  width = 6,
                  timevisOutput("schedule")
                )
              ))
    )
  )
)

server <- function(input, output, session) {
  
  all <- calendar1
  
  observeEvent(input$add, {
    p <- data.frame(stringsAsFactors = F,
                    id = paste0("task", input$add),
                    content = input$work,
                    title = input$work,
                    start = trimws(as.character(input$start)),
                    end = trimws(as.character(input$end)))
    df1 <<- as.data.frame(p)
    id_now <<- paste0("task", input$add)
  })
  
  # df <- reactive(df1)
  # 
  # observeEvent(input$add, {
  #   df2 = df()
  #   df2 = rbind(all, df2)
  #   df(all)
  # })
  # 
  # observeEvent(input$delete, {
  #   df3 = df()
  #   df3 = df3[-1,]
  #   df(df3)
  # })

  df <- eventReactive(input$add, {
    
    assign("all1", rbind(all, df1), envir = .GlobalEnv)
    all <<- rbind(all, df1)
  }, ignoreNULL = FALSE)
  
  values <- reactiveValues()
  
  

  observeEvent(df(), {
    if(!is.null(df())){
      values$testdf <- df()
    } else {
      values$testdf <- calendar1
    }
  })

  observeEvent(input$delete,{
    
    all <<- all[-nrow(all), ]
    values$testdf <- all

  })
  
  output$schedule <- renderTimevis({
    timevis(values$testdf, fit = F) %>% 
      centerItem(id_now)
  })

}

shinyApp(ui = ui, server = server)


