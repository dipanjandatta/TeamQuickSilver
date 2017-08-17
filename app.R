library(shiny)
#write.csv(model_data,"model_data.csv")
#write.csv(model_selector1_1_all,"model_selector1_1_all.csv")

source("global_1.R")

source("global_2.R")

ui <- shinyUI(
  fluidPage(
    titlePanel("Prediction Panel"),
    sidebarLayout(
      sidebarPanel(
        # tags$b("STEP 1:Please download file format for new opportunities "),
        # actionButton("action2", "Download Template", class = "btn-primary",
        #              icon("save", lib = "glyphicon")),
        # tags$hr(),
        
        tags$b("STEP 1:Please go to the 'Read Me' tab "),
        tags$hr(),
        fileInput('file1', 'STEP 2:Upload new opportunities (.csv file) ',
                  accept=c('text/csv','text/comma-separated-values,text/plain',
                           '.csv')),
        
        tags$hr(),
        tags$b("STEP 3:Please click the following 
                button to get your prediction and wait for some time "),
        actionButton("action", label = "Get Prediction", 
                     icon("ok", lib = "glyphicon")),
        
        tags$hr(),
        tags$b("STEP 4:See 'Prediction' tab "),
        tags$hr(),
        
        tags$b("STEP 5:Download your prediction values, to save please put '.csv' extention after the file name "),
        
        downloadButton('downloadData', 'Download')
        
        # 				actionButton("action2", "Save Output", class = "btn-primary",
        #                        icon("save", lib = "glyphicon"))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("New Opportunity",
                   fluidRow(tableOutput('contents'))
          ),
          
          tabPanel("Prediction",
                   fluidRow(tableOutput('content_1')) ),
          
          tabPanel("Read Me",fluidRow(
            strong("Instructions to upload new data for prediction:"),
            br(),
            p("1. In the submitted excel file there is a tab,namely,", span("Template,", style = "color:blue"), "please fill in the template with new opportunities for which you want prediction. For help, please use the sample rows in", span("SampleDataForTemplate", style = "color:blue")," tab",style = "font-family: 'times'; font-si16pt"),
            br(),
            p("2. Please fill in at least two rows. You can copy and paste your single record in two successive rows. Then clear the content of other rows"
               , style = "font-family: 'times'; font-si16pt"),
            br(),
            code("3. Save the template tab with your records and create a separate comma separated value formatted excel book (.csv) "),
            br(),
            br(),
            p("4.You are ready to upload your new opportunities" , style = "font-family: 'times'; font-si16pt")
            
          ))
        )))
  ))


server = shinyServer(function(input, output) {
  
  output$contents = renderTable({
    inFile = input$file1
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath)
  })
  
  score <- eventReactive(input$action, {
    isolate({
      inFile = input$file1
      if (is.null(inFile)) return(NULL)
      read.csv(inFile$datapath)
    })
  })
  
  output$content_1 = renderTable({test_function(score())})
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(test_function(score()), file)
    }
  )
  
})

# Run the application 
shinyApp(ui = ui, server = server)