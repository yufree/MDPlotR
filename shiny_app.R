
library(shiny)
library(dplyr)
library(shinythemes)
library(DT)
library(plotly)
library(crosstalk)


# UI function -------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme('spacelab'),
  titlePanel("Mass Defect Plot"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 tags$br(),
                 
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"'),
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select number of rows to display ----
                 radioButtons("disp", "Display",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head"),
                 actionButton('go_file', 'Upload')
                 
               ),
               mainPanel(
                 tableOutput('summary'),
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Raw plots",
             pageWithSidebar(
               headerPanel('Raw data plots'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', ""),
                 selectInput('intensity', 'Intensity Markers', ""),
                 selectInput('hover', 'Hover text', ""),
                 actionButton('go_1', 'Plot')
               ),
               mainPanel(
                 plotlyOutput('RawPlot1'),
                 plotlyOutput('RawPlot2')
               )
             )
    ),
    tabPanel("MD Plots",
             titlePanel("PlotPanel"),
             sidebarLayout(
               sidebarPanel(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('mz1', 'Specify the m/z Variable', ""),
                 selectInput("intensity2", "Specify intensity variable", ""),
                 actionButton("go_2", "Calculate"),
                 tags$hr(),
                 selectInput('xvar1', 'X variable', ""),
                 selectInput("yvar1", "Y variable", ""),
                 sliderInput("slide1", "Mass error (ppm)",min = 1, max = 500, value = 10, step = 1),
                 textInput("text1", "MD base (optional)"),
                 numericInput("num1", label = "Input MD base nominal mass (Da)", value = 12),
                 numericInput("num2", label = "Input IUPAC exact mass (Da)", value = 12.0000),
                 actionButton('go_3', 'Plot')
                 
               ),
               mainPanel(
                 plotlyOutput("x2"),
                 DT::dataTableOutput("x1"),
                 fluidRow(
                   p(class = 'text-center', downloadButton('x3', 'Download Filtered Data'))
                 ))
             )
    )
  )
)


# Server function ---------------------------------------------------------


server <- function(input, output, session) {
  
  ## For uploading Files Panel ## 
  
  MD_data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    df <- read.csv(input$file1$datapath, 
                   header = input$header, 
                   sep = input$sep,
                   quote = input$quote)
    
    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'intensity', label = 'Intensity Markers',
                      choices = names(df), selected = names(df)[3])
    updateSelectInput(session, inputId = 'hover', label = 'Hover text',
                      choices = names(df), selected = names(df)[4])
    updateSelectInput(session, inputId = 'mz1', label = 'Specify the m/z of the dataset',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'intensity2', label = 'Specify the intensity variable',
                      choices = names(df), selected = names(df)[2])
    return(df)
  })
  
  
  # Adds a summary table for variables:
  # output$summary <- renderTable({
  # if(is.null(MD_data())){return()}
  # summary(MD_data())
  # })
  
  output$contents <- renderTable({
    if(is.null(MD_data())){return()}
    
    if(input$disp == "head") {
      return(head(MD_data()))
    }
    else {
      return(MD_data())
    }
  })
  
  
  ## For Raw Plots Panel ##
  
  # Defining input variables for plots
  
  
  plot_x <- reactive({
    MD_data()[,input$xcol]
  })
  plot_y <- reactive({
    MD_data()[,input$ycol]
  })
  plot_intensity <- reactive({
    MD_data()[,input$intensity]
  })
  plot_hover <- reactive({
    MD_data()[,input$hover]
  })
  
  # using oberveEvent for actionbutton 'go_1' to initiate plotting instead of automatically plot when uploading file  
  observeEvent(input$go_1, {
    
    output$RawPlot1 <- renderPlotly({
      Plot1 <- plot_ly(
        x = plot_x(),
        type = 'histogram')
    })
    
    # A scatterplot using Plotly
    
    
    output$RawPlot2 <- renderPlotly({
      Plot2 <- plot_ly(
        x = plot_x(),
        y = plot_y(), 
        type = 'scatter',
        color = plot_intensity(),
        colors = 'YlOrRd',
        text = ~paste(input$hover, plot_hover())
      )
    })
    
  }) # end observeEvent
  
  
  ## For MD Plot Panel ## 
  
  # nominal mass
  MD_num1 <- reactive({
    input$num1
  })
  #exact mass
  MD_num2 <- reactive({
    input$num2
  })
  
  xmass <- reactive({
    MD_data()[,input$mz1]})
  
  
  
  #OE#  
  observeEvent(input$go_2, {  
    m <- MD_data ()
    
    # calculating the mass defect
    m <- m %>% 
      mutate(Kmass = xmass()*MD_num1()/MD_num2()) %>% 
      mutate(Knom = round(Kmass, digits=0)) %>% 
      mutate(KMD = round((Kmass - Knom), digits = 6)) %>%
      mutate(Kmass = round(Kmass, digits = 6))
    
    
    
    
    d <- SharedData$new(m)
    
    
    # highlight selected rows in the scatterplot
    output$x2 <- renderPlotly({
      
      s <- input$x1_rows_selected
      
      if (!length(s)) {
        p <- d %>%
          plot_ly(x = ~Kmass, y = ~KMD, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
          layout(showlegend = T) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
      } else if (length(s)) {
        pp <- m %>%
          plot_ly() %>% 
          add_trace(x = ~Kmass, y = ~KMD, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
          layout(showlegend = T)
        
        # selected data
        pp <- add_trace(pp, data = m[s, , drop = F], x = ~Kmass, y = ~KMD, mode = "markers",
                        color = I('red'), name = 'Filtered')
      }
      
    })
    
    # highlight selected rows in the table
    output$x1 <- DT::renderDataTable({
      m2 <- m[d$selection(),]
      dt <- DT::datatable(m, editable = TRUE, rownames = FALSE, filter = "top")
      if (NROW(m2) == 0) {
        dt
      } else {
        m2
        # To display whole table and highlight selected rows then replace "m2" with below code chunk:
        # DT::formatStyle(dt, "rowname", target = "row",
        # color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
        # backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
      }
    })
    
    # download the filtered data
    output$x3 = downloadHandler('MDplot-filtered.csv', content = function(file) {
      s <- input$x1_rows_selected
      if (length(s)) {
        write.csv(m[s, , drop = FALSE], file)
      } else if (!length(s)) {
        write.csv(m[d$selection(),], file)
      }
    })
  }) 
  #OE#
  
  
}

shinyApp(ui, server)
