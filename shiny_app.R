
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
    
    # Upload Files Panel
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
    
    # Raw Plots Panel
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
    # Mass Defect plots Panel
    tabPanel("MD Plots",
             titlePanel("PlotPanel"),
             sidebarLayout(
               sidebarPanel(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('mz1', 'Specify the m/z Variable', ""),
                 selectInput("intensity2", "Specify intensity variable", ""),
                 actionButton("go_2", "Calculate"),
                 tags$hr(),
                 textInput("text1", "MD base (optional)"),
                 numericInput("num1", label = "Input MD base nominal mass (Da)", value = 12),
                 numericInput("num2", label = "Input IUPAC exact mass (Da)", value = 12.0000),
                 actionButton('go_3', 'Plot'),
                 selectInput('xvar1', 'X variable', ""),
                 selectInput("yvar1", "Y variable", ""),
                 uiOutput("slide1")
               ),
               mainPanel(
                 plotlyOutput("DTPlot1"),
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
  ##ADD https://plot.ly/r/shinyapp-linked-brush/ 
  
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
  
  #OE#
  # using oberveEvent for actionbutton 'go_1' to trigger plotting instead of automatically plot when uploading file  
  
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
    
  }) 
  #OE#
  
  
  #### For MD Plot Panel ####
  
  # input masses 
  MD_num1 <- eventReactive(input$go_2, {
    input$num1
  })
  
  MD_num2 <- eventReactive(input$go_2, {
    input$num2
  })
  
  xmass <- eventReactive(input$go_2, {
    MD_data()[,input$mz1]
    
  })
  
  output$slide1 <- renderUI({
    int1 <- MD_data()[,input$intensity2]
    
    minZ <- round(min(int1), 2)
    maxZ <- round(max(int1), 2)
    
    sliderInput("slide1", "Intensity filter",
                min = minZ, max = maxZ, value = minZ)
  })
  
  #OE#  
  observeEvent(input$go_3, {
    m <- MD_data ()
    
    # calculating the mass defect
    m <- m %>% 
      mutate(Kmass = xmass()*MD_num1()/MD_num2()) %>% 
      mutate(Knom = round(Kmass, digits=0)) %>% 
      mutate(KMD = round((Kmass - Knom), digits = 6)) %>%
      mutate(Kmass = round(Kmass, digits = 6))
    
    # Problem with update X variable###  
    
    updateSelectInput(session, inputId = 'xvar1', label = 'Specify the x variable for plot',
                      choices = names(m), selected = names(m)[2])
    updateSelectInput(session, inputId = 'yvar1', label = 'Specify the y variable for plot',
                      choices = names(m), selected = names(m)[2])
    
    MDplot_x1 <- reactive({
      m[,input$xvar1]})
    
    MDplot_y1 <- reactive({
      m[,input$yvar1]})
    
    ########   
    d <- SharedData$new(m)
    
    
    # highlight selected rows in the scatterplot
    output$DTPlot1 <- renderPlotly({
      
      s <- input$x1_rows_selected
      
      if (!length(s)) {
        p <- d %>%
          plot_ly(x = MDplot_x1(), y = MDplot_y1(), type = "scatter", mode = "markers", color = I('black'), name = 'Unfiltered') %>%
          layout(showlegend = T) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
      } else if (length(s)) {
        pp <- m %>%
          plot_ly() %>% 
          add_trace(x = MDplot_x1(), y = MDplot_y1(), type = "scatter", mode = "markers", color = I('black'), name = 'Unfiltered') %>%
          layout(showlegend = T)
        
        # selected data
        pp <- add_trace(pp, data = m[s, , drop = F], x = MDplot_x1(), y = MDplot_y1(), type = "scatter", mode = "markers",
                        color = I('red'), name = 'Filtered')
      }
      
    })
    
    # highlight selected rows in the table
    output$x1 <- DT::renderDataTable({
      T_out1 <- m[d$selection(),]
      dt <- DT::datatable(m, editable = TRUE, rownames = FALSE, filter = "top")
      if (NROW(T_out1) == 0) {
        dt
      } else {
        T_out1
        # To display whole table and highlight selected rows then replace "T_out1" with below code chunk:
        # DT::formatStyle(dt, "rowname", target = "row",
        # color = DT::styleEqual(T_out1$rowname, rep("white", length(T_out1$rowname))),
        # backgroundColor = DT::styleEqual(T_out1$rowname, rep("black", length(T_out1$rowname))))
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
