
library(shiny)
library(dplyr)
library(shinythemes)
library(DT)
library(plotly)
library(crosstalk)
library(shinyjs)

# UI function -------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
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
    
    
    # Mass Defect plots Panel #
    tabPanel("MD Plots",
             sidebarLayout(
               sidebarPanel(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('mz1', 'Specify the m/z Variable', ""),
                 selectInput("intensity2", "Specify intensity variable", ""),
                 actionButton("go_2", "Calculate"),
                 tags$hr(),
                 div(
                   fluidRow(
                     h4("Mass defect calculations"),
                     column(3,
                            textInput("text1", HTML("1st MD base<br/>(optional)"))),
                     column(3,
                            numericInput("num1", label = HTML("1. Nominal<br/>mass (Da)"), value = 12)),
                     column(3,
                            numericInput("num2", label = HTML("1. Exact mass<br/>(Da)"), value = 12.0000)),
                     column(2,
                            selectInput("round_1", HTML("Rounding<br/>&nbsp"), choices = c("round", "ceiling", "floor"), selected = "ceiling"))
                   ), style = "font-size:90%;"
                 ),
                 div(
                   fluidRow(
                     column(3,
                            textInput("text2", HTML("2nd MD base<br/>(optional)"))),
                     column(3,
                            numericInput("num3", label = HTML("2. Nominal<br/>mass (Da)"), value = 14)),
                     column(3,
                            numericInput("num4", label = HTML("2. Exact mass<br/>(Da)"), value = 14.01565)),
                     column(2,
                            selectInput("round_1", HTML("Rounding<br/>&nbsp"), choices = c("round", "ceiling", "floor"), selected = "ceiling"))
                   ), style = "font-size:90%;"
                 ),
                 actionButton('go_3', 'Update'),
                 
                 # Click to open a help box
                 actionButton("helpbutton_1", "Help?"),
                 uiOutput("helpbox_1"),
                 
                 tags$br(),
                 tags$hr(),
                 
                 fluidRow(h4("Plot controls"),
                          tags$br(),
                          column(5,
                                 selectInput('xvar1', 'X variable', "")),
                          column(5,
                                 selectInput("yvar1", "Y variable", ""))
                 ),
                 fluidRow(column(5,
                                 selectInput('xvar2', 'X variable', "")),
                          column(5,
                                 selectInput("yvar2", "Y variable", ""))
                 ),
                 
                 tags$br(),
                 checkboxInput("box5", "Display intensity in plot"),
                 tags$br(),
                 uiOutput("slide1"),
                 actionButton("go_4", "Plot")
               ),
               mainPanel(
                 fluidRow(column(6, plotlyOutput("DTPlot1")),
                          column(6, plotlyOutput("DTPlot2"))
                 ),
                 DT::dataTableOutput("x1"),
                 fluidRow(
                   column(3, downloadButton("x3", "Download Filtered Data"))
                 ),
                 tags$br(),
                 
                 # Using Shinyjs to open websites
                 fluidRow(
                   h4("Links to web tools for compound search"),
                   column(3, align = "left", actionButton("go_5", "Chemistry Dashboard",
                                                          onclick ="window.open('https://comptox.epa.gov/dashboard/dsstoxdb/advanced_search')")),
                   column(3, align = "left", actionButton("open_1", "ChemSpider",
                                                          onclick ="window.open('http://www.chemspider.com/FullSearch.aspx')")),
                   column(2, align = "left", actionButton("open_2","EnviPat",
                                                          onclick ="window.open('http://www.envipat.eawag.ch')")),
                   column(2, align = "left", actionButton("open_3","EnviHomolog",
                                                          onclick ="window.open('http://www.envihomolog.eawag.ch')")),
                   column(2, align = "left", actionButton("open_4","Norman Massbank",
                                                          onclick ="window.open('https://massbank.eu/MassBank/QuickSearch.html')"))
                   
                   
                 )
               )
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
    
    
    
    updateSelectInput(session, inputId = 'mz1', label = 'Specify the m/z of the dataset',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'intensity2', label = 'Specify the intensity variable',
                      choices = names(df), selected = names(df)[2])
    
    
    return(df)
  })
  
  
  # add a table of the file
  output$contents <- renderTable({
    if(is.null(MD_data())){return()}
    
    if(input$disp == "head") {
      return(head(MD_data()))
    }
    else {
      return(MD_data())
    }
  })
  
  
  
  
  #### For MD Plot Panel ####
  
  # input masses 
  MD_num1 <- eventReactive(input$go_3, {
    input$num1
  })
  
  MD_num2 <- eventReactive(input$go_3, {
    input$num2
  })
  
  MD_num3 <- eventReactive(input$go_3, {
    input$num3
  })
  
  MD_num4 <- eventReactive(input$go_3, {
    input$num4
  })
  
  xmass <- eventReactive(input$go_3, {
    MD_data()[,input$mz1]
    
  })
  
  # input a helpbox to explain MD and give examples of MD bases
  output$helpbox_1 = renderUI({
    if (input$helpbutton_1 %% 2){
      helpText(HTML("<b>Elements</b> <br/>
                    H: 1.007825, C: 12.0000, N: 14.003074, O: 15.994915, Si: 27.976928, P: 30.973763<br/>
                    F: 18.998403, Cl: 34.968853, Br: 78.918336, I: 126.904477<br/>
                    <b>Some MD bases</b> <br/>
                    CH2: 14.01565, <br/>
                    -H/+Cl: 33.961028, <br/>
                    -H/+Br: 77.910511, <br/>
                    CF2: 49.996806
                    
                    "))
    } else {
      return()
    }
  })
  
  # Sliderinput for intensity
  ab1 <- reactive({
    MD_data()[,input$intensity2]
  })
  
  
  output$slide1 <- renderUI({
    int1 <- MD_data()[,input$intensity2]
    
    minZ <- min(int1)
    maxZ <- max(int1)
    
    sliderInput("slide1", "Intensity filter",
                min = minZ, max = maxZ, value = minZ)
  })
  
  
  
  #OE#  
  observeEvent(input$go_3, {
    
    m <- MD_data ()
    
    # calculating the mass defect
    m <- m %>% 
      mutate(Kmass1 = xmass()*MD_num1()/MD_num2()) %>% 
      mutate(Knom1 = round(Kmass1, digits=0)) %>% 
      mutate(KMD1 = round((Kmass1 - Knom1), digits = 5)) %>%
      mutate(Kmass1 = round(Kmass1, digits = 5)) %>%
      mutate(Kmass2 = xmass()*MD_num3()/MD_num4()) %>%
      mutate(Knom2 = round(Kmass2, digits=0)) %>%
      mutate(KMD2 = round((Kmass2 - Knom2), digits = 5)) %>%
      mutate(Kmass2 = round(Kmass2, digits = 5)) %>%
      # add column with filtered intensity
      mutate(intensity = ab1()) %>%
      filter(intensity > input$slide1)
    
    
    # Update variables#  
    
    updateSelectInput(session, inputId = 'xvar1', label = 'Specify the x variable for plot',
                      choices = names(m), selected = names(m)[2])
    updateSelectInput(session, inputId = 'yvar1', label = 'Specify the y variable for plot',
                      choices = names(m), selected = names(m)[2])
    updateSelectInput(session, inputId = 'xvar2', label = 'Specify the x variable for plot',
                      choices = names(m), selected = names(m)[2])
    updateSelectInput(session, inputId = 'yvar2', label = 'Specify the y variable for plot',
                      choices = names(m), selected = names(m)[2])
    
    MDplot_x1 <- reactive({
      m[,input$xvar1]})
    
    MDplot_y1 <- reactive({
      m[,input$yvar1]})
    
    MDplot_x2 <- reactive({
      m[,input$xvar2]})
    
    MDplot_y2 <- reactive({
      m[,input$yvar2]})
    
    ########   
    d <- SharedData$new(m)
    
    
    # highlight selected rows in the scatterplot
    output$DTPlot1 <- renderPlotly({
      
      s <- input$x1_rows_selected
      
      if (!length(s)) {
        p <- d %>%
          plot_ly(x = MDplot_x1(), y = MDplot_y1(), type = "scatter", size = ~intensity, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
          layout(showlegend = T) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
      } else if (length(s)) {
        pp <- m %>%
          plot_ly() %>% 
          add_trace(x = MDplot_x1(), y = MDplot_y1(), type = "scatter", size = ~intensity, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
          layout(showlegend = T)
        
        # selected data
        pp <- add_trace(pp, data = m[s, , drop = F], x = MDplot_x1(), y = MDplot_y1(), type = "scatter", size = ~intensity, mode = "markers",
                        color = I('red'), name = 'Filtered')
      }
      
    })
    
    # Plot 2
    output$DTPlot2 <- renderPlotly({
      
      t <- input$x1_rows_selected
      
      if (!length(t)) {
        p <- d %>%
          plot_ly(x = MDplot_x2(), y = MDplot_y2(), type = "scatter", size = ~intensity, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
          layout(showlegend = T) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
      } else if (length(t)) {
        pp <- m %>%
          plot_ly() %>% 
          add_trace(x = MDplot_x2(), y = MDplot_y2(), type = "scatter", size = ~intensity, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
          layout(showlegend = T)
        
        # selected data
        pp <- add_trace(pp, data = m[t, , drop = F], x = MDplot_x2(), y = MDplot_y2(), type = "scatter", size = ~intensity, mode = "markers",
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
