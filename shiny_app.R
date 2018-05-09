library(shiny)
library(dplyr)
library(shinythemes)
library(DT)
library(plotly)
library(crosstalk)
library(shinyjs)

# UI function -------------------------------------------------------------

ui <- navbarPage(
  useShinyjs(),
  # Include shinyjs
  theme = shinytheme('spacelab'),
  tabPanel("Mass Defect Interactive Plot",
           fluidPage(
             titlePanel('Interactive MDPlot'),
             sidebarLayout(
               sidebarPanel(
                 fileInput(
                   'file1',
                   'Choose CSV File',
                   accept = c('text/csv',
                              'text/comma-separated-values,text/plain',
                              '.csv')
                 ),
                 radioButtons(inputId="single", label="Single or Double plots", 
                              choices=c("single","double"), inline = TRUE),
                 
                 uiOutput("slide1"),
                 textInput("cus1", "Mass Defect Base 1", 0),
                 textInput("cus2", "Mass Defect Base 2", 0),
                 uiOutput("plotctr"),
                 checkboxInput('ins', 'show intensity as size', F),
                 actionButton('go', 'Plot')
               ),
               mainPanel(
                 uiOutput("plot"),
                 DT::dataTableOutput("x1"),
                 fluidRow(column(
                   3, downloadButton("x3", "Download Filtered Data")
                 )),
                 tags$br(),
                 
                 # Using Shinyjs to open websites
                 fluidRow(
                   h4("Links to web tools for compound search"),
                   column(
                     3,
                     align = "left",
                     actionButton("open_1", "Chemistry Dashboard",
                                  onclick =
                                    "window.open('https://comptox.epa.gov/dashboard/dsstoxdb/advanced_search')")
                   ),
                   column(
                     3,
                     align = "left",
                     actionButton("open_2", "ChemSpider",
                                  onclick =
                                    "window.open('http://www.chemspider.com/FullSearch.aspx')")
                   ),
                   column(
                     2,
                     align = "left",
                     actionButton("open_3", "EnviPat",
                                  onclick =
                                    "window.open('http://www.envipat.eawag.ch')")
                   ),
                   column(
                     2,
                     align = "left",
                     actionButton("open_4", "EnviHomolog",
                                  onclick =
                                    "window.open('http://www.envihomolog.eawag.ch')")
                   ),
                   column(
                     2,
                     align = "left",
                     actionButton("open_5", "Norman Massbank",
                                  onclick =
                                    "window.open('https://massbank.eu/MassBank/QuickSearch.html')")
                   )
                   
                   
                 )
               )
             )
           )),
  tabPanel(
    "Instructions",
    p(
      "Uploaded csv files should contain three columns with name 'mz','rt', 'intensity' and contain mass to charge(m/z), retention time and intensity data. Here is the screenshot of one demo csv file:"
    ),
    br(),
    img(
      src = "csv.png",
      height = 500,
      width = 300
    ),
    br(),
    p(
      "After you uploaded the csv data, input your mass defect base in the input box(es) and click plot to show the MD plots. When you make changes on the left panel, you need to click plot to update the plot. However, you could explore interactively on the plot and table."
    )
  ),
  tabPanel(
    "Mass defect calculation",
    h5('Equation'),
    p(
      "Round: Mass defect = IUPAC Mass * round(exact mass)/exact mass - round(IUPAC Mass * round(exact mass)/exact mass) "
    ),
    br(),
    p(
      "Floor: Mass defect = IUPAC Mass * round(exact mass)/exact mass - floor(IUPAC Mass * round(exact mass)/exact mass) "
    ),
    br(),
    p(
      "Ceiling: Mass defect = IUPAC Mass * round(exact mass)/exact mass - ceiling(IUPAC Mass * round(exact mass)/exact mass) "
    ),
    br(),
    h5('Reference exact mass'),
    p("H: 1.007825, C: 12.0000, N: 14.003074, O: 15.994915,Si: 27.976928"),
    br(),
    p("P: 30.973763, F: 18.998403, Cl: 34.968853, Br: 78.918336, I: 126.904477"),
    br(),
    p("CH2: 14.01565, -H/+Cl: 33.961028, -H/+Br: 77.910511, CF2: 49.996806")
  )
)


# Server function ---------------------------------------------------------


server <- function(input, output, session) {
  MD_data <- reactive({
    #  require that the input is available
    req(input$file1) 
    df <- read.csv(input$file1$datapath)
    # H: 1.007825, C: 12.0000, N: 14.003074, O: 15.994915, Si: 27.976928, P: 30.973763, F: 18.998403, Cl: 34.968853, Br: 78.918336, I: 126.904477, CH2: 14.01565, -H/+Cl: 33.961028, -H/+Br: 77.910511, CF2: 49.996806
    
    if (input$cus1 != 0) {
      cus <- as.numeric(input$cus1)
      df$custom1_round <-
        df$mz * round(cus)/cus  - round(df$mz * round(cus) / cus, digits = 0)
      df$custom1_floor <-
        df$mz * round(cus) / cus  - floor(df$mz * round(cus) / cus) 
      df$custom1_ceiling <-
        df$mz * round(cus)/cus  - ceiling(df$mz * round(cus)/cus)
      
    }
    if (input$cus2 != 0) {
      cus <- as.numeric(input$cus2)
      df$custom2_round <-
        df$mz * round(cus)/cus  - round(df$mz * round(cus) / cus, digits = 0)
      df$custom2_floor <-
        df$mz * round(cus) / cus  - floor(df$mz * round(cus) / cus) 
      df$custom3_ceiling <-
        df$mz * round(cus)/cus  - ceiling(df$mz * round(cus)/cus)
    }
    return(df)
  })
  
  # For uploading Files Panel ##
  output$slide1 <- renderUI({
    minZ <- min(MD_data()$intensity)
    maxZ <- max(MD_data()$intensity)
    
    sliderInput(
      "slide1",
      "Intensity filter",
      min = minZ,
      max = maxZ,
      value = minZ
    )
  })
  
  # for plot control
  output$plot <- renderUI({
    if (input$single == "single") {
      plotlyOutput("DTPlot1")
    } else{
      fluidRow(column(6, plotlyOutput("DTPlot1")),
               column(6, plotlyOutput("DTPlot2")))
    }
  })
  output$plotctr <- renderUI({
    if (input$single == "single") {
      fluidRow(
        h4("Plot controls"),
        tags$br(),
        column(
          5,
          selectInput(
            inputId = 'yvar1',
            label = 'Specify the y variable for plot',
            choices = names(MD_data()),
            selected = names(MD_data())[4]
          )
        ),
        column(
          5,
          selectInput(
            inputId = 'xvar1',
            label = 'Specify the x variable for plot',
            choices = names(MD_data()),
            selected = names(MD_data())[1]
          )
        )
      )
      
    } else{
      fluidRow(
        h4("Plot controls"),
        tags$br(),
        column(
          5,
          selectInput(
            inputId = 'yvar1',
            label = 'Y variable for Plot 1',
            choices = names(MD_data()),
            selected = names(MD_data())[4]
          )
        ),
        column(
          5,
          selectInput(
            inputId = 'yvar2',
            label = 'Y variable for Plot 2',
            choices = names(MD_data()),
            selected = names(MD_data())[5]
          )
        ),
        column(
          5,
          selectInput(
            inputId = 'xvar1',
            label = 'X variable for Plot 1',
            choices = names(MD_data()),
            selected = names(MD_data())[1]
          )
        ),
        column(
          5,
          selectInput(
            inputId = 'xvar2',
            label = 'X variable for Plot 2',
            choices = names(MD_data()),
            selected = names(MD_data())[1]
          )
        )
      )
      
    }
  })
  # add a table of the file
  output$contents <- renderTable({
    if (is.null(MD_data())) {
      return()
    }
    
    if (input$disp == "head") {
      return(head(MD_data()))
    }
    else {
      return(MD_data())
    }
  })
  #### For MD Plot Panel ####
  
  #OE#
  observeEvent(input$go, {
    m <- MD_data()
    m <- m[m$intensity > input$slide1, ]
    d <- SharedData$new(m)
    
    MDplot_y1 <- 
      m[, input$yvar1]
    
    
    MDplot_x1 <- 
      m[, input$xvar1]
    
    if (input$ins) {
      intensity <- m$intensity
    } else{
      intensity <- NULL
    }
    
    if (input$single == "double") {
      MDplot_x2 <- 
        m[, input$xvar2]
      
      
      MDplot_y2 <- 
        m[, input$yvar2]
      
    }
    
    # highlight selected rows in the scatterplot
    output$DTPlot1 <- renderPlotly({
      s <- input$x1_rows_selected
      if (!length(s)) {
        p <- d %>%
          plot_ly(
            x = MDplot_x1,
            y = MDplot_y1,
            type = "scatter",
            size = intensity,
            mode = "markers",
            color = I('black'),
            name = 'Unfiltered'
          ) %>%
          layout(showlegend = T) %>%
          highlight(
            "plotly_selected",
            color = I('red'),
            selected = attrs_selected(name = 'Filtered')
          )
      } else if (length(s)) {
        pp <- m %>%
          plot_ly() %>%
          add_trace(
            x = MDplot_x1,
            y = MDplot_y1,
            type = "scatter",
            size = intensity,
            mode = "markers",
            color = I('black'),
            name = 'Unfiltered'
          ) %>%
          layout(showlegend = T)
        
        # selected data
        pp <-
          add_trace(
            pp,
            data = m[s, , drop = F],
            x = MDplot_x1[s],
            y = MDplot_y1[s],
            type = "scatter",
            size = intensity[s],
            mode = "markers",
            color = I('red'),
            name = 'Filtered'
          )
      }
      
    })
    
    # Plot 2
    if (input$single == "double") {
      output$DTPlot2 <- renderPlotly({
        t <- input$x1_rows_selected
        
        if (!length(t)) {
          p <- d %>%
            plot_ly(
              x = MDplot_x2,
              y = MDplot_y2,
              type = "scatter",
              size = intensity,
              mode = "markers",
              color = I('black'),
              name = 'Unfiltered'
            ) %>%
            layout(showlegend = T) %>%
            highlight(
              "plotly_selected",
              color = I('red'),
              selected = attrs_selected(name = 'Filtered')
            )
        } else if (length(t)) {
          pp <- m %>%
            plot_ly() %>%
            add_trace(
              x = MDplot_x2,
              y = MDplot_y2,
              type = "scatter",
              size = intensity,
              mode = "markers",
              color = I('black'),
              name = 'Unfiltered'
            ) %>%
            layout(showlegend = T)
          
          # selected data
          pp <-
            add_trace(
              pp,
              data = m[t, , drop = F],
              x = MDplot_x2[t],
              y = MDplot_y2[t],
              type = "scatter",
              size = intensity[t],
              mode = "markers",
              color = I('red'),
              name = 'Filtered'
            )
        }
        
      })
    }
    # highlight selected rows in the table
    output$x1 <- DT::renderDataTable({
      T_out1 <- m[d$selection(),]
      dt <-
        DT::datatable(
          m,
          editable = TRUE,
          rownames = FALSE,
          filter = "top"
        )
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
    output$x3 = downloadHandler(
      'MDplot-filtered.csv',
      content = function(file) {
        s <- input$x1_rows_selected
        if (length(s)) {
          write.csv(m[s, , drop = FALSE], file)
        } else if (!length(s)) {
          write.csv(m[d$selection(),], file)
        }
      }
    )
  })
  #OE#
  
  
}

shinyApp(ui, server)
