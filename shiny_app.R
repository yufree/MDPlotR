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
  tabPanel(
    "MDPlotR: interactive mass defect plots",
    fluidPage(sidebarLayout(
      sidebarPanel(
        fileInput(
          'file1',
          'Choose CSV File',
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        ),
        radioButtons(
          inputId = "single",
          label = "Single or Double plots",
          choices = c("Single", "Double"),
          selected = "Single",
          inline = TRUE
        ),
        fluidRow(column(6,
                        textInput(
                          "cus1", "MD Base 1", value = 0
                        )),
                 column(
                   6,
                   selectInput(
                     inputId = "mdr1",
                     label = "Rounding 1",
                     choices = c("round", "floor", "ceiling"),
                     selected = "round"
                   )
                 )),
        
        fluidRow(column(6,
                        textInput(
                          "cus2", "MD Base 2", value = 0
                        )),
                 column(
                   6,
                   selectInput(
                     inputId = "mdr2",
                     label = "Rounding 2",
                     choices = c("round", "floor", "ceiling"),
                     selected = "round"
                   )
                 )),
        actionButton('go', 'Plot'),
        uiOutput("plotctr"),
        uiOutput("plotctr2"),
        uiOutput("slide1"),
        uiOutput("slide2"),
        uiOutput("slide3"),
        checkboxInput('ins', 'Show intensity as size', F),
        width = 3
      ),
      mainPanel(
        uiOutput("plot"),
        DTOutput("x1"),
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
    ))
  ),
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
    p(
      "H: 1.007825, C: 12.0000, N: 14.003074, O: 15.994915,Si: 27.976928"
    ),
    br(),
    p(
      "P: 30.973763, F: 18.998403, Cl: 34.968853, Br: 78.918336, I: 126.904477"
    ),
    br(),
    p(
      "CH2: 14.01565, -H/+Cl: 33.961028, -H/+Br: 77.910511, CF2: 49.996806"
    )
  )
)


# Server function ---------------------------------------------------------


server <- function(input, output, session) {
  MD_data <- reactive({
    #  require that the input is available
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    
    
    if (input$cus1 != 0) {
      cus <- as.numeric(input$cus1)
      if (input$mdr1 == 'round') {
        df$MD1 <-
          round(
            df$mz * round(cus) / cus  - round(
              df$mz * round(cus) / cus,
              digits = 0
            ),
            digits = 6
          )
      } else if (input$mdr1 == 'floor') {
        df$MD1 <-
          round(
            df$mz * round(cus) / cus  - floor(df$mz * round(cus) / cus) ,
            digits = 6
          )
      } else{
        df$MD1 <-
          round(
            df$mz * round(cus) / cus  - ceiling(df$mz * round(cus) / cus),
            digits = 6
          )
      }
    }
    if (input$cus2 != 0) {
      cus <- as.numeric(input$cus2)
      if (input$mdr2 == 'round') {
        df$MD2 <-
          round(
            df$mz * round(cus) / cus  - round(
              df$mz * round(cus) / cus,
              digits = 0
            ),
            digits = 6
          )
      } else if (input$mdr2 == 'floor') {
        df$MD2 <-
          round(
            df$mz * round(cus) / cus  - floor(df$mz * round(cus) / cus) ,
            digits = 6
          )
      } else{
        df$MD2 <-
          round(
            df$mz * round(cus) / cus  - ceiling(df$mz * round(cus) / cus),
            digits = 6
          )
      }
    }
    return(df)
  })
  
  # Filtering the intensity, mz, and rt
  output$slide1 <- renderUI({
    minZ <- min(MD_data()$intensity)
    maxZ <- max(MD_data()$intensity)
    
    sliderInput(
      "slide1",
      "Intensity range filter",
      min = minZ,
      max = maxZ,
      value = c(minZ, maxZ)
    )
  })
  output$slide2 <- renderUI({
    minZ <- min(MD_data()$mz)
    maxZ <- max(MD_data()$mz)
    
    sliderInput(
      "slide2",
      "mass to charge ratio range",
      min = minZ,
      max = maxZ,
      value = c(minZ, maxZ)
    )
  })
  output$slide3 <- renderUI({
    minZ <- min(MD_data()$rt)
    maxZ <- max(MD_data()$rt)
    
    sliderInput(
      "slide3",
      "retention time range",
      min = minZ,
      max = maxZ,
      value = c(minZ, maxZ)
    )
  })
  
  
  ## for plot control ##
  output$plot <- renderUI({
    if (input$single == "Single") {
      plotlyOutput("DTPlot1")
    } else{
      fluidRow(column(6, plotlyOutput("DTPlot1")),
               column(6, plotlyOutput("DTPlot2")))
    }
  })
  output$plotctr <- renderUI({
    if (input$single == "Single") {
      fluidRow(
        h4("Plot controls"),
        tags$br(),
        column(
          6,
          selectInput(
            inputId = 'xvar1',
            label = 'X variable for plot',
            choices = names(MD_data()),
            selected = names(MD_data())[1]
          )
        ),
        column(
          6,
          selectInput(
            inputId = 'yvar1',
            label = 'Y variable for plot',
            choices = names(MD_data()),
            selected = names(MD_data())[4]
          )
        )
      )
      
    } else{
      fluidRow(
        h4("Plot controls"),
        tags$br(),
        column(
          6,
          selectInput(
            inputId = 'xvar1',
            label = 'X variable for Plot 1',
            choices = names(MD_data()),
            selected = names(MD_data())[1]
          )
        ),
        column(
          6,
          selectInput(
            inputId = 'yvar1',
            label = 'Y variable for Plot 1',
            choices = names(MD_data()),
            selected = names(MD_data())[4]
          )
        ),
        column(
          6,
          selectInput(
            inputId = 'xvar2',
            label = 'X variable for Plot 2',
            choices = names(MD_data()),
            selected = names(MD_data())[1]
          )
        ),
        column(
          6,
          selectInput(
            inputId = 'yvar2',
            label = 'Y variable for Plot 2',
            choices = names(MD_data()),
            selected = names(MD_data())[4]
          )
        )
      )
    }
  })
  output$plotctr2 <- renderUI({
          if (input$single == "Single") {
                  fluidRow(
                          tags$br(),
                          textInput('x1', 'x axis label', input$xvar1),
                          textInput('y1', 'y axis label', input$yvar1))
                  } else{
                                  fluidRow(
                                          tags$br(),
                                          textInput('x1', 'x axis label for plot 1', input$xvar1),
                                          textInput('y1', 'y axis label for plot 1', input$yvar1),
                                          textInput('x2', 'x axis label for plot 2', input$xvar2),
                                          textInput('y2', 'y axis label for plot 2', input$yvar2)          
                          )}
          
  })
  #### For MD Plot Panel ####
  
  #OE#
  observeEvent(input$go, {
    m <- MD_data()
    m <-
      m[m$intensity >= input$slide1[1] &
          m$intensity <= input$slide1[2] &
          m$mz >= input$slide2[1] &
          m$mz <= input$slide2[2] &
          m$rt >= input$slide3[1] & m$rt <= input$slide3[2],]
    d <- SharedData$new(m)
    
    MDplot_y1 <-
      m[, input$yvar1]
    
    MDplot_x1 <-
      m[, input$xvar1]
    
    # Checkbox option for size of markers by intensity
    if (input$ins) {
      intensity <- m$intensity
    } else{
      intensity <- NULL
    }
    
    if (input$single == "Double") {
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
            marker = list(
              line = list(
                width = 1,
                color = '#FFFFFF'
              )
            ),
            color = I('black'),
            name = 'Unfiltered'
          ) %>%
          layout(
            legend = list(
              orientation = "h",
              xanchor = "center",
              x = 0.5,
              y = 100
            ),
            showlegend = T,
            xaxis = list(title = input$x1),
            yaxis = list(title = input$y1)
          ) %>%
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
            marker = list(
              line = list(
                width = 1,
                color = '#FFFFFF'
              )
            ),
            color = I('black'),
            name = 'Unfiltered'
          ) %>%
          layout(
            legend = list(
              orientation = "h",
              xanchor = "center",
              x = 0.5,
              y = 100
            ),
            showlegend = T,
            xaxis = list(title = input$x1),
            yaxis = list(title = input$y1)
          )
        
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
            marker = list(
              line = list(
                width = 1,
                color = '#FFFFFF'
              )
            ),
            color = I('red'),
            name = 'Filtered'
          )
      }
      
    })
    
    # Plot 2
    if (input$single == "Double") {
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
              marker = list(
                line = list(
                  width = 1,
                  color = '#FFFFFF'
                )
              ),
              color = I('black'),
              name = 'Unfiltered'
            ) %>%
            layout(
              legend = list(
                orientation = "h",
                xanchor = "center",
                x = 0.5,
                y = 100
              ),
              showlegend = T,
              xaxis = list(title = input$x2),
              yaxis = list(title = input$y2)
            ) %>%
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
              marker = list(
                line = list(
                  width = 1,
                  color = '#FFFFFF'
                )
              ),
              color = I('black'),
              name = 'Unfiltered'
            ) %>%
            layout(
              legend = list(
                orientation = "h",
                xanchor = "center",
                x = 0.5,
                y = 100
              ),
              showlegend = T,
              xaxis = list(title = input$x2),
              yaxis = list(title = input$y2)
            )
          
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
              marker = list(
                line = list(
                  width = 1,
                  color = '#FFFFFF'
                )
              ),
              color = I('red'),
              name = 'Filtered'
            )
        }
        
      })
    }
    # highlight selected rows in the table
    output$x1 <- renderDT({
      T_out1 <- m[d$selection(), ]
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
          write.csv(m[d$selection(), ], file)
        }
      }
    )
  })
  #OE#
  
  
}

shinyApp(ui, server)
