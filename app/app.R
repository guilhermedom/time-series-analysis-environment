library(shiny)
library(shinythemes)
library(forecast)
library(ggplot2)

# Define UI for application
ui = fluidPage(
    theme = shinytheme("sandstone"),

    # Application title
    titlePanel(h1(align = "center", "Time Series Analysis Environment"),
               windowTitle = "Time Series Analysis Environment"),
    
    br(),
    br(),
    
    fluidRow(
        column(4, align = "center",
               fileInput("fileInputID",
                         "Choose file to load time series:",
                         accept = c("text/csv",
                                    "text/comma-separated-values",
                                    "text/plain",
                                    ".csv")),
               helpText("Note: file must contain only one column, without header.
                        Time series frequency must be in months.")
        ),
        column(4, align = "center",
               dateRangeInput("dateRangeInputID",
                              label = "Time series date range",
                              format = "mm/yyyy",
                              start = "2000/01/01",
                              end = "2013/12/31",
                              startview = "year",
                              separator = " to "),
               helpText("Note: you may select any day when defining month
                        and year.")
        ),
        column(4, align = "center",
               numericInput("forecastPeriodID", "How many months do you want to
                            forecast?", value = 12, min = 1, max = 48),
               actionButton("forecastButtonID", "Forecast!")
        )
    ),
    hr(),
    fluidRow(
        column(6, plotOutput("histogramPlotID")),
        column(6, plotOutput("boxplotPlotID"))
    ),
    fluidRow(
        column(6, plotOutput("timeseriesPlotID")),
        column(6, plotOutput("decompositionPlotID"))
    ),
    hr(),
    fluidRow(
        column(6, plotOutput("forecastPlotID")),
        column(2, align = "center",
               h4(textOutput("lowerBoundTitleID")), tableOutput("lowerBoundTableID")
        ),
        column(2, align = "center",
               h4(textOutput("meanTitleID")), tableOutput("meanTableID")
        ),
        column(2, align = "center",
               h4(textOutput("upperBoundTitleID")), tableOutput("upperBoundTableID")
        )
    )
)

# Define server logic
server = function(input, output) {
    observeEvent(input$forecastButtonID, {
        validate(
            need(input$fileInputID, "Please provide a file having a time series
                 to analyze.")
        )
        
        data = read.csv(input$fileInputID$datapath, header = F)
        
        startYear = as.integer(substr(input$dateRangeInputID[1], 1, 4))
        startMonth = as.integer(substr(input$dateRangeInputID[1], 6, 7))
        endYear = as.integer(substr(input$dateRangeInputID[2], 1, 4))
        endMonth = as.integer(substr(input$dateRangeInputID[2], 6, 7))
        
        timeSeries = ts(data, start = c(startYear, startMonth),
                        end = c(endYear, endMonth), frequency = 12)
        
        output$histogramPlotID = renderPlot({
            hist(timeSeries, xlab = "Data", main = "Data Histogram")
        })
        output$boxplotPlotID = renderPlot({
            boxplot(timeSeries, main = "Data Boxplot")
        })
        output$timeseriesPlotID = renderPlot({
            autoplot(timeSeries, ylab = "Data", main = "Original Time Series")
        })
        
        decomposedSeries = decompose(timeSeries)
        output$decompositionPlotID = renderPlot({
            autoplot(decomposedSeries, main = "Decomposed Time Series")
        })
        
        arimaModel = auto.arima(timeSeries)
        forecastPeriod = input$forecastPeriodID
        prediction = forecast(arimaModel, h = forecastPeriod)
        
        output$lowerBoundTitleID = renderText({
            "Lower Bound"
        })
        output$meanTitleID = renderText({
            "Mean"
        })
        output$upperBoundTitleID = renderText({
            "Upper Bound"
        })
        
        output$lowerBoundTableID = renderTable({
            prediction$lower
        })
        output$meanTableID = renderTable({
            prediction$mean
        })
        output$upperBoundTableID = renderTable({
            prediction$upper
        })
        
        output$forecastPlotID = renderPlot({
            autoplot(prediction, main = "Forecasted Time Series", ylab = "Data")
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
