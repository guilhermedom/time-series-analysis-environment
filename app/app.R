library(shiny)
library(shinythemes)
library(forecast)
library(ggplot2)

# Define UI for application.
ui = fluidPage(
    theme = shinytheme("sandstone"),

    # Application title.
    titlePanel(h1(align = "center", "Time Series Analysis Environment"),
               windowTitle = "Time Series Analysis Environment"),
    
    br(),
    br(),
    
    fluidRow(
        # Input file browsing.
        column(4, align = "center",
               fileInput("fileInputID",
                         "Choose file to load time series:",
                         accept = c("text/csv",
                                    "text/comma-separated-values",
                                    "text/plain",
                                    ".csv")),
               helpText("Note: file must contain only one column without header.
                        Time series frequency must be in months.")
        ),
        
        # Date range to plot time series. Hint start and end dates are set like
        # this because it fits better the example file.
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
        
        # Forecast period input. Hint set as 12 because it works best for the
        # example dataset. max is set as 48 because predictions outside this
        # range are unreliable with currently available models.
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
    
    # Plot original time series and its decomposition next to it. The
    # decomposition has trend, season and remainder information plotted.
    fluidRow(
        column(6, plotOutput("timeseriesPlotID")),
        column(6, plotOutput("decompositionPlotID"))
    ),
    hr(),
    
    # Tables with mean, lower and upper bound for forecasted time series.
    fluidRow(
        column(6, plotOutput("forecastPlotID")),
        column(2, align = "center",
               h4(textOutput("lowerBoundTitleID")),
               tableOutput("lowerBoundTableID")
        ),
        column(2, align = "center",
               h4(textOutput("meanTitleID")),
               tableOutput("meanTableID")
        ),
        column(2, align = "center",
               h4(textOutput("upperBoundTitleID")),
               tableOutput("upperBoundTableID")
        )
    )
)

# Define server logic.
server = function(input, output) {
    # All events happen when the user clicks the "Forecast!" button.
    observeEvent(input$forecastButtonID, {
        # Validation needed to avoid crashes when no input file is given.
        validate(
            need(input$fileInputID, "Please provide a file having a time series
                 to analyze.")
        )
        
        data = read.csv(input$fileInputID$datapath, header = F)
        
        startYear = as.integer(substr(input$dateRangeInputID[1], 1, 4))
        startMonth = as.integer(substr(input$dateRangeInputID[1], 6, 7))
        endYear = as.integer(substr(input$dateRangeInputID[2], 1, 4))
        endMonth = as.integer(substr(input$dateRangeInputID[2], 6, 7))
        
        # As the time series has a monthly basis, frequency is equal to 12.
        timeSeries = ts(data, start = c(startYear, startMonth),
                        end = c(endYear, endMonth), frequency = 12)
        
        output$histogramPlotID = renderPlot({
            hist(timeSeries, main = "Data Histogram", xlab = "Data")
        })
        output$boxplotPlotID = renderPlot({
            boxplot(timeSeries, main = "Data Boxplot")
        })
        
        output$timeseriesPlotID = renderPlot({
            autoplot(timeSeries, main = "Original Time Series", ylab = "Data")
        })
        decomposedSeries = decompose(timeSeries)
        output$decompositionPlotID = renderPlot({
            autoplot(decomposedSeries, main = "Decomposed Time Series")
        })
        
        # auto.arima automatically find appropriate parameters using the
        # training data.
        arimaModel = auto.arima(timeSeries)
        forecastPeriod = input$forecastPeriodID
        prediction = forecast(arimaModel, h = forecastPeriod)
        
        # Mean, lower and upper bound tables for forecasted time series.
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

# Run the application.
shinyApp(ui = ui, server = server)
