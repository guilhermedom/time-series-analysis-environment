# Time Series Analysis Environment

A web-based environment for analyzing time series and forecasting with them.

### Features in This App

A series of plots present the time series provided by the user and its forecasting. Many interactions can be performed with the app:

* The user can browse the file system and input any file having a monthly time series;
* A date range can be defined for the input time series with start and end date. This range is used to form the "Time" x-axis of the plots;
* Additionally, the user can inform any number of months to forecast using the ARIMA model on the time series;
* A histogram illustrates the data distribution;
* A boxplot visually provides more details about the data;
* The original time series is plotted as received from input;
* A decomposition plot presents the following details about the time series:
    1. Trend;
    2. Seasonality;
    3. Remainder.
* The forecasted time series is plotted with the forecast appended to the end of the original time series;
* To the right of the forecasting plot, a table gives the lower bound, mean and upper bound for each month forecasted.

### User Interface Sample

![ui_time-series-analysis-environment](https://user-images.githubusercontent.com/33037020/185267643-c48dfafc-5923-40e4-be2a-afc7712e138e.png)

*[Shiny] is a framework that allows users to develop web apps using R and embedded web languages, such as CSS and HTML. Shiny apps focus on objectiveness and simplicity: only one or two R scripts have all the code for the app.*

*This app development started with knowledge and tools discussed during the course "Data Science Bootcamp" by Fernando Amaral. The app has been upgraded and personalized, adding new functionalities.*

[//]: #

[Shiny]: <https://www.shinyapps.io>
