library(dplyr)
library(ggplot2)
library(forecast)

# normalize names
n <- tolower(names(dataset))
measure.name <- n[n!="selected" & n!="date"]
measure.name <- paste0(toupper(substr(measure.name, 1,1)),
                       tolower(substr(measure.name,2,100)))
n[n!="selected" & n!="date"] <- "measure"
dataset <- setNames(dataset, n)

# date
dataset$date <- as.Date(dataset$date)

# parse selected
v <- strsplit(dataset[1,"selected"],"_")
selected.start <- as.Date(v[[1]][1])
selected.end <- as.Date(v[[1]][2])

# original end point
last.date <- max(filter(dataset, !is.na(measure))$date)

# remove added blank data points
dataset <- filter(dataset, date<=last.date)

start <- min(dataset$date)
end <- max(dataset$date)

cal <- seq(as.Date(start), as.Date(end), 1) 
df.cal <- data.frame(x=1:length(cal), date=cal) 

dataset <- left_join(df.cal, dataset, by=c("date"="date"))

dataset[is.na(dataset$measure),"measure"] <- 0

d <- selected.end-selected.start

labels <- "%b %d, %y"
breaks <- "1 month"
breaks.minor <- structure(list(), class = "waiver")

if(d<=366)
{
  breaks.minor = "1 day"
}

if(d<=183)
{
  breaks = "1 week"
}

if(d<=31)
{
  breaks = "1 day"
}

theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
  plot.subtitle = element_text(hjust = 0.5, size = 11),
  plot.caption = element_text(face = "italic", size = 11),
  axis.title = element_text(face = "italic", size = 11),
  axis.text.x=element_text(angle = 45, hjust = 1, size = 11),
  axis.text.y=element_text(size = 11)
)
df <- filter(dataset, date>=selected.start & date<=selected.end)

gg <- ggplot(df) +
  geom_line(aes(x=date,y=measure)) +
  scale_x_date(date_labels=labels, 
               breaks=breaks,
               date_minor_breaks=breaks.minor
  ) +
  theme +
  labs(title = "Data and Forecast", 
       x = "Date", 
       y = measure.name, 
       subtitle = "with 95% confidence interval",
       caption = "Forecast is blue line") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# forecast
fd <- selected.end-last.date

if(fd > 0)
{
  ts <- msts(dataset$measure, seasonal.periods=c(7,50))
  mstl.br <- mstl(ts)
  
  f <- forecast(mstl.br, h=fd)
  
  df.f <- as.data.frame(f)
  df.f$date = seq(last.date,last.date+count(df.f)$n-1,1)
  
  gg <- gg +
    geom_line(data=df.f, aes(x=date,y=`Point Forecast`), colour="blue") +
    geom_line(data=df.f, aes(x=date,y=`Lo 95`), colour="steelblue") +
    geom_line(data=df.f, aes(x=date,y=`Hi 95`), colour="steelblue") +
    geom_ribbon(data=df.f, aes(x=date,ymin=`Lo 95`,ymax=`Hi 95`), alpha=0.3, fill="blue")
  
}
gg

###
library(shiny)
library(ggplot2)

ui <- fluidPage(
  mainPanel(width=6, style="border-style: solid; border-color: black; border-width: thin",
            plotOutput(outputId="Plot1", width="100%", height="200px")
  ),
  
  mainPanel(width=6, style="border-style: solid; border-color: black; border-width: thin",
            plotOutput(outputId="Plot2", width="100%", height="200px")
  ),
  
  mainPanel(width=6, style="border-style: solid; border-color: black; border-width: thin",
            plotOutput(outputId="Plot3", width="100%", height="200px")
  ),
  
  mainPanel(width=6, style="border-style: solid; border-color: black; border-width: thin",
            plotOutput(outputId="Plot4", width="100%", height="200px")
  ),
  
  mainPanel(width=12, style="border-style: solid; border-color: black; border-width: thin",
            actionButton(inputId="Btn1", "Refresh top"),
            actionButton(inputId="Btn2", "Refresh bottom")
  )  
)

server <- function(input, output){
  output$Plot1 <- renderPlot({
    if(input$Btn1%%2==0) y<-1:10 else y<-10:1
    ggplot(data.frame(x=1:10,y=y)) +geom_line(aes(x=x,y=y), colour="red")
  })
  output$Plot2 <- renderPlot({
    if(input$Btn1%%2==0) y<-1:10 else y<-10:1
    ggplot(data.frame(x=1:10,y=y)) +geom_col(aes(x=x,y=y), fill="red")
  })
  
  output$Plot3 <- renderPlot({
    if(input$Btn2%%2==0) y<-1:10 else y<-10:1
    ggplot(data.frame(x=1:10,y=y)) +geom_line(aes(x=x,y=y), colour="blue")
  })
  output$Plot4 <- renderPlot({
    if(input$Btn2%%2==0) y<-1:10 else y<-10:1
    ggplot(data.frame(x=1:10,y=y)) +geom_col(aes(x=x,y=y), fill="blue")
  })
  
}

shinyApp(ui=ui, server=server)

appForecast <- function(dataset, selected.start, selected.end, measure.name)
{
  
  # dataset: dataframe, has to be defined as follows:
  #   date, <measures you want to forecast for, for instance orders, sales, units>
  #   column date must be of Date type!
  # selected.start, selected.end: selected date range
  # measure.name: is the name of the measure you plot 
  
  require(dplyr)
  require(ggplot2)
  require(forecast)
  
  # for all days beyond the original end point a forecast will be built and displayed
  # for days before the original data will be used.
  
  measure <- tolower(measure.name) 
  dataset <- dataset[,c("date", measure)]
  
  n <- names(dataset)
  n[n!="date"] <- "measure"
  dataset <- setNames(dataset, n)
  
  start <- min(dataset$date)
  end <- max(dataset$date)
  
  cal <- seq(as.Date(start), as.Date(end), 1) 
  df.cal <- data.frame(x=1:length(cal), date=cal) 
  
  dataset <- left_join(df.cal, dataset, by=c("date"="date"))
  
  dataset[is.na(dataset$measure),"measure"] <- 0
  
  # original end point
  last.date <- max(dataset$date)
  
  theme <- theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.caption = element_text(face = "italic", size = 11),
    axis.title = element_text(face = "italic", size = 11),
    axis.text.x=element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y=element_text(size = 11)
  )
  
  d <- selected.end-selected.start
  
  labels <- "%b %d, %y"
  breaks <- "1 month"
  breaks.minor <- structure(list(), class = "waiver")
  
  if(d<=366)
  {
    breaks.minor = "1 day"
  }
  
  if(d<=183)
  {
    breaks = "1 week"
  }
  
  if(d<=31)
  {
    breaks = "1 day"
  }
  
  df <- filter(dataset, date>=selected.start & date<=selected.end)
  
  gg <- ggplot(df) +
    geom_line(aes(x=date,y=measure)) +
    scale_x_date(date_labels=labels, 
                 breaks=breaks,
                 date_minor_breaks=breaks.minor
    ) +
    theme +
    labs(title = "Data and Forecast", 
         x = "Date", 
         y = measure.name, 
         subtitle = "with 95% confidence interval",
         caption = "Forecast is blue line") +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
  # forecast
  fd <- selected.end-last.date
  
  if(fd > 0)
  {
    ts <- msts(dataset$measure, seasonal.periods=c(7,50))
    mstl.br <- mstl(ts)
    
    f <- forecast(mstl.br, h=fd)
    
    df.f <- as.data.frame(f)
    df.f$date = seq(last.date,last.date+count(df.f)$n-1,1)
    
    gg <- gg +
      geom_line(data=df.f, aes(x=date,y=`Point Forecast`), colour="blue") +
      geom_line(data=df.f, aes(x=date,y=`Lo 95`), colour="steelblue") +
      geom_line(data=df.f, aes(x=date,y=`Hi 95`), colour="steelblue") +
      geom_ribbon(data=df.f, aes(x=date,ymin=`Lo 95`,ymax=`Hi 95`), alpha=0.3, fill="blue")
    
  }
  gg
}

# install.packages("shiny", dependencies = TRUE)

library(shiny)
library(dplyr)
library(lubridate)

# >> data
source("../../Modules/dbData.R", local = TRUE)
source("../appForecast.R", local = TRUE)

dataset <- db.data(vw="vwOrdersByDay",db="BrazilianEcommerce")
# << data

# normalize names
n <- tolower(names(dataset))
dataset <- setNames(dataset, n)

# date
dataset$date <- as.Date(dataset$date)

# remove data points in question
dataset <- filter(dataset, date>="2017-01-05" & date<"2018-08-20")

date.start <- min(dataset$date)
date.end <- max(dataset$date)

date.min <- date.start
date.max <- date.end +180

ui <- fluidPage(
  mainPanel(width=12,
            style = "border-style: solid; border-color: black; border-width: thin",
            div(style="display: table-cell; vertical-align: middle",
                dateRangeInput(inputId="range",
                               label="Select Date Range",
                               start=date.start,
                               end=date.end,
                               min=date.min,
                               max=date.max)),
            div(style="display: table-cell; vertical-align: middle; padding-left: 100px",
                div(style="padding-top: 15%",
                    actionButton(inputId="btnGo", label="Refresh")))
  ),
  mainPanel(width = 12, 
            plotOutput("Plot1", width = "100%")
  ),
  mainPanel(width = 12, 
            style = "border-style: solid; border-color: black; border-width: thin",
            radioButtons(inputId="measure", 
                         label=NULL,
                         choiceNames=list("Orders", "Sales", "Units"),
                         choiceValues=list("orders","sales","units"),
                         inline=TRUE,
                         selected=c("orders"))
  )
)


server <- function(input, output)
{
  
  observeEvent(c(input$btnGo, input$measure), {
    output$Plot1 <- renderPlot({
      appForecast(dataset,
                  selected.start = isolate(input$range[1]),
                  selected.end = isolate(input$range[2]),
                  measure.name = isolate(input$measure))
      
    })
  }, ignoreNULL = FALSE)
  
  # render other panels here if applicable...
}

shinyApp(ui=ui, server=server)

