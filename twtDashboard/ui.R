library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("University of Chicago Tweet Mention Tracking and Abnormality Detection"),
  
  # Buttons
  # actionButton("refresh", "Refresh Data"),
  # selectInput("KPI_switch", "High Fav/RT Count or High Trending Speed?", choices = c('Fav/RT', 'Trendiness')),
  # sliderInput("howold", "Up to how old should the tweets be (30min increments)?", min = 0, max = 240, value = 100),
  # actionButton("populate", "Get Top Unusual Tweets"),
  
  # Sidebar Tweet Select 
  sidebarLayout(
    sidebarPanel(
            selectInput("KPI_switch", "High Fav/RT Count or High Trending Speed?", choices = c('Fav/RT', 'Trendiness')),
            sliderInput("howold", "Up to how old should the tweets be (30min increments)?", min = 0, max = 240, value = 100),
            actionButton("populate", "Get Top Unusual Tweets"),
            selectInput("tweetsKPI", "Tweet ID", choices = NULL)
            # selectInput("tweetsKPI2", "Tweets that Trend the Fastest", choices = NULL)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
            textOutput("abnormScore"),
            textOutput("trendScore"),
            tabsetPanel(
                    tabPanel("Favorite", plotOutput("plotDist_fv")),
                    tabPanel("Retweet", plotOutput("plotDist_rt")),
                    tabPanel("Trendiness", plotOutput("plotDist_trend"))
            )
    )
  )
))
