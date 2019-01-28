
library(shiny)
library(RMySQL)
library(dplyr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
        
        # Helper function to get dataframe from tweet ID
        
        twtID_to_DF <- function(twt_id) {
                twt <- data1[data1$id_str == twt_id, ]
                return(twt)
        }
        
        # KPI Calculation functions
        
        KPI_abnorm <- 
                function(some_dataframe, max_periods) {
                        # Note: dataframe must include columns retweet_count, favorite_count, and t
                        # data1 would be global reference data
                        output <- 0
                        for(i in 1:min(length(some_dataframe$t), max_periods)) {
                                tm <- some_dataframe$t[i]
                                distro_rt <- ecdf(data1$retweet_count[data1$t==tm])
                                distro_fv <- ecdf(data1$favorite_count[data1$t==tm])
                                output <- output + (1 - distro_rt(some_dataframe$retweet_count[some_dataframe$t==tm]))
                                output <- output + (1 - distro_fv(some_dataframe$favorite_count[some_dataframe$t==tm]))
                                
                        }
                        return(output/(2*min(length(some_dataframe$t), max_periods)))
                }
        
        KPI_trendy <- 
                function(some_dataframe, periods=4) {
                        # Looks at last n periods of data, calculate the slope of fav and rt
                        # See where these lie on the distribution of slopes
                        # Return percentile
                        # Note: periods must be at least 1
                        # Note: this function uses global data1
                        # Recommend to look at new tweets because all distros taper off as time increases
                        
                        # Calculate slopes
                        period_end <- some_dataframe$t[nrow(some_dataframe)]
                        period_start <- max((period_end - periods), min(some_dataframe$t))
                        slope_fv <- 
                                (some_dataframe$favorite_count[some_dataframe$t==period_end] - some_dataframe$favorite_count[some_dataframe$t==period_start]) / periods
                        slope_rt <- 
                                (some_dataframe$retweet_count[some_dataframe$t==period_end] - some_dataframe$retweet_count[some_dataframe$t==period_start]) / periods
                        
                        # Get distribution of FV and RT slopes
                        df_start <- data1[data1$t==period_start, ]
                        df_end <- data1[data1$t==period_end, ]
                        common_twts <- intersect(df_start$id_str, df_end$id_str)
                        df_start <- df_start[(df_start$id_str %in% common_twts), ]
                        df_end <- df_end[(df_end$id_str %in% common_twts), ]
                        distro_fv <- ecdf((df_end$favorite_count - df_start$favorite_count)/periods)
                        distro_rt <- ecdf((df_end$retweet_count - df_start$retweet_count)/periods)
                        
                        # Find out percentiles and return result
                        return((distro_fv(slope_fv) + distro_rt(slope_rt))/2)
                }
        
        # Plotting Functions
        
        plot_fv <- function(some_dataframe, limit) {
                # Dataframe of tweet info using same format as above function
                tp <- min(length(some_dataframe$t), limit)
                time_periods <- some_dataframe$t[1:tp]
                ggplot(aes(y = log10(favorite_count+1), x = factor(t)), data = data1[(data1$t %in% time_periods), ]) + 
                        geom_violin() +
                        geom_point(data = some_dataframe[1:tp, ], aes(x = factor(t), y = log10(favorite_count+1)), color = 'red', size = 3) +
                        ggtitle("Tweet Favs Distribution by Time (30 min increments)") +
                        xlab('Time After Creation in 30 min Increments') + 
                        ylab('Log 10 Favorite Count')
        }
        
        plot_rt <- function(some_dataframe, limit) {
                # Dataframe of tweet info using same format as above function
                tp <- min(length(some_dataframe$t), limit)
                time_periods <- some_dataframe$t[1:tp]
                ggplot(aes(y = log10(retweet_count+1), x = factor(t)), data = data1[(data1$t %in% time_periods), ]) + 
                        geom_violin() +
                        geom_point(data = some_dataframe[1:tp, ], aes(x = factor(t), y = log10(retweet_count+1)), color = 'red', size = 3) +
                        ggtitle("Tweet Favs Distribution by Time (30 min increments)") +
                        xlab('Time After Creation in 30 min Increments') + 
                        ylab('Log 10 Favorite Count')
        }
        
        plot_trend <- 
                function(some_dataframe, periods) {
                        # Looks at last n periods of data, calculate the slope of fav and rt
                        # See where these lie on the distribution of slopes
                        # Return plot of trends
                        # Note: periods must be at least 1
                        # Note: this function uses global data1
                        # Recommend to look at new tweets because all distros taper off as time increases
                        
                        # Calculate slopes
                        some_dataframe$trend <- some_dataframe$retweet_count + some_dataframe$favorite_count
                        period_end <- some_dataframe$t[nrow(some_dataframe)]
                        period_start <- max((period_end - periods), min(some_dataframe$t))
                        slope_fv <- 
                                (some_dataframe$trend[some_dataframe$t==period_end] - some_dataframe$trend[some_dataframe$t==period_start]) / periods
                        
                        # Get distribution of FV and RT slopes
                        df_start <- data1[data1$t==period_start, ]
                        df_end <- data1[data1$t==period_end, ]
                        common_twts <- intersect(df_start$id_str, df_end$id_str)
                        df_start <- df_start[(df_start$id_str %in% common_twts), ]
                        df_end <- df_end[(df_end$id_str %in% common_twts), ]
                        df_start$trend <- df_start$retweet_count + df_start$retweet_count
                        df_end$trend <- df_end$retweet_count + df_end$favorite_count
                        distro_fv <- data.frame(slope = (df_end$trend - df_start$trend)/periods)
                        
                        # plot result
                        ggplot(data = distro_fv, aes(slope)) + 
                                geom_histogram() + 
                                geom_vline(aes(xintercept = slope_fv), color = 'red') +
                                ggtitle('Tweet Trendiness and Distribution for Favorites + RTs') +
                                xlab('Trendiness during the past 2 hours') +
                                ylab('')
                }
        
        # Load data from local MySQL server
        
        connection <- 
                dbConnect(MySQL(),user="rtis", password="rtis", dbname="tweets", host="104.154.35.216")
        data1 <- 
                dbGetQuery(connection,
                           "SELECT id_str, t, retweet_count, favorite_count FROM UCHICAGO;")
        dbDisconnect(connection)
        
        # In case you need to load the local copy use:
        # data1 <- read.csv('uchicago.csv')
        
        data1 <- data1[complete.cases(data1), ]
        
        # Remove duplicated scrapes (multiple records of a tweet within the same time bin)
        data1$row_id <- paste(data1$id_str, data1$t, sep = '-')
        dp <- duplicated(data1$row_id)
        data1 <- data1[!dp,]
        
        # Get top popular tweets
        
        observeEvent(input$populate, {
                if(input$KPI_switch == 'Fav/RT') {
                        top <- data1 %>% group_by(id_str) %>% summarize(maxT = max(t), totPop = sum(retweet_count, favorite_count))
                        top <- top %>% filter(maxT <= input$howold) %>% arrange(desc(totPop))
                        top <- top[1:min(10, nrow(top)),1]
                        options <- pull(top, id_str)
                        updateSelectInput(session, 'tweetsKPI', 'Tweets with High Fav + RT Counts', choices = options)
                        print(options)
                } else if(input$KPI_switch == 'Trendiness') {
                        temp <- data1 %>% group_by(id_str) %>% summarize(maxT = max(t))
                        temp <- temp %>% filter(maxT < 20) %>% select(id_str)
                        ids <- pull(temp, id_str)
                        calcs <- NULL
                        for(id in ids) {
                                calc <- KPI_trendy(twtID_to_DF(id))
                                calcs <- c(calcs, calc)
                        }
                        df <- data.frame(id_str = ids, kpi = calcs, stringsAsFactors = FALSE)
                        df <- df[order(-df$kpi), ]
                        vars <- df[1:min(nrow(df), 10), 1]
                        updateSelectInput(session, 'tweetsKPI', 'Tweets that Trend the Fastest', choices = vars)
                        print(vars)
                }
                # top <- data1 %>% group_by(id_str) %>% summarize(maxT = max(t), totPop = sum(retweet_count, favorite_count))
                # top <- top %>% filter(maxT <= input$howold) %>% arrange(desc(totPop))
                # top <- top[1:min(10, nrow(top)),1]
                # options <- pull(top, id_str)
                # updateSelectInput(session, 'tweetsKPI1', 'Tweets with High Fav + RT Counts', choices = options)
                # print(options)
        })
        
        # observeEvent(input$populate, {
        #         temp <- data1 %>% group_by(id_str) %>% summarize(maxT = max(t))
        #         temp <- temp %>% filter(maxT < 6) %>% select(id_str)
        #         ids <- pull(temp, id_str)
        #         calcs <- NULL
        #         for(id in ids) {
        #                 calc <- KPI_trendy(twtID_to_DF(id))
        #                 calcs <- c(calcs, calc)
        #         }
        #         df <- data.frame(id_str = ids, kpi = calcs)
        #         df <- df[order(-df$kpi), ]
        #         vars <- df[min(nrow(df), 10), 1]
        #         updateSelectInput(session, 'tweetKPI2', 'Tweets that Trend the Fastest', choices = vars)
        #         print(vars)
        # 
        # })
        
        
        output$abnormScore <- renderText({
                paste("Prob. Normal Tweet:", KPI_abnorm(twtID_to_DF(input$tweetsKPI), nrow(twtID_to_DF(input$tweetsKPI))))
        })
        output$trendScore <- renderText({
                paste("Trendiness Percentile:", KPI_trendy(twtID_to_DF(input$tweetsKPI), 4))
        })
        output$testTable <- renderTable({
                head(data1)
        })
        
        output$plotDist_fv <- renderPlot({
                plot_fv(twtID_to_DF(input$tweetsKPI), 10)
        })
        
        output$plotDist_rt <- renderPlot({
                plot_rt(twtID_to_DF(input$tweetsKPI), 10)
        })
        
        output$plotDist_trend <- renderPlot({
                plot_trend(twtID_to_DF(input$tweetsKPI), 4)
        })
  
})
