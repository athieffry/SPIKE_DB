#### R SHINY APP FOR SPIKE DATA - SERVER
#### Axel Thieffry - August 2019
library(tidyverse)
library(tidylog)
library(magrittr)
library(ggplot2)
library(ggridges)
library(ggplotify)
library(patchwork)
library(reshape2)
library(treemapify)
library(anytime)
library(scales)
library(timevis)
library(shiny)
library(shinythemes)
library(DT)
library(RColorBrewer)
library(plotly)
library(lubridate)
library(RSQLite)
library(waffle)
options(scipen=999)
'rename' <- dplyr::rename

#### TODO LIST
# Change pie-chart to barplot !!!!
# Add a "Date range" input for visualizing several days in a row (with max?)
# Add checkbox to enable/disable piechart on the side (+ adapt length of fluidRow)


# 0. PARAMETERS ####
# ------------------
# make many colors
fill_colors <- rep(brewer.pal(n=12, name='Paired'), 3)
# day start & stop (for coloring daytime)
day_start <- as.POSIXct('08:00', format="%H:%M", tz='UTC')
day_stop <- as.POSIXct('23:00', format="%H:%M", tz='UTC')
# day start & stop (for BG readings time axis)
x_start <- as.POSIXct('00:00', format="%H:%M", tz='UTC')
x_stop <- as.POSIXct('23:59:59', format="%H:%M:%S", tz='UTC')
# colors for low, nominal and high
range_cols <- setNames(brewer.pal(n=6, name='Set1')[c(1, 3, 5)], c('low', 'nominal', 'high'))
# hack for piecharts to have different scales
cp <- coord_polar(theta = "y")
cp$is_free <- function() TRUE




# 1. GET & PARSE DATA ####
# ------------------------
#### SPIKE
spike_db <- dbConnect(RSQLite::SQLite(), 'spike_2019_November_13.db')
spike <- dbReadTable(spike_db, 'bgreading')
# remove un-necessary columns
spike %<>% select(bgreadingid, timestamp, sensorid, noise, calibrationid, calibrationFlag, filteredCalculatedValue)
colnames(spike) <- c('reading_id', 'timestamp', 'sensor_id', 'noise', 'calib_id', 'calib_flag', 'value')
# convert timestamp (if timestamp with milliseconds, then 13 numbers long, otherwise 10)
spike %<>% mutate('datetime'=as_datetime(timestamp/1000, tz='UTC')) %>%
           mutate('day'=as.Date(datetime)) %>%
           mutate('time'=strftime(datetime, format='%Y-%m-%d %H:%M:%S', tz='GMT')) %>%
           mutate('time'=as.POSIXct(time) %>% format(format='%H:%M:%S')) %>% as_tibble()
# get variable names
varnames <- colnames(spike) %>% as.list()
names(varnames) <- c('Reading ID', 'Timestamp', 'Sensor ID', 'Noise', 'Calibration ID', 'Calibration flag',
                     'BG Reading', 'DateTime', 'Day', 'Time')


dbListTables(spike_db)
dbReadTable(spike_db, 'basalrates')


#### CALIBRATION DATA
calib <- dbReadTable(spike_db, 'calibration') %>% as_tibble()
dbDisconnect(spike_db)
calib %<>% select(timestamp, sensorid, calibrationid)
# convert timestamp
calib %<>% mutate('datetime'=as_datetime(timestamp/1000, tz='UTC')) %>%
          mutate('day'=as.Date(datetime)) %>%
          mutate('time'=strftime(datetime, format='%Y-%m-%d %H:%M:%S', tz='GMT')) %>%
          mutate('time'=as.POSIXct(time) %>% format(format='%H:%M:%S')) %>%
          rename('sensor_id'='sensorid', 'calib_id'='calibrationid') %>% as_tibble()


# 2. BUILD UI ####
# ----------------
ui <- fluidPage(theme=shinytheme('flatly'),
  navbarPage('SPIKE Explorer v0.1',
    tabPanel(title='DB Summary',
             fluidRow(
               column(3, tableOutput('summary_db')),
               column(3, tableOutput('summary_bg')),
               column(3, tableOutput('summary_sensors')),
               column(3, tableOutput('summary_calib')))),
    tabPanel(title='BG Daily Readings',
             wellPanel(
               fluidRow(
                   column(2, dateInput(inputId='day', label=h4('Day:'), value=spike$day[1], min=spike$day[1], max=spike$day[nrow(spike)], startview='month', format='yyyy-mm-dd', weekstart=1, language='en', width='100%')),
                   column(3, sliderInput(inputId='bglimits', label=h4('BG range limits'), min=0, max = 15, value = c(3.2, 8.2), animate=F, step=.1, ticks=T)),
                   column(2, selectInput(inputId='unit', label='BG unit', choices=list('mmol/L'=T, 'mg/dL'=F), selected=T)),
                   column(1, checkboxInput(inputId='show_calib', label='Calibration', value=TRUE)),
                   column(1, checkboxInput(inputId='show_daytime', label='Daytime', value=TRUE))
                       )
                      ),
             fluidRow(splitLayout(cellWidths=c('80%', '20%'), plotlyOutput('bg_line'), plotOutput('bg_pie')))
             ),

    tabPanel(title='FreeStyle Libre sensor',
             fluidRow(splitLayout(cellWidths=c('50%', '25%', '25%'), plotOutput('sensor_bg_density_plot'), plotOutput('gg_sensor_readings'), plotOutput('sensor_calibrations')))
             ),
    
    tabPanel(title='Sensor details',
             mainPanel(DTOutput('sensor_reading_table'))),
    
    tabPanel(title='Spike calibrations',
             wellPanel(
               fluidRow(
                 column(2, sliderInput(inputId='before_min', label='Minutes before calibration', min=5, max=60, value=30, step=5, animate=F)),
                 column(2, sliderInput(inputId='after_min', label='Minutes after calibration', min=5, max=60, value=15, step=5, animate=F))
               )
             ),
             mainPanel(plotOutput('gg_interval')))
    )
  )



# 3. SERVER ####
# --------------
server <- function(input, output) {
  
  # classify by bg range limits
  spike_ranged <- reactive({
                      mutate(spike, 'range'=case_when(value/18 <= input$bglimits[1]  ~ 'low',
                                                      value/18 >= input$bglimits[2] ~ 'high',
                                                      value/18 > input$bglimits[1] & value/18 < input$bglimits[2] ~ 'nominal',
                                                      TRUE ~ 'other_range'))
                  })
  
          # //// DEBUG /////
  
    
  # number of calibrations per sensor
  output$sensor_calibrations <- renderPlot({
                                    ggplot(calib, aes(x=sensor_id, fill=sensor_id)) +
                                           geom_bar(alpha=.7, lwd=.3, col='black') +
                                           cowplot::theme_cowplot() + theme(legend.position='none', aspect.ratio=1, axis.text.x=element_text(angle=90)) +
                                           scale_fill_manual(values=fill_colors) +
                                           labs(x='Sensor ID', y='Nb. of calibrations', title='Calibrations')
                                })
  
  # BG density per sensor
  output$sensor_bg_density_plot <- renderPlot({
                                        ggplot(spike_ranged(), aes(x=value/18, y=sensor_id, fill=sensor_id)) +
                                               geom_density_ridges2(alpha=0.7) +
                                               geom_vline(xintercept=c(input$bglimits[1], input$bglimits[2]), lty=2) +
                                               cowplot::theme_cowplot() + theme(legend.position='none') +
                                               labs(title='Distribution of BG readings per sensor', x='BG (mg/dL)', y='Sensor ID') +
                                               scale_fill_manual(values=fill_colors)
                                    })
    
  # Sensor readings table
  sensor_df <- reactive({
                  spike_ranged() %>%
                  select(reading_id, sensor_id, calib_id, day, noise, datetime) %>%
                  group_by(sensor_id) %>%
                  summarise('readings'=n_distinct(reading_id),
                            'calibrations'=n_distinct(calib_id),
                            'mean noise'=round(mean(as.numeric(noise)), digits=2),
                            'start'=min(day), 'end'=max(day),
                            'uptime'=paste(max(day)-min(day), 'days'))
                })
    
  output$sensor_reading_table <- renderDT( sensor_df(), options=list(pageLength=20) )
    
  # Number of readings per sensor
  output$gg_sensor_readings <- renderPlot({
                                  ggplot(sensor_df(), aes(x=sensor_id, y=readings, fill=sensor_id)) +
                                         geom_bar(stat='identity', alpha=0.7, lwd=.3, col='black') +
                                         cowplot::theme_cowplot() +
                                         theme(axis.text.x=element_text(angle=90), legend.position='none', aspect.ratio=1) +
                                         scale_fill_manual(values=fill_colors) +
                                         labs(title='Sensor summary', y='Nb. readings', x='Sensor ID')
                                })
    
  # get calibrations of the day
  day_calibrations_df <- reactive({
                              calib %>%
                              subset(day == input$day) %>%
                              mutate('day'= format(day, '%d %B %Y'),
                                     'time'=as.POSIXct(time, format="%H:%M:%S", tz='UTC'))
                                 })
  
  calibration_exists <- reactive({ ifelse(nrow(day_calibrations_df())==0, FALSE, TRUE) })
  show_calibration <- reactive({ input$show_calib })
  
  # day-based bgreadings
  bg_line_df <- reactive({
                      spike_ranged() %>%
                        subset(day == input$day) %>%
                        mutate('value'=value/18,
                               'time'=as.POSIXct(time, format="%H:%M:%S", tz='UTC'),
                               'day'=format(day, '%d %B %Y'),
                               'text'=paste("BG (mmol/dL): ", value))
                         })
  
  output$bg_line <- renderPlotly({
                            p <- ggplot(bg_line_df(), aes(x=time, y=value, col=range, group=day)) +
                                        { if(input$show_daytime) geom_rect(aes(xmin=day_start, xmax=day_stop, ymin=-Inf, ymax=1), fill='lightblue', col=NA, alpha=0.2) } +
                                        geom_hline(yintercept=input$bglimits, col='red', lty=2) +
                                        { if(calibration_exists() & show_calibration()) geom_point(data=day_calibrations_df(), aes(x=time, y=0), col='blue', shape=17, size=3) } +
                                        { if(calibration_exists() & show_calibration()) geom_vline(xintercept=day_calibrations_df()$time, col='blue', lty=2, lwd=.5) } +
                                        geom_line(col='grey60') + geom_point(size=.5) + facet_wrap(~day, ncol=1) +
                                        cowplot::theme_cowplot() + cowplot::panel_border(colour='black', size=1, linetype=1) +
                                        theme(axis.text.x=element_text(angle=90), legend.position='none', axis.line=element_blank()) +
                                        labs(title='Daily Blood Glucose (BG)', y='BG (mmol/L)', x='Time') +
                                        scale_x_datetime(breaks=date_breaks('1 hour'), labels=date_format('%H:%M'), expand=c(0.02, 0.02), limits=c(x_start, x_stop)) +
                                        scale_color_manual(values=range_cols) +
                                        scale_y_continuous(expand=c(0.02, 0.1), limits=c(0, NA), breaks=seq(0, 20, 2.5))
                            ggplotly(p)
                                })
  
  
  # summary table
  output$summary_db <- renderTable(
                                   data.frame('Number of days'=interval(min(spike_ranged()$datetime), max(spike_ranged()$datetime)) %>%
                                                               int_length() %>%
                                                               seconds_to_period() %>%
                                                               day(),
                                              'From'=min(spike_ranged()$datetime) %>% as.character(),
                                              'To'=max(spike_ranged()$datetime) %>% as.character()) %>%
                                   t() %>%
                                   as.data.frame() %>%
                                   rownames_to_column('Database') %>%
                                   set_colnames(c('Database', ' ')) %>%
                                   mutate('Database'=str_replace_all(Database, '\\.', ' '))
                                  )
  
  output$summary_bg <- renderTable(
                                  data.frame('Number of BG readings'=n_distinct(spike_ranged()$reading_id) %>% as.character(),
                                             'Average BG'=mean(spike_ranged()$value/18) %>% round(2) %>% as.character(),
                                             'Number of low readings'=sum(spike_ranged()$range == 'low') %>% as.character(),
                                             'Number of nominal readings'=sum(spike_ranged()$range == 'nominal') %>% as.character(),
                                             'Number of high readings'=sum(spike_ranged()$range == 'high') %>% as.character()) %>%
                                    t() %>%
                                    as.data.frame() %>%
                                    rownames_to_column('BG readings') %>%
                                    set_colnames(c('BG readings', ' ')) %>%
                                    mutate('BG readings'=str_replace_all(`BG readings`, '\\.', ' '))
                                   )
  
  output$summary_sensors <- renderTable(
                                        data.frame('Number of sensors'=n_distinct(sensor_df()$sensor_id)) %>%
                                        melt(variable.name='Sensors', value.name=' ') %>%
                                        mutate('Sensors'=str_replace_all(Sensors, '\\.', ' '))
                                        )
  
  output$summary_calib <- renderTable(
                                      data.frame('Number of calibrations'=n_distinct(calib$calib_id)) %>%
                                      melt(variable.name='Calibration', value.name=' ') %>%
                                      mutate('Calibration'=str_replace_all(Calibration, '\\.', ' '))
                                      )
  
  # day-based percent in-range
  output$bg_pie <- renderPlot ({
                      spike_ranged() %>%
                      subset(day == input$day) %>%
                      group_by(day) %>%
                      summarise('total'=n(),
                                'nominal'=sum(range=='nominal')/total*100,
                                'low'=sum(range=='low')/total*100,
                                'high'=sum(range=='high')/total*100) %>%
                      select(-total) %>%
                      melt(id.vars='day', variable.name='range', value.name='readings') %>%
                      ggplot(aes(x=0, y=readings, fill=range)) +
                             geom_bar(stat='identity', lwd=.2, col='black', alpha=.75) +
                             geom_text(aes(label=paste0(round(readings, 1), '%')), position=position_stack(vjust=0.5), col='black') +
                             scale_x_continuous(expand=c(0,0)) +
                             cp +
                             theme_void() + theme(aspect.ratio=1, legend.position='bottom', plot.title=element_text(face='bold', hjust=.5)) +
                             scale_fill_manual(values=range_cols) + labs(title='Summary')
                             })

  # Spike calibration: range & stability
      # 1- make intervals around calibration, with minutes + and - as provided by user
      interval_df <- reactive({
                        calib %>%
                        select(calib_id, datetime) %>%
                        mutate('interval'=interval(datetime - input$before_min * 60, datetime + input$after_min * 60))
                              })
        
      # subset readings in calibration intervals & set names
      bg_intervals <- reactive({
                          lapply(interval_df()$interval, function(x) subset(spike_ranged(), datetime %within% x)) %>%
                          set_names(interval_df()$calib_id)
                              })
        
      # do all the rest
      output$gg_interval <- renderPlot({
                              bg_intervals() %>%
                              plyr::ldply(.id='calib_anchor_id') %>% # de-list dataframes
                              select(calib_anchor_id, sensor_id, calib_id, value, datetime, range, time) %>% # keep useful only
                              filter(calib_id != '-') %>% # clean
                              filter(value != 0) %>% # clean
                              rename('reading_dttm'='datetime') %>% # rename
                              left_join(select(calib, calib_id, 'calib_anchor_dttm'=datetime), by=c('calib_anchor_id'='calib_id')) %>% # add calibration anchor time
                              mutate('rel_time' = as.numeric(reading_dttm - calib_anchor_dttm)) %>% # compute relative time
                              ggplot(aes(x=rel_time/60, y=value/18, group=calib_anchor_id, col=calib_anchor_id)) + # plot
                                     geom_rect(aes(xmin=0, xmax=Inf, ymin=-Inf, ymax=Inf), fill='grey90', col=NA) +
                                     geom_vline(xintercept=0, col='blue', lty=2) +
                                     geom_hline(yintercept=input$bglimits, lty=2) +
                                     geom_line() + geom_point() + ylim(0, 15) +
                                     cowplot::theme_cowplot() + theme(legend.position='none') +
                                     labs(x='Time relative to Spike calibration (min)', title='BG range & stability prior to calibration', y='BG (mmol/L)')
                                    })
}

shinyApp(ui=ui, server=server)



# //// DEBUGS //// #
if(FALSE){
          spike_ranged_test <- spike %>%
                               mutate('range'=case_when(value/18 <= 3.2  ~ 'low',
                                                        value/18 >= 8.2 ~ 'high',
                                                        value/18 > 3.2 & value/18 < 8.2 ~ 'nominal',
                                                        TRUE ~ 'other_range'))
          
          day_calibrations_test_df <- calib %>%
                                      subset(day == '2019-10-18') %>%
                                      mutate('day'= format(day, '%d %B %Y'),
                                             'time'=as.POSIXct(time, format="%H:%M:%S", tz='UTC'))
          
          interval_df_test <- calib %>%
                              select(calib_id, datetime) %>%
                              mutate('interval'=interval(datetime - 20 * 60, datetime + 20 * 60))
          
          sensor_test_df <- spike_ranged_test %>%
            select(reading_id, sensor_id, calib_id, day, noise, datetime) %>%
            group_by(sensor_id) %>%
            summarise('readings'=n_distinct(reading_id),
                      'calibrations'=n_distinct(calib_id),
                      'mean noise'=round(mean(as.numeric(noise)), digits=2),
                      'start'=min(day), 'end'=max(day),
                      'uptime'=paste(max(day)-min(day), 'days'))
          
          bg_line_df_test <- spike_ranged_test %>%
            subset(day == '2019-10-15') %>%
            mutate('value'=value/18,
                   'time'=as.POSIXct(time, format="%H:%M:%S", tz='UTC'),
                   'day'=format(day, '%d %B %Y'),
                   'text'=paste("BG (mmol/dL): ", value/18))
}
