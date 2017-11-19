library(leaflet)
library(scales)
library(lattice)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(xml2)
library(jsonlite)
library(shinyjs)
library(XML)
library(shiny)
library(shinydashboard)
library(plotly)


#general purpose function
convert_html_to_text <- function(html) {
      if(html == ""){
            return("All good!")      
      }
      doc <- htmlParse(html, asText = TRUE)
      text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
      return(paste(text, collapse = "\n"))
      
}




# turnstile data
turnstiles = read.csv("data/turnstiles_agg.csv")

# make sure days of week are in the correct order
days_me = factor(c("Tuesday", 
                   "Wednesday", 
                   "Thursday", 
                   "Friday", 
                   "Saturday", 
                   "Sunday", 
                   "Monday"), 
                 levels = c("Tuesday", 
                                         "Wednesday", 
                                         "Thursday", 
                                         "Friday", 
                                         "Saturday", 
                                         "Sunday", 
                                         "Monday"))

turnstiles = turnstiles %>% mutate(day_of_week = days_me[(day_week + 1)])

# get the subway stations data. Only get one row per station (don't care about different entrances)
stations <- read.csv("data/stations.csv") %>% 
      group_by(Station.Name) %>%
      filter(row_number() == 1) %>%
      ungroup() %>% 
      select(Line, Station.Name, Station.Latitude, Station.Longitude, 
             Route1, Route2, Route3, Route4, Route5, Route6, Route7,
             Route8, Route9, Route10, Route11)

stations = melt(stations, id.vars = colnames(stations)[1:4]) %>%
      mutate(route = value) %>%
      select(-variable, -value) %>%
      filter(!is.null(route) & route != '')

# get the station colors
colors.table = read.csv("data/colors.csv")

stations =  stations %>%
      left_join(
            colors.table %>% 
                  mutate(color = hex) %>%
                  select(color, line), 
            by = c("route" = "line")
      )

# convert all factor variables to strings
fctr.cols <- sapply(stations, is.factor)
stations[, fctr.cols] <- sapply(stations[, fctr.cols], as.character)

# add station group to 
stations <- stations %>% mutate(stations.group = 
            case_when(
             route == "A" ~ "A/C/E",
             route == "C" ~ "A/C/E", 
             route == "E" ~ "A/C/E", 
             route == "1" ~ "1/2/3",
             route == "2" ~ "1/2/3",
             route == "3" ~ "1/2/3",
             route == "4" ~ "4/5/6",
             route == "5" ~ "4/5/6",
             route == "6" ~ "4/5/6",
             route == "G" ~ "G", 
             route == "7" ~ "7", 
             route == "B" ~ "B/D/F/M",
             route == "D" ~ "B/D/F/M",
             route == "F" ~ "B/D/F/M",
             route == "M" ~ "B/D/F/M",
             route == "N" ~ "N/Q/R",
             route == "Q" ~ "N/Q/R",
             route == "R" ~ "N/Q/R",
             route == "J" ~ "J/Z",
             route == "Z" ~ "J/Z",
             route == "L" ~ "L"
            )
      )

# capitalize station names. This is for joining purposes
stations = stations %>%
      mutate(Station.Name = toupper(Station.Name))


# get the peak hour at each station
stations = stations %>%
      left_join(
            turnstiles %>% 
                  group_by(station,hour) %>%
                  mutate(average.entries = mean(entries)) %>%
                  ungroup() %>%
                  group_by(station) %>% 
                  arrange(desc(average.entries)) %>%
                  filter(row_number() == 1) %>%
                  ungroup() %>%
                  mutate(Peak.Hour = paste(hour, ":00", sep = "")) %>%
                  select(station,Peak.Hour), 
            by = c("Station.Name" = "station")
      )


# Get MTA statusus in dataframe

# API Link
link = "http://web.mta.info/status/serviceStatus.txt"
# parse XML
status.xml = read_xml(link)
# put in dataframe
# 
# commented out so that I don't over-use the API
# 
mta.status = data.frame( name = character(),
                  status = character(),
                  text = character())

for (i in 1:(length(xml_find_all(status.xml, ".//line")))){
      name = xml_text(xml_find_first(xml_find_all(status.xml, ".//line")[i], "name"))
      status = xml_text(xml_find_first(xml_find_all(status.xml, ".//line")[i], "status"))
      text = xml_text(xml_find_first(xml_find_all(status.xml, ".//line")[i], "text"))

      mta.status = rbind(mta.status, data_frame(name = name, status = status, text = text))
}

# TODO: change to live. Temporarily read static csv
# mta.status = read.csv("data/statuses.csv")

ui <- dashboardPage(
      dashboardHeader(title = "MTA Station Delays"),
      dashboardSidebar(
            sidebarMenu(id="mytabs",
                        sidebarMenuOutput("menu")
                        
            ),
            disable = TRUE
      ),
      dashboardBody(
            
            #use shiny js
            useShinyjs(),
            
            tabItems(
                  # First tab content
                  tabItem(tabName = "dashboard",
                          
                          fluidRow(
                        # Boxes need to be put in a row (or column)
                              column(9, 
                                    box(
                                     div(class="outer",
                                                
                                                tags$head(
                                                      # Include our custom CSS
                                                      includeCSS("styles.css")
                                                ),
                                                
                                                
                                                leafletOutput("map", width="100%", height="100%"),
                                                
                                                
                                                
                                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                              draggable = TRUE, top = 50, left = 30, right = 20, bottom = "auto",
                                                              width = 500, height = "auto",
                                                              
                                                              h2("Today will be be crazy, New Yorker."),
                                                              h3("But your commute doesn't have to be. Stay informed to stay ahead of the rush."),
                                                              
                                                              fluidRow(
                                                                    column(6,
                                                                           selectInput("selected_lines", "Lines: ", 
                                                                                             choices = c("1/2/3", "4/5/6", "A/C/E", "B/D/F/M", "N/Q/R", 
                                                                                                         "7", "G", "L", "J/Z"), multiple = TRUE,
                                                                                             selected =  c(  "A/C/E", "G", "N/Q/R", "L"))
                                                                    ),
                                                                    column(6,
                                                                          selectizeInput("selected.stations", "Stations", choices = c("", 
                                                                               unique(
                                                                                arrange(stations, desc(Peak.Hour))$Station.Name
                                                                                )), 
                                                                                         options = list(placeholder = 'select stations'), selected = NULL, multiple = TRUE)
                                                                    )
                                                            ),
                                                            plotlyOutput('heatmap'),
                                                            plotOutput('linechart')
                                                              
                                                              
                        
                                                              
                                                              
                                                              
                                                              
                                                              
                                                )
                                                
                                            )
                                                  
                                    )
                              ),
                              column(3,
                                    box( width = 17, align = "center",
                                                title = "Line Status", status = "primary", solidHeader = TRUE,
                                                collapsible = TRUE,
                                    
                                         conditionalPanel(
                                               condition = "output.include_123",
                                               column(12,
                                                      # Status of 1/2/3 line
                                                      
                                                      fluidRow(width = 15, 
                                                               column(width = 3,
                                                                      imageOutput("svg_1", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               column(width = 3,
                                                                      imageOutput("svg_2", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               column(width = 3,
                                                                      imageOutput("svg_3", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               valueBoxOutput('infoBox_123', width = 12), 
                                                               actionButton("info_123", icon("info"))
                                                      )
                                               )
                                         ),
                                         conditionalPanel(
                                               condition = "output.include_456",
                                               column(12, 
                                                      fluidRow(width = 15, 
                                                               column(width = 3,
                                                                      imageOutput("svg_4", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               column(width = 3,
                                                                      imageOutput("svg_5", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               column(width = 3,
                                                                      imageOutput("svg_6", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               valueBoxOutput('infoBox_456', width = 12),
                                                               actionButton("info_456", icon("info"))
                                                      ) 
                                               )
                                         ),
                                         conditionalPanel(
                                               condition = "output.include_ace",
                                               column(12, 
                                                      fluidRow(width = 15, 
                                                               column(width = 3,
                                                                      imageOutput("svg_a", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               column(width = 3,
                                                                      imageOutput("svg_c", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               column(width = 3,
                                                                      imageOutput("svg_e", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               valueBoxOutput('infoBox_ace', width = 12),
                                                               actionButton("info_ace", icon("info"))
                                                      ) 
                                               )
                                          ),
                                         conditionalPanel(
                                               condition = "output.include_nqr",
                                               column(12, 
                                                      fluidRow(width = 15, 
                                                               column(width = 3,
                                                                      imageOutput("svg_n", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               column(width = 3,
                                                                      imageOutput("svg_q", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               column(width = 3,
                                                                      imageOutput("svg_r", width = 50, height = 50)
                                                                      
                                                               ), 
                                                               valueBoxOutput('infoBox_nqr', width = 12),
                                                               actionButton("info_nqr", icon("info"))
                                                      ) 
                                               )
                                         ),
                                         conditionalPanel(
                                                condition = "output.include_bdfm",
                                                column(12, 
                                                       fluidRow(width = 15, 
                                                                column(width = 3,
                                                                       imageOutput("svg_b", width = 50, height = 50)
                                                                       
                                                                ), 
                                                                column(width = 3,
                                                                       imageOutput("svg_d", width = 50, height = 50)
                                                                       
                                                                ), 
                                                                column(width = 3,
                                                                       imageOutput("svg_f", width = 50, height = 50)
                                                                       
                                                                ), 
                                                                column(width = 3,
                                                                       imageOutput("svg_m", width = 50, height = 50)
                                                                       
                                                                ),
                                                                valueBoxOutput('infoBox_bdfm', width = 12),
                                                                actionButton("info_bdfm", icon("info"))
                                                       ) 
                                                )
                                         ),
                                         conditionalPanel(
                                               condition = "output.include_l",
                                               column(12, 
                                                      fluidRow(width = 15, 
                                                               column(width = 3,
                                                                      imageOutput("svg_l", width = 50, height = 50)
                                                                      
                                                               ),
                                                               valueBoxOutput('infoBox_l', width = 12),
                                                               actionButton("info_l", icon("info"))
                                                      ) 
                                               )
                                         ),
                                         conditionalPanel(
                                               condition = "output.include_7", 
                                               column(12, 
                                                      fluidRow(width = 15, 
                                                               column(width = 3,
                                                                      imageOutput("svg_7", width = 50, height = 50)
                                                                      
                                                               ),
                                                               valueBoxOutput('infoBox_7', width = 12),
                                                               actionButton("info_7", icon("info"))
                                                      ) 
                                               )
                                          ),
                                         conditionalPanel(
                                            condition = "output.include_g",
                                            column(12, 
                                                   fluidRow(width = 15, 
                                                            column(width = 3,
                                                                   imageOutput("svg_g", width = 50, height = 50)
                                                                   
                                                            ),
                                                            valueBoxOutput('infoBox_g', width = 12),
                                                            actionButton("info_g", icon("info"))
                                                   ) 
                                            )
                                         ),
                                         conditionalPanel(
                                               condition = "output.include_jz",
                                               column(12, 
                                                      fluidRow(width = 15, 
                                                               column(width = 3,
                                                                      imageOutput("svg_j", width = 50, height = 50)
                                                                      
                                                               ),
                                                               column(width = 3,
                                                                      imageOutput("svg_z", width = 50, height = 50)
                                                                      
                                                               ),
                                                               valueBoxOutput('infoBox_jz', width = 12),
                                                               actionButton("info_jz", icon("info"))
                                                      ) 
                                               )
                                         ),
                                         
                                         p("Live data scraped from MTA.org. Availability of delay information subject to API status.")
                                    )
                            )
                              
                        )
                  )
            )
      )
)







server <- function(input, output, session) {
      
      
      # ---------------------------------------------
      # JavaScript Stuff that needs to be run on init
      # ---------------------------------------------
      
      # collapse sidebar by default
      addClass(selector = "body", class = "sidebar-collapse")
      
      # populate the sidebar
      output$menu <- renderMenu({
            sidebarMenu(
                  menuItem("Dashboard", tabName="dashboard", icon = icon("dashboard"))
            )
      })
      isolate({updateTabItems(session, "mytabs", "dashboard")})
      
      
      # ---------------------------------------------
      # Render the train logos
      # ---------------------------------------------
      
      output$svg_1 <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/1.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_2 <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/2.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_3 <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/3.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_3 <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/3.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_4 <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/4.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_5 <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/5.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_6 <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/6.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_a <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/a.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_c <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/c.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_e <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/e.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_n <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/n.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_q <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/q.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_r <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/r.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_b <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/b.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_d <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/d.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_m <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/m.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_f <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/f.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_7 <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/7.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_g <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/g.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_l <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/l.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_j <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/j.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      output$svg_z <- renderImage({
            # Return a list containing the filename
            list(src = "./data/svg/z.svg",
                 contentType = 'image/svg+xml',
                 width = 50,
                 height = 50,
                 alt = "")
      }, deleteFile = FALSE)
      
      
      
      
      # ---------------------------------------------
      # Create info boxes for status
      # ---------------------------------------------
      output$infoBox_123 <- renderValueBox({
            # get status
            status.123 = (mta.status %>% filter(name == "123"))$status
            
            if(status.123 == "GOOD SERVICE"){
                  color = "green"
            }
            else if (status.123 == "PLANNED WORK"){
                  color = "orange"  
            }
            else{
                  color = "red"
            }
            valueBox(value = tags$p(status.123, style = "font-size: 20px;"), NULL, color = color )
      })
      output$infoBox_456 <- renderValueBox({
            # get status
            status.456 = (mta.status %>% filter(name == "456"))$status
            
            if(status.456 == "GOOD SERVICE"){
                  color = "green"
            }
            else if (status.456 == "PLANNED WORK"){
                  color = "orange"  
            }
            else{
                  color = "red"
            }
            valueBox(value = tags$p(status.456, style = "font-size: 20px;"), NULL, color = color )
      })
      output$infoBox_ace <- renderValueBox({
            # get status
            status.ace = (mta.status %>% filter(name == "ACE"))$status
            
            if(status.ace == "GOOD SERVICE"){
                  color = "green"
            }
            else if (status.ace == "PLANNED WORK"){
                  color = "orange"  
            }
            else{
                  color = "red"
            }
            valueBox(value = tags$p(status.ace, style = "font-size: 20px;"), NULL, color = color )
      })
      output$infoBox_nqr <- renderValueBox({
            # get status
            status.nqr = (mta.status %>% filter(name == "NQR"))$status
            
            if(status.nqr == "GOOD SERVICE"){
                  color = "green"
            }
            else if (status.nqr == "PLANNED WORK"){
                  color = "orange"  
            }
            else{
                  color = "red"
            }
            valueBox(value = tags$p(status.nqr, style = "font-size: 20px;"), NULL, color = color )
      })
      output$infoBox_bdfm <- renderValueBox({
            # get status
            status.bdfm = (mta.status %>% filter(name == "BDFM"))$status
            
            if(status.bdfm == "GOOD SERVICE"){
                  color = "green"
            }
            else if (status.bdfm == "PLANNED WORK"){
                  color = "orange"  
            }
            else{
                  color = "red"
            }
            valueBox(value = tags$p(status.bdfm, style = "font-size: 20px;"), NULL, color = color )
      })
      output$infoBox_l <- renderValueBox({
            # get status
            status.l = (mta.status %>% filter(name == "L"))$status
            
            if(status.l == "GOOD SERVICE"){
                  color = "green"
            }
            else if (status.l == "PLANNED WORK"){
                  color = "orange"  
            }
            else{
                  color = "red"
            }
            valueBox(value = tags$p(status.l, style = "font-size: 20px;"), NULL, color = color )
      })
      output$infoBox_7 <- renderValueBox({
            # get status
            status.7 = (mta.status %>% filter(name == "7"))$status
            
            if(status.7 == "GOOD SERVICE"){
                  color = "green"
            }
            else if (status.7 == "PLANNED WORK"){
                  color = "orange" 
            }
            else{
                  color = "red"
            }
            valueBox(value = tags$p(status.7, style = "font-size: 20px;"), NULL, color = color )
      })
      output$infoBox_g <- renderValueBox({
            # get status
            status.g = (mta.status %>% filter(name == "G"))$status
            
            if(status.g == "GOOD SERVICE"){
                  color = "green"
            }
            else if (status.g == "PLANNED WORK"){
                  color = "orange"  
            }
            else{
                  color = "red"
            }
            valueBox(value = tags$p(status.g, style = "font-size: 20px;"), NULL, color = color )
      })
      output$infoBox_jz <- renderValueBox({
            # get status
            status.jz = (mta.status %>% filter(name == "JZ"))$status
            
            if(status.jz == "GOOD SERVICE"){
                  color = "green"
            }
            else if (status.jz == "PLANNED WORK"){
                  color = "orange"  
            }
            else{
                  color = "red"
            }
            valueBox(value = tags$p(status.jz, style = "font-size: 20px;"), NULL, color = color )
      })
      
      
      
      
      # ---------------------------------------------
      # Conditions that indicate which group was selected. 
      # These will be used to hide/show statusus using conditional panesl
      # ---------------------------------------------
      
      output$include_123 = reactive({ 
            "1/2/3" %in% input$selected_lines 
            })
      outputOptions(output, "include_123", suspendWhenHidden = FALSE)
      
      output$include_456 = reactive({ 
            "4/5/6" %in% input$selected_lines   
      })
      outputOptions(output, "include_456", suspendWhenHidden = FALSE)
      
      output$include_ace = reactive({ 
            "A/C/E" %in% input$selected_lines   
      })
      outputOptions(output, "include_ace", suspendWhenHidden = FALSE)
      
      output$include_bdfm = reactive({ 
            "B/D/F/M" %in% input$selected_lines   
      })
      outputOptions(output, "include_bdfm", suspendWhenHidden = FALSE)
      
      output$include_jz = reactive({ 
            "J/Z" %in% input$selected_lines   
      })
      outputOptions(output, "include_jz", suspendWhenHidden = FALSE)
      
      output$include_7 = reactive({ 
            "7" %in% input$selected_lines   
      })
      outputOptions(output, "include_7", suspendWhenHidden = FALSE)
      
      output$include_g = reactive({ 
            "G" %in% input$selected_lines   
      })
      outputOptions(output, "include_g", suspendWhenHidden = FALSE)
      
      
      output$include_nqr = reactive({ 
            "N/Q/R" %in% input$selected_lines   
      })
      outputOptions(output, "include_nqr", suspendWhenHidden = FALSE)
      
      output$include_l = reactive({ 
            "L" %in% input$selected_lines   
      })
      outputOptions(output, "include_l", suspendWhenHidden = FALSE)
      
      # A reactive expression that returns the set of stations
      # in bounds right now
      stationsInView<- reactive({
            if (is.null(input$map_bounds))
                  return(stations[FALSE,])
            bounds <- input$map_bounds
            latRng <- range(bounds$north, bounds$south)
            lngRng <- range(bounds$east, bounds$west)
            
            subset(stations,
                   Station.Latitude >= latRng[1] & Station.Latitude <= latRng[2] &
                         Station.Longitude >= lngRng[1] & Station.Longitude <= lngRng[2])
      })
      
      # Limit stations to selected lines
      limitStations <- reactive({
            # limit to the stations in the map view
            limited = stationsInView()
            
            # see if the user has selected specific stations
            if(!is.null(input$selected.stations)){
               limited = limited %>%
                     filter(Station.Name %in% input$selected.stations)
               
               return(limited)
            }
            else{
                  # limit stations to selected lines
                  limited = limited %>% 
                        filter(stations.group %in% input$selected_lines)
                  
                  return(limited)
            }
      })
      
      ## Interactive Map ###########################################
      
      # Create the map
      output$map <- renderLeaflet({
            leaflet() %>%
                  addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                  ) %>%
                  setView(lat = 40.7507, lng = -74., zoom = 13) #initialize map at NYC coordinates
      })
      
      # Update the station locations on map
      observe({
            leafletProxy("map", data = limitStations()) %>%
                  clearShapes() %>%
                  addCircles(~Station.Longitude, ~Station.Latitude, radius=100,
                             layerId=~Station.Name, stroke=FALSE, fillOpacity=0.8, fillColor= ~color)
      })
      
      # Show a popup at the given location
      showStationPopup <- function(station.name, lat, lng) {
            selectedStation <- stations[stations$Station.Name == station.name,]
            content <- as.character(tagList(
                  tags$h4("Station:", selectedStation$Station.Name),
                  if(!is.null(selectedStation$Peak.Hour)){
                        sprintf("Peak Traffic Hour: %s", selectedStation$Peak.Hour)
                  }
                  
                  
                  # sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
                  # sprintf("Adult population: %s", selectedZip$adultpop)
            ))
            leafletProxy("map") %>% addPopups(lng, lat, content, layerId = station.name)
      }
      
      # When map is clicked, show a popup with city info
      observe({
            leafletProxy("map") %>% clearPopups()
            event <- input$map_shape_click
            if (is.null(event))
                  return()
            
            isolate({
                  showStationPopup(event$id, event$lat, event$lng)
            })
      })
      
      
      # Get the turnstile, according to limit
      limitTurnstiles <- reactive({
            limited.stations = limitStations() %>% select(Station.Name)
            turnstiles %>% left_join(
                  limited.stations,
                  by = c("station" = "Station.Name")
            )
            
      })
      
      output$heatmap = renderPlotly({
            ggplotly(
                  limitTurnstiles() %>%
                        group_by(day_of_week, hour) %>%
                        summarise(average.entries = mean(entries)) %>%
                        ggplot(aes(hour, day_of_week, fill = average.entries)) +
                        geom_tile() +
                        ggtitle("Average Traffic For Stations In View (1985-)") %>%
                        labs(x = "Hour of the day", y = "Day of the week") +
                        scale_fill_distiller(palette = "Spectral")  
            )
            
      })
      
      output$linechart = renderPlot({
            if(is.null(input$selected.stations)){
                  return(NULL)      
            }
            else{
                  selected.data = turnstiles %>%
                        filter(station %in% input$selected.stations)
                  
                  if(nrow(selected.data) == 0){
                        return(NULL)      
                  }
                  else{
                        selected.data %>%
                              ggplot(aes(hour, entries, color = station)) +
                              geom_smooth() +
                              labs(x = "Hour of the day", y = "Number Of Entries")       
                  }
            }
      })
      
      
      # ---------------------------------------------
      # Modal buttons for info on trafic
      # ---------------------------------------------
      
      observeEvent(input$info_123, {
            
            text = convert_html_to_text((mta.status %>% filter(name == "123"))$text)
            
            showModal(modalDialog(
                  title = "Delays Update",
                  text,
                  easyClose = TRUE,
                  footer = NULL
            ))
      })
      
      observeEvent(input$info_456, {
            
            text = convert_html_to_text((mta.status %>% filter(name == "456"))$text)
            
            showModal(modalDialog(
                  title = "Delays Update",
                  text,
                  easyClose = TRUE,
                  footer = NULL
            ))
      })
      
      observeEvent(input$info_nqr, {
            
            text = convert_html_to_text((mta.status %>% filter(name == "NQR"))$text)
            
            showModal(modalDialog(
                  title = "Delays Update",
                  text,
                  easyClose = TRUE,
                  footer = NULL
            ))
      })
      
      observeEvent(input$info_ace, {
            
            text = convert_html_to_text((mta.status %>% filter(name == "ACE"))$text)
            
            showModal(modalDialog(
                  title = "Delays Update",
                  text,
                  easyClose = TRUE,
                  footer = NULL
            ))
      })
      
      observeEvent(input$info_bdfm, {
            
            text = convert_html_to_text((mta.status %>% filter(name == "BDFM"))$text)
            
            showModal(modalDialog(
                  title = "Delays Update",
                  text,
                  easyClose = TRUE,
                  footer = NULL
            ))
      })
      
      observeEvent(input$info_l, {
            
            text = convert_html_to_text((mta.status %>% filter(name == "L"))$text)
            
            showModal(modalDialog(
                  title = "Delays Update",
                  text,
                  easyClose = TRUE,
                  footer = NULL
            ))
      })
      
      observeEvent(input$info_g, {
            
            text = convert_html_to_text((mta.status %>% filter(name == "G"))$text)
            
            showModal(modalDialog(
                  title = "Delays Update",
                  text,
                  easyClose = TRUE,
                  footer = NULL
            ))
      })
      
      observeEvent(input$info_7, {
            
            text = convert_html_to_text((mta.status %>% filter(name == "7"))$text)
            
            showModal(modalDialog(
                  title = "Delays Update",
                  text,
                  easyClose = TRUE,
                  footer = NULL
            ))
      })
      observeEvent(input$info_jz, {
            
            text = convert_html_to_text((mta.status %>% filter(name == "JZ"))$text)
            
            showModal(modalDialog(
                  title = "Delays Update",
                  text,
                  easyClose = TRUE,
                  footer = NULL
            ))
      })

      
      
      # output$table = renderTable({limitTurnstiles()})
      
}

shinyApp(ui = ui, server = server)
