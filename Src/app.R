#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#   https://varunagl10.shinyapps.io/Tokyo_2020/


library(bslib)
library(shiny)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(RCurl)
library(dplyr)
library(tidyverse)
library(reshape2)
library(latex2exp)
library(stringb)
library(plotly)
library(shinyjs)
library(shinydashboard)
library(rlang)
library(ggpubr)
library(gridExtra)
library(dgof)
library(data.table)
library(curl)
library(fuzzyjoin)
library(rgdal)
library(rgeos)
library(httr)
library(magrittr)
library(countrycode)

#datasets
medal <- getURL("https://raw.githubusercontent.com/vpagl/Tokyo_Olympics_2020_MDS202251/main/Data/tokyo_2021.csv")
gender <- getURL("https://raw.githubusercontent.com/vpagl/Tokyo_Olympics_2020_MDS202251/main/Data/EntriesGender.csv")
team <- getURL("https://raw.githubusercontent.com/vpagl/Tokyo_Olympics_2020_MDS202251/main/Data/Teams.csv")


m_df <- as.data.frame(read.csv(text = medal))
g_df <- as.data.frame(read.csv(text = gender))
t_df <- as.data.frame(read.csv(text = team))
#gender
males <- sum(g_df[,'Male'])
females <- sum(g_df[,'Female'])

df1 <- data.frame(
  group = c("Male", "Female"),
  value = c(males,females)
)
g_ttl_melt_df <- melt(g_df, id = c('Discipline'))
g_melt_df <- melt(g_df, id = c('Total',"Discipline"))
#medal
melt_df <- melt(m_df, id = c("X", "Country"))
#new_df = m_df[,c('X','Country','Gold','Silver','Bronze')]
#new_melt_df = melt(new_df, id = c('X','Country'))
#for_line_df = m_df[c(1,2,3,4,5,6,7,8,9,10),]
#line_melt_df = melt(for_line_df, id = c('X','Country'))

#team
tnum_df <- as.data.frame(table(t_df$NOC))
tnum_df <- tnum_df[with(tnum_df, order(-Freq)),]
tnum_df <- tnum_df[1:20,]
dist_df <- t_df %>%
  group_by(NOC) %>%
  summarize(Number = n_distinct(Discipline))
dist_df <- as.data.frame(dist_df)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title="Tokyo Olympics 2020"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "M0"),
      menuItem("Performance Overview",tabName = "M1"),
      menuItem("Country Comparisions",tabName = "M2"),
      menuItem("Sports Discipline", tabName = 'M3')
    )
  ),
  dashboardBody(
    useShinyjs(),  
    tabItems(
      
      tabItem("M0",
              h2("Introduction",align = "center"),
              
              fluidRow(align = "center",
                       
                       verbatimTextOutput("textIni"),
                       hr(),
                       plotOutput("world"),
                       hr(),
                       verbatimTextOutput("textIni2"),
                       
              )
              
      ),
      tabItem("M1",
              titlePanel("Tokyo Olympics: An Overview"),
              tabsetPanel(
                tabPanel("Total",
                         sidebarLayout(
                           sidebarPanel(
                             sliderInput("idt",
                                         "Country Rank (Based on overall ranking)",
                                         min = 1,
                                         max = 93,
                                         value = c(1,10)),
                             hr(),fluid = FALSE, width = 10
                             
                           ),
                           tabsetPanel(
                             tabPanel('Bar Plot',mainPanel(
                               fluidRow(
                                 plotOutput(outputId = "TotalPlots1",width = '100%'
                                            # ,
                                            # hover = hoverOpts(id ="Total_hover1"
                                                              )),
                                 hr(),
                                 width = 10
                                 #,verbatimTextOutput("Total_info"))
                               
                             )
                               
                             ),
                             tabPanel('Line Plot',mainPanel(
                               fluidRow(
                                 hr(),plotOutput("TotalPlots2"),
                                 width = 20)
                               
                             )
                             
                             )
                             
                           )
                           # Show a plot of the generated distribution
                         )  
                              
                ),
                tabPanel("Gold",
                         sidebarLayout(
                           sidebarPanel(
                             sliderInput("idg",
                                         "Country Rank (Based on overall ranking)",
                                         min = 1,
                                         max = 93,
                                         value = c(1,10)),
                             hr(),fluid = FALSE, width = 10
                             
                           ),
                           tabsetPanel(
                             tabPanel('Bar Plot',mainPanel(
                               fluidRow(
                                 plotOutput(outputId = "GoldPlots1",width = '100%'),
                                 hr(),
                                 width = 10)
                               
                             )
                             
                             ),
                             tabPanel('Line Plot',mainPanel(
                               fluidRow(
                                 hr(),plotOutput("GoldPlots2"),
                                 width = 20)
                               
                             )
                             
                             )
                             
                           )
                         )     
                ),
                tabPanel("Silver",
                         sidebarLayout(
                           sidebarPanel(
                             sliderInput("ids",
                                         "Country Rank (Based on overall ranking)",
                                         min = 1,
                                         max = 93,
                                         value = c(1,10)),
                             hr(),fluid = FALSE, width = 10
                             
                           ),
                           tabsetPanel(
                             tabPanel('Bar Plot',mainPanel(
                               fluidRow(
                                 plotOutput(outputId = "SilverPlots1",width = '100%'),
                                 hr(),
                                 width = 10)
                               
                             )
                             
                             ),
                             tabPanel('Line Plot',mainPanel(
                               fluidRow(
                                 hr(),plotOutput("SilverPlots2"),
                                 width = 20)
                               
                             )
                             
                             )
                             
                           )
                         )     
                ),
                tabPanel("Bronze",
                         sidebarLayout(
                           sidebarPanel(
                             sliderInput("idb",
                                         "Country Rank (Based on overall ranking)",
                                         min = 1,
                                         max = 93,
                                         value = c(1,10)),
                             hr(), fluid = FALSE, width = 10
                             
                           ),
                           tabsetPanel(
                             tabPanel('Bar Plot',mainPanel(
                               fluidRow(
                                 plotOutput(outputId = "BronzePlots1",width = '100%'),
                                 hr(),
                                 width = 10)
                               
                             )
                             
                             ),
                             tabPanel('Line Plot',mainPanel(
                               fluidRow(
                                 hr(),plotOutput("BronzePlots2"),
                                 width = 20)
                               
                             )
                             
                             )
                             
                           )
                         )     
                ),
                  tabPanel("Country",
                           sidebarLayout(
                             sidebarPanel(
                               
                               selectInput("Mult1", "Country Name", 
                                           choices=
                                             c('United States ','China','Japan','Great Britain','ROC','Australia','Netherlands','France','Germany','Italy','Canada','Brazil','New Zealand ','Cuba','Hungary','South Korea','Poland','Czech Republic','Kenya','Norway','Jamaica','Spain','Sweden','Switzerland','Denmark','Croatia','Iran','Serbia','Belgium','Bulgaria','Slovenia','Uzbekistan','Georgia','Chinese Taipei','Turkey','Greece','Uganda','Ecuador','Ireland','Israel','Qatar','Bahamas','Kosovo','Ukraine','Belarus','Romania','Venezuela','India','Hong Kong','Philippines','Slovakia','South Africa','Austria','Egypt','Indonesia','Ethiopia','Portugal','Tunisia','Estonia','Fiji','Latvia','Thailand','Bermuda','Morocco','Puerto Rico','Colombia','Azerbaijan','Dominican Republic','Armenia','Kyrgyzstan','Mongolia','Argentina','San Marino','Jordan','Malaysia','Nigeria','Bahrain','Saudi Arabia','Lithuania','North Macedonia','Namibia','Turkmenistan','Kazakhstan','Mexico','Finland','Botswana','Burkina Faso','Ivory Coast','Ghana','Grenada','Kuwait','Republic of Moldova','Syria')
                                           
                                           ,selected = c("United States")),
                               hr(), fluid = FALSE, width = 10,
                               h5(class="radioSelect", radioButtons(inputId="r1", "Plot", choices = c("Bar Plot", "Percentile Plot","Line Plot"))) 
                                                                                    
                               
                             ),
                             # Show a plot of the generated distribution
                             mainPanel(
                               plotOutput("CountryPlots")
                             )
                           )     
                  ),
              ),
            
      ),
      tabItem("M2",
              titlePanel("Performance Comparisions Between The Countries "),
              sidebarLayout(
                sidebarPanel(
                  
                  checkboxGroupInput("noc", label = h3("Choose Countries to compare"), 
                                     choices = 
                                       c('United States ','China','Japan','Great Britain','ROC','Australia','Netherlands','France','Germany','Italy','Canada','Brazil','New Zealand ','Cuba','Hungary','South Korea','Poland','Czech Republic','Kenya','Norway','Jamaica','Spain','Sweden','Switzerland','Denmark','Croatia','Iran','Serbia','Belgium','Bulgaria','Slovenia','Uzbekistan','Georgia','Chinese Taipei','Turkey','Greece','Uganda','Ecuador','Ireland','Israel','Qatar','Bahamas','Kosovo','Ukraine','Belarus','Romania','Venezuela','India','Hong Kong','Philippines','Slovakia','South Africa','Austria','Egypt','Indonesia','Ethiopia','Portugal','Tunisia','Estonia','Fiji','Latvia','Thailand','Bermuda','Morocco','Puerto Rico','Colombia','Azerbaijan','Dominican Republic','Armenia','Kyrgyzstan','Mongolia','Argentina','San Marino','Jordan','Malaysia','Nigeria','Bahrain','Saudi Arabia','Lithuania','North Macedonia','Namibia','Turkmenistan','Kazakhstan','Mexico','Finland','Botswana','Burkina Faso','Ivory Coast','Ghana','Grenada','Kuwait','Republic of Moldova','Syria')
                                     , selected = c('United States ','China','Japan'), 
                                     inline = TRUE),
                                     
                                   
                  hr(), fluid = FALSE, width = 200,
                  
                ),
                tabsetPanel(
                  tabPanel('Bar Plot',mainPanel(
                    fluidRow(
                      plotOutput(outputId = "CP1",width = '100%'),
                      hr(),
                      width = 20)
                    
                  )
                  
                  ),
                  tabPanel('Line Plot',mainPanel(
                    fluidRow(
                      hr(),plotOutput("CP2"),
                      width = 20)
                    
                  )
                  
                  )
                  
                )
                # Show a plot of the generated distribution
                              )     
              
      ),
      tabItem("M3",
              titlePanel("Participation Across various Sports "),
              sidebarLayout(
                sidebarPanel(
                  
                  selectInput("Mult2", "Sports Discipline", 
                              choices=
                                c('3x3 Basketball',
                                   'Archery',
                                   'Artistic Gymnastics',
                                   'Artistic Swimming',
                                   'Athletics',
                                   'Badminton',
                                   'Baseball/Softball',
                                   'Basketball',
                                   'Beach Volleyball',
                                   'Boxing',
                                   'Canoe Slalom',
                                   'Canoe Sprint',
                                   'Cycling BMX Freestyle',
                                   'Cycling BMX Racing',
                                   'Cycling Mountain Bike',
                                   'Cycling Road',
                                   'Cycling Track',
                                   'Diving',
                                   'Equestrian',
                                   'Fencing',
                                   'Football',
                                   'Golf',
                                   'Handball',
                                   'Hockey',
                                   'Judo',
                                   'Karate',
                                   'Marathon Swimming',
                                   'Modern Pentathlon',
                                   'Rhythmic Gymnastics',
                                   'Rowing',
                                   'Rugby Sevens',
                                   'Sailing',
                                   'Shooting',
                                   'Skateboarding',
                                   'Sport Climbing',
                                   'Surfing',
                                   'Swimming',
                                   'Table Tennis',
                                   'Taekwondo',
                                   'Tennis',
                                   'Trampoline Gymnastics',
                                   'Triathlon',
                                   'Volleyball',
                                   'Water Polo',
                                   'Weightlifting',
                                   'Wrestling')
                              
                              ,selected = c('3x3 Basketball')),
                  hr(), fluid = FALSE, width = 10
                  
                ),
                # Show a plot of the generated distribution
                mainPanel(
                  plotOutput("SportsPlot")
                )
              )     
              
      )
      
      
      
      
      
              )
              
      )
    )
  











# Define server logic required to draw a histogram
server <- function(input, output) {

  output$textIni <- renderText({
    paste0('The Tokyo 2020 Games were an unprecedented demonstration of unity and solidarity \nas the world came together for the first time following the onset of the COVID-19 pandemic \nfor an Olympic Games focused on the pure essentials:\n a celebration of athletes and sport.\n \n 
The event program for the 2020 Summer Olympics was approved by the IOC executive board on 9 June 2017. \nIOC president Thomas Bach stated that their goal was to give the Games "youthful" and "urban" appeal\n and to increase the number of female participants.\nThe Games featured 339 events in 33 different sports, encompassing a total of 50 disciplines with around 206 National Olympic Committees\n and over 11,000 athletes.')
   })
  output$world <- renderPlot({
    amap<-maps::iso3166
    country_data <- dist_df %>% 
      inner_join(maps::iso3166, by = c('NOC' = 'ISOname'))
    map_data("world") %>% 
      as_tibble() %>% 
      filter(region != "Antarctica") %>% 
      regex_left_join(country_data, by = c(region = "mapname")) %>% 
      ggplot(aes(long, lat, group = group , fill = Number)) +
      geom_polygon(color = "black", size = 0.1)  + theme_classic() + 
      ggtitle("No. of teams Participating")+ 
      labs(
        caption = "Few countries are blank due to irregularties in available data."
      )
    
  },width = 700)
  
  output$textIni2 <- renderText({
    paste0("In this project, We will perform \nEDA on Countries and their performances in Tokyo Olympics 2020. \nWe will also see the gender trends across various sports disciplines.\n For this, we will use visualization techniques to describe the performance and other patterns in the data. \n Then we aim to deploy an  Analytics Dashboard that will present the analysis in a user-friendly and attractive way.
           \n\nFor this the Dataset we use are: 
           \n* [Medals](https://www.kaggle.com/datasets/jeronimojr/tokyo-2021-medal-table?select=tokyo_2021.csv) 
  \n  - Have details of the medal tally and the ranking of the 93 countries \nand the Country names having been translated to English while uploading 
\n* [Teams](https://www.kaggle.com/datasets/arjunprasadsarkhel/2021-olympics-in-tokyo?select=Teams.xlsx)
  \n  - Have details of teams of the countries taking part in which disciplines in total of 743 rows 
\n* [Gender Entries](https://www.kaggle.com/datasets/arjunprasadsarkhel/2021-olympics-in-tokyo?select=EntriesGender.xlsx)
\n  - The distribution of Males and Females across 47 different sports disciplines.")
  })
  
  tp1 <- reactive({
    new_df = m_df[c(input$idt[1]:input$idt[2]),c('X','Country','Gold','Silver','Bronze')]
    melt(new_df, id = c('X','Country'))
  })
  
  
  output$TotalPlots1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    #new_df = m_df[c(input$idt[1]:input$idt[2]),c('X','Country','Gold','Silver','Bronze')]
    new_melt_df = data.frame(tp1())
    
    # draw the histogram with the specified number of bins
    ggplot(new_melt_df,aes(reorder(Country, -X),value, fill = factor(variable, levels = c("Bronze", "Silver", "Gold")) )) +
      geom_col() + theme_minimal() + theme(
        legend.position = "top",
        legend.justification = c("top"),
        legend.box.just = "top",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
      ) + coord_flip() +
      scale_fill_discrete(breaks=c("Gold","Silver","Bronze")) +
      scale_fill_manual(values = c("Gold" = "#d6af36",
                                   "Silver" = "#d7d7d7",
                                   "Bronze" = "#a77044")) +
      xlab('No. of Medals') + ylab('Country Name') +
      ggtitle('Country Performance') + 
      labs(fill = "",
           caption = "We see the distribution of Gold, Silver and, Bronze medals for various countries ranked by no. of  Gold medals awarded." ,
           subtitle = "Ranked Highest(Top) to Lowest(Bottom) "
      )
  }, height = 800, width = 1000)
  
  
  tp2 <- reactive({
    new_df_line = m_df[c(input$idt[1]:input$idt[2]),c('X','Country','Gold','Silver','Bronze','Total')]
    melt(new_df_line, id = c('X','Country'))   
    })
  
  output$TotalPlots2 <- renderPlot({
      # generate bins based on input$bins from ui.R
      
      #new_df_line = m_df[c(input$idt[1]:input$idt[2]),c('X','Country','Gold','Silver','Bronze','Total')]
      new_melt_df_line = data.frame(tp2())    
    
    ggplot(new_melt_df_line,aes(reorder(Country, +X),value, color = variable)) +
      geom_point(size = 4) + 
      geom_line(aes(group = variable, color = variable, fill = variable))+
      theme_minimal() + 
      theme(
        legend.position = "right",
        legend.justification = c("right"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
      ) + ylim(0,(m_df[input$idt[1],'Total']+20)) + xlab("Country Name") + ylab("Number of Medals")+
      ggtitle('Medal Tally') +
      labs(fill = "",
           caption = "Representing performance of selected Countries." ,
      ) + 
      scale_fill_discrete(breaks=c("Total","Gold","Silver","Bronze")) +
      scale_fill_manual(values = c( "Total" = "magenta",
                                    "Gold" = "#d6af36",
                                    "Silver" = "#d7d7d7",
                                    "Bronze" = "#a77044"
                                   ))+ 
      scale_color_discrete(breaks=c("Total","Gold","Silver","Bronze")) +
      scale_color_manual(values = c( "Total" = "skyblue",
                                    "Gold" = "#d6af36",
                                    "Silver" = "#d7d7d7",
                                    "Bronze" = "#a77044"
      ))

  }, width = 1000)
  
  gp1 <- reactive({
    new_df_g1 = m_df[c(input$idg[1]:input$idg[2]),c('X','Country','Gold')]
    melt(new_df_g1, id = c('X','Country'))
  })
  
  output$GoldPlots1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    
    new_melt_df_g1 = data.frame(gp1())
    
    # draw the histogram with the specified number of bins
    ggplot(new_melt_df_g1,aes(reorder(Country, -X),value, fill = factor(variable, levels = c("Gold")) )) +
      geom_col() + theme_minimal() + theme(
        legend.position = "none",
        
      ) + coord_flip() +
      scale_fill_discrete(breaks=c("Gold")) +
      scale_fill_manual(values = c("Gold" = "#d6af36"
                                   )) +
      xlab('No. of Medals') + ylab('Country Name') +
      ggtitle('Gold Medals Won') + 
      labs(fill = "",
           caption = "We see the distribution of Gold medals for various countries ranked by no. of  Gold medals awarded." ,
           subtitle = "Ranked Highest(Top) to Lowest(Bottom) "
      )
  }, height = 800, width = 1000)
  output$GoldPlots2 <- renderPlot({
    
    #new_df_g1 = m_df[c(input$idg[1]:input$idg[2]),c('X','Country','Gold')]
    new_melt_df_g1 = data.frame(gp1())
    
    # generate bins based on input$bins from ui.R

    ggplot(new_melt_df_g1,aes(reorder(Country, +X),value, color = variable)) +
      geom_point(size = 4) + 
      geom_line(aes(group = variable, color = variable, fill = variable))+
      theme_minimal() + 
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
      ) + ylim(0,(m_df[input$idg[1],'Gold']+10)) + xlab("Country Name") + ylab("Number of Medals")+
      ggtitle('Gold Medal Tally') +
      labs(fill = "",
           caption = "Representing performance of selected Countries." ,
      ) + 
      scale_fill_discrete(breaks=c("Gold")) +
      scale_fill_manual(values = c( 
                                    "Gold" = "#d6af36"
                                    
      ))+ 
      scale_color_discrete(breaks=c("Gold")) +
      scale_color_manual(values = c( 
                                     "Gold" = "#d6af36"
                                    
      ))
    
  }, width = 1000)
  
  sp1 <- reactive({
    new_df_s1 = m_df[c(input$ids[1]:input$ids[2]),c('X','Country','Silver')]
    melt(new_df_s1, id = c('X','Country'))
  })
  
  output$SilverPlots1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    
    new_melt_df_s1 = data.frame(sp1())
    
    # draw the histogram with the specified number of bins
    ggplot(new_melt_df_s1,aes(reorder(Country, -X),value, fill = factor(variable, levels = c("Silver")) )) +
      geom_col() + theme_minimal() + theme(
        legend.position = "none",
        
      ) + coord_flip() +
      scale_fill_discrete(breaks=c("Silver")) +
      scale_fill_manual(values = c("Silver" = "#d7d7d7"
      )) +
      xlab('No. of Medals') + ylab('Country Name') +
      ggtitle('Silver Medals Won') + 
      labs(fill = "",
           caption = "We see the distribution of Silver medals for various countries ranked by no. of  Gold medals awarded." ,
           subtitle = "Ranked Highest(Top) to Lowest(Bottom) "
      )
  }, height = 800, width = 600)
  output$SilverPlots2 <- renderPlot({
    
    #new_df_s1 = m_df[c(input$ids[1]:input$ids[2]),c('X','Country','Silver')]
    new_melt_df_s1 = data.frame(sp1())
    
    # generate bins based on input$bins from ui.R
    
    ggplot(new_melt_df_s1,aes(reorder(Country, +X),value, color = variable)) +
      geom_point(size = 4) + 
      geom_line(aes(group = variable, color = variable, fill = variable))+
      theme_minimal() + 
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
      ) + ylim(0,(m_df[input$ids[1],'Silver']+20)) + xlab("Country Name") + ylab("Number of Medals")+
      ggtitle('Silver Medal Tally') +
      labs(fill = "",
           caption = "Representing performance of selected Countries." ,
      ) + 
      scale_fill_discrete(breaks=c("Silver")) +
      scale_fill_manual(values = c( 
        "Silver" = "#d7d7d7"
        
      ))+ 
      scale_color_discrete(breaks=c("Silver")) +
      scale_color_manual(values = c( 
        "Silver" = "#d7d7d7"
        
      ))
    
  }, width = 1000)
  
  bp1 <- reactive({
    new_df_b1 = m_df[c(input$idb[1]:input$idb[2]),c('X','Country','Bronze')]
    melt(new_df_b1, id = c('X','Country'))
  })
  
  output$BronzePlots1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    
    new_melt_df_b1 = data.frame(bp1())
    
    # draw the histogram with the specified number of bins
    ggplot(new_melt_df_b1,aes(reorder(Country, -X),value, fill = factor(variable, levels = c("Bronze")) )) +
      geom_col() + theme_minimal() + theme(
        legend.position = "none",
        
      ) + coord_flip() +
      scale_fill_discrete(breaks=c("Bronze")) +
      scale_fill_manual(values = c("Bronze" = "#a77044"
      )) +
      xlab('No. of Medals') + ylab('Country Name') +
      ggtitle('Bronze Medals Won') + 
      labs(fill = "",
           caption = "We see the distribution of Bronze medals for various countries ranked by no. of  Gold medals awarded." ,
           subtitle = "Ranked Highest(Top) to Lowest(Bottom) "
      )
  }, height = 800, width = 600)
  output$BronzePlots2 <- renderPlot({
    
    #new_df_b1 = m_df[c(input$idb[1]:input$idb[2]),c('X','Country','Bronze')]
    new_melt_df_b1 = data.frame(bp1())
    
    # generate bins based on input$bins from ui.R
    
    ggplot(new_melt_df_b1,aes(reorder(Country, +X),value, color = variable)) +
      geom_point(size = 4) + 
      geom_line(aes(group = variable, color = variable, fill = variable))+
      theme_minimal() + 
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
      ) + ylim(0,(m_df[input$idb[1],'Bronze']+20)) + xlab("Country Name") + ylab("Number of Medals")+
      ggtitle('Bronze Medal Tally') +
      labs(fill = "",
           caption = "Representing performance of selected Countries." ,
      ) + 
      scale_fill_discrete(breaks=c("Bronze")) +
      scale_fill_manual(values = c( 
        "Bronze" = "#a77044"
        
      ))+ 
      scale_color_discrete(breaks=c("Bronze")) +
      scale_color_manual(values = c( 
        "Bronze" = "#a77044"
        
      ))
    
  }, width = 1000)
  
  
  
    output$CountryPlots <- renderPlot({
      if (input$r1 == 'Percentile Plot'){
        df_pc <- as.data.frame(m_df)
        df_pc$gold_p <- (93-df_pc[,1]+1)*100/93
        df_pc <- df_pc[order(df_pc$Silver, decreasing = TRUE),]
        df_pc$silver_p <- df_pc$gold_p
        for(i in 1:93){
          df_pc[i,8] <- (93-i+1)*100/93
        }
        df_pc <- df_pc[order(df_pc$Bronze, decreasing = TRUE),]
        df_pc$bronze_p <- df_pc$gold_p
        for(i in 1:93){
          df_pc[i,9] <- (93-i+1)*100/93
        }
        df_pc <- df_pc[order(df_pc$Gold,df_pc$Silver, df_pc$Bronze, decreasing = TRUE),]
        
        
        ndf_pc <- as.data.frame(df_pc[,c('X','Country','gold_p','silver_p','bronze_p')])
        head(ndf_pc)
        melt_df_pc <- melt(ndf_pc, id = c("X", "Country"))
        data_selected <- melt_df_pc[melt_df_pc$Country == input$Mult1,]
        glabl = data_selected$value[data_selected$variable == 'gold_p'] 
        slabl = data_selected$value[data_selected$variable == 'silver_p'] 
        blabl = data_selected$value[data_selected$variable == 'bronze_p'] 
        
        ggplot(data_selected, aes(variable, value)) +
          geom_bar(aes(y = 100, fill = variable), stat = "identity", width = 1, color = "white",
                   alpha = 0.1, show.legend = FALSE) +
          geom_bar(stat = "identity", width = 1, aes(fill = variable), color = "white", alpha = 1) +
          coord_polar(clip = "off") +
          geom_hline(yintercept = 25, color = "white", linetype = "dashed", alpha = 0.8) +
          geom_hline(yintercept = 50, color = "white", linetype = "dashed", alpha = 0.8) +
          geom_hline(yintercept = 75, color = "white", linetype = "dashed", alpha = 0.8) +
          scale_fill_manual(values = c("gold_p" = "#d6af36",
                                       "silver_p" = "#d7d7d7",
                                       "bronze_p" = "#a77044"),
                            labels = c("Gold", "Silver", "Bronze")) +
          geom_label(aes(y = 90, label = c(glabl,slabl,blabl), fill = variable), size = 3, color = "white", show.legend = FALSE) +
          scale_y_continuous(limits = c(-20, 100)) +
          labs(fill = "",
                 caption = 'Medals',
               title = input$Mult1,
               subtitle = "Country Preformance wrt Other Countries") +
          theme_minimal() +
          theme(plot.background = element_rect(fill = "white", color = "white"),
                panel.background = element_rect(fill = "white", color = "white"),
                legend.position = "bottom",
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_blank(),
                text = element_text(color = "black", size = 20),
                plot.title = element_text(hjust = 0.5, size = 26, color = "black", face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size = 20, color = "black"),
                plot.caption = element_text(hjust = 0.5, size = 15, color = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      }
      
    
      
      else if(input$r1 == 'Bar Plot'){
      
        nb_df = m_df[,c('X','Country','Gold','Silver','Bronze')]
        nb_melt_df = melt(nb_df, id = c('X','Country'))
        nb_req = nb_melt_df[nb_melt_df$Country == input$Mult1,]
        
        
        ggplot(nb_req,aes(reorder(Country, +X),value, fill = factor(variable, levels = c("Gold", "Silver", "Bronze")) )) +
          geom_col(position = position_dodge()) +theme_minimal()+ theme(
            legend.position = "top",
            legend.justification = c("top"),
            legend.box.just = "top",
            legend.margin = margin(6, 6, 6, 6),
            legend.title = element_blank(),
          ) + 
          scale_fill_manual(values = c("Gold" = "#d6af36",
                                       "Silver" = "#d7d7d7",
                                       "Bronze" = "#a77044"),
                            labels = c("Gold", "Silver", "Bronze"))+
          xlab('Country') + ylab('Medals') }
      
      
      else if(input$r1 == 'Line Plot'){
      
        nb_df = m_df[,c('X','Country','Gold','Silver','Bronze')]
        nb_melt_df = melt(nb_df, id = c('X','Country'))
        nb_req = nb_melt_df[nb_melt_df$Country == input$Mult1,]
        
        ggplot(nb_req, aes(x = variable, y = value, group = 1)) +
          geom_point(size = 4) + geom_line(color = 'gold', size = 2) + theme_minimal() +
        scale_fill_manual(values = c("Gold" = "#d6af36",
                                       "Silver" = "#d7d7d7",
                                       "Bronze" = "#a77044"),
                              labels = c("Gold", "Silver", "Bronze"))+
          xlab('Country') + ylab('Medals') +
          scale_color_manual(values = c("Gold" = "#d6af36",
                                        "Silver" = "#d7d7d7",
                                        "Bronze" = "#a77044"),
                             labels = c("Gold", "Silver", "Bronze"))+
          xlab('Medal') + ylab('Number Of Medals') + ylim(0,45) + 
          ggtitle(input$Mult1)
      }
    },width = 1000)
    
    cp1 <- reactive({
      nb_df1 = m_df[,c('X','Country','Gold','Silver','Bronze')]
      nb_melt_df1 = melt(nb_df1, id = c('X','Country'))
      nb_melt_df1[nb_melt_df1$Country %in% input$noc,]
    })
    
    output$CP1 <- renderPlot({
      
      nb_req1 = data.frame(cp1())
      
      ggplot(nb_req1,aes(reorder(Country, +X),value, fill = factor(variable, levels = c("Gold", "Silver", "Bronze")) )) +
        geom_col(position = position_dodge()) +theme_minimal()+ theme(
          legend.position = "top",
          legend.justification = c("top"),
          legend.box.just = "top",
          legend.margin = margin(6, 6, 6, 6),
          legend.title = element_blank(),
        ) + 
        scale_fill_manual(values = c("Gold" = "#d6af36",
                                     "Silver" = "#d7d7d7",
                                     "Bronze" = "#a77044"),
                          labels = c("Gold", "Silver", "Bronze"))+
        xlab('Country') + ylab('Medals')
      
    },width = 1000)
    
    cp2 <- reactive({
      nb_df2 = m_df[,c('X','Country','Gold','Silver','Bronze','Total')]
      nb_melt_df2 = melt(nb_df2, id = c('X','Country'))
      nb_melt_df2[nb_melt_df2$Country %in% input$noc,]
    })


    output$CP2 <- renderPlot({
      
      nb_req2 = data.frame(cp2())
      
      
      ggplot(nb_req2,aes(reorder(Country, +X),value, color = variable)) +
        geom_point(size = 4) + 
        geom_line(aes(group = variable, color = variable, fill = variable))+
        theme_minimal() + 
        theme(
          legend.position = "right",
          legend.justification = c("right"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        ) + xlab("Country Name") + ylab("Number of Medals")+
        ggtitle('Medal Tally of Selected Countries') +
        labs(fill = "",
             caption = "Representing performance of selected Countries." ,
        ) + 
        scale_fill_discrete(breaks=c("Total","Gold","Silver","Bronze")) +
        scale_fill_manual(values = c( "Total" = "magenta",
                                      "Gold" = "#d6af36",
                                      "Silver" = "#d7d7d7",
                                      "Bronze" = "#a77044"
        ))+ 
        scale_color_discrete(breaks=c("Total","Gold","Silver","Bronze")) +
        scale_color_manual(values = c( "Total" = "skyblue",
                                       "Gold" = "#d6af36",
                                       "Silver" = "#d7d7d7",
                                       "Bronze" = "#a77044"
        ))
      

    },width = 1000)
    
    
    

    output$SportsPlot <- renderPlot({
      
      g_melt_df <- melt(g_df, id = c('Total',"Discipline"))
      req = g_melt_df[g_melt_df$Discipline == input$Mult2,c('variable','value')]
      
      df2 <- req %>% 
        mutate(csum = rev(cumsum(rev(value))), 
               pos = value/2 + lead(csum, 1),
               pos = if_else(is.na(pos), value/2, pos))
      ggplot(req, aes(x = "" , y = value, fill = fct_inorder(variable))) +
        geom_col(width = 1, color = 1) +
        coord_polar(theta = "y") +
        scale_fill_brewer(palette = "Pastel1") +
        geom_label_repel(data = df2,
                         aes(y = pos, label = paste0(value)),
                         size = 4.5, nudge_x = 1, show.legend = FALSE) +
        guides(fill = guide_legend(title = "Gender")) +
        theme_void() + ggtitle(input$Mult2)
  })
    }

#
  #
  ###########
  ########
############################

# Run the application 
shinyApp(ui = ui, server = server)























