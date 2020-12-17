library(shiny)
library(shinydashboard)
library(reactable)
library(tidyverse)
library(tidylo)



## loading data and simple cleaning
##df <- read_csv("ninja_warrior.txt")

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')

df <- df %>% 
    mutate(round_stage = str_remove(round_stage," \\(Regional/City\\)" ))

ui <- dashboardPage(skin = "blue",
    dashboardHeader(title = "Ninja Warrior"),
    
    dashboardSidebar(
        sidebarUserPanel("ANW", subtitle = "A simple Shiny Dashboard", image = "anw.png"),
        
        selectInput("location","Select Location", choices = df %>% 
                        select(location) %>% 
                        distinct() %>% 
                        drop_na() %>% 
                        arrange(location)),
        
        uiOutput("round_stage")
    ),
    
    dashboardBody(
        fluidPage(
        fluidRow(box(width = 12,footer = em("Data Source: Data.World"), 
                     status = "info",infoBox(icon = icon("list-alt"),
                                             title = strong("Introduction"),
                                             width = NULL,
                                             subtitle = "Hi there!\n I'm Emmanuel, thanks for stopping by. This is my first 
                                                         published shiny dashboard, i really learnt alot while building this dashboard.
                                                         The main libraries i used were the tidyverse, tidylo, reactable, shiny and shinydashboard.
                                                         It was really fun building this.\nAll you have to do here is to select a location and round.
                                                         The table gives a list of all activities that has occurred at a particular round and location throughout its history with number of times.
                                                         A special thanks to the #tidytuesday community."))),
        
        fluidRow(box(plotOutput("plot"),width = 7,status = "info"), box(plotOutput("season_plot"),width = 5,status = "info")),
        
        fluidRow(box(reactableOutput("table"),width = 12)))
))



server <- function(input, output) {
    
    ## creating the round filter inputs
    output$round_stage <- renderUI({
        selectInput("round_stage","Select Round", choices = df %>%
                        filter(location == input$location) %>%
                        distinct(round_stage))
    })
    
    
    ## likeness plot
   output$plot <- renderPlot({
       ## Data manipulation
       df  %>% 
           filter(location == input$location) %>%
           count(round_stage,obstacle_name, sort = T) %>%
           bind_log_odds(round_stage,obstacle_name,n) %>%
           filter(round_stage == input$round_stage) %>%
           mutate(obstacle_name = fct_reorder(obstacle_name,log_odds_weighted)) %>%
       ## Plot
           ggplot(aes(log_odds_weighted,obstacle_name))+
           geom_vline(xintercept = 0)+
           geom_point(size = 2)+
           geom_segment(aes(x =0, xend = log_odds_weighted, y = obstacle_name, yend =obstacle_name))+
       ## Text
           labs(
               title = paste0("Likelihood of an event been perfomed at\n", input$location,
                              " in the ",input$round_stage," stage.", collapse = "\n"),
               y = NULL,
               x = "Unlikely (< 0), Likely (> 0)"
           )+
        
        theme_minimal()+
       ## Editing
        theme(
            text = element_text(family = "verdana, sans-serif"),
            legend.position = "none",
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 13, face = "italic"),
            axis.title = element_text(size = 13, face = "italic")
        )
    })
        
    
   ## Season/Activity Plot
   output$season_plot <- renderPlot({
       
       ## Data manipulation
       df %>%
           filter(location == input$location) %>%
           count(season,obstacle_name) %>%
           count(season) %>%
           mutate(season = as.factor(str_c("S",season))) %>%
       ##  Plot
           ggplot(aes(season,n, col = season))+
           geom_point()+
           geom_segment(aes(y = n, yend = 0, x = season, xend = season))+
           scale_y_continuous(breaks = seq(0,100,5))+
           coord_flip()+
       ##  Text       
           labs(
               title = paste0("Number of obstacle challanges performed at\n ",input$location," by seasons", collapse = "\n"),
               y = NULL,
               x = NULL
           )+
           
           theme_minimal()+
       ##  Editing  
           theme(
               text = element_text(family = "verdana, sans-serif"),
               legend.position = "none",
               plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
               axis.text = element_text(size = 13, face = "italic"),
               axis.title = element_text(size = 13, face = "italic")
           )
   })
   
   
    ## Table
   output$table <- renderReactable({
    df %>%
        filter(location == input$location) %>%
        count(round_stage,obstacle_name, name = "times", sort = T) %>%
        filter(round_stage == input$round_stage) %>% 
        select(-round_stage) %>%
           reactable(
               ## Column editing
               columns = list(
                   obstacle_name = colDef(name = "Obstacle Name", 
                                          align = "center", 
                                          sortable = F),
                   times = colDef(name = "Times", 
                                  align = "center")),
               defaultPageSize = 5,
               paginationType = "simple",
               borderless = T,
               highlight = F,
               searchable = F,
               wrap = T,
               style = list("padding-top" = 0, 
                            border = "none", 
                            fontFamily = "verdana, sans-serif", 
                            color = "black"),
               
               ## Custom CSS editing
               theme = reactableTheme(
                   backgroundColor = "#FFFFFF",
                   borderColor = "#247BA0",
                   cellPadding = "8px 12px",
                   headerStyle = list(
                       "font-weight" = 800,
                       "background-color" = "#247BA0",
                       "border-bottom-color" = "#247BA0",
                       "border-bottom-width" = "1px",
                       color = "black",
                       padding = "10px 10px 10px",
                       "margin-bottom" = "8px"
                   )
                   ),
               language = reactableLang(
                   pageInfo = "{rowStart} to {rowEnd} of {rows} entries",
                   pagePrevious = "\u276e",
                   pageNext = "\u276f",
                   pagePreviousLabel = "Previous page",
                   pageNextLabel = "Next page"
               )
           )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)