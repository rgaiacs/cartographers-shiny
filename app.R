#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/cartographers.css")
    ),
    titlePanel("Cartographers"),
    fluidRow(
        column(2,
       ),
       column(2,
              div(
                  class="card",
                  # textOutput("season.name"),
                  p(textOutput("season.counter", inline = TRUE), "/", textOutput("season.max", inline = TRUE)),
                  textOutput("season.description")
              )
       ),
       column(2,
              div(
                  class="card",
                  p("A")
              )
       ),
       column(2,
              div(
                  class="card",
                  p("B")
              )
       ),
       column(2,
              div(
                  class="card",
                  p("C")
              )
       ),
       column(2,
              div(
                  class="card",
                  p("D")
              )
       )
    ),
    fluidRow(
        column(2,
               div(
                   class="card",
                   actionButton("next.card", label = "Next Card")
               )
        ),
        column(2,
               div(
                   class="card",
                   textOutput("top.card.name"),
                   textOutput("top.card.cost"),
                   imageOutput("top.card.illustration"),
                   textOutput("top.card.description")
               )
               #textOutput("card.counter"),
        ),
        column(2,
               div(
                   class="card",
                   textOutput("A.name"),
                   img(src = "img/placeholder.png", height = 100, width = 100),
                   textOutput("A.description")
               )
        ),
        column(2,
               div(
                   class="card",
                   textOutput("B.name"),
                   img(src = "img/placeholder.png", height = 100, width = 100),
                   textOutput("B.description")
               )
        ),
        column(2,
               div(
                   class="card",
                   textOutput("C.name"),
                   img(src = "img/placeholder.png", height = 100, width = 100),
                   textOutput("C.description")
               )
        ),
        column(2,
               div(
                   class="card",
                   textOutput("D.name"),
                   img(src = "img/placeholder.png", height = 100, width = 100),
                   textOutput("D.description")
               )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    start_new_season <- 1
    this_season <- 0
    this_card <- 0
    time_now <- 0
    
    seasons <- read_csv('data/season.csv')
    
    scoring <- read_csv('data/scoring.csv') %>%
        group_by(group) %>%
        group_modify(~ sample_n(.x, 1)) %>%
        ungroup() %>%
        sample_frac(1)
    
    explore <- read_csv('data/explore.csv') %>%
        sample_frac(1)
    
    # Main logic
    observeEvent(input$next.card, {
        if (start_new_season) {
            start_new_season <<- 0
            this_season <<- this_season + 1
            this_card <<- 1
            time_now <<- time_now <<-  explore$cost[1:this_card] |>
                sum(na.rm = TRUE)
        }
        else {
            this_card <<- this_card + 1
            time_now <<-  explore$cost[1:this_card] |>
                sum(na.rm = TRUE)
            
            if (time_now > seasons$max.time[this_season]) {
                start_new_season <<- 1
            }
            
        }
    })
    
    # Update state
    output$season.name <- renderText({
        input$next.card
        seasons$name[this_season]
    })
    output$season.counter <- renderText({
        input$next.card
        time_now
    })
    output$season.max <- renderText({
        input$next.card
        if(this_season == 0) 0 else seasons$max.time[this_season]
    })
    output$season.description <- renderText({""})

    output$A.name <- renderText({scoring$en_name[1]})
    output$A.description <- renderText({scoring$en_description[1]})
    output$B.name <- renderText({scoring$en_name[2]})
    output$B.description <- renderText({scoring$en_description[2]})
    output$C.name <- renderText({scoring$en_name[3]})
    output$C.description <- renderText({scoring$en_description[3]})
    output$D.name <- renderText({scoring$en_name[4]})
    output$D.description <- renderText({scoring$en_description[4]})
        
    output$card.counter <- renderText({input$next.card})
    output$top.card.name <- renderText({
        input$next.card

        if (this_card > 0) {
            text <- explore$en_name[this_card]
        }
        else {
            text <- ""
        }
        text
    })
    output$top.card.cost <- renderText({
        input$next.card

        if (this_card > 0) {
            text <- explore$cost[this_card]
        }
        else {
            text <- ""
        }
        text
    })
    output$top.card.illustration <- renderImage({
        input$next.card

        if (this_card > 0) {
            text <-  normalizePath(file.path('./images', explore$illustration[this_card]))
        }
        else {
            text <- ""
        }

        list(src=text)
    }, deleteFile = FALSE)
    output$top.card.description <- renderText({ "" })
}

# Run the application 
shinyApp(ui = ui, server = server)
