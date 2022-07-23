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
               div(
                   class="card-empty"
               )
       ),
       column(2,
              div(
                  class="card season",
                  textOutput("season.name"),
                  div(textOutput("season.counter", inline = TRUE), "/", textOutput("season.max", inline = TRUE)),
                  textOutput("season.description")
              )
       ),
       column(2,
              div(
                  class="card edict",
                  p("A")
              )
       ),
       column(2,
              div(
                  class="card edict",
                  p("B")
              )
       ),
       column(2,
              div(
                  class="card edict",
                  p("C")
              )
       ),
       column(2,
              div(
                  class="card edict",
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
               uiOutput("cards")
        ),
        column(2,
               div(
                   class="card score",
                   textOutput("A.name"),
                   imageOutput("A.illustration", height="auto"),
                   textOutput("A.description")
               )
        ),
        column(2,
               div(
                   class="card score",
                   textOutput("B.name"),
                   imageOutput("B.illustration", height="auto"),
                   textOutput("B.description")
               )
        ),
        column(2,
               div(
                   class="card score",
                   textOutput("C.name"),
                   imageOutput("C.illustration", height="auto"),
                   textOutput("C.description")
               )
        ),
        column(2,
               div(
                   class="card score",
                   textOutput("D.name"),
                   imageOutput("D.illustration", height="auto"),
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

    ambush <- read_csv('data/ambush.csv') %>%
        sample_frac(1)

    # Main logic
    observeEvent(input$next.card, {
        if (start_new_season) {
            start_new_season <<- 0
            this_season <<- this_season + 1
            this_card <<- 1

            if (this_season > 4) {
                removeUI(
                    selector = "#next.card"
                )
            }
            else {
                deck <<- merge(
                    explore,
                    head(ambush, seasons$ambush.number[this_season]),
                    by=c("en_name", "illustration"),
                    all=TRUE
                )
                deck <<- sample_frac(deck, 1)

                time_now <<- time_now <<-  deck$cost[1:this_card] |>
                    sum(na.rm = TRUE)
            }
        }
        else {
            this_card <<- this_card + 1
            time_now <<-  deck$cost[1:this_card] |>
                sum(na.rm = TRUE)

            if (time_now >= seasons$max.time[this_season]) {
                start_new_season <<- 1
            }

        }

        output$cards <- renderUI({
            tagList(
                lapply(this_card:1, function(i) {
                    div(
                        class="card explore",
                        div(
                            class="title",
                            span(class="cost", deck$cost[i]),
                            span(class="name", deck$en_name[i])
                        ),
                        div(
                            img(src=file.path('./images', deck$illustration[i]))
                        ),
                        div(
                            span(class="coin",paste(deck$coin[i], "coin"))
                        ),
                        div(
                            span(deck$power[i])
                        )
                    )
                })
            )
        })
    })

    # Update state
    output$season.name <- renderText({
        input$next.card
        seasons$en_name[this_season]
    })
    output$season.counter <- renderText({
        input$next.card
        time_now
    })
    output$season.max <- renderText({
        input$next.card
        if(this_season == 0) 0 else seasons$max.time[this_season]
    })
    output$season.description <- renderText({
        input$next.card
        if(this_season == 0) 0 else seasons$edict[this_season]
    })

    output$A.name <- renderText({scoring$en_name[1]})
    output$A.description <- renderText({scoring$en_description[1]})
    output$A.illustration <- renderImage({
        text <-  normalizePath(file.path('./images', scoring$illustration[1]))
        list(src=text)
    }, deleteFile = FALSE)
    output$B.name <- renderText({scoring$en_name[2]})
    output$B.description <- renderText({scoring$en_description[2]})
    output$B.illustration <- renderImage({
        text <-  normalizePath(file.path('./images', scoring$illustration[2]))
        list(src=text)
    }, deleteFile = FALSE)
    output$C.name <- renderText({scoring$en_name[3]})
    output$C.description <- renderText({scoring$en_description[3]})
    output$C.illustration <- renderImage({
        text <-  normalizePath(file.path('./images', scoring$illustration[3]))
        list(src=text)
    }, deleteFile = FALSE)
    output$D.name <- renderText({scoring$en_name[4]})
    output$D.description <- renderText({scoring$en_description[4]})
    output$D.illustration <- renderImage({
        text <-  normalizePath(file.path('./images', scoring$illustration[4]))
        list(src=text)
    }, deleteFile = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
