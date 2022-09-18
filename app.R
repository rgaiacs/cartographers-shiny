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

ALL_SEASON_CARDS <- read_csv('data/season.csv')
season_deck <-
    ALL_SEASON_CARDS$game |>
    unique()

ALL_SCORING_CARDS <- read_csv('data/scoring.csv')
scoring_deck <-
    ALL_SCORING_CARDS$game |>
    unique()

ALL_EXPLORE_CARDS <- read_csv('data/explore.csv')
explore_deck <-
    ALL_EXPLORE_CARDS$game |>
    unique()

ALL_AMBUSH_CARDS <- read_csv('data/ambush.csv')
ambush_deck <-
    ALL_AMBUSH_CARDS$game |>
    unique()

ALL_HEROES_CARDS <- read_csv('data/heroes.csv')
heroes_deck <-
    ALL_HEROES_CARDS$game |>
    unique()

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/cartographers.css")
    ),
    titlePanel("Cartographers"),
    tabsetPanel(
        type = "tabs",
        tabPanel(
            "Configuration",
            div(
                selectInput("season_deck", "Season Deck:",
                            season_deck, selected = "original"),
                checkboxGroupInput("explore_deck", "Explore Deck:",
                                   explore_deck, selected = "original"),
                selectInput("scoring_deck", "Scoring Deck:",
                                   c("original", "heroes"), selected = "original"),
                selectInput("scoring_deck_expansion", "Scoring Deck Expansion:",
                            c("", "undercity", "affril"), selected = ""),
                checkboxGroupInput("ambush_deck", "Ambush Deck:",
                                   ambush_deck, selected = "original"),
                checkboxGroupInput("heroes_deck", "Heroes Deck:",
                                   heroes_deck),
                actionButton("new.game", label = "New Game")
            )
        ),
        tabPanel(
            "Table",
            fluidRow(
                column(2,
                       div(class = "card-empty")),
                column(2,
                       div(
                           class = "card season",
                           textOutput("season.name"),
                           div(
                               textOutput("season.counter", inline = TRUE),
                               "/",
                               textOutput("season.max", inline = TRUE)
                           ),
                           textOutput("season.description")
                       )),
                column(2,
                       div(class = "card edict",
                           p("A"))),
                column(2,
                       div(class = "card edict",
                           p("B"))),
                column(2,
                       div(class = "card edict",
                           p("C"))),
                column(2,
                       div(class = "card edict",
                           p("D")))
            ),
            fluidRow(
                column(2,
                       div(
                           class = "card",
                           actionButton("next.card", label = "Next Card")
                       )),
                column(2,
                       uiOutput("cards")),
                column(
                    2,
                    div(
                        class = "card score",
                        textOutput("A.name"),
                        imageOutput("A.illustration", height = "auto"),
                        textOutput("A.description")
                    )
                ),
                column(
                    2,
                    div(
                        class = "card score",
                        textOutput("B.name"),
                        imageOutput("B.illustration", height = "auto"),
                        textOutput("B.description")
                    )
                ),
                column(
                    2,
                    div(
                        class = "card score",
                        textOutput("C.name"),
                        imageOutput("C.illustration", height = "auto"),
                        textOutput("C.description")
                    )
                ),
                column(
                    2,
                    div(
                        class = "card score",
                        textOutput("D.name"),
                        imageOutput("D.illustration", height = "auto"),
                        textOutput("D.description")
                    )
                )
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

    seasons <- ALL_SEASON_CARDS %>%
        filter(game == 'original')

    scoring <- ALL_SCORING_CARDS %>%
        filter(game == 'original') %>%
        group_by(group) %>%
        group_modify(~ sample_n(.x, 1)) %>%
        ungroup() %>%
        sample_frac(1)

    explore <- ALL_EXPLORE_CARDS %>%
        filter(game %in% c('original', 'ruin')) %>%
        sample_frac(1)

    ambush <- ALL_AMBUSH_CARDS %>%
        filter(game == 'original') %>%
        sample_frac(1)

    heroes <- ALL_HEROES_CARDS %>%
        filter(game == 'original') %>%
        sample_frac(1)

    observeEvent(input$new.game, {
        cat("Explore Deck:", input$explore_deck, "\n")
        cat("Scoring Deck:", input$scoring_deck, "\n")
        cat("Scoring Deck Expansion:", typeof(input$scoring_deck_expansion), "\n")
        cat("Scoring Deck Expansion:", length(input$scoring_deck_expansion), "\n")
        cat("Scoring Deck Expansion:", input$scoring_deck_expansion == "", "\n")

        start_new_season <<- 1
        this_season <<- 0
        this_card <<- 0
        time_now <<- 0

        seasons <<- ALL_SEASON_CARDS %>%
            filter(game %in% input$season_deck)

        if (input$scoring_deck_expansion == "") {
            scoring_deck_expansion <- input$scoring_deck
        }
        else {
            scoring_deck_expansion <- input$scoring_deck_expansion
        }
        scoring <<- ALL_SCORING_CARDS %>%
            filter((game %in% input$scoring_deck & group != 'shape') | (game == scoring_deck_expansion & group == 'shape')) %>%
            group_by(group) %>%
            group_modify(~ sample_n(.x, 1)) %>%
            ungroup() %>%
            sample_frac(1)

        explore <<- ALL_EXPLORE_CARDS %>%
            filter(game %in% c(input$explore_deck, 'ruin')) %>%
            sample_frac(1)

        ambush <<- ALL_AMBUSH_CARDS %>%
            filter(game %in% input$ambush_deck) %>%
            sample_frac(1)

        heroes <<- ALL_HEROES_CARDS %>%
            filter(game %in% input$heroes_deck) %>%
            sample_frac(1)

        output$cards <- renderUI({
            div(class = "card explore",)
        })
    })

    # Main logic
    observeEvent(input$next.card, {
        if (start_new_season) {
            start_new_season <<- 0
            this_season <<- this_season + 1
            this_card <<- 1

            if (this_season > 4) {
                removeUI(selector = "#next.card")
            }
            else {
                deck <<- merge(
                    explore,
                    head(ambush, seasons$ambush.number[this_season]),
                    by = c("en_name", "illustration"),
                    all = TRUE
                )
                deck <<- merge(
                    deck,
                    head(heroes, seasons$ambush.number[this_season]),
                    by = c("en_name", "illustration"),
                    all = TRUE
                )
                deck <<- sample_frac(deck, 1)

                time_now <<-
                    time_now <<-  deck$cost[1:this_card] |>
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
            tagList(lapply(this_card:1, function(i) {
                div(
                    class = "card explore",
                    div(
                        class = "title",
                        span(class = "cost", deck$cost[i]),
                        span(class = "name", deck$en_name[i])
                    ),
                    div(img(
                        src = file.path('./images', deck$illustration[i])
                    )),
                    div(span(
                        class = "coin", paste(deck$coin[i], "coin")
                    )),
                    div(span(deck$power[i]))
                )
            }))
        })
    })

    # Update state
    output$season.name <- renderText({
        input$new.game && input$next.card
        seasons$en_name[this_season]
    })
    output$season.counter <- renderText({
        input$new.game && input$next.card
        time_now
    })
    output$season.max <- renderText({
        input$new.game && input$next.card
        if (this_season == 0)
            0
        else
            seasons$max.time[this_season]
    })
    output$season.description <- renderText({
        input$new.game && input$next.card
        if (this_season == 0)
            0
        else
            seasons$edict[this_season]
    })

    output$A.name <- renderText({
        input$new.game
        scoring$en_name[1]
    })
    output$A.description <- renderText({
        input$new.game
        scoring$en_description[1]
    })
    output$A.illustration <- renderImage({
        input$new.game
        text <-
            normalizePath(file.path('.', 'www', 'images', scoring$illustration[1]))
        list(src = text)
    }, deleteFile = FALSE)
    output$B.name <- renderText({
        input$new.game
        scoring$en_name[2]
    })
    output$B.description <- renderText({
        input$new.game
        scoring$en_description[2]
    })
    output$B.illustration <- renderImage({
        input$new.game
        text <-
            normalizePath(file.path('.', 'www', 'images', scoring$illustration[2]))
        list(src = text)
    }, deleteFile = FALSE)
    output$C.name <- renderText({
        input$new.game
        scoring$en_name[3]
    })
    output$C.description <- renderText({
        input$new.game
        scoring$en_description[3]
    })
    output$C.illustration <- renderImage({
        input$new.game
        text <-
            normalizePath(file.path('.', 'www', 'images', scoring$illustration[3]))
        list(src = text)
    }, deleteFile = FALSE)
    output$D.name <- renderText({
        input$new.game
        scoring$en_name[4]
    })
    output$D.description <- renderText({
        input$new.game
        scoring$en_description[4]
    })
    output$D.illustration <- renderImage({
        input$new.game
        text <-
            normalizePath(file.path('.', 'www', 'images', scoring$illustration[4]))
        list(src = text)
    }, deleteFile = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
