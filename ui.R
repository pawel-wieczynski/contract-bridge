library(shiny)
source('functions.R')

# Define the UI for openings game
openingsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      radioButtons(ns('bid'),
                   'Choose your bid:',
                   choices = get_all_bids()[c(1:8, 36)],
                   selected = '1♣'
      ),
      actionButton(ns('submit'), 'Submit Bid')
    ),
    mainPanel(
      actionButton(ns("new_hand"), "New Hand"),
      verbatimTextOutput(ns('hand')),
      verbatimTextOutput(ns('result'))
    )
  )
}

# Define UI for the application
ui <- navbarPage("Bridge Games",
                 tabPanel("Openings", openingsUI("openings")),
                 
)