library(shiny)
source('functions.R')

# Define the server logic for openings game
openingsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Your hand
    hand = reactiveVal({
      # Call your function to generate a hand
      shuffle_cards()$south
    })
    
    output$hand = renderText({
      paste0('Your hand: \n', print_hand(hand()))
    })
    
    observeEvent(input$submit, {
      # Your bid
      bid = input$bid
      
      # Check the bid
      result = (replace_icon(bid) == open_bid(hand()))
      
      output$result = renderText({
        if(result) {
          'Your bid is valid.'
        } else {
          paste0('Your bid is not valid. The correct one is: ', open_bid(hand()))
        }
      })
    })
    
    observeEvent(input$new_hand, {
      hand(shuffle_cards()$south)
    })
  })
}

# Define server logic 
server <- function(input, output, session) {
  openingsServer("openings")
}