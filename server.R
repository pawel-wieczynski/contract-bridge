library(shiny)
source('functions.R')
source('bidding_system\\openings.R')

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
    
    # Create a reactive value for the result text
    result = reactiveVal('')  
    
    observeEvent(input$submit, {
      # Your bid
      bid = input$bid
      
      # Check the bid
      valid = (replace_icon(bid) == open_bid(hand()))
      
      result(
        if(valid) {
          'Your bid is valid.'
        } else {
          paste0('Your bid is not valid. The correct one is: ', open_bid(hand()))
        }
      )
    })
    
    output$result = renderText({
      # Use the reactive value here
      result()
    })
    
    observeEvent(input$new_hand, {
      hand(shuffle_cards()$south)
      # Clear the result when a new hand is generated
      result('')
    })
  })
}

# Define server logic 
server <- function(input, output, session) {
  openingsServer('openings')
}