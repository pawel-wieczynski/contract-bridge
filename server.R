library(shiny)
source('functions.R')

server = function(input, output) {
  
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
  
}
