library(shiny)
source('functions.R')

# Define UI for the application
ui = fluidPage(
  
  # Application title
  titlePanel('Bridge Game'),
  
  # Sidebar with a select input for bids
  sidebarLayout(
    sidebarPanel(
      radioButtons('bid',
                  'Choose your bid:',
                  choices = get_all_bids()[c(1:8, 36)],
                  selected = '1♣'
      ),
      actionButton('submit', 'Submit Bid')
    ),
    
    # Show a verbatimTextOutput object for displaying the hand
    mainPanel(
      actionButton("new_hand", "New Hand"),
      verbatimTextOutput('hand'),
      verbatimTextOutput('result')
    )
  )
)