#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# for hosting shiny apps, I'm using 
# macstrelioff.shinyapps.io
# to host an app, run:
# rsconnect::deployApp("<PATH TO APP>")
# rsconnect::deployApp("/Users/mac/git/macstrelioff.github.io/MacStrelioff_Source/content/Unlisted/Example")
# URL on shinyapps.io will be folder name of this project, the last part of the path

# for a rock-paper-scissors bot, can enumerate 5-gram outcomes with; 
# all_5_grams <- expand.grid(p1 = c(1,2,3), p2 = c(1,2,3), p3 = c(1,2,3),p4 = c(1,2,3),p5 = c(1,2,3), stringsAsFactors = FALSE) 
# I think the original idea/app was by 
# Jonathan Reardon, Twitter: @waterlego 

# for including .md files; 
# https://github.com/rstudio/shiny-examples/tree/master/048-including-html-text-and-markdown-files

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Rock Paper Scissors Bot"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         actionButton("rock",
                      "Rock"),
         actionButton("paper",
                      "Paper"),
         actionButton("scissors",
                      "Scissors")
         ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot") # breaks for a name other than distPlot?
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # initialize variables (runs once when app visited)
  values <- reactiveValues()
  values$round = 0; # track round
  values$score= 0   # track score
  values$scores=0
  values$ts=0
  
  # text feedback
  #observeEvent(input$rock,{
  #  values$t = values$t+1 # step in time
  #})
  
  # use strings to code, then just take last 5 strings and use as the key for the dictionary of 5-grams...
  output$distPlot <- renderPlot({
    values$t = values$t+1
    values$ts = c(values$ts,values$t)
    values$scores = c(values$scores,1)
     x = seq(0,values$round)
     y = values$scores
     # draw the histogram with the specified number of bins
     plot(x,values$score,type="l",xlab = "Rounds",ylab="Score")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

