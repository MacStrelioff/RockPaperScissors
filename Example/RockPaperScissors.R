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
  values$round =0; # track round
  values$opp_actions = c() # track opponent actions
  values$score =0; # track score
  values$scores=0; # track score history for feedback
  values$grams = data.frame('rrrrr'=0) # initialize to store gram counts
  

  # text feedback
  #observeEvent(input$rock,{
  #  values$t = values$t+1 # step in time
  #})
  
  observeEvent(input$rock | input$paper | input$scissors,{
      # if any action taken (done to block the first run when these are all NULL->0)
      if(input$rock | input$paper | input$scissors){
        # increment round
        values$round  = values$round+1;
        # policy -- code to greedily pick best action
        ## if fewer than 5 actions taken, draw uniformly
        if(length(values$opp_actions)<5){
          values$a=sample(c("r","p","s"),1)
          } else{ # if at least 5 actions taken
          nobs = length(values$opp_actions)
          ngram = values$opp_actions[(nobs-5):nobs]
        }
        ## if ngram has been observed
        
        
        
        ## else, use 5-grams
        # draw action
        sample(c("r","p","s"),1,prob=c(.1,.1,.8))
        # evaluate outcome
        
        # update opponent model
        cat(input$rock,input$paper,input$scissors)
        if(input$rock    ==1){opp_action="r"}
        if(input$paper   ==1){opp_action="p"}
        if(input$scissors==1){opp_action="s"}
        values$opp_actions = c(values$opp_actions,opp_action);

      values$score  = values$score+(-1)^(rbinom(n=1,size=1,.7))
      values$scores = c(values$scores,values$score);
      }
    
    # use strings to code, then just take last 5 strings and use as the key for the dictionary of 5-grams...
    output$distPlot <- renderPlot({
      try({
      x = seq(0,values$round);
      y = values$scores;
      cat("\n round: ",values$round, ", score: ",values$score,", len(x): ",length(x)," len(y):",length(y),"opp_act: ",values$opp_actions,
          sep="")
      # draw the histogram with the specified number of bins
      plot(x,y,type="l",xlab = "Rounds",ylab="Score")
      })
    })
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

