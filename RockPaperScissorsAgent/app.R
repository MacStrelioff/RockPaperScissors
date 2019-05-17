#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# for integrating with a SQL database; 
# https://shiny.rstudio.com/articles/overview.html

# more docs on shiny and including servers; 
# https://docs.rstudio.com/shinyapps.io/applications.html#accessing-databases-with-odbc

# on hosting with amazon AWS:
# https://www.charlesbordet.com/en/shiny-aws-1/
# and here;
# https://medium.com/@CharlesBordet/how-to-deploy-a-shiny-app-on-aws-part-1-4893d0a7432f
# and here
# https://stackoverflow.com/questions/47725234/understanding-the-scalability-of-rshiny-apps-hosted-on-shinyserver

# TODO: 
# add databasing, log user data
# plot data from other sessions in a lighter, different color (light blue)


# for hosting shiny apps, I'm using 
# macstrelioff.shinyapps.io
# to host an app, run:
# rsconnect::deployApp("<PATH TO APP>")
# rsconnect::deployApp("/Users/mac/git/RockPaperScissors/RockPaperScissorsAgent")
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
  titlePanel("Rock Paper Scissors!"),
  # figure
  fluidRow(
    column(width=5,
           plotOutput("distPlot")
    )
  ),
  # buttons
  fluidRow(
    column(width=5,offest=2,
           actionButton("rock","Rock"),
           actionButton("paper","Paper"),
           actionButton("scissors","Scissors")
    )
  ),
  fluidRow(width=5,offset=5,
           textOutput("result"),br(),
           p("Source code available at: https://github.com/MacStrelioff/RockPaperScissors")
           )
  
   #sidebarLayout(
   #   sidebarPanel(position="right",
   #      position="right",
   #      actionButton("rock","Rock"),
   #      actionButton("paper","Paper"),
   #      actionButton("scissors","Scissors")
   #      ),
   #   # Show a plot of the generated distribution
   #   mainPanel(
   #     plotOutput("distPlot") # breaks for a name other than distPlot?
   #   )
   #)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # initialize variables (runs once when app visited)
  values <- reactiveValues()
  values$round =0; # track round
  values$opp_actions = c() # track opponent actions
  values$score =0; # track score
  values$scores=0; # track score history for feedback
  values$grams = data.frame('rrrrr'=rep(0,3)) # initialize to store gram counts
  values$a = "init";
  values$as = c("r","p","s") # possible actions

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
          values$a=sample(values$as,1)
          } else{ # if at least 5 actions taken
          nobs = length(values$opp_actions)
          ngram = paste(values$opp_actions[(nobs-4):nobs],collapse = "")
          #cat("\n",ngram)
          # if this pattern not observed before, initialize it and choose randomly
          if(!any(names(values$grams)==ngram)){
            values$grams[ngram]=rep(0,3)
            values$a=sample(values$as,1)
          } else { # if at least 5 actions taken, and this pattern has been seen before, 
            pred = values$as[which.max(values$grams[ngram][[1]])]
            values$a=switch(pred,"r"="p","p"="s","s"="r")
          }
          #cat("\n",names(values$grams))
          #cat("\n",values$grams[ngram][[1]])
        }
        
        # get opponent action and outcome
        if(input$rock    -sum(values$opp_actions=="r")==1){
          opp_action="r"
          dscore = switch(values$a,"r"=0,"p"=-1,"s"=1)
          }
        if(input$paper   -sum(values$opp_actions=="p")==1){
          opp_action="p"
          dscore = switch(values$a,"r"=1,"p"=0,"s"=-1)
        }
        if(input$scissors-sum(values$opp_actions=="s")==1){
          opp_action="s"
          dscore = switch(values$a,"r"=-1,"p"=1,"s"=0)
        }
        
        # evaluate outcome
        values$score  = values$score+dscore
        values$scores = c(values$scores,values$score);
        
        # update opponent model 
        values$opp_actions = c(values$opp_actions,opp_action);
        
        if(length(values$opp_actions)>5){
          if(any(names(values$grams)==ngram)){
            values$grams[ngram][[1]]=values$grams[ngram][[1]]+(values$as==opp_action)
          }
        }
        
      }
    
    # use strings to code, then just take last 5 strings and use as the key for the dictionary of 5-grams...
    output$distPlot <- renderPlot({
      try({
      x = seq(0,values$round);
      y = values$scores;
      cat("\n round:",values$round, ", score:",values$score,", len(x): ",length(x)," len(y):",length(y),", opp_act:",values$opp_actions,
          "\n a: ",values$a,
          sep="")
      # draw the histogram with the specified number of bins
      plot(x,y,type="l",xlab = "Rounds",ylab="Score",main="Cumulative Score")
      })
    })
    })
  
  output$result = renderText({
    paste("Opponent chose: ",switch(values$a,"r"="Rock","p"="Paper","s"="Scissors","init"="Nothing yet, ..."))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

