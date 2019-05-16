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
   titlePanel("Rescorla Wagner Model Example"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("learning_rate",
                     "Learning Rate:",
                     min = 0,
                     max = 1,
                     value = .2,
                     step = .01),
         actionButton("step",
                      "Step")
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
  values$t=0;     # timestep
  values$v=0;     # value
  values$vs = 0   # values to track
  values$rs = 0
  
  # text feedback
  observeEvent(input$step,{
    # Create dependency on the button 'step'
    input$step
    values$t = values$t+1 # step in time
    r = rbinom(size=1,n=1,.7)
    pe= r-values$v # prediction error
    values$v = values$v+isolate(input$learning_rate) * pe
    values$vs= c(values$vs,values$v)
    values$rs= c(values$rs,r)
    cat("\n event observed:","t=",values$t,sep="")
  })
  
  output$distPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     x = seq(0,values$t)
     # draw the histogram with the specified number of bins
     plot(x,values$vs,type="l",xlab = "Trials",ylab="Value Estimate",
          ylim=c(0,1))
     points(x,values$vs,pch=values$rs*15+4,col=values$rs+2)
     abline(h=.7,lty=2)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

