# rm(list=ls())
# options(shiny.reactlog=TRUE)

library(shiny)

source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = spatial2Ui(), server = spatial2Server)

#rsconnect::setAccountInfo(name='poliong', token='DB1705251FDE9946B251DAEC0AD3E7F5', secret='WWIxW9QBH6lTqOZ5vbHs5yK7lfknMm5hpcrgYsM9')
#options(rsconnect.max.bundle.files=50000)