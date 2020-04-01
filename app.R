#SSHAARP R Shiny Application
# v 0.5
# by: Livia Tran
# 3/31/20


library(shiny)
library(SSHAARP)
library(shinybusy)

#get wd
wd=getwd()

ui <- fixedPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  add_busy_spinner(spin = "fading-circle"),
  titlePanel("SSHAARP - Searching Shared HLA Amino Acid Prevalence"),
  sidebarPanel(
    textInput("motif", h4("Enter motif")),
    selectInput("colorcheck", h4("Color"), 
                choices = list("Color" = T, "Greyscale" = F), selected = 1),
    selectInput("filterMig", h3("Filter migrant populations?"), 
                choices = list("Yes" = T, "No" = F), selected = 1),
    actionButton("makemap", "Make my map!"),
    hr(),
    div(style="margin-bottom:10px"),
    downloadButton("downloadData", label = "Download my map!"),
    div(style="margin-bottom:20px"),
    actionButton("reset","Clear")
  ),
  mainPanel(
    imageOutput("map", height="525px"),
    tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
    verbatimTextOutput("text"))
)


server<-function(input, output) {
  
  message <- reactiveValues(PALMoutput=NULL)
  wide=707
  high=500
  observeEvent(input$makemap, {
    
    message$PALMoutput<-SSHAARP::PALM(input$motif, color=input$colorcheck, filterMigrant = input$filterMig)
    
    output$text<-renderPrint({message$PALMoutput})
    
    
    if(any((grepl("Your motif", message$PALMoutput)==TRUE) | (grepl("not present in the alignment", message$PALMoutput)==TRUE) | (grepl("not a valid locus", message$PALMoutput)==TRUE)) | (is.data.frame(message$PALMoutput)==TRUE)){
      output$map <- renderImage({
        
        # Return a list containing the filename
        isolate(return(list(src = paste(wd,"/", "OOPS3.jpg", sep=""),
                    contentType = 'image/jpg',
                    width = wide,
                    height = high)))
      }, deleteFile = FALSE)
      
    }
    
    else{
      output$map <- renderImage({
       
        #Return a list containing the filename
        isolate(return(list(src = paste(wd,"/", input$motif, ".jpg", sep=""),
                    contentType = 'image/jpg',
                    width = wide,
                    height = high)))
      }, deleteFile = FALSE)}
    
    
    output$downloadData <- downloadHandler(
      filename <- function() {
        paste(input$motif, ".jpg", sep="")
      },
      
      content <- function(file) {
        file.copy(paste(input$motif, ".jpg", sep=""), file)
        file.remove(paste(input$motif, ".jpg", sep=""))
      }
      
      )
    

  })
  
  
  
  observeEvent(input$reset, {
    output$text <- NULL
    output$map<-NULL
  })
}


shinyApp(ui, server)


