#SSHAARP R Shiny Application
# v 0.2
# by: Livia Tran
# 3/30/20


library(shiny)
library(SSHAARP)
library(shinybusy)

ui <- fixedPage(
  
  add_busy_spinner(spin = "fading-circle"),
  
  titlePanel("SSHAARP - Searching Shared HLA Amino Acid Prevalence"),
  
  sidebarPanel(
    textInput("motif", "Enter motif"),
    selectInput("colorcheck", h3("Color"), 
                choices = list("Color" = T, "Black and Grey" = F), selected = 1),
    selectInput("filterMig", h3("Filter migrant populations?"), 
                choices = list("Yes" = T, "No" = F), selected = 1),
    actionButton("makemap", "Make my map!"),
    div(style="margin-bottom:50px"),
    downloadButton("downloadData", label = "Download your map here!"),
    div(style="margin-bottom:50px"),
    actionButton("reset","Clear map")

    
  ),
  
  mainPanel(
    imageOutput("map", height="525px"),
    tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
    verbatimTextOutput("text"))
  
)


server<-function(input, output) {
  
  message <- reactiveValues(PALMoutput=NULL)
  
  observeEvent(input$makemap, {
    
  
    message$PALMoutput<-SSHAARP::PALM(input$motif, color=input$colorcheck, filterMigrant = input$filterMig)
    
      output$text<-renderPrint({message$PALMoutput})
    

    
    if(any((grepl("Your motif", message$PALMoutput)==TRUE) | (grepl("not present in the alignment", message$PALMoutput)==TRUE) | (grepl("not a valid locus", message$PALMoutput)==TRUE)) | (is.data.frame(message$PALMoutput)==TRUE)){
      output$map <- renderImage({
        validate(need(input$makemap, ""))
        input$makemap
        # Return a list containing the filename
        return(list(src = paste(wd,"/", "OOPS3.jpg", sep=""),
                    contentType = 'image/jpg',
                    width = 580,
                    height = 500))
      }, deleteFile = FALSE)
      
    }
    
    else{
      output$map <- renderImage({
        validate(need(input$makemap, ""))
        input$makemap
        #Return a list containing the filename
        return(list(src = paste(wd,"/", input$motif, ".jpg", sep=""),
                    contentType = 'image/jpg',
                    width = 707,
                    height = 500))
      }, deleteFile = FALSE)}
    
    
    output$downloadData <- downloadHandler(
      filename <- function() {
        paste(input$motif, ".jpg", sep="")
      },
      
      content <- function(file) {
        file.copy(paste(input$motif, ".jpg", sep=""), file)
      },
      contentType = "image/jpg"
    )

    
    })
  
  
  
  observeEvent(input$reset, {
    output$text <- NULL
    output$map<-NULL
    })
}


shinyApp(ui, server)


