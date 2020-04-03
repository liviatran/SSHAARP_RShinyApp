#SSHAARP R Shiny Application
# v 0.7
# by: Livia Tran
# 4/2/20


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
    selectInput("colorcheck", h4("Map type"), 
                choices = list("Color" = T, "Greyscale" = F), selected = 1),
    selectInput("filterMig", h4("Exclude migrant populations?"), 
                choices = list("Yes" = T, "No" = F), selected = 1),
    actionButton("makemap", "Make map!"),
    hr(),
    div(style="margin-bottom:10px"),
    downloadButton("downloadData", label = "Download my map!"),
    div(style="margin-bottom:20px"),
    actionButton("reset","Clear")
  ),
  

  mainPanel(
    tabsetPanel(type="tabs",
                
                tabPanel("Map maker", 
                         imageOutput("map", height="525px"),
                         tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                         verbatimTextOutput("text")),
                
                tabPanel("About", 
                         uiOutput("aboutText")))
    
    )
)


server<-function(input, output) {
  
  message <- reactiveValues(PALMoutput=NULL)
  wide=707
  high=500
  
  observeEvent(input$makemap, {
    
    message$PALMoutput<-SSHAARP::PALM(input$motif, color=input$colorcheck, filterMigrant = input$filterMig)
    
    output$text<-renderPrint({
      if(is.data.frame(message$PALMoutput)==TRUE){
        cat(paste(message$PALMoutput[1,1],message$PALMoutput[1,2],sep=": "))
      }
      else{
        cat(message$PALMoutput)
      }

    })
    
    
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
        paste(input$motif, ".jpg", sep="")},
      
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
  
  output$aboutText<-renderUI({
    HTML("<font size='4'>Authors:</font><br>
                   Livia Tran - <i>livia.tran@ucsf.edu</i> <br>
                   Steven Mack - <i>sjmack@ucsf.edu </i>
                   <br>
                   <br>
                   This application utilizes the <b>SSHAARP</b> package to create a frequency heatmap for a user-defined HLA amino acid motif. Users are able to create frequency heatmaps based on allele frequency data obtained from the Solberg dataset, or from the Allele Frequency Network Database (AFND).
                   <br>
                   The Solberg dataset (See: <u>doi: 10.1016/j.humimm.2008.05.001</u>) is a meta-analysis of HLA allele frequency data from 497 population samples, representing ~66,800 individuals around the world. 
                   <br>
                   The dataset describes the continent a given population is from based on 10 major world regions. To account for admixed and migratory populations, significantly admixed populations were placed in an 'other' group and assigned a higher complexity(e.g an estimation of the degree of potential admixture in a population sample) rating. Migrant populations were documented with 'mig' following their complexity rating.  
                   <br>
                   The user has the ability to exclude migrant populations in the generated map by selecting the 'Yes' option in the drop-down menu for 'Exclude migrant populations?'. The color of the map output may also be defined; from the 'Color' drop-down menu, select color for a color frequency heatmap, or greyscale for a black and white frequency heatmap. 
                   <br>
                   <br>
                   To download the map generated, click on 'Download map' in the sidebar panel.
                   <br>
                   To clear the map in the main panel, click on 'Clear'.
                   <br>
                   To get started, enter a motif. The motif should be in the following format: Locus*##$~##$~##$, where ## identifies a peptide position, and $ identifies an amino acid residue.
                   <br>
                   <br>
                   <br>
                   <font size='3'>Possible error messages:</font>
                   <br>
                   <br>
                   <b><font size='2'>Your motif is formatted incorrectly. Please use the Locus*##$~##$~##$ format, where ## identifies a peptide position, and $ identifies an amino acid residue.</b></font>
                      <br>
                      -The format of the motif you entered is incorrect. Follow the Locus*##$~##$~##$ format to resolve this error message.
                      <br>
                      <br>
                   <b><font size='2'>'Locus' does not exist</b></font>
                      <br>
                      -The HLA locus you entered is not a locus for which petpide alignments are available in the ANHIG/IMGTHLA Github Repository.
                     <br>
                     <br>
                   <b><font size='2'>One or more of your amino acid positions is not present in the alignment. Please make sure amino acid positions of interest are present in the current release of ANHIG/IMGTHLA alignments. </b></font>
                      <br>
                      - The motif entered has one or more amino acid positions not present in the alignment. To see which amino acid positions are in the alignment, use one of the 'Locus'_prot.txt files at https://github.com/ANHIG/IMGTHLA/tree/Latest/alignments.
                   <br>
                   <br>
                   <b><font size='2'>'Motif entered': No alleles possess this motif</b></font>
                  <br>             
                   - The motif entered is not found in any alleles in the current release of ANHIG/IMGTHLA alignments."
                   )
  })
    
    
              
    

}


shinyApp(ui, server)


