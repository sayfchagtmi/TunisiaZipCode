
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

TunisiaZipCode <- function() {
  library(shiny)
  df = read.csv("https://raw.githubusercontent.com/sayfchagtmi/zipcodetunisia/master/MyData.csv?token=AeshStOgwkiBUoSQ3aoxNAEY9f6S8fxuks5cChiRwA%3D%3D")
  df$Gouvernorat = as.character(df$Gouvernorat)
  df$Délégation = as.character(df$Délégation)
  df$Localité = as.character(df$Localité)

  ui = fluidPage(

    titlePanel("Codes postaux de Tunisie"),

    sidebarLayout(
      sidebarPanel(
        textInput("zip_code", "Saisir le code"),
        actionButton("submit", "Submit"),
        br(),
        downloadButton("downloadData", "Download")
      ),

      mainPanel(
        verbatimTextOutput("Infos")
      )
    )
  )

  # Define server logic required to draw a histogram
  server = function(input, output) {

    TextInfos = eventReactive(input$submit, {

      if(input$zip_code %in% df[,4])
      {
        loc = df[which(df[,4] == input$zip_code),"Localité"]
        del = df[which(df[,4] == input$zip_code),"Délégation"]
        gov = df[which(df[,4] == input$zip_code),"Gouvernorat"]


        text_info = paste(paste("Gouvernorat:",gov,sep ="  "),
                          paste("Délégation:",del,sep ="  "),
                          paste("Localité:",loc,sep="  "),
                          sep = "\n")
      }
      else text_info = "Code invalide"
      return(text_info)
    })

    output$Infos = renderText({
      TextInfos()
    })

    output$downloadData = downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(df, file)
      }
    )
  }

  shinyApp(ui = ui, server = server)

}
