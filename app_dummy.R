## Only run examples in interactive R sessions
library(shiny)
library(shinythemes)
library(shinydashboard)
setwd("~/Desktop/WebApp")
filenames=read.table("WavFiles/wavfiles.txt")

if (interactive()) {
  ui = dashboardPage(
    dashboardHeader(title = "Accent Adapted Speech Recognizer"),
    dashboardSidebar(
      selectInput("filename", "File:",filenames$V1),
      actionButton("upload", "Upload File!!")
    ),
    dashboardBody(
      fluidRow(
        column(width = 12,
               valueBoxOutput("result1",3),
               valueBoxOutput("result2",3),
               valueBoxOutput("result5",3),
               valueBoxOutput("result6",3))
      ),
      fluidRow(
        column(width = 12,
               box(title = "Baseline Output",
                   background = "red",
                   width = 6,
                   plotOutput("result3")),
               box(title = "Siamese Output",
                   background = "green",
                   width = 6,
                   plotOutput("result4")))
      ),
      fluidRow(
        column(width = 12,
               box(title = "Reference",
                   width = 12,
                   background = "light-blue",
                   h4(textOutput("result9")))
               )
        ),
      fluidRow(
        column(width = 12,
               box(title = "Baseline Decoding",
                   width = 12,
                   background = "red",
                   h4(textOutput("result7")))
                  )
      ),
      fluidRow(
        column(width = 12,
               box(title = "Adapted Decoding",
                   width = 12,
                   background = "green",
                   h4(textOutput("result8"))
        )
      )
    ),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;}")))
    ))
  server = function(input, output) {
    fname=eventReactive(input$upload,input$filename)
    rc=reactive({
      fname()
      write(fname(),"/home/aditya/Desktop/WebApp/input")
      0})
    checkpoint=reactive({
      rc()
      fname()
      withProgress(message = 'Identifying Accent', value = 0, {
        n=4
        verbose =c('Extracting Features','Building iVector','Generating Predictions',
                   'Identification Complete')
        predictions1=read.csv("AccentID/lda.csv",stringsAsFactors = F)
        predictions1$PRED=names(predictions1[,2:11])[apply(predictions1[,2:11],1,which.max)]
        predictions2=read.csv("AccentID/siamese.csv",stringsAsFactors = F)
        predictions2$PRED=names(predictions2[,2:11])[apply(predictions2[,2:11],1,which.max)]
        for (i in 1:n) {
          incProgress(1/n, detail = verbose[i])
          Sys.sleep(0.2)
        }
      })
      list(predictions1,predictions2)
    })
    
    output$result1=renderValueBox({
      fname()
      predictions=checkpoint()[[1]]
      valueBox(predictions$PRED[predictions$X==fname()],subtitle = "Baseline Prediction",color = "red")
      })
    output$result2=renderValueBox({
      fname()
      predictions=checkpoint()[[2]]
      valueBox(predictions$PRED[predictions$X==fname()],subtitle = "Siamese Prediction",color = "green")
    })
    output$result3=renderPlot({
      fname()
      predictions=checkpoint()[[1]]
      tmp=as.matrix(sort(predictions[predictions$X==fname(),2:11],decreasing = T)[1:3])
      bp=barplot(tmp,ylim=c(0,1),axes = F,col = "darkblue")
      text(x=bp, y=tmp, labels=round(tmp,2), pos=3, xpd=NA)
      bp
    })
    output$result4=renderPlot({
      fname()
      predictions=checkpoint()[[2]]
      tmp=as.matrix(sort(predictions[predictions$X==fname(),2:11],decreasing = T)[1:3])
      bp=barplot(tmp,ylim=c(0,1),axes = F,col = "darkblue")
      text(x=bp, y=tmp, labels=round(tmp,2), pos=3, xpd=NA)
      bp
    })
    
    decode=reactive({
      fname()
      checkpoint()
      dfile=paste("/home/aditya/Desktop/WebApp/AccentAdapt/",fname(),sep="")
      withProgress(message = 'Decoding Speech', value = 0, {
        verbose =c('Extracting Features','Processing Features','Decoding',
                   'Decoding:Just a little bit more','Decoding:I wish I had more cores',
                   'Decoding Complete: Loading Results')
        m=6
        n=1
        while((!file.exists(dfile))|n<m){
          incProgress(1/m, detail = verbose[n])
          if(n<m){n=n+1}
          Sys.sleep(0.5)
        }
      })
      read.csv(dfile,header = FALSE)
    })
    
    output$result5=renderValueBox({
      valueBox(decode()$V2,subtitle = "Baseline WER",color = "red")
    })
    output$result6=renderValueBox({
      valueBox(decode()$V3,subtitle = "Adapted WER",color = "green")
    })
    output$result7=renderText({
      as.character(decode()$V4)
    })
    output$result8=renderText({
      as.character(decode()$V5)
    })
    output$result9=renderText({
      as.character(decode()$V6)
    })
  }
  shinyApp(ui, server)
}