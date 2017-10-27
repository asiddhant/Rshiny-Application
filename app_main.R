## Only run examples in interactive R sessions
library(shiny)
library(shinythemes)
library(shinydashboard)
setwd("~/Desktop/WebApplication")
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
               box(title = "Our Output",
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
               box(title = "Our Decoding",
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
    )
  )
  server = function(input, output) {
    fname=eventReactive(input$upload,input$filename)
    
    rc=reactive({
      fname()
      write(fname(),"/home/aditya/Desktop/WebApplication/AccentAdapt/input")
      0})
    
    checkpoint=reactive({
      rc()
      fname()
      withProgress(message = 'Identifying Accent', value = 0, {
        n=4
        verbose =c('Extracting Features','Building iVector','Generating Predictions',
                   'Identification Complete')
        for (i in 1:(n-1)) {
          incProgress(1/n, detail = verbose[i])
          Sys.sleep(0.5)
        }
        
        system('python AccentID/lda.py')
        predictions1=read.csv("AccentID/lda.csv",stringsAsFactors = F)
        file.remove("AccentID/lda.csv")
        predictions1$PRED=names(predictions1[,2:11])[apply(predictions1[,2:11],1,which.max)]
        
        system('python AccentID/siamese.py')
        predictions2=read.csv("AccentID/siamese.csv",stringsAsFactors = F)
        file.remove("AccentID/siamese.csv")
        predictions2$PRED=names(predictions2[,2:11])[apply(predictions2[,2:11],1,which.max)]
        
        i=4
        incProgress(1/n, detail = verbose[i])
        Sys.sleep(1)
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
      valueBox(predictions$PRED[predictions$X==fname()],subtitle = "Our Prediction",color = "green")
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
      rc()
      fname()
      checkpoint()
      dfile=paste("/home/aditya/Desktop/WebApplication/AccentAdapt/",fname(),sep="")
      withProgress(message = 'Decoding Speech', value = 0, {
        verbose =c('Extracting Features','Processing Features','Decoding',
                   'Decoding:Just a little bit more','Decoding:I wish I had more cores',
                   'Decoding Complete: Loading Results')

        n=12
        m=6
        i=1
        while(!file.exists(dfile)){
          incProgress(1/n, detail = verbose[i])
          if(i<m){i=i+1}
          Sys.sleep(5)
        }
      })
      read.csv(dfile,header = FALSE)
    })
    
    rmdone=reactive({
      decode()
      dfile=paste("/home/aditya/Desktop/WebApplication/AccentAdapt/",fname(),sep="")
      file.remove(dfile)
      0
    })
    
    output$result5=renderValueBox({
      rmdone()
      valueBox(decode()$V2,subtitle = "Baseline WER",color = "red")
    })
    output$result6=renderValueBox({
      rmdone()
      valueBox(decode()$V3,subtitle = "Our WER",color = "green")
    })
    output$result7=renderText({
      rmdone()
      as.character(decode()$V4)
    })
    output$result8=renderText({
      rmdone()
      as.character(decode()$V5)
    })
    output$result9=renderText({
      rmdone()
      as.character(decode()$V6)
    })
  }
  shinyApp(ui, server)
  }