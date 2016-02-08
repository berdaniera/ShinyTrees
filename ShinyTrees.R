library(shiny)
library(leaflet)
library(raster)
library(sp)
a.n <- function(x) as.numeric(x)

load("treeData.Rdata")

ui <- fluidPage(
  fluidRow(
    column(8,
    leafletOutput("mymap")
    ),
    column(4,
      h3("Duke Forest trees, 2013"),
      checkboxInput(inputId = "ras",
                         label = strong("Show wetness raster"),
                         value = FALSE),
      textOutput("treedat"),
      p("Aaron Berdanier, Duke University")
    )
  ),
  fluidRow(
    column(8,
      plotOutput("lplot")
    ),
    column(4,
      plotOutput("wplot")
    )
  )
)

server <- function(input, output, session) {
  
  
  output$mymap <- renderLeaflet({
    
    if(input$ras) pal <- rev(terrain.colors(255))
    else pal <- "#FFFFFF00"
    
    leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap") %>% 
      addRasterImage(twip,opacity=0.5,colors=pal) %>%
      addCircleMarkers(data=points,radius=trcoo[,4]/10,weight=1,color="#001A57",fill=T,opacity=0.1,layerId=as.character(1:76))
  })

  observe({
    click<-input$mymap_marker_click

    output$lplot <- renderPlot({
      par(mar=c(4,4,0.4,0.4),bty="n",las=1)
      plot(Qquant[2,],type="l",ylim=c(0,150),xlim=c(100,300),xaxp=c(0,360,12),log="",xlab="Day",ylab="Water use (kg)")
      lines(Qquant[1,],lty=3)
      lines(Qquant[3,],lty=3)
      if(!is.null(click)) points(Qday[,a.n(click$id)+2],lwd=2,col="#001A57",pch=19)
    })
    
    output$wplot <- renderPlot({
      par(mar=c(4,4,0.4,0.4),bty="n",las=1)
      plot(Qmn~trcoo[,4],cex=exp(wets)/300,log="",ylim=c(0,150),xlim=c(0,80),xlab="Diameter (cm)",ylab="Average water use (kg)",las=1,bty="n",col="#00000075")
      lines(exp(dns$x)~ydns)
      abline(v=0,col="#00000050")
      if(!is.null(click)){
        if(!is.na(Qmn[a.n(click$id)])){
        points(Qmn[a.n(click$id)]~trcoo[a.n(click$id),4],
               lwd=3,cex=exp(wets[a.n(click$id)])/300,col="#001A57")
        segments(0,Qmn[a.n(click$id)],
                 ydns[which.min(abs(exp(dns$x)-Qmn[a.n(click$id)]))],Qmn[a.n(click$id)],
                 lwd=3,col="#001A57")
        }
      }
    })
    
    output$treedat <- renderText({
      if(!is.null(click)) paste0("A ",floor(trcoo[a.n(click$id),4])," cm ",trspp[a.n(click$id)]," tree!")
    })
    
  })

}

shinyApp(ui, server)