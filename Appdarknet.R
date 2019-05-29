#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



PredictImage=function(imagem,modelo,pickle,linhas){
  require(reticulate)
  use_python("/usr/bin/python3")
  require(keras)
  u=modelo
 # u=load_model_hdf5(modelo)
  source_python("classify_function.py")
  l1=prediction(u,pickle,imagem)
  l1=data.frame(l1)
  l1=l1[order(l1$probability,decreasing=TRUE),]
  if(nrow(l1)<linhas)
    print(l1)
  else
    print(l1[1:linhas,])
}

library(shiny)
library(keras)
#library(reticulate)
#use_python("/usr/bin/python3")

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
   # Application title
   titlePanel("Deep Learning Road Detection"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput(inputId = "SelectImage",label = "Selecione a imagem",accept = c('.jpg','.png')),
        # fileInput(inputId = "SelectModel",label = "Selecione o modelo",accept = c('.model','.hdf5')),
        # fileInput(inputId = "SelectPickle",label = "Selecione o arquivo binarizer pickle",accept = c('.pickle')),
#	numericInput(inputId="linhas",label="numero de classes preditas",min=1,max=1000,value=5)
      sliderInput("Confidence",label = "Confidence Level (0 a 1)",min =0,max=1,step = 0.01,value = 0.2 )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
       # tabsetPanel(id="Referenciador",
        #  tabPanel("Classify Species",verbatimTextOutput("Probabilities")),
         # tabPanel("Classify Health",verbatimTextOutput("Probabilities2"))
        #),
        #plotOutput("distPlot"),
         plotOutput("ImgPlot"),
         h3("Application developed by Rafael Silva Pereira!\n\n"),
         h4("Doubts or problems enter in contant by email:\n\n"),
         h4("This application uses trained models in a yolo implementation called darknet\n\n"),
         h4("Contact email: r.s.p.models@gmail.com")
      
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000000*1024^2)

  plot_jpeg = function(path, add=FALSE)
  {
    require('jpeg')
    jpg = readJPEG(path, native=T) # read the file
    res = dim(jpg)[2:1] # get the resolution, [x, y]
    if (!add) # initialize an empty plot area if add==FALSE
      plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
  }
   
 
   
   output$ImgPlot <- renderPlot({
     if(!is.null(input$SelectImage)){
       executar_darknet()
       print(list.files())
      plot_jpeg(path="darknet/predictions.jpg")       
     }
   })

    executar_darknet<-reactive({
      if(!is.null(input$SelectImage)){
        if(sum(list.files() %in% 'darknet')>0){
          setwd("darknet")
        }
        system("chmod +x darknet")
        strings=paste("./darknet detector test ../Model/obj.data ../Model/yolo-obj.cfg ../Model/yolo-obj_last.weights",input$SelectImage$datapath,sep=" ")
        strings=paste(strings,"-thresh ",sep=" ")
        strings=paste(strings,input$Confidence,sep=" ")
      system(strings)
      setwd("..")
      }
    })
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

