library(downloader)
library(e1071)
library(shiny)
library(ggplot2)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

TomorrowDate <- function(x) 
  format(
    as.Date(x, format = "%Y.%m.%d") + 1, 
    format = "%Y.%m.%d"
  )

setwd("~/Documents/monero/")

download("https://coinmetrics.io/data/all.zip", dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./")
file.remove("dataset.zip")

file_vec <- list.files(pattern = ".csv")

expl<- file_vec[-which(file_vec=="xmr.csv")]

l <- lapply(expl, read.csv)

crip_to_remove <- sapply(l, function(x) nrow(x) < 1000)

# remove found elements
l <- l[!crip_to_remove]
l <- lapply(l, function(x) as.data.frame(x[nrow(x)-(nrow(x)-1000):nrow(x),  ]))
l <- lapply(l, function(x) as.data.frame(x[,-1  ]))

# create data.frame with input variables
df_exp <- do.call(cbind, l)

# read data.frame with target and paste it to explanatory variables data.frame

t <- read.csv("xmr.csv")



# Define UI for application that draws a plot
ui <- fluidPage(
   
   # Application title
   titlePanel("Aprendizaje automatizado para predecir precio del Monero de mañana"),
   h5("Creada por Rodrigo Díaz Lupanow"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("range", "Días (desde Mañana menos 1000):",
                     min = 800, max = 999, value = c(999-60,999)),
         h5("Parámetros del modelo en línea ROJA"),
         numericInput("bins2",value = 0.5,label = "Cost"),
         selectInput("bins3","Kernel",c("radial","sigmoid","polynomial","linear"),selected = "polynomial"),
         numericInput("bins4",value = 0.5,label = "Gamma"),
         numericInput("bins5",value = 0,label = "coef0"),
         textOutput('text1')
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h5("Resultados de SVM entrenado con 1000 días antes de hoy",Sys.Date()),
        plotOutput("distPlot"),
        h5("Línea contínua negra: Datos reales, Línea azul: Modelo SVM (radial cost 1), Línea Roja: SVM afinable por ti"),
        h5("Esta app está en período de prueba, es principalmente un material de investigación y difusión. "),
        h5("Dirección Monero para apoyar al proyecto: "),
        h5("45YgkhGVzYjHSHs5LKpuK1b8Qzt8NewBBQEGAM94MdzPabGCCeKxrT85d6UVGqav5raJwgKNQkn47chLVuoZ6taK4t7h7Ah")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     target <- t$price.USD.
     target <-  na.omit(target)
     target <- target[NROW(target)-(NROW(target)-1000):NROW(target)]
     
     #finalmente la data lista para implementar Machine Learning
     xsn<- data.frame(target,df_exp)
     x <- data.frame(target[-1000],df_exp[-1,])
     
     x<-x[sapply(x, function(x) !any(is.na(x)))] 
     
     names(x)[names(x) == "target..1000."] <- "target"
     
     xsn<-xsn[sapply(xsn, function(x) !any(is.na(x)))] 
     
     names(xsn)[names(xsn) == "target..1000."] <- "target"
     
     xo<-x
     
     x<-lapply(x, normalize)
     xsn<-lapply(xsn,normalize)
     x<-as.data.frame(x)
     xsn<-as.data.frame(xsn)
     
     set.seed (123) #para poder repetir las mismas secuencias aleatoreas
     alea <- sample (1:nrow(x), round(nrow(x)/3), replace = FALSE) # index random
     
     tr <- x [-alea,] #quitamos 1/3 de la data
     
     cv <- x [alea,] # nos quedamos con 1/3 para evaluar
     
     model<- svm(tr$target~.,data = tr,scale = T)
     model2<- svm(tr$target~.,data = tr,scale = T,
                  cost=input$bins2,kernel=input$bins3,gamma=input$bins4,coef0=input$bins5)
     
     r<-predict(model,xsn[2:1000,-1])
     r2<-predict(model2,xsn[2:1000,-1])
     
     a=input$range[1];b=input$range[2]
     g<- ggplot(data = xsn[a:b,],aes(x=a:b,y=r[a:b]))
     
     g<-g+geom_point(col="blue",size=0.5)+geom_line(col="blue",linetype=2,size=0.5)+
       geom_point(aes(y=x[(a+1):(b+1),1]),col="black",size=0.5)+
       geom_line(aes(y=x[(a+1):(b+1),1]),col="black",size=0.5)+
       geom_line(aes(y=r2[a:b]),col="red")+
       annotate("text",x=b,y=r[b],label=TomorrowDate(Sys.Date()),color="blue",size=3)+
       annotate("text",x=b,y=r[b-2],label=(Sys.Date()),color="blue",size=3)+
       annotate("text",x=b,y=x[b,1],label=(Sys.Date()),color="black",size=3)+
       xlab("Tiempo (Días)")+ylab("Precio escalado")+ 
       theme(panel.grid.minor = element_line(colour="black"))
       scale_x_continuous(minor_breaks = seq(a,b, 1))
       
      
     
     
g
     
   })
   
  
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

