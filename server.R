library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function
source("model.R")#Source for our reliabilty models
source("JMmodel.R")
source("GO_BM.R")
source("Data_Format.R")

shinyServer(function(input, output) {#reactive shiny fuction
  
  if(.Platform$OS.type == "windows")
    perl <- "C:/Strawberry/perl/bin/perl.exe"
  else 
    perl <- "/usr/bin/perl"
  
  
  output$distPlot <- renderPlot({ #reactive function, basically Main()
    
    inFile <- input$file #Read of input file
    if (is.null(inFile))#error handling for null file pointer
      return("Please Upload a CSV File")
    else if (input$type==1)
      data <- read.xls(inFile$datapath,sheet=1,perl=perl)#Reads xls and xlsx files. Perl needed for local windows machines if using newest versions
    else if (input$type==2)
      data <- read.csv(inFile$datapath, header = input$header, sep = input$sep , quote = " % ")#same as before needs error handling
    
    
    len <- nrow(data[1]) #length of data
    FC<-data.frame(c(1:len))  #vector of failure counts from 1 to length
    names(FC)<-"FC" #naming the vector
    
    if (names(data[1]) =="FT"){ #if the first column is failure times, convert to interfail, add failure count
      FT <- data[,1]
      names(FT)<-"FT"
      IF <- data.frame(failureT_to_interF(data[,1])) #converts from failure times to interfailure times
      names(IF)<-"IF"
    #data<-cbind(FC,data) #combines the two columns
    }else if(names(data[1]) == "IF"){
      IF <- data[,1]
      names(IF)<-"IF"
      FT <-data.frame(interF_to_failureT(data)) #do we need to convert back from IF to FT
      names(FT)<-"FT"
    }else if(names(data[1]) == "FC") {
      FC <- data[1]
      names(FC)<-"FC"
      if(names(data[2])=="FT"){
        FT <- data[,2]
        names(FT)<-"FT"
        IF <- data.frame(failureT_to_interF(data[,2]))
        names(IF)<-"IF"
      }else if(names(data[2])=="IF"){
        IF <- data[,2]
        names(IF)<-"IF"
        FT <-data.frame(interF_to_failureT(data[,2]))}
    }
    data <- cbind(FC,IF)

    
    
    Time <- names(IF)#data[1])#generic name of column name of data frame (x-axis)
    Failure <- names(FC)#data[2])#(y-axis)
    p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
    value <- c("red","blue") 
    model <- ""
    if (input$OD == TRUE){
      p <- p + geom_point(data = data,aes(color="blue",group="Original Data")) # geom_line(data = data,aes(color="blue",group="Original Data"))#adds scatter plot points to plot object
      label <- c("Original Data","")
      value <- c("blue","red")
    }
    p <- p + scale_color_manual(name = "Legend",  labels = c("Original Data"),values = c("blue"))
    if (input$Model == "JM"){
      newdata <- JMmodel(data)
      p <- p + geom_point(data=newdata,aes(color="red",group="Jolinski-Moranda Model"))
      model <- c("Jolinski-Moranda Model")
    }
    if (input$Model == "GEO"){
      newdata <- GeoModel(data)
      p <- p + geom_point(data=newdata,aes(color="red",group="Geometric Model"))
      p <- p + geom_line(data=newdata,aes(color="red",group="Geometric Model"))
      model <- c("Geometric Model")
    }
    if (input$Model == "GO"){
      #y <- as.vector(data[,2])
      #print(y)
      
      GO_BM <- GO_BM_MLE(FT)#y)
      newdata <- FT * as.vector(GO_BM)
      newdata <- data.frame(newdata)
      
      data <- cbind(FC, newdata)
      print(data)
      p <- p + geom_point(data=data,aes(color="red",group="Geol-Okumoto Model"))
      p <- p + geom_line(data=data,aes(color="red",group="Geol-Okumoto Model"))
      model <- c("Geol-Okumoto Model")
    }
    if (input$Model == "YS"){
      newdata <- YamadaModel(data)
      p <- p + geom_point(data=newdata,aes(color="red",group="Yamada S-Shaped Model"))
      p <- p + geom_line(data=newdata,aes(color="red",group="Yamada S-Shaped Model"))
      model <- c("Yamada S-Shaped Model")
    }
    if(input$OD == FALSE){
      label = c(model,"")
    }else{
      label = c("Original Data",model)
    }
    p <- p + scale_color_manual(name = "Legend",  labels = label,values = value)
    
    p
    #plot(data) Leave this here to use if ggplot() stops working. 
  } )
})

