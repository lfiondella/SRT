library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function
source("model.R")#Source for our reliabilty models
source("JMmodel.R")
source("GO_BM_FT.R")
source("Data_Format.R")
source("GO_MVF_lnl_.R")

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
    
    if (names(data[1]) =="FT"){ #if the first column is failure times, convert to interfail
      FT <- data[,1]
      names(FT)<-"FT"
      IF <- data.frame(failureT_to_interF(data[,1])) #converts from failure times to interfailure times
      names(IF)<-"IF"
    }else if(names(data[1]) == "IF"){ #if the first column is interfailure times, convert to failure time
      IF <- data[,1]
      names(IF)<-"IF"
      FT <-data.frame(interF_to_failureT(data[,1])) 
      names(FT)<-"FT"
    }else if(names(data[1]) == "FC") { #if the first column is failure count and next rows are IF or FT
      FC <- data[1]
      names(FC)<-"FC"
      if(names(data[2])=="FT"){#if second row is failure time find IF
        FT <- data[,2]
        names(FT)<-"FT"
        IF <- data.frame(failureT_to_interF(data[,2]))
        names(IF)<-"IF"
      }else if(names(data[2])=="IF"){#if second row is interfailure times find FT
        IF <- data[,2]
        names(IF)<-"IF"
        FT <-data.frame(interF_to_failureT(data[,2]))
        names(FT)<-"FT"}
    }
    data <- cbind(FC,IF) #combines Failure Count and Interfailure, used for plotting original data DO NOT PASS TO MODELS

    
    
    Time <- names(data[2])#generic name of column name of data frame (x-axis)
    Failure <- names(data[1])#(y-axis)
    p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
    p<- p+ ggtitle("Original Data")
    value <- c("red","blue") 
    model <- ""
    if (input$OD == TRUE){
      p <- p + geom_point(data = data,aes(color="blue",group="Original Data")) # geom_line(data = data,aes(color="blue",group="Original Data"))#adds scatter plot points to plot object
      label <- c("Original Data","")
      value <- c("blue","red")
    }
    p <- p + scale_color_manual(name = "Legend",  labels = c("Original Data"),values = c("blue"))
    if (input$Model == "JM"){
      data <- cbind(FC,FT)
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
      
      GO_BM <- GO_BM_MLE(FT)#finds aMLE and bMLE from failure times
      aMLE <- as.numeric(GO_BM[1])#aMLE returned from GO_BM_MLE
      bMLE <- as.numeric(GO_BM[2])#bMLE returned from GO_BM_MLE
     
      MVF_data <- data.frame(MVF(FT,aMLE,bMLE))#Mean Value Function that takes Failure Count and the two MLE variables #data frame
      names(MVF_data)<-"MVF"
      data <- cbind(MVF_data, FT)#added a column of Failure Time to the Mean Value Function Return
      #colnames(data) <- c("FC","IF")#ggplot complains if it doesnt match 
      
      Time <- names(data[2])#generic name of column name of data frame (x-axis)
      Failure <- names(data[1])#(y-axis)
      p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
      
      #p <- p + geom_point(data=data,aes(color="red",group="Geol-Okumoto Model"))
      p <- p + geom_line(data=data,aes(color="red",group="Geol-Okumoto Model"))  
      #p <- p + stat_function(fun = MVF(FT,aMLE,bMLE),aes(color="red",group="Geol-Okumoto Model"))
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
    p<- p+ ggtitle(model)
    
    p
    #plot(data) Leave this here to use if ggplot() stops working. 
  } )
})
