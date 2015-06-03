library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function
source("model.R")#Source for our reliabilty models
source("JM_BM.R")
source("GO_BM_FT.R")
source("Data_Format.R")
source("GO_MVF_lnl_.R")
source("GO_EM_FT.R")

shinyServer(function(input, output) {#reactive shiny fuction
  
  if(.Platform$OS.type == "windows")
    perl <- "C:/Strawberry/perl/bin/perl.exe"
  else 
    perl <- "/usr/bin/perl"
  
  JMR <- reactive({JM_BM_MLE(typeConvert()$IF)})#these are reactive functions of the originals 
  GOR <- reactive({GO_BM_MLE(typeConvert()$FT)})#so that they may be called outside the plot
  
  output$distPlot <- renderPlot({ #reactive function, basically Main()
    
<<<<<<< HEAD
    uploadData<-fileInput()
    if(is.null(uploadData))
      return("upload file")
      
    data <- typeConvert()
    FC <- data$FC#[,1] 
    IF <- data$IF#[,2] 
    FT <- data$FT#[,3]
   
=======

    
    inFile <- input$file #Read of input file
    if (is.null(inFile))#error handling for null file pointer
      return("Please Upload a CSV File")
    else if (input$type==1)
      data <- read.xls(inFile$datapath,sheet=1,perl=perl)#Reads xls and xlsx files. Perl needed for local windows machines if using newest versions
    else if (input$type==2)
      data <- read.csv(inFile$datapath, header = input$header, sep = input$sep , quote = " % ")#same as before needs error handling
    
    temp <- createFields(data)
    FC <- temp$FC
    FT <- temp$FT
    IF <- temp$IF
    
>>>>>>> 1d1e0cba1306ef95ae081c0ab65b5ffcd7bd7b17
    data <- cbind(data.frame(FC),data.frame(IF))#combines Failure Count and Interfailure, used for plotting original data DO NOT PASS TO MODELS

    GO_EM <- GO_EM_FT(FT)
    GO_BM <- GO_BM_MLE(FT)
    JM_BM <- JM_BM_MLE(IF)
    
    Time <- names(data[2])#generic name of column name of data frame (x-axis)
    Failure <- names(data[1])#(y-axis)
    p <- ggplot(,aes_string(x=Time,y=Failure))#This function invokes the ggplot function and assigns it to our plot object p. Any changes must be made to p.
    p<- p+ ggtitle("Original Data")
    value <- c("red","blue") 
    model <- ""
    p <- p + geom_point(data = data,aes(color="blue",group="Original Data")) # geom_line(data = data,aes(color="blue",group="Original Data"))#adds scatter plot points to plot object
    label <- c("Original Data","")
    value <- c("blue","red")
    p <- p + scale_color_manual(name = "Legend",  labels = c("Original Data"),values = c("blue"))
    
    
    if (input$Model == "JM"){
<<<<<<< HEAD
      model <- c("Jelinski-Moranda Model")
      JM_BM <- JMR()
=======
      
>>>>>>> 1d1e0cba1306ef95ae081c0ab65b5ffcd7bd7b17
      aMLE <- as.numeric(JM_BM[1])#aMLE returned from GO_BM_MLE
      bMLE <- as.numeric(JM_BM[2])#bMLE returned from GO_BM_MLE
      
      
      MVF_data <- data.frame(MVF(FT,aMLE,bMLE))#Mean Value Function that takes Failure Count and the two MLE variables #data frame
      names(MVF_data)<-"MVF"
      data <- cbind(MVF_data, FT)#added a column of Failure Time to the Mean Value Function Return
      #colnames(data) <- c("FC","IF")#ggplot complains if it doesnt match 
      
      Time <- names(data[2])#generic name of column name of data frame (x-axis)
      Failure <- names(data[1])#(y-axis)
      p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
<<<<<<< HEAD
      p <- p + geom_point(data=data,aes(color="red",group="Jelinski-Moranda Model"))
=======
>>>>>>> 1d1e0cba1306ef95ae081c0ab65b5ffcd7bd7b17
      p <- p + geom_line(data=data,aes(color="red",group="Jelinski-Moranda Model"))
      #p <- p + geom_line(data=data,aes(color="red",group="Jelinski-Moranda Model"))
      
      
    }
    if (input$Model == "GO_EM_FT"){
      
      aMLE <- as.numeric(GO_EM[1])#aMLE returned from GO_BM_MLE
      bMLE <- as.numeric(GO_EM[2])#bMLE returned from GO_BM_MLE
      
      MVF_data <- data.frame(MVF(FT,aMLE,bMLE))#Mean Value Function that takes Failure Count and the two MLE variables #data frame
      names(MVF_data)<-"MVF"
      data <- cbind(MVF_data, FT)#added a column of Failure Time to the Mean Value Function Return
      #colnames(data) <- c("FC","IF")#ggplot complains if it doesnt match 
      
      Time <- names(data[2])#generic name of column name of data frame (x-axis)
      Failure <- names(data[1])#(y-axis)
      p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
      p <- p + geom_line(data=data,aes(color="red",group="Goel-Okumoto Model(EM)"))
      
      model <- c("Goel-Okumoto Model(EM)")
    }

    if (input$Model == "GO"){
<<<<<<< HEAD
      model <- c("Geol-Okumoto Model")
      GO_BM <- GO_BM_MLE(FT)#finds aMLE and bMLE from failure times
=======
      
      #finds aMLE and bMLE from failure times
>>>>>>> 1d1e0cba1306ef95ae081c0ab65b5ffcd7bd7b17
      aMLE <- as.numeric(GO_BM[1])#aMLE returned from GO_BM_MLE
      bMLE <- as.numeric(GO_BM[2])#bMLE returned from GO_BM_MLE
      MVF_data <- data.frame(MVF(FT,aMLE,bMLE))#Mean Value Function that takes Failure Count and the two MLE variables #data frame
      names(MVF_data)<-"MVF"
      data <- cbind(MVF_data, FT)#added a column of Failure Time to the Mean Value Function Return
      #colnames(data) <- c("FC","IF")#ggplot complains if it doesnt match 
      
      Time <- names(data[2])#generic name of column name of data frame (x-axis)
      Failure <- names(data[1])#(y-axis)
      p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
<<<<<<< HEAD
      
      p <- p + geom_point(data=data,aes(color="red",group="Geol-Okumoto Model"))
      p <- p + geom_line(data=data,aes(color="red",group="Geol-Okumoto Model"))
      data <- cbind(FC, FT)
      p <- p + geom_point(data=data,aes(color="blue",group="Geol-Okumoto Model"))
=======
      #p <- p + geom_point(data=data,aes(color="red",group="Geol-Okumoto Model"))
      p <- p + geom_line(data=data,aes(color="red",group="Geol-Okumoto Model"))  
>>>>>>> 1d1e0cba1306ef95ae081c0ab65b5ffcd7bd7b17
      #p <- p + stat_function(fun = MVF(FT,aMLE,bMLE),aes(color="red",group="Geol-Okumoto Model"))
      
    }
    if (input$Model == "GEO"){
      newdata <- GeoModel(data)
      p <- p + geom_point(data=newdata,aes(color="red",group="Geometric Model"))
      p <- p + geom_line(data=newdata,aes(color="red",group="Geometric Model"))
      model <- c("Geometric Model")
    }
    if (input$Model == "YS"){
      newdata <- YamadaModel(data)
      p <- p + geom_point(data=newdata,aes(color="red",group="Yamada S-Shaped Model"))
      p <- p + geom_line(data=newdata,aes(color="red",group="Yamada S-Shaped Model"))
      model <- c("Yamada S-Shaped Model")
    }
    if(input$Model == "NM"){
      label = c("Original Data","")
      model = c("Original Data")
    }else{
      label = c(model,"")
      value = c("red","")
    }
    

    p <- p + scale_color_manual(name = "Legend",  labels = label,values = value)
    p<- p+ ggtitle(model)
 
    p

    #plot(data) Leave this here to use if ggplot() stops working. 
  } )
<<<<<<< HEAD
  #data<-reactiveValues(FT=typeConvert()$FT)
  output$aMLEText<-  renderText({
    if (!is.null(input$file))
      switch(input$Model,
             GO = GOR(),
             JM = JMR())        
       })
  fileInput<-reactive({
    inFile <- input$file #Read of input file
    if (is.null(inFile))#error handling for null file pointer
      data<-NULL#return("Please Upload a CSV File")
    else if (input$type==1)
      data <- read.xls(inFile$datapath,sheet=1,perl=perl)#Reads xls and xlsx files. Perl needed for local windows machines if using newest versions
    else if (input$type==2)
      data <- read.csv(inFile$datapath, header = input$header, sep = input$sep , quote = " % ")#same as before needs error handling
    data
  })
  
  typeConvert<-reactive({
    data<-fileInput()
    len <- nrow(data[1]) #length of data
    FC<-c(1:len)  #vector of failure counts from 1 to length
    names(FC)<-"FC" #naming the vector
    
    if (names(data[1]) =="FT"){ #if the first column is failure times, convert to interfail
      FT <- data[,1]
      names(FT)<-"FT"
      IF <- failureT_to_interF(data[,1]) #converts from failure times to interfailure times
      names(IF)<-"IF"
    }else if(names(data[1]) == "IF"){ #if the first column is interfailure times, convert to failure time
      IF <- data[,1]
      names(IF)<-"IF"
      FT <-interF_to_failureT(data[,1]) 
      names(FT)<-"FT"
    }else if(names(data[1]) == "FC") { #if the first column is failure count and next rows are IF or FT
      FC <- data[,1]
      names(FC)<-"FC"
      if(names(data[2])=="FT"){#if second row is failure time find IF
        FT <- data[,2]
        names(FT)<-"FT"
        IF <- failureT_to_interF(data[,2])
        names(IF)<-"IF"
      }else if(names(data[2])=="IF"){#if second row is interfailure times find FT
        IF <- data[,2]
        names(IF)<-"IF"
        FT <-interF_to_failureT(data[,2])
        names(FT)<-"FT"}
    }
    data <- new.env()#cbind(data.frame(FC),data.frame(IF),data.frame(FT))
    data$FT<-FT#assign("FC",FC,"IF",IF,"FT",FT,envir=data)
    data$IF<-IF
    data$FC<-FC
    data
  })
  
  
=======
#  output$text1 <- renderText({ "dumb"
#    if(exists("aMLE")){"stuff"}
#  }) 
>>>>>>> 1d1e0cba1306ef95ae081c0ab65b5ffcd7bd7b17
})
