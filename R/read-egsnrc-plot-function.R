#' @title Read plot file produced by the EGSnrc usercode dosrznrc or flurznrc
#' @description Read plot file produced by the EGSnrc usercode dosrznrc or flurznrc. These
#' plots are in a format designed for Grace. With this script we can get the data into
#' an R dataframe.
#' @usage
#' df1  <- read.egsnrc( file.path="C:/data/projects/Theoretical-Dosimetry",
#' file.name="mcclan-10410-thin-slab.spectra", 
#' file.name.save="mcclan-10410-thin-slab",
#' read.vec=c(1,3),
#' trace=!TRUE)
#  xyplot(y~x|paste("plt.no",plt.no),data=df1)
#' @name read.egsnrc.plot
#' @author Claus E. Andersen
#' @return A single dataframe (which can be stacked, if more data files of the same type are read).
#' @param file.path: Path to folder where file to be read is located.
#' @param file.name: Name of file to be read 
#' @param file.name.save: If "" then nothing is saved. Otherwise files
#' are saved for each individual plot. Not implemented.
#' @param read.vec: Vector of element that should be processes. c(1,5) means
#' that only plot 1 and 5 should be processed. 1:99 means that
#' all plots up to 99 should be processed.
#' @param plot.wanted: Not implemented.
#' @param trace: TRUE means that the function becomes verbose. FALSE means silience.
#' @export read.egsnrc.plot
read.egsnrc.plot <- function(file.path = "C://MC//egsnrc_mp//flurznrc", 
                        file.name="flurznrc_template.spectra",
                        file.name.save="",
                        read.vec=1:999,
                        plot.wanted=FALSE,
                        trace=FALSE){
  # Created: December 7, 2009
  # Revised: December 7, 2009
  # Revised: June 24, 2012
  # Revised: June 16, 2013
  # Revised: June 17, 2013
  # Revised: July 30, 2014 (can now handle both xydy and xy graphs).
  # Revised: August 1, 2014 
  
  # Input:
  #   file.path: Path to folder where file to be read is located.
  #   file.name: Name of file to be read 
  #   file.name.save: If "" then nothing is saved. Otherwise files
  #                   are saved for each individual plot. Not implemented.
  #   read.vec: Vector of element that should be processes. c(1,5) means
  #             that only plot 1 and 5 should be processed. 1:99 means that
  #             all plots up to 99 should be processed.
  #   plot.wanted: Not implemented.
  #   trace: TRUE means that the function becomes verbose. FALSE means
  #          silience.
  
  # Return value: A single dataframe (which can be stacked, if more data files
  #               of the same type are read. 
  
  
  # Notes  : This version is based on the following syntax-separation
  #          of the egsnrc plotfile:
  #          (1) Each line is terminated by "LF".
  #          (2) Each "graph" is terminated by a "&".
  #          (3) The header consists of lines starting with "@" (i.e. the
  #              first character is a header line is "@"). 
  #          (4) The remaining lines are assumed to be data-lines with
  #              x,y,dy-data or x,y-data. 
  
  # Name   : Claus E. Andersen 
  
  ###########################################################
  # Sample code
  ###########################################################
  # df1  <- read.egsnrc( file.path="C:/data/projects/Theoretical-Dosimetry",
  # file.name="mcclan-10410-thin-slab.spectra", 
  # file.name.save="mcclan-10410-thin-slab",
  # read.vec=c(1,3),
  # trace=!TRUE)
  
  # xyplot(y~x|paste("plt.no",plt.no),data=df1)
  # fp <- "C:/data/projects/Theoretical-Dosimetry"
  ###########################################################
  
  print("read.egsnrc")
  file.full.path <- paste(file.path,file.name,sep='//')
  file.full.path
  
  # Read file first
  file.txt <- scan(file.full.path, what="", multi.line=FALSE, sep="\n")
  ok <- substring(file.txt,1,1) == "&"
  line.index  <- 1:length(file.txt)
  line.stops  <- line.index[ok]-1
  no.of.graphs <- length(line.stops)
  line.starts <- c(1,line.stops[1:no.of.graphs-1]+2)
  
  # no.of.graphs
  df <- NULL
  txt.title <- NA
  txt.subtitle <- NA
  txt.legend <- NA
  txt.xaxis <- NA
  txt.yaxis <- NA
  txt.type <- NA
  
  for (i in 1:no.of.graphs){
    #Grand loop
    if(is.element(i,read.vec)){
      
      line.start <- line.starts[i] 
      line.stop  <- line.stops[i] 
      raw.txt <- file.txt[line.start:line.stop]
      ok <- substring(raw.txt,1,1) == "@"
      raw.head <- raw.txt[ok]
      raw.data <- raw.txt[!ok]
      
      if(trace){
        print(paste("Graph no",i))
        print(raw.head)
        print(raw.data)
      }
      
      ##############################
      # Header
      ##############################
      for (k in 1:length(raw.head)){
        xxx <- raw.head[k]
        
        ee <- '@[[:space:]]+title'
        ok <- length(grep(ee, xxx, value=FALSE)) > 0  
        if(ok){
          yyy <- sub(ee,'', xxx)
          yyy <- sub('\\\\N', '', yyy)  
          yyy <- sub('\\\\S', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          txt.title <- yyy
        }  
        
        ee <- '@[[:space:]]+subtitle'
        ok <- length(grep(ee, xxx, value=FALSE)) > 0  
        if(ok){
          yyy <- sub(ee,'', xxx)
          yyy <- sub('\\\\N', '', yyy)  
          yyy <- sub('\\\\S', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          txt.subtitle <- yyy
        }  
        
        ee <- '@[[:space:]]+legend[[:space:]]+string[[:space:]]+'
        ok <- length(grep(ee, xxx, value=FALSE)) > 0  
        if(ok){
          yyy <- sub(ee,'', xxx)
          yyy <- sub('\\\\N', '', yyy)  
          yyy <- sub('\\\\S', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          txt.legend <- yyy
        }  
        
        ee <- '@[[:space:]]+xaxis[[:space:]]+label[[:space:]]+'
        ok <- length(grep(ee, xxx, value=FALSE)) > 0  
        if(ok){
          yyy <- sub(ee,'', xxx)
          yyy <- sub('\\\\N', '', yyy)  
          yyy <- sub('\\\\S', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          txt.xaxis <- yyy
        }  
        
        ee <- '@[[:space:]]+yaxis[[:space:]]+label[[:space:]]+'
        ok <- length(grep(ee, xxx, value=FALSE)) > 0  
        if(ok){
          yyy <- sub(ee,'', xxx)
          yyy <- sub('\\\\N', '', yyy)  
          yyy <- sub('\\\\S', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          txt.yaxis <- yyy
        }  
        
        ee <- '@TYPE[[:space:]]+'
        ok <- length(grep(ee, xxx, value=FALSE)) > 0  
        if(ok){
          yyy <- sub(ee,'', xxx)
          yyy <- sub('\\\\N', '', yyy)  
          yyy <- sub('\\\\S', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          yyy <- sub('\\"', '', yyy)  
          txt.type <- yyy
        }  
        
      } # raw.header line loop 
      
      if(trace){
        if(!is.na(txt.title )) print(txt.title)
        if(!is.na(txt.legend)) print(txt.legend)
        if(!is.na(txt.xaxis))  print(txt.xaxis)
        if(!is.na(txt.yaxis))  print(txt.yaxis)
        if(!is.na(txt.type))   print(txt.type)
      }
      
      txt.IRL <- NA
      txt.IQ  <- NA
      txt.IP  <- NA
      
      ee <- 'IRL[[:space:]]*=[[:space:]]*[+]*[-]*[0-9]+'
      ok <- length(y <- grep(ee, txt.legend, value=FALSE)) > 0  
      if(ok){
        ii  <- regexpr(ee, txt.legend)
        txt.IRL <- substring(txt.legend,ii,ii+attr(ii,"match.length"))
        txt.IRL <- extract.given.number(txt.IRL,1)
      }  
      
      ee <- 'IQ[[:space:]]*=[[:space:]]*[+]*[-]*[0-9]+'
      ok <- length(y <- grep(ee, txt.legend, value=FALSE)) > 0  
      if(ok){
        ii  <- regexpr(ee, txt.legend)
        txt.IQ <- substring(txt.legend,ii,ii+attr(ii,"match.length"))
        txt.IQ <- extract.given.number(txt.IQ,1)
      }  
      
      ee <- 'IP[[:space:]]*=[[:space:]]*[+]*[-]*[0-9]+'
      ok <- length(y <- grep(ee, txt.legend, value=FALSE)) > 0  
      if(ok){
        ii  <- regexpr(ee, txt.legend)
        txt.IP <- substring(txt.legend,ii,ii+attr(ii,"match.length"))
        txt.IP <- extract.given.number(txt.IP,1)
      }  
      
      if(trace){
        if(!is.na(txt.IRL))  print(paste('IRL =', txt.IRL))
        if(!is.na(txt.IQ ))  print(paste('IQ  =', txt.IQ ))
        if(!is.na(txt.IP ))  print(paste('IP  =', txt.IP ))
      }
      
      ##############################
      # Data
      ##############################
      if(txt.type=="xydy"){
        xx.txt <- paste(raw.data,collapse=" ")
        #xx <- unlist(strsplit(xx.txt,split=". "))
        xx <- unlist(strsplit(xx.txt,split="[[:space:]]+"))
        ok <- !xx==""
        xx <- as.numeric(as.character(xx[ok]))
        if(trace){print(xx[ok])}
        
        nn <- length(xx)/3
        vec <- data.frame(value=xx,group=rep(c("x","y","dy"),nn))
        ok.x <- vec$group=="x"
        ok.y <- vec$group=="y"
        ok.dy <- vec$group=="dy"
        df0 <- data.frame(plt.no=i,x=vec$value[ok.x],y=vec$value[ok.y],dy=vec$value[ok.dy],
                          title=txt.title, subtitle=txt.subtitle, legend=txt.legend, xaxis=txt.xaxis,
                          yaxis=txt.yaxis,graph.type=txt.type,IRL=txt.IRL,IQ=txt.IQ,IP=txt.IP,file.name=file.name)
      }#xydy
      
      if(txt.type=="xy"){
        xx.txt <- paste(raw.data,collapse=" ")
        #xx <- unlist(strsplit(xx.txt,split=". "))
        xx <- unlist(strsplit(xx.txt,split="[[:space:]]+"))
        ok <- !xx==""
        xx <- as.numeric(as.character(xx[ok]))
        if(trace){print(xx[ok])}
        
        nn <- length(xx)/2
        vec <- data.frame(value=xx,group=rep(c("x","y"),nn))
        ok.x <- vec$group=="x"
        ok.y <- vec$group=="y"
        ok.dy <- vec$group=="y"
        df0 <- data.frame(plt.no=i, x=vec$value[ok.x],y=vec$value[ok.y],dy=0*vec$value[ok.dy],
                          title=txt.title, subtitle=txt.subtitle, legend=txt.legend, xaxis=txt.xaxis,
                          yaxis=txt.yaxis,graph.type=txt.type,IRL=txt.IRL,IQ=txt.IQ,IP=txt.IP,file.name=file.name)
      }#xy
      
      if(!file.name.save==""){
        fn <- paste(file.name.save,'-read-egsnrc-',leading.zeros(i,4),".txt",sep="")
        write.table(df0,fn,quote=TRUE,row.names=FALSE,col.names=names(df0))
        print(paste("Saved file:",fn))
      }
      
      
      if(is.null(df)){ df <- df0 }else {df <- rbind(df,df0)}
    } # End read.vec test
  } # End Grand loop
  df
}
