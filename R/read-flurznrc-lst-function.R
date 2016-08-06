#' @title Read fluence spactre in lst file produced by the EGSnrc usercode flurznrc
#' @description Read fluence spactre in lst file produced by the EGSnrc usercode flurznrc.
#' See body of function for further details.
#' @usage
#' df1  <- read.egsnrc( file.path="C:/data/projects/Theoretical-Dosimetry",
#' file.name="mcclan-10410-thin-slab.spectra", 
#' file.name.save="mcclan-10410-thin-slab",
#' read.vec=c(1,3),
#' trace=!TRUE)
#  xyplot(y~x|paste("plt.no",plt.no),data=df1)
#' @name read.flurznrc.lst
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
#' @export read.flurznrc.lst
read.flurznrc.lst <- function(file.path = "C://MC//egsnrc_mp//flurznrc", 
                              file.name="flurznrc_template.spectra",
                              file.name.save="",
                              read.vec=1:999,
                              plot.wanted=FALSE,
                              trace=FALSE
){
  # This function can read lst-files from the egsnrc user-code FLURZnrc.
  # Created: July 31, 2013
  # Revised: July 31, 2014 
  # Revised: August 1, 2014 
  # Name   : Claus E. Andersen
  
  # Input:
  #   file.path: Path to folder where file to be read is located.
  #   file.name: Name of file to be read 
  #   file.name.save: If "" then nothing is saved. Otherwise files
  #                   are saved for each individual plot.
  #   read.vec: Vector of element that should be processes. c(1,5) means
  #             that only plot 1 and 5 should be processed. 1:99 means that
  #             all plots up to 99 should be processed.
  #   plot.wanted: Not implemented.
  #   trace: TRUE means that the function becomes verbose. FALSE means
  #          silience.
  
  # Return value: A single dataframe (which can be stacked, if more data files
  #               of the same type are read. 
  
  
  # The format of the individual data parts in a flurznrc lst-file is shown below.
  # This is clearly not a very easy format to work with, and it is unknown
  # if other types of output could produce other variations. The starting point
  # is the \f-starting character. We then test for the occurance of a
  # Depth coordinate line and a radial coordinate line 3 and 4 lines after the 
  # starting line, respectively.  
  #\f
  #mcclan-10450-thin-slab                                                          
  #Wed Jul 30 14:23:44 2014
  #
  #          Depth coordinates:    0.0000 TO   0.1000 cm  REGION =  2
  #
  #                Radial coordinates:    0.0000 TO  10.0000 cm
  #
  #                  Total fluence/(MeV)/unit incident fluence
  #
  #    Bintop      electrons        photons         positrons        e(-) + e(+)
  #    ------      ---------        -------         ---------        -----------
  #    0.0010  1.689E+00+- 0.0% 0.000E+00+-99.9% 0.000E+00+-99.9% 1.689E+00+- 0.0%
  #    0.0020  1.083E+00+- 0.0% 0.000E+00+-99.9% 0.000E+00+-99.9% 1.083E+00+- 0.0%
  #    0.0030  8.652E-01+- 0.0% 0.000E+00+-99.9% 0.000E+00+-99.9% 8.652E-01+- 0.0%
  #
  #    .....
  #
  #    0.9990  0.000E+00+-99.9% 0.000E+00+-99.9% 0.000E+00+-99.9% 0.000E+00+-99.9%
  #    1.0000--0.000E+00+-99.9%-0.000E+00+-99.9%-0.000E+00+-99.9%-0.000E+00+-99.9%
  # ---------  ---------------- ---------------- ---------------- ----------------
  #    Totals  1.297E+00+- 0.0% 2.032E-02+- 0.4% 0.000E+00+-99.9% 1.297E+00+- 0.0%
  #     Avg E  7.696E-01+- 0.0% 1.147E-01+- 0.5% 0.000E+00+- NaN% 7.696E-01+- 0.0%
  
  
  # Sample call:
  #   df <- read.flurznrc.lst(file.path="C://data//projects//Theoretical-Dosimetry",
  #   file.name="mcclan-10450-thin-slab.egslst",
  #   file.name.save="mcclan-10450-thin-slab",
  #   read.vec=c(1:3,8),
  #   plot.wanted=FALSE,
  #   trace=FALSE)
  #
  #   xyplot(electrons~Bintop|paste("plt.no",plt.no),data=df)
  #   xyplot(photons~Bintop|paste("plt.no",plt.no),data=df)
  #   xyplot(electrons~Bintop|txt.legend,groups=paste("plt.no",plt.no),data=df,auto.key=list(TRUE,columns=4))
  #   xyplot(photons~Bintop|txt.legend,groups=paste("plt.no",plt.no),data=df,auto.key=list(TRUE,columns=4))
  
  print("read.flurznrc.lst")
  df <- NULL
  file.full.path <- paste(file.path,file.name,sep='//')
  file.full.path
  
  # Read file first
  file.txt <- scan(file.full.path, what="", multi.line=FALSE, sep="\n")
  line.index  <- 1:length(file.txt)
  
  # All lines containing \f
  ee <- "\\\\f"
  ii1 <- grep(ee, file.txt, value=FALSE)
  
  # All lines containing Depth coordinates: 
  # We subtract 3, because for the real header, it should appear as line 3 
  ee <- '[[:space:]]+Depth coordinates:[[:space:]]'
  ii2 <- grep(ee, file.txt, value=FALSE) - 3 
  
  
  # All lines containing Radial coordinates:
  # We subtract 3, because for the real header, it should appear as line 4 
  ee <- '[[:space:]]+Radial coordinates:[[:space:]]'
  ii3 <- grep(ee, file.txt, value=FALSE) - 4
  
  # All lines containing Totals:
  ee <- '[[:space:]]+Totals[[:space:]]+[+]*[-]*[0-9]+'
  ii4 <- grep(ee, file.txt, value=FALSE)
  
  # All lines containing Avg E:
  ee <- '[[:space:]]+Avg E[[:space:]]+[+]*[-]*[0-9]+'
  ii5 <- grep(ee, file.txt, value=FALSE) - 1
  
  header.starts <- intersect(intersect(ii1,ii2),ii3)
  data.starts <- header.starts+8
  data.stops <- intersect(ii4,ii5)-2
  
  for(i in 1:length(data.starts)){
    if(trace){
      print("Data, first five:")
      print(file.txt[data.starts[i]:(data.starts[i]+5)])
      print("last five:")
      print(file.txt[(data.stops[i]-5):(data.stops[i])])
    }}
  
  
  
  file.txt[data.starts]
 
  
  for(iii in 1:length(header.starts)){
    # Grand loop
    if(is.element(iii,read.vec)){
      i <- header.starts[iii]
      txt.depth  <- trim.whitespace(file.txt[i+3])
      txt.radial <- trim.whitespace(file.txt[i+4])
      txt.legend <- trim.whitespace(file.txt[i+5])
      
      if(trace){
        print("Header:")
        print(i)
        print(file.txt[i:(i+5)])
        print(txt.depth)
        print(txt.radial)
        print(txt.legend)
      }
      
      txt <- txt.depth
      txt.x <- NA
      ee <- 'Depth coordinates:[[:space:]]*[[:space:]]*[+]*[-]*[0-9]+[.]?[0-9]*'
      ok <- length(y <- grep(ee, txt, value=FALSE)) > 0  
      if(ok){
        ii  <- regexpr(ee, txt)
        txt.x <- substring(txt,ii,ii+attr(ii,"match.length"))
        txt.x <- extract.given.number(txt.x,1)
      }  
      txt.depth.start <- txt.x
      
      txt <- txt.depth
      txt.x <- NA
      ee <- 'TO[[:space:]]*[[:space:]]*[+]*[-]*[0-9]+[.]?[0-9]*'
      ok <- length(y <- grep(ee, txt, value=FALSE)) > 0  
      if(ok){
        ii  <- regexpr(ee, txt)
        txt.x <- substring(txt,ii,ii+attr(ii,"match.length"))
        txt.x <- extract.given.number(txt.x,1)
      }  
      txt.depth.stop <- txt.x
      
      txt <- txt.depth
      txt.x <- NA
      ee <- 'REGION =[[:space:]]*[[:space:]]*[+]*[-]*[0-9]+[.]?[0-9]*'
      ok <- length(y <- grep(ee, txt, value=FALSE)) > 0  
      if(ok){
        ii  <- regexpr(ee, txt)
        txt.x <- substring(txt,ii,ii+attr(ii,"match.length"))
        txt.x <- extract.given.number(txt.x,1)
      }  
      txt.region <- txt.x
      
      txt <- txt.radial
      txt.x <- NA
      ee <- 'Radial coordinates:[[:space:]]*[[:space:]]*[+]*[-]*[0-9]+[.]?[0-9]*'
      ok <- length(y <- grep(ee, txt, value=FALSE)) > 0  
      if(ok){
        ii  <- regexpr(ee, txt)
        txt.x <- substring(txt,ii,ii+attr(ii,"match.length"))
        txt.x <- extract.given.number(txt.x,1)
      }  
      txt.radial.start <- txt.x
      
      txt <- txt.radial
      txt.x <- NA
      ee <- 'TO[[:space:]]*[[:space:]]*[+]*[-]*[0-9]+[.]?[0-9]*'
      ok <- length(y <- grep(ee, txt, value=FALSE)) > 0  
      if(ok){
        ii  <- regexpr(ee, txt)
        txt.x <- substring(txt,ii,ii+attr(ii,"match.length"))
        txt.x <- extract.given.number(txt.x,1)
      }  
      txt.radial.stop <- txt.x
      
      
      txt.columns <- file.txt[i+6]
      txt <- txt.columns
      xx <- strsplit(txt,"  ")[[1]]
      ok <- !xx==""
      txt.columns <- xx[ok]
      N.columns <- length(txt.columns)
      
      if(trace){
        print(txt.columns)
        print(N.columns)
        print("Data, first five:")
        print(file.txt[data.starts[iii]:(data.starts[iii]+5)])
        print("last five:")
        print(file.txt[(data.stops[iii]-5):(data.stops[iii])])
      }
      
      txt.columns <- gsub(" ",".",trim.whitespace(txt.columns))
      xxx <- rep(txt.columns[-1],each=2)
      yyy <- rep(c("","u.pct."),length(txt.columns[-1]))
      txt.columns.final <- c(txt.columns[1],paste(yyy,xxx,sep=""))
      
      # Clean up the data part (remove -- +/- and %)
      txt <- file.txt[data.starts[iii]:(data.stops[iii])]
      txt <- gsub("--", "  ", txt)
      txt <- gsub("\\+-", "  ", txt)
      txt <- gsub("%-", "  ", txt)
      txt <- gsub("%", " ", txt)
      
      df0 <-  read.table(text=txt,sep="")
      names(df0)<- txt.columns.final
      if(!file.name.save==""){
        fn <- paste(file.name.save,'-flurznrc-spectrum-',leading.zeros(iii,4),".txt",sep="")
        write.table(df0,fn,quote=TRUE,row.names=FALSE,col.names=txt.columns.final)
        print(paste("Saved file:",fn))
      }
      
      header.list <- list(region=txt.region,
                          depth.start=txt.depth.start, 
                          depth.stop=txt.depth.stop,
                          radial.start=txt.radial.start,
                          radial.stop=txt.radial.stop,
                          N.columns=N.columns,
                          txt.columns=txt.columns,
                          txt.columns.final=txt.columns.final,
                          txt.depth=txt.depth,  
                          txt.radial=txt.radial, 
                          txt.legend=txt.legend,
                          df0=df0[1:10,])
      #print(header.list)
      df0 <- data.frame(plt.no=iii,
                        df0,
                        region=txt.region,                    
                        depth.start=txt.depth.start, 
                        depth.stop=txt.depth.stop,
                        radial.start=txt.radial.start,
                        radial.stop=txt.radial.stop,
                        N.columns=N.columns,
                        txt.depth=txt.depth,  
                        txt.radial=txt.radial, 
                        txt.legend=txt.legend,
                        file.name=file.name)
      if(is.null(df)){df <- df0} else {df <- rbind(df,df0)}
    }# Test if it is in the read.vec
  }# Grand loop
  df
}# End read.egsnrc.lst
