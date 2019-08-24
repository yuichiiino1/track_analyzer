version<-"track_analyzer1.3.r"


library(gWidgets)
options("guiToolkit"="RGtk2")
workingdir <- getwd()
datadir <- paste(workingdir, '\\data', sep="")


# Functions for showing commands.
# usage: command(func1)
co <- c()
va <- c()
# command
command<-function(a){
query <- paste(match.call())[-1]
if(length(query)==0 || query=="all"){
  cat("--------------------------------------")
  cat(unlist(co[names(co)!="all"]),sep="--------------------------------------")
  cat("--------------------------------------\n")
}
else if(query=="list") print(names(co)[names(co)!="all"
& names(co)!="o" & names(co)!="oo" & names(co)!="ooo" & names(co)!="oooo" & names(co)!="ooooo" & names(co)!="oooooo"])
else cat(sub("\n","",co[[query]]))
}
com<-function(a){
query <- paste(match.call())[-1]
if(length(query)==0 || query=="all"){
  cat("--------------------------------------")
  cat(unlist(co[names(co)!="all"]),sep="--------------------------------------")
  cat("--------------------------------------\n")
}
else if(query=="list") print(names(co)[names(co)!="all"
& names(co)!="o" & names(co)!="oo" & names(co)!="ooo" & names(co)!="oooo" & names(co)!="ooooo" & names(co)!="oooooo"])
else cat(sub("\n","",co[[query]]))
}


# variable
variable<-function(a){
query <- paste(match.call())[-1]
cat(sub("\n","",va$all))
}


va$all<-''

###########################
#  Parameter settings
###########################


##############################################################################
################                           ###################################
################    GUI Window display     ###################################
################                           ###################################
##############################################################################

basiccommandgroup1 <- c("refresh","adjust.position","calc.dL","findPir","calc.PirRun","calc.TurnRate")
basiccommandgroup2 <- c("findPirA","calc.PirRunA","findShortTurn")

mainWindow <- function(){
  window <- gwindow(title=version, width = 800, height= 600)
  maingroup <- ggroup(container=window, horizontal=TRUE, spacing = 10)
  
  group1 <- ggroup(container=maingroup, horizontal=FALSE)
  w1 <- 10
  tbl <<- glayout(container=group1, spacing=3)
  #tbl[1,1] <- glabel("workingdir",container=tbl)
  #tbl[1,2] <- gedit("./data/",container=tbl,width=w1) #workingdir
  tbl[2,1] <- glabel("MinMove",container=tbl)
  tbl[2,2] <- gedit(1,container=tbl, coerce.with=as.numeric,width=w1,handler=refresh) # MinMove
  tbl[3,1] <- glabel("gauge",container=tbl)
  tbl[3,2] <- gedit(0.3,container=tbl, coerce.with=as.numeric, width=w1,handler=refresh) # gauge
  tbl[4,1] <- glabel("gauge2",container=tbl)
  tbl[4,2] <- gedit(1,container=tbl, coerce.with=as.numeric,width=w1,handler=refresh) # gauge2
  tbl[5,1] <- glabel("gauge3",container=tbl)
  tbl[5,2] <- gedit(0.1,container=tbl, coerce.with=as.numeric,width=w1,handler=refresh) # gauge3
  tbl[6,1] <- glabel("pirangle",container=tbl)
  tbl[6,2] <- gedit(80,container=tbl, coerce.with=as.numeric,width=w1,handler=refresh) # pirangle
  tbl[7,1] <- glabel("Tcrit",container=tbl)
  tbl[7,2] <- gedit(3.18,container=tbl, coerce.with=as.numeric ,width=w1,handler=refresh) # Tcrit
  tbl[8,1] <- glabel("maxdist",container=tbl)
  tbl[8,2] <- gedit(60,container=tbl, coerce.with=as.numeric ,width=w1,handler=refresh) # maxdist
  tbl[9,1] <- glabel("C0",container=tbl)
  tbl[9,2] <- gedit(0.2,container=tbl, coerce.with=as.numeric ,width=w1,handler=refresh) # C0
  tbl[10,1] <- glabel("plate_thickness",container=tbl)
  tbl[10,2] <- gedit(0.18,container=tbl, coerce.with=as.numeric,handler=refresh ,width=w1) # plate_thickness
  tbl[11,1] <- glabel("DiffusionConst",container=tbl)
  tbl[11,2] <- gedit(0.000015,container=tbl, coerce.with=as.numeric,handler=refresh ,width=w1) # DiffusionConst
  tbl[12,1] <- glabel("source",container=tbl)
  tbl[12,2] <- gcombobox(c("none","salt","odor"),selected=2,container=tbl,handler=refresh) #source
  tbl[13,1] <- glabel("gradient",container=tbl)
  tbl[13,2] <- gcombobox(c("geometrical","numerical"),selected=2,container=tbl,handler=refresh) #gradient
  tbl[14,1] <- glabel("plate_format",container=tbl)
  tbl[14,2] <- gcombobox(c("none","kunitomo","center-peak","2spot","12point","2odornew","1point"),selected=2,container=tbl,handler=refresh) #plate_format
  tbl[15,1] <- glabel("datatype",container=tbl)
  tbl[15,2] <- gcombobox(c("multi","single","none"),selected=1,container=tbl,editable=TRUE,handler=refresh) #datatype
  tbl[16,1] <- glabel("mark_left_to_right",container=tbl)
  tbl[16,2] <- gcombobox(c("TRUE","FALSE"),selected=1,container=tbl, coerce.with=as.logical,editable=TRUE,handler=refresh) #mark_left_to_right
  #gbutton("Choose contour data folder", container=group1, handler=contourdatafolderChoose)
  #contourdatafoldername <<- gedit(container=group1, width = w1)
  #contourdatafoldercontent <<- gtext(container=group1)
  
  w2 <- 40
  group2 <- ggroup(container=maingroup, horizontal=FALSE)
  gbutton("Change working folder", container=group2, handler=workfolderChoose)
  workfoldername <<- gedit(workingdir, container=group2, width = w2)
  loadcontour <<- gcheckboxgroup(c("Load contour data from files"), container=group2, handler=check.loadcontour)
  gbutton("Choose data folder", container=group2, handler=datafolderChoose)
  datafoldername <<- gedit(container=group2, width = w2)
  subfolder <<- gcheckboxgroup(c("Data in subfolders"), container=group2, handler=check.subfolder)
  group21 <- ggroup(container=group2, horizontal=TRUE)
  glabel("Common data file name",container=group21)
  data.file.name <<- gedit("not applicable", container=group21)
  group22 <- ggroup(container=group2, horizontal=TRUE)
  glabel("Common contour file name",container=group22)
  contour.file.name <<- gedit("not applicable", container=group22)
  datafoldercontent <<- gtext(container=group2)
  gbutton("Read selected files",container=group2, handler=read.files)
  group21 <- ggroup(container=group2, horizontal=T, spacing=15)
  basiccommands1 <<- gcheckboxgroup(basiccommandgroup1,checked = T, container=group21)
  basiccommands2 <<- gcheckboxgroup(basiccommandgroup2,checked = F, container=group21)
  gbutton("Execute selected commands", container=group2, handler=routine)
  gmessagebox <<- gtext(container=group2)
  glabel("",container=group2)
  glabel("optional command",container=group2)
  gbutton("Read diffusion simulation data", container=group2, handler=read.C_window)
  
  group3 <- ggroup(container=maingroup, horizontal=F)
  glabel("Optional commands",container=group3)
  gbutton("calc.C", container=group3, handler=function_window, action="calc.C")
  gbutton("calc.Bearing", container=group3, handler=function_window, action="calc.Bearing")
  gbutton("before.after", container=group3, handler=function_window, action="before.after")
  gbutton("calc.speed", container=group3, handler=function_window, action="calc.speed")
  gbutton("classify", container=group3, handler=function_window, action="classify")
  gbutton("plota", container=group3, handler=function_window, action="plota")
  gbutton("plotxysimple", container=group3, handler=function_window, action="plotxysimple")
  gbutton("multiplotxy", container=group3, handler=function_window, action="multiplotxy")
  gbutton("movieplotxy", container=group3, handler=function_window, action="movieplotxy")
  gbutton("plotxy", container=group3, handler=function_window, action="plotxy")
  gbutton("plottx", container=group3, handler=function_window, action="plottx")
  gbutton("plotaroundpir", container=group3, handler=function_window, action="plotaroundpir")
  gbutton("plotxyhist", container=group3, handler=function_window, action="plotxyhist")

  group4 <- ggroup(container=maingroup, horizontal=F)
  glabel("",container=group4)
  glabel("",container=group4)
  gbutton("weathervane.graph", container=group4, handler=function_window, action="weathervane.graph")
  gbutton("pirouette.graph", container=group4, handler=function_window, action="pirouette.graph")
  gbutton("plotTRB", container=group4, handler=function_window, action="plotTRB")
  gbutton("plotTRAT", container=group4, handler=function_window, action="plotTRAT")
  gbutton("plot.PirdCdT", container=group4, handler=function_window, action="plot.PirdCdT")
  gbutton("plot.TRdCdLat", container=group4, handler=function_window, action="plot.TRdCdLat")
  gbutton("plot.PirCdCdT", container=group4, handler=function_window, action="plot.PirCdCdT")
  gbutton("plot.PirTdCdT", container=group4, handler=function_window, action="plot.PirTdCdT")
  gbutton("plot.PirTC", container=group4, handler=function_window, action="plot.PirTC")
  gbutton("plot.TRCdCdLat", container=group4, handler=function_window, action="plot.TRCdCdLat")
  gbutton("plot.TRTdCdLat", container=group4, handler=function_window, action="plot.TRTdCdLat")
  gbutton("chemotaxis.index", container=group4, handler=function_window, action="chemotaxis.index")
  glabel("", container=group4)
  glabel("", container=group4)
  glabel("", container=group4)
  glabel("", container=group4)
  glabel("", container=group4)
  glabel("", container=group4)
  gbutton("Export data", container=group4, handler=export_data_window)
  gbutton("Save all data", container=group4, handler=save_image_window)
  
}

check.subfolder <- function(h,...){
  if(length(svalue(subfolder))!=0){
    svalue(data.file.name) <- "dustless-result.txt"
    if(length(svalue(loadcontour))!=0){
      svalue(contour.file.name) <- "contour.data"
    }
  }else{
    svalue(data.file.name) <- "not applicable"
    if(length(svalue(loadcontour))!=0){
      svalue(contour.file.name) <- "not applicable"
    }
  }
  setdf()
}

check.loadcontour <- function(h,...){
  if(length(svalue(loadcontour))!=0 && length(svalue(subfolder))!=0){
    svalue(contour.file.name) <- "contour.data"
  }else{
    svalue(contour.file.name) <- "not applicable"
  }
  setdf()
}

workfolderChoose <- function(h,...){
  gfile(text="Select working folder...", type="selectdir", 
        # , initialfilename = 'E:\\data'
        action = "setworkfolder",
        handler =
          function(h,...) {
            do.call(h$action, list(h$file))
          })
}

setworkfolder <- function(f){
  setwd(f)
  svalue(workfoldername) <- f[1]
}

datafolderChoose <- function(h,...){
  gfile(text="Select data folder...", type="selectdir", 
        # , initialfilename = 'E:\\data'
        action = "setdatafolder",
        handler =
          function(h,...) {
            do.call(h$action, list(h$file))
          })
}


setdatafolder <- function(f){
  #cat("class f =",class(f),"length f = ",length(f),"\n")
  #print(f)
  svalue(datafoldername) <- f
  workingdir <<- getwd()
  datadir <<- paste(f,"\\",sep="")
  setdf()
}

setdf <- function(){
  tempfiles <- list.files(datadir)
  dispose(datafoldercontent)
  if(length(svalue(subfolder))>0){
    for(fil in tempfiles){
    #cat(paste0(datadir,fil,"\\","\n"))
      if(file.info(paste0(datadir,fil))$isdir){
        insert(datafoldercontent, fil)
      }
    }
  }else{
    for(file.i in which(regexpr("\\.txt$", tempfiles)>0)){
      insert(datafoldercontent, tempfiles[file.i])
    }
    if(length(svalue(loadcontour))!=0){
    for(file.i in which(regexpr("\\.data$", tempfiles)>0)){
      insert(datafoldercontent, tempfiles[file.i])
    }
    }
  }
  focus(datafoldercontent)
}


# ：read.files
co$read.files<-'
read.files(file.numbers=1:length(files), datatype="multi"/"single"/"processed")
'
read.files <- function(h,...){ #file.numbers=1:length(files), datatype){

  tempfiles <- strsplit(svalue(datafoldercontent, drop=TRUE)[[1]],"\n")[[1]]
  if(length(tempfiles)==0){
    tempfiles <- strsplit(svalue(datafoldercontent, drop=FALSE)[[1]],"\n")[[1]]
  }
  tempfiles[tempfiles == ""] <- NULL
  
  if(length(svalue(subfolder))>0){
    files <<- tempfiles
  }else{
    files <<- c()
    for(fil in tempfiles){
      if(regexpr("\\.txt$", fil)>0){
        files <<- c(files, substr(fil,1,nchar(fil)-4))
      }
      if(length(svalue(loadcontour))!=0){
      if(regexpr("\\.data$", fil)>0){
        files <<- c(files, substr(fil,1,nchar(fil)-5))
      }
      }
    }
    files <<- unique(files)
  }
  
  file.numbers <- 1:length(files)
  cat("Reading following files\n")
  for(file.i in  file.numbers){
  	cat(files[file.i],"\n")
  }
  track.n <<- 0
  point.n <<- c()
  dT <<- list()
  dX <<- list()
  dY <<- list()
  markpoints <<- list()
  mags <<- list()
  origfile <<- c()
  cat("Reading file no:\n")
  oldtrack <- 1
  plate.track <<- list()
  Contour <<- list()
  
  if(datatype == "multi")
  {
    track.i <- 0
    for (file.i in file.numbers)
    {
      cat(file.i,"\n")
      
      if(length(svalue(subfolder))>0){
        pathfilename <- paste0(datadir,files[file.i],"\\",svalue(data.file.name))
      }else{
        pathfilename <- paste0(datadir,files[file.i],".txt")
      }
      if(!file.exists(pathfilename)){
        cat("Error: No file named",pathfilename,"was found.\n")
        return()
      }
      cat("Reading from", pathfilename,"\n")
      d0<<-read.table(pathfilename, header=F, comment.char=c("/"), fill=T, colClasses=c("character","numeric","numeric"))
      
      #5.1  Read Markpoints
      lines <- readLines(pathfilename, n=10)
      if(length(grep("Magnification",lines))==0){cat("\nNo description of Magnification in file and unable to execute.\n"); return()}
      mag<-as.numeric(strsplit(lines[grep("Magnification",lines)],": ")[[1]][2])
      mags <<- c(mags, list(mag))
      if(length(grep("Markpoints",lines))==0){
        markpoints <<- c(markpoints,list())
      }else{
        coord<-strsplit(lines[grep("Markpoints",lines)],": ")[[1]][2]
        coords<-strsplit(coord,";")[[1]]
        coordinates<-strsplit(coords,",")
        n<-length(coordinates)
        markpoint<-data.frame(x=I(sapply(1:n, function(i) as.numeric(coordinates[[i]][1])*mag)),
                              y=I(sapply(1:n, function(i) as.numeric(coordinates[[i]][2])*mag)))
        markpoints<<-c(markpoints,list(markpoint))
      }
      
      # Read data
      names(d0)<<-c("T","X","Y")
      bound <<- grep(">>",d0$T)
      bound <<- c(bound, length(d0$T)+1)
      for(itrack in 1:(length(bound)-1))
      {
        track.i <- track.i + 1
        dT <<- c(dT, list(as.numeric(d0$T[(bound[itrack]+1):(bound[itrack+1]-1)])))
        dX <<- c(dX, list(d0$X[(bound[itrack]+1):(bound[itrack+1]-1)]))
        dY <<- c(dY, list(d0$Y[(bound[itrack]+1):(bound[itrack+1]-1)]))
        point.n[track.i] <<- bound[itrack+1]-bound[itrack]-1
        origfile[track.i] <<- file.i
      }
      plate.track <<- c(plate.track, list(oldtrack:track.i))
      oldtrack <- track.i + 1
      
      #######  Coutour file Read in  ############
      
      if(length(svalue(loadcontour))!=0){
      
        # read contour.data file and return contour.plate
        # contour.plate is a list of list(each t) of list(origin.x, origin.y, total, contour)(each worm)

        #
        if(length(svalue(subfolder))>0){
          contourpathfilename <- paste0(datadir,files[file.i],"\\",svalue(contour.file.name))
        }else{
          contourpathfilename <- paste0(datadir,files[file.i],".data")
        }
        if(!file.exists(contourpathfilename)){
          cat("Error: No file named",contourpathfilename,"was found.\n")
          return()
        }
        
        ## read contour file ###
        con <- file(contourpathfilename, "rb")
        contour.plate <- c()
        time <- 1
        sumsize <- 0
        cat("Reading from",contourpathfilename,"\n")
        if(show.message) cat(".",sep="")
        
        repeat{ # time loop
          if(show.message && time%%50==0) cat(".",sep="")
          endoffile <- F
          contour.t <- c()
          contour.centroid.t <- c()
          repeat{ # worm loop
            origin.x <- readBin(con, "integer", n = 1L, size = NA_integer_, signed = TRUE,
                                endian = .Platform$endian)
            if(length(origin.x)==0){
              endoffile <- T
              break
            }
            origin.y <- readBin(con, "integer", n = 1L, size = NA_integer_, signed = TRUE,
                                endian = .Platform$endian)
            sumsize <- sumsize + 4
            if(origin.x == 0 && origin.y == 0){
              break
            }
            total <- readBin(con, "integer", n = 1L, size = NA_integer_, signed = TRUE)
            sumsize <- sumsize + 1
            contour.worm.raw <- raw(0)
            if(total > 0){
              contour.worm.raw <- readBin(con, raw(), total)
            }
            contour.tw <- list(origin.x, origin.y, total, contour.worm.raw)
            contour.t <- c(contour.t, list(contour.tw))
          } # end worm loop
          if(endoffile) break
          contour.plate <- c(contour.plate, list(contour.t))
          time <- time + 1
        } # end time loop
        if(show.message) cat("\n")
        close(con)
        
        if(show.message){
          cat(time-1, "time points\n", sumsize, "bytes\n")
        }
        # list by time of list by worm
      Contour <<- c(Contour, list(contour.plate))
      }
    }
    track.n <<- track.i
  }
  
  if(datatype == "single")
  {
    track.i <- 0
    for(file.i in file.numbers)
    {
      cat(paste(" ",file.i,sep=""))
      pathfilename <- paste0(datadir,files[file.i])
      track.i <- track.i + 1
      d0<<-read.table(pathfilename, header=T, skip=2, fill=T, colClasses="numeric")
      dT <<- c(dT, list(d0$Time))
      dX <<- c(dX, list(d0$StgX+d0$ScrX))
      dY <<- c(dY, list(d0$StgY+d0$ScrY))
      point.n[track.i] <<- length(d0$Time)
      origfile[track.i] <<- file.i
      plate.track <<- c(plate.track, list(oldtrack:track.i))
      oldtrack <- track.i + 1
    }
    track.n <<- track.i
  }
  
  #5.1
  if(datatype == "processed")
  {
    track.i <- 0
    dV <<- list()
    Theta <<- list()
    dC <<- list()
    dCdT <<- list()
    TurnStart <<- list()
    TurnEnd <<- list()
    PirStart <<- list()
    PirEnd <<- list()
    PirRun <<- list()
    AvTheta <<- list()
    TurnRate <<- list()
    Bearing <<- list()
    dCdX <<- list()
    dCdY <<- list()
    dCdLat <<- list()
    for(file.i in file.numbers)
    {
      cat(paste(" ",file.i,sep=""))
      track.i <- track.i + 1
      d0<<-read.table(paste(datadir,files[file.i],sep=""), header=T, skip=2, fill=T, colClasses=c(rep("numeric",3), rep(NA,20)))
      dT <<- c(dT, list(d0$T))
      dX <<- c(dX, list(d0$X))
      dY <<- c(dY, list(d0$Y))
      dV <<- c(dV, list(d0$V))
      Theta <<- c(Theta, list(c(NA,d0$Theta[2:length(d0$Theta)])))
      dC <<- c(dC, list(d0$C))
      dCdT <<- c(dCdT, list(d0$dC.dT))
      TurnStart <<- c(TurnStart, list(d0$TurnStart=="*"))
      TurnEnd <<- c(TurnEnd, list(d0$TurnEnd=="*"))
      PirStart <<- c(PirStart, list(d0$PirStart=="*"))
      PirEnd <<- c(PirEnd, list(d0$PirEnd=="*"))
      PirRun <<- c(PirRun, list(d0$PirRun))
      temp <- d0$AvTheta
      temp[temp==-500] <- NA
      AvTheta <<- c(AvTheta, list(temp))
      temp <- d0$TurnRate
      temp[temp==1000] <- NA
      TurnRate <<- c(TurnRate, list(temp))
      Bearing <<- c(Bearing, list(d0$Bearing))
      dCdX <<- c(dCdX, list(d0$dCdX))
      dCdY <<- c(dCdY, list(d0$dCdY))
      temp <- d0$dCdLat
      temp[temp==-10000] <- NA
      dCdLat <<- c(dCdLat, list(temp))
      
      point.n[track.i] <<- length(d0$T)
      origfile[track.i] <<- file.i
      plate.track <<- c(plate.track, list(oldtrack:track.i))
      oldtrack <- track.i + 1
    }
    track.n <<- track.i
  }
  plate.n <<- length(plate.track)
  maxT <<- max(unlist(dT))
  #font(datafoldercontent, weight=c("bold")) # does not work
  cat("\nDone.\n")
  insert(gmessagebox, paste(length(files),"files were loaded.\n"))
  focus(gmessagebox)
}



#routine
co$routine<-'
routine()
Batch execute
calc.dL()
findPir()
calc.PirRun()
calc.TurnRate()
'
routine<-function(h,...){
  coms1 <- svalue(basiccommands1)
  for(cmnd in basiccommandgroup1){
    if(length(grep(cmnd,coms1))>0){
      cat("\n>Performing ",cmnd,"()\n",sep="")
      insert(gmessagebox,paste("Performing ",cmnd,"...",sep=""))
      focus(gmessagebox)
      eval(parse(text=paste(cmnd,"()",sep="")))
    }
  }
  coms2 <- svalue(basiccommands2)
  for(cmnd in basiccommandgroup2){
    if(length(grep(cmnd,coms2))>0){
      cat("\n>Performing ",cmnd,"()\n",sep="")
      insert(gmessagebox,paste("Performing ",cmnd,"...",sep=""))
      focus(gmessagebox)
      eval(parse(text=paste(cmnd,"()",sep="")))
    }
  }
  cat("\nAll completed.\n")
  insert(gmessagebox,"All completed.")
  focus(gmessagebox)
}


### read.C window ###

read.C_window <- function(h,...){
  mainwindow <- gwindow(title="Read diffusion simulation data", width=30)
  maingroup <- ggroup(container=mainwindow, horizontal=F)
  glabel('Read Diffusion simulator output',container=maingroup)
  gbutton("Select diffusion data file", container=maingroup, handler=CfolderChoose)
  concdirname <<- gedit("", container=maingroup)
  tbl <<- glayout(container=maingroup, spacing=3)
  tbl[1,1] <- glabel("z",container=tbl)
  tbl[1,2] <- gedit(9,container=tbl,coerce=as.numeric)
  tbl[2,1] <- glabel("timestart",container=tbl)
  tbl[2,2] <- gedit(12960,container=tbl,coerce=as.numeric)
  tbl[3,1] <- glabel("timestep",container=tbl)
  tbl[3,2] <- gedit(12,container=tbl,coerce=as.numeric)
  tbl[4,1] <- glabel("time.n",container=tbl)
  tbl[4,2] <- gedit(60,container=tbl,coerce=as.numeric)
  tbl[5,1] <- glabel("simulator.unit",container=tbl)
  tbl[5,2] <- gedit(0.2,container=tbl,coerce=as.numeric)
  gbutton("Read from file", container=maingroup, handler=
    function(h,...) read.C(concdir, svalue(tbl[1,2]),svalue(tbl[2,2]),svalue(tbl[3,2]),svalue(tbl[4,2]),svalue(tbl[5,2]))
  )
  readCmessage <<- gtext("",container=maingroup)
}

CfolderChoose <- function(h,...){
  gfile(text="Select diffusion data folder...", type="selectdir", 
        action = "setCfolder",
        handler =
          function(h,...) {
            do.call(h$action, list(h$file))
          })
}

setCfolder <- function(f){
  concdir <<- f
  svalue(concdirname) <<- f
}


############ Export data window #######################

varnames <- list(T="dT",X="dX",Y="dY",V="dV",dL="dL",C="dC",dCdT="dCdT",dCdX="dCdX",dCdY="dCdY",dCdLat="dCdLat",
Theta="Theta",AvTheta="AvTheta",
Bearing="Bearing",Dist="Dist",TurnRate="TurnRate",PirRun="PirRun",TurnRun="TurnRun",TurnStart="TurnStart",
TurnEnd="TurnEnd",PirStart="PirStart",PirEnd="PirEnd",PirSurround="PirSurround",ShortTurn="ShortTurn",
ShortTurnStart="ShortTurnStart",TurnRunA="TurnRunA",PirRunA="PirRunA",PirSurroundA="PirSurroundA",
TurnStartA="TurnStartA",TurnEndA="TurnEndA",PirStartA="PirStartA",PirEndA="PirEndA")

export_data_window <- function(h,...){
  mainwindow <- gwindow(title="Export data", width=30)
  maingroup <- ggroup(container=mainwindow, horizontal=F)
  glabel("Select variables to be included", container=maingroup)
  selected_vars <<- gcheckboxgroup(c("plate_no","track_no",names(varnames)[sapply(varnames, exists)]),checked = c(rep(T,5),rep(F,length(names(varnames)[sapply(varnames, exists)])-3)), container=maingroup)
  
  gbutton("Select file to write", container=maingroup, handler=exportfolderChoose
  )
}

exportfolderChoose <- function(h,...){
  gfile(text="Select file to write...", type="save", initialfilename="mydata.csv", 
        action = "export_data",
        handler =
          function(h,...) {
            do.call(h$action, list(h$file))
          })
}

export_data <- function(f){
  svars <- svalue(selected_vars)
  track_no <- unlist(sapply(1:track.n, function(x) rep(x, point.n[x])))
  row_n <- length(track_no)
  plate_no <- c()
  for(i in 1:plate.n){
    plate_no <- c(plate_no, rep(i,sum(point.n[plate.track[[i]]])))
  }
  if(length(plate_no)!=row_n){
    cat("length of plate_no is different from others.\n")
    browser()
  }
  outdata <- c()
  row_n <- 0
  for(i in 1:length(svars)){
    if(svars[i] == "plate_no"){
      colname <- "plate_no"
      columndata <- plate_no
    } else if(svars[i] == "track_no"){
      colname <- "track_no"
      columndata <- track_no
    }else{
      colname <- eval(parse(text=paste("varnames$",svars[i],sep="")))
      columndata <- unlist(eval(parse(text=colname)))
    }
    if(row_n == 0){
      row_n == length(columndata)
    }else if(length(columndata)!=row_n){
      cat("length of",colname, "is different from others.\n")
    }
    if(length(outdata)==0){
      outdata <- data.frame(columndata)
    }else{
      outdata <- cbind(outdata, columndata)
    }
  }
  names(outdata) <- svars
  #print(svars)
  write.csv(outdata, file=f,row.names=F)
  insert(gmessagebox, paste("Wrote data to ",f,"\n",sep=""))
  cat("Wrote data to ",f,"\n",sep="")
}

##################### save all data window ###############

save_image_window <- function(h,...){
  gfile(text="Select file to save all data...", type="save", initialfilename="mydata.RData", 
        handler =
          function(h,...) {
            do.call(save.image, list(h$file))
            insert(gmessagebox, paste("Wrote data to ",h$file,"\n",sep=""))
            cat("Wrote data to ",h$file,"\n",sep="")
          })
}

#################### Function information for windows  ###################

function_info <- list(

calc.C=list(
'calc.C(type="12point"|"plug")'
,
c('type','"plug"')
)
,

calc.Bearing=list(
'calc.Bearing(plate_format, odordirection="closer", suppress.numerical=F)'
,
c('plate_format', '"kunitomo"', 'odordirection','"closer"', 'suppress.numerical','F')
)
,

before.after=list(
'before.after(tracks=1:track.n, division=12, timewindow=c(0,maxT))'
,
c('tracks','1:track.n', 'division','12', 'timewindow','c(0,maxT)')
)
,

calc.speed=list(
''
,
c('timespan','100', 'maxtime','maxT')
)
,

classify=list(
'generic
classify<-function(x=dT, y=dV, spacer1=NULL, spacer2=NULL, spacer3=NA, spacer4=NULL,
 xmin=0, xbin=100, xmax=maxT, plate=F, PirRunSelect="", timemin=0, timemax=maxT)'
,
c('x','dT','y','dV','spacer1','NULL','spacer2','NULL','spacer3','NA','spacer4','NULL','xmin','0','xbin','100','xmax','maxT','plate','F','PirRunSelect','""','timemin','0','timemax','maxT')
)
,

plota=list(
'plota(track.i,from=1, to=point.n[track.i], text="i", regression=F, xlim=c(), ylim=c(), cex=0, type="p", main="", Pircolor="green", Turncolor="red")'
,
c("track.i","1","from","1", "to","point.n[track.i]", "text","\"i\"", "regression","F", "xlim","c()", "ylim","c()", "cex","0", "type","\"p\"", "main","\"\"", "Pircolor","\"green\"", "Turncolor","\"red\"")
)
,
plotxysimple=list(
'plotxysimple(tracks=1:track.n, spanT=c(0,maxT))'
,
c("tracks","1:track.n","spanT","c(1,maxT)")
)
,

multiplotxy=list(
'multiplotxy(tracks=1:track.n, division.t=12, duration=NA, nrow=NA, ncol=NA, maxtime=NA,
      type="line"/"density", density.division=40, eachmax=T, suppress.dots=F, xlim=c(5,95), ylim=c(5,95),
             centerx=50, centery=50, radius=42.5, sampling=c(1,1))'
,
c('tracks','1:track.n','division.t','12','duration','NA','nrow','NA','ncol','NA','maxtime','NA','type','"line"','density.division','40','eachmax','T','cutoff.radius','NA','suppress.dots','F','xlim','c(5,95)','ylim','c(5,95)','centerx','50','centery','50','radius','42.5','sampling','c(1,1)')
)
,

movieplotxy=list(
'movieplotxy(tracks=1:track.n, division.t=12, duration=NA, maxtime=NA,
      type="line"/"density", density.division=40, eachmax=T, suppress.dots=F, xlim=c(5,95), ylim=c(5,95),
             centerx=50, centery=50, radius=42.5, sampling=c(1,1))'
,
c('tracks','1:track.n','division.t','12','duration','NA','maxtime','NA','type','"line"','density.division','40','eachmax','T','cutoff.radius','NA','suppress.dots','F','xlim','c(5,95)','ylim','c(5,95)','centerx','50','centery','50','radius','42.5','sampling','c(1,1)')
)
,

plotxy=list(
'plotxy(tracks=1:track.n, spanT=c(0,maxT), xlim=c(0,100), ylim=c(0,100))'
,
c('tracks','1:track.n','spanT','c(0,maxT)','xlim','c(0,100)','ylim','c(0,100)')
)
,

plottx=list(
'plottx(tracks=1:track.n, spanT=c(0,maxT))'
,
c('tracks','1:track.n','spanT','c(0,maxT)')
)
,

plotaroundpir=list(
'plotaroundpir(tracks=1:track.n, nfigs=1, save=TRUE)'
,
c('tracks','1:track.n','nfigs','1','save','TRUE')
)
,

plotxyhist=list(
''
,
c('tracks','1:track.n','division','20')
)
,

weathervane.graph=list(
'weathervane.graph(xfrom=-2.5,xto=2.5,xby=0.5,plates=1:plate.n, ylim=c(-40,40), Cfrom=NA, Cto=NA, Tfrom=NA, Tto=NA)'
,
c('xfrom','-2.5','xto','2.5','xby','0.5','plates','1:plate.n','ylim','c(-40,40)','Cfrom','NA','Cto','NA','Tfrom','NA','Tto','NA')
)
,

pirouette.graph=list(
'pirouette.graph(xfrom=-0.045,xto=0.045,xby=0.01,plates=1:plate.n, ylim=c(0,0.14), Cfrom=NA, Cto=NA, Tfrom=NA, Tto=NA, mincount=10)'
,
c('xfrom','-0.045','xto','0.045','xby','0.01','plates','1:plate.n','ylim','c(0,0.14)','Cfrom','NA','Cto','NA','Tfrom','NA','Tto','NA','mincount','10')
)
,

plotTRB=list(
'plotTRB(tracks=1:track.n, plates=1:plate.n, division=12, timewindow=c(), type="all")'
,
c('tracks','1:track.n','plates','1:plate.n','division','12','timewindow','c()','type','"all"')
)
,

plotTRAT=list(
'plotTRAT(tracks=1:track.n, plates=1:plate.n, division=12, timewindow=c(), type="all")'
,
c('tracks','1:track.n','plates','1:plate.n','division','12','timewindow','c()','type','"all"')
)
,

plot.PirdCdT=list(
'plot.PirdCdT(from=-0.1, to=0.1, by=0.02, Cfrom=NA, Cto=NA, ylim=NA)'
,
c('from','-0.1','to','0.1','by','0.02','Cfrom','NA','Cto','NA','ylim','NA')
)
,

plot.TRdCdLat=list(
'plot.TRdCdLat(from=-3, to=3, by=0.5, Cfrom=NA, Cto=NA, ylim=NA)'
,
c('from','-3','to','3','by','0.5','Cfrom','NA','Cto','NA','ylim','NA')
)
,

plot.PirCdCdT=list(
'plot.PirCdCdT(xfrom=30, xto=100, xby=2.5, yfrom=-0.6, yto=0.6, yby=0.025, maxprobab=0.1, cutoff=20, 
persp=FALSE, Tfrom=0, Tto=maxT, tracks=1:track.n, legend = TRUE)'
,
c('xfrom','30','xto','100','xby','2.5','yfrom','-0.6','yto','0.6','yby','0.025','maxprobab','0.1','cutoff','20','persp','FALSE','Tfrom','0','Tto','maxT','tracks','1:track.n','legend','TRUE')
)
,

plot.PirTdCdT=list(
'plot.PirTdCdT(xfrom=0, xto=NA, xby=NA, yfrom=-0.6, yto=0.6, yby=0.025, maxprobab=0.1, cutoff=20, persp=FALSE)'
,
c('xfrom','0','xto','NA','xby','NA','yfrom','-0.6','yto','0.6','yby','0.025','maxprobab','0.1','cutoff','20','persp','FALSE')
)
,

plot.PirTC=list(
'plot.PirTC(xfrom=0, xto=NA, xby=NA, yfrom=30, yto=100, yby=2.5, dCdTfrom=-0.6, 
     dCdTto=0.6, maxprobab=0.1, cutoff=20, persp=FALSE, tracks=1:track.n)'
,
c('xfrom','0','xto','NA','xby','NA','yfrom','30','yto','100','yby','2.5','dCdTfrom','-0.6','dCdTto','0.6','maxprobab','0.1','cutoff','20','persp','FALSE','tracks','1:track.n')
)
,

plot.TRdCdLat=list(
'plot.TRdCdLat(from=-3, to=3, by=0.5, Cfrom=NA, Cto=NA, ylim=NA)'
,
c('from','-3','to','3','by','0.5','Cfrom','NA','Cto','NA','ylim','NA')
)
,

plot.TRCdCdLat=list(
'plot.TRCdCdLat(xfrom=30, xto=100, xby=2.5, yfrom=-3, yto=3, yby=0.2, minTR=-20, maxTR=20, cutoff=5, Tfrom=0, Tto=maxT, tracks=1:track.n, legend=TRUE, legend = "Y")'
,
c('xfrom','30','xto','100','xby','2.5','yfrom','-3','yto','3','yby','0.2','minTR','-20','maxTR','20','cutoff','5','Tfrom','0','Tto','maxT','tracks','1:track.n','legend','TRUE')
)
,

plot.TRTdCdLat=list(
'plot.TRTdCdLat(xfrom=0, xto=NA, xby=NA, yfrom=-0.6, yto=0.6, yby=0.025, maxprobab=0.1, cutoff=20, persp=FALSE)'
,
c('xfrom','0','xto','NA','xby','NA','yfrom','-3','yto','3','yby','0.2','minTR','-20','maxTR','20','cutoff','5','Tfrom','0','Tto','maxT','tracks','1:track.n','persp','FALSE')
)
,

chemotaxis.index=list(
'chemotaxis.index(tpoints = seq(0,round(maxT/60)*60,by=120), plates=1:plate.n, left.low=T)'
,
c('tpoints','seq(0,round(maxT/60)*60,by=120)', 'plates','1:plate.n', 'left.low','TRUE')
)

)


###########################################################
#####################  Window  ####################

function_window <- function(h,...){
  mainwindow <- gwindow(title=h$action, width=30)
  maingroup <- ggroup(container=mainwindow, horizontal=F)
  contents <- function_info[[h$action]][[2]]
  items <- matrix(contents, ceiling(length(contents)/2), 2, byrow=T)
  nitems <- nrow(items)
  comment <- function_info[[h$action]][[1]]
  glabel(comment,container=maingroup)
  tbl <- glayout(container=maingroup, spacing=1)
  for(i in 1:nitems){
    tbl[i,1] <- glabel(items[i,1],container=tbl)
    tbl[i,2] <- gedit(items[i,2],container=tbl)
  }
  
  gbutton("Execute", container=maingroup,
    handler=eval(parse(text=paste("function(h,...) ",h$action,"(...,tbl=tbl)",sep="")))
  )
}


##########################  Main window display and execution    ######################

refresh<-function(h,...){
  read.tbl()
  if(plate_format=="2spot"){
    peak.positionX<<-c(19.46,19.46) # 50-sqrt(33^2-12.5^2)=19.46
    peak.positionY<<-c(37.5,37.5)
  }
  if(plate_format=="12point"){
  }
  if(plate_format=="kunitomo"){
    peak.positionX<<-c(20,80)
    peak.positionY<<-c(50,50)
  }
  if(plate_format=="center-peak"){
    peak.positionX<<-c(50)
    peak.positionY<<-c(50)
  }
}

read.tbl <- function(){
#workingdir <<- svalue(tbl[1,2]) # "./data/"
MinMove <<- svalue(tbl[2,2]) # 1
gauge <<- svalue(tbl[3,2]) # 0.3
gaugegauge <<- gauge*gauge
gauge2 <<- svalue(tbl[4,2]) # 1
gauge22 <<- gauge2/2.0
gauge3 <<- svalue(tbl[5,2]) # 0.1
gaugegauge3 <<- gauge3 * gauge3
pirangle <<- svalue(tbl[6,2]) # 80
cospirangle <<- cos(pirangle*pi/180)
atanpirangle <<- tan((90-pirangle)*pi/180)
Tcrit <<- svalue(tbl[7,2]) # 3.18
#maxT <<- 1500
maxdist <<- svalue(tbl[8,2]) # 60
C0 <<- svalue(tbl[9,2]) # 0.2
plate_thickness <<- svalue(tbl[10,2]) # 0.18
DiffusionConst <<- svalue(tbl[11,2]) # 0.000015
source <<- svalue(tbl[12,2]) # "salt"
gradient <<- svalue(tbl[13,2]) # "geometrical"
plate_format <<- svalue(tbl[14,2]) # "kunitomo"
datatype <<- svalue(tbl[15,2]) # "multi"
mark_left_to_right <<- svalue(tbl[16,2]) # TRUE
}

mainWindow()

focus(workfoldername)
setdatafolder(datadir)

#workingdir <- svalue(tbl[1,2]) # "./data/"
MinMove <- svalue(tbl[2,2]) # 1
gauge <- svalue(tbl[3,2]) # 0.3
gaugegauge <- gauge*gauge
gauge2 <- svalue(tbl[4,2]) # 1
gauge22 <- gauge2/2.0
gauge3 <- svalue(tbl[5,2]) # 0.1
gaugegauge3 <- gauge3 * gauge3
pirangle <- svalue(tbl[6,2]) # 80
cospirangle <- cos(pirangle*pi/180)
atanpirangle <- tan((90-pirangle)*pi/180)
Tcrit <- svalue(tbl[7,2]) # 3.18
#maxT <- 1500
maxdist <- svalue(tbl[8,2]) # 60
C0 <- svalue(tbl[9,2]) # 0.2
plate_thickness <- svalue(tbl[10,2]) # 0.18
DiffusionConst <- svalue(tbl[11,2]) # 0.000015
source <- svalue(tbl[12,2]) # "salt"
gradient <- svalue(tbl[13,2]) # "geometrical"
plate_format <- svalue(tbl[14,2]) # "kunitomo"
datatype <- svalue(tbl[15,2]) # "multi"
mark_left_to_right <- svalue(tbl[16,2]) # TRUE



#######################################################
#################                              ########
co$o<-'\n########     File inputs reated     ########
'################                              ########
#######################################################

#read.spots
co$read.spots<-'
read.spots(filename)
'
read.spots <- function(filename){
  spot<<-read.table(paste("./",filename,sep=""), sep="\t", header=F, fill=T, colClasses=c("character","numeric","numeric","numeric","numeric"))
  names(spot)<<-c("file","X1","Y1","X2","Y2")
}

#read.C
co$read.C<-'
read.C(concdir="conc", z=9, timestart=12960, timestep=12, time.n=60, simulator.unit=0.2)
'
read.C<-function(concdir="conc", z=9, timestart=12960, timestep=12, time.n=60, simulator.unit=0.2){
  cat("reading data...\n")
  insert(readCmessage, "Reading...")
  focus(readCmessage)
  for(i in 0:time.n){
    Cxy<-read.csv(paste(concdir,"/time",timestart+i*timestep,"z",z,".txt",sep=""), header=F, comment.char=c(">"), fill=T, colClasses=c("numeric"))
    if(i==0){Cxyt<<-array(NA,c(dim(Cxy),time.n+1))}
    Cxyt[,,i+1]<<-as.matrix(Cxy)
  }
  dCdXxyt<<-(Cxyt[-1,,]-Cxyt[-dim(Cxy)[1],,])/simulator.unit
  dCdYxyt<<-(Cxyt[,-1,]-Cxyt[,-dim(Cxy)[2],])/simulator.unit
  dCdTxyt<<-Cxyt[,,-1]-Cxyt[,,-(time.n+1)]
  simulator.unit <<- svalue(tbl[5,2])
  cat("read.C done.\n")
  dispose(readCmessage)
  insert(readCmessage, "Concentration data successfully loaded.")
  focus(readCmessage)
}

#######################################################
#################                           ##########
co$oo<-'\n########     Basic plots     ########
'################                           ##########
#######################################################



#plota
co$plota<-'
plota(track.i,from=1, to=point.n[track.i], text="i", regression=F, xlim=c(), ylim=c(), cex=0, type="p", main="", Pircolor="green", Turncolor="red")
'
plota <- function(track.i,from=1, to=point.n[track.i], text="i", regression=F, xlim=c(), ylim=c(), cex=0, type="p", main="", Pircolor="green", Turncolor="red",tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  maxX <- max(dX[[track.i]][from:to])
  minX <- min(dX[[track.i]][from:to])
  meanX <- mean(dX[[track.i]][from:to])
  midX <- (maxX+minX)/2
  maxY <- max(dY[[track.i]][from:to])
  minY <- min(dY[[track.i]][from:to])
  meanY <- mean(dY[[track.i]][from:to])
  midY <- (maxY+minY)/2
  span2 <- max(maxX-minX,maxY-minY)/2
  if(length(xlim) == 0) xlim <- c(midX-span2,midX+span2)
  if(length(ylim) == 0) ylim <- c(midY-span2,midY+span2)
  if(cex==0) cex<-1
  plot(dX[[track.i]][from:to],dY[[track.i]][from:to], xlab="X", ylab="Y", xlim=xlim, ylim=ylim, cex=cex, type=type, main=main)
  points(dX[[track.i]][from:to][PirRun[[track.i]][from:to]=="P"],dY[[track.i]][from:to][PirRun[[track.i]][from:to]=="P"],col=Pircolor)
  points(dX[[track.i]][from:to][TurnRun[[track.i]][from:to]=="T"],dY[[track.i]][from:to][TurnRun[[track.i]][from:to]=="T"],col=Turncolor)
    if(text=="i"){  
    text(dX[[track.i]][from:to],dY[[track.i]][from:to],from:to,pos=4,cex=0.5)
  }
  if(text=="TR"){
    text(dX[[track.i]][from:to],dY[[track.i]][from:to],TurnRate[[track.i]][from:to],pos=4,cex=0.5)
  }

  if(regression)
  {
  theta <<- testav(track.i,from,to)
  #print(theta)
  lines(c(meanX-span2,meanX+span2),c(meanY-span2*tan(theta/180*pi),meanY+span2*tan(theta/180*pi)))
  }
}
#plotxysimple
co$plotxysimple<-'
plotxysimple(tracks=1:track.n, spanT=c(0,maxT))
'
plotxysimple <- function(tracks=1:track.n, spanT=c(0,maxT), tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }

  plot(c(),c(),xlim=c(0,90), ylim=c(0,90), xlab="X", ylab="Y", asp=1)
    for(track.i in tracks){
      rangeT <- dT[[track.i]]>=spanT[1] & dT[[track.i]]<spanT[2]
      lines(dX[[track.i]][rangeT], dY[[track.i]][rangeT])
    }
}

#multiplotxy
co$multiplotxy<-'
multiplotxy(tracks=1:track.n, division.t=12, duration=NA, nrow=NA, ncol=NA, maxtime=NA,
      type="line"/"density", density.division=40, eachmax=T, suppress.dots=F, xlim=c(5,95), ylim=c(5,95),
             centerx=50, centery=50, radius=42.5, sampling=c(1,1))
'
multiplotxy<-function(tracks=1:track.n, division.t=12, duration=NA, nrow=NA, ncol=NA, maxtime=NA, 
             type="line", density.division=40, eachmax=T, cutoff.radius=NA, suppress.dots=F, xlim=c(5,95), ylim=c(5,95),
             centerx=50, centery=50, radius=42.5, sampling=c(1,1), tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  if(is.na(maxtime)) maxtime<-maxT
  div<-division.t
  if(type=="density")div<-division.t+1
  if(is.na(nrow)){
    if(is.na(ncol)) ncol<-ceiling(sqrt(div))
    nrow<-ceiling((div)/ncol)
  }else if(is.na(ncol)){
    ncol<-ceiling((div)/nrow)
  }
  par(mfrow=c(nrow, ncol), mai=c(0,0,0.2,0))
  if(is.na(duration)) duration<-ceiling(maxtime/division.t)
  if(type=="line"){
    for(i in 0:(division.t-1)){
      from <- maxtime/division.t*i
      to <- maxtime/division.t*i+duration
      plot(c(),c(),xlim=xlim, ylim=ylim, xlab="", ylab="", main=paste("T=",from,"-",to,sep="") , asp=1, axes=F)
      symbols(centerx,centery,circles=radius, bg=c(NA,"black","black"), inches=F, add=T)
      if(!suppress.dots){
      symbols(peak.positionX,peak.positionY,circles=rep(1,length(peak.positionX)), bg=c(NA,"black","black"), inches=F, add=T)
      }
      for(track.i in tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]){
        rangeT <- dT[[track.i]]>=from & dT[[track.i]]<to
        if(is.na(cutoff.radius)){lines(dX[[track.i]][rangeT], dY[[track.i]][rangeT])}
        else{
          cutoff2 <- cutoff.radius*cutoff.radius
          tempX <- dX[[track.i]][rangeT]
          tempY <- dY[[track.i]][rangeT]
          inside <- (tempX-50)*(tempX-50) + (tempY-50)*(tempY-50) < cutoff2
          lines(tempX[inside], tempY[inside])
        }
      }
    }
  }
  if(type=="density"){
    counts<-array(0,c(division.t,density.division,density.division))
    unit<-100/density.division
    unlistdT <- unlist(dT[tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]])
    unlistdX <- unlist(dX[tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]])
    unlistdY <- unlist(dY[tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]])
    for(point in 1:sum(point.n[tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]])){
      Tto <- floor(unlistdT[point]/maxtime*division.t)+1
      if(Tto>=division.t) Tto<-division.t
      Tfrom <- floor((unlistdT[point]-duration)/maxtime*division.t)+1
      if(Tfrom<1) Tfrom<-1
      if(Tfrom<=division.t){
        Xi <- floor(unlistdX[point]/unit)+1
        Yi <- floor(unlistdY[point]/unit)+1
        for(Ti in Tfrom:Tto){
          if(Xi>=1 && Xi<=density.division && Yi>=1 && Yi<=density.division) counts[Ti,Xi,Yi]<-counts[Ti,Xi,Yi]+1
        }
      }
    }
    if(eachmax){
      maxcount<-sapply(1:division.t, function(i) max(counts[i,,]))
    }else{
      temp<-max(counts)
      maxcount<-rep(temp,division.t)
    }
    for(i in 1:division.t){
      r2<-(radius/unit)^2
      from <- maxtime/division.t*(i-1)
      to <- maxtime/division.t*(i-1)+duration
      plot(c(),c(),xlim=xlim, ylim=ylim, xlab="", ylab="", main=paste("T=",from,"-",to,sep="") , asp=1, axes=F)
      for(Xi in 1:density.division){
      for(Yi in 1:density.division){
        if((Xi-(density.division+1)/2)^2+(Yi-(density.division+1)/2)^2<=r2){
          if(maxcount[i]==0) {rank <- 0 }else{
          rank<-floor(counts[i,Xi,Yi]/maxcount[i]*20)+1}
          if(rank>20)rank<-20
          symbols((Xi-0.5)*unit,(Yi-0.5)*unit,rectangle=cbind(unit,unit),bg=rev(rainbow(20,start=1.0,end=0.67))[rank],fg=rev(rainbow(20,start=1.0,end=0.67))[rank],inches=F,add=T)
        }
      }
      }
      symbols(centerx,centery,circles=radius, bg=c(NA,"black","black"), inches=F, add=T)
      if(!suppress.dots){
      symbols(peak.positionX,peak.positionY,circles=rep(1,length(peak.positionX)), bg=c(NA,"black","black"), inches=F, add=T)
      }
    }
    plot(c(),c(),xlim=c(5,95), ylim=c(5,95), xlab="", ylab="", main="" , asp=1, axes=F)
    legend(60, 90, c("maxcount",rep("",18),0), xjust=1, x.intersp=1, fill=rainbow(20,start=1.0,end=0.67), bty="n",y.intersp=0.5,border=rainbow(20,start=1.0,end=0.67),cex=1)
  }
  par(mfrow=c(1,1),mar=c(5,5,2,2))
}

#movieplotxy
co$movieplotxy<-'
movieplotxy(tracks=1:track.n, division.t=12, duration=NA, maxtime=NA,
      type="line"/"density", density.division=40, eachmax=T, suppress.dots=F, xlim=c(5,95), ylim=c(5,95),
             centerx=50, centery=50, radius=42.5, sampling=c(1,1))
'
movieplotxy<-function(tracks=1:track.n, division.t=12, duration=NA, maxtime=NA, 
             type="line", density.division=40, eachmax=T, cutoff.radius=NA, suppress.dots=F, xlim=c(5,95), ylim=c(5,95),
             centerx=50, centery=50, radius=42.5, sampling=c(1,1), tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  if(is.na(maxtime)) maxtime<-maxT
  div<-division.t
  if(type=="density")div<-division.t+1

  par(mfrow=c(1, 1), mai=c(0,0,0.2,0))
  if(is.na(duration)) duration<-ceiling(maxtime/division.t)
  if(type=="line"){
    for(i in 0:(division.t-1)){
      from <- maxtime/division.t*i
      to <- maxtime/division.t*i+duration
      plot(c(),c(),xlim=xlim, ylim=ylim, xlab="", ylab="", main=paste("T=",from,"-",to,sep="") , asp=1, axes=F)
      symbols(centerx,centery,circles=radius, bg=c(NA,"black","black"), inches=F, add=T)
      if(!suppress.dots){
      symbols(peak.positionX,peak.positionY,circles=rep(1,length(peak.positionX)), bg=c(NA,"black","black"), inches=F, add=T)
      }
      for(track.i in tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]){
        rangeT <- dT[[track.i]]>=from & dT[[track.i]]<to
        if(is.na(cutoff.radius)){lines(dX[[track.i]][rangeT], dY[[track.i]][rangeT])}
        else{
          cutoff2 <- cutoff.radius*cutoff.radius
          tempX <- dX[[track.i]][rangeT]
          tempY <- dY[[track.i]][rangeT]
          inside <- (tempX-50)*(tempX-50) + (tempY-50)*(tempY-50) < cutoff2
          lines(tempX[inside], tempY[inside])
        }
      }
      savePlot(paste("movie/plotxy",i,".tiff",sep=""),"tiff")
    }
  }
  if(type=="density"){
    counts<-array(0,c(division.t,density.division,density.division))
    unit<-100/density.division
    unlistdT <- unlist(dT[tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]])
    unlistdX <- unlist(dX[tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]])
    unlistdY <- unlist(dY[tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]])
    for(point in 1:sum(point.n[tracks[c(rep(TRUE,sampling[1]),rep(FALSE,sampling[2]-sampling[1]))]])){
      Tto <- floor(unlistdT[point]/maxtime*division.t)+1
      if(Tto>=division.t) Tto<-division.t
      Tfrom <- floor((unlistdT[point]-duration)/maxtime*division.t)+1
      if(Tfrom<1) Tfrom<-1
      if(Tfrom<=division.t){
        Xi <- floor(unlistdX[point]/unit)+1
        Yi <- floor(unlistdY[point]/unit)+1
        for(Ti in Tfrom:Tto){
          if(Xi>=1 && Xi<=density.division && Yi>=1 && Yi<=density.division) counts[Ti,Xi,Yi]<-counts[Ti,Xi,Yi]+1
        }
      }
    }
    if(eachmax){
      maxcount<-sapply(1:division.t, function(i) max(counts[i,,]))
    }else{
      temp<-max(counts)
      maxcount<-rep(temp,division.t)
    }
    for(i in 1:division.t){
      r2<-(radius/unit)^2
      from <- maxtime/division.t*(i-1)
      to <- maxtime/division.t*(i-1)+duration
      plot(c(),c(),xlim=xlim, ylim=ylim, xlab="", ylab="", main=paste("T=",from,"-",to,sep="") , asp=1, axes=F)
      for(Xi in 1:density.division){
      for(Yi in 1:density.division){
        if((Xi-(density.division+1)/2)^2+(Yi-(density.division+1)/2)^2<=r2){
          rank<-floor(counts[i,Xi,Yi]/maxcount[i]*20)+1
          if(rank>20)rank<-20
          symbols((Xi-0.5)*unit,(Yi-0.5)*unit,rectangle=cbind(unit,unit),bg=rev(rainbow(20,start=1.0,end=0.67))[rank],fg=rev(rainbow(20,start=1.0,end=0.67))[rank],inches=F,add=T)
        }
      }
      }
      symbols(centerx,centery,circles=radius, bg=c(NA,"black","black"), inches=F, add=T)
      if(!suppress.dots){
      symbols(peak.positionX,peak.positionY,circles=rep(1,length(peak.positionX)), bg=c(NA,"black","black"), inches=F, add=T)
      }
    }
    plot(c(),c(),xlim=c(5,95), ylim=c(5,95), xlab="", ylab="", main="" , asp=1, axes=F)
    legend(60, 90, c("maxcount",rep("",18),0), xjust=1, x.intersp=1, fill=rainbow(20,start=1.0,end=0.67), bty="n",y.intersp=0.5,border=rainbow(20,start=1.0,end=0.67),cex=1)
  }
  par(mfrow=c(1,1),mar=c(5,5,2,2))
}

#plotxy
co$plotxy<-'
plotxy(tracks=1:track.n, spanT=c(0,maxT), xlim=c(0,100), ylim=c(0,100))
'
plotxy <- function(tracks=1:track.n, spanT=c(0,maxT), xlim=c(0,100), ylim=c(0,100), tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  rb <- rainbow(12, start=.7, end=.1)
  plot(c(),c(),xlim=xlim, ylim=ylim, xlab="X", ylab="Y", asp=1, main=paste("T=",spanT[1],"-",spanT[2]))
  for(division in 1:12){
    lowT <- spanT[1] + (spanT[2]-spanT[1]) * (division-1)/12
    highT <- spanT[1] + (spanT[2]-spanT[1]) * division/12
    for(track.i in tracks){
      rangeT <- dT[[track.i]]>=lowT & dT[[track.i]]<highT
      lines(dX[[track.i]][rangeT], dY[[track.i]][rangeT], col=rb[division])
    }
  }
}

#plottx
co$plottx<-'
plottx(tracks=1:track.n, spanT=c(0,maxT))
'
plottx <- function(tracks=1:track.n, spanT=c(0,maxT), tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  plot(c(),c(),xlim=spanT, ylim=c(0,100), xlab="T", ylab="X")
    for(track.i in tracks){
      rangeT <- dT[[track.i]]>=spanT[1] & dT[[track.i]]<spanT[2]
      lines(dT[[track.i]][rangeT], dX[[track.i]][rangeT])
    }
}

#plotaroundpir
co$plotaroundpir<-'
plotaroundpir(tracks=1:track.n, nfigs=1, save=TRUE)
'
plotaroundpir <- function(tracks=1:track.n, nfigs=1, save=TRUE, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  j<-0
  for(track.i in tracks){
    n <- point.n[track.i]
    R <- (PirRun[[track.i]] == "R")
    P <- (PirRun[[track.i]] == "P")
    start <- which(R & c(P[2:n],F))
    end <- which(R & c(F,P[1:(n-1)]))
    if(length(end)>0 && length(start)>0){
    if(end[1]<=start[1]) end <- end[-1]
    if(length(end)>0)
    {
      if(length(start)>length(end)) start <- start[-length(start)]
    }
    for(i in 1:length(start))
    {
      j<-j+1
      plota(track.i, max(c(1,start[i]-20)), min(c(end[i]+20, point.n[track.i])), main=paste("Track",track.i," T=",dT[[track.i]][max(c(1,start[i]-20))],"-",dT[[track.i]][min(c(end[i]+20, point.n[track.i]))],sep=""), type="b", cex=0.5) #pircolor="green", 
      if(save){savePlot(paste("PlotPir_track",track.i,"_T=",dT[[track.i]][max(c(1,start[i]-20))],"-",dT[[track.i]][min(c(end[i]+20, point.n[track.i]))],".tiff",sep=""),"tiff")}
      if(j==nfigs) return()
    }
    }
  }
}

plotxyhist<-function(tracks=1:track.n, division=20, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  histdata<<-matrix(0, division, division)
    for(track.i in tracks)
    {
      for(i in 1:point.n[track.i])
      {
        l <- ceiling(dX[[track.i]][i]*division/100)
        m <- ceiling(dY[[track.i]][i]*division/100)
        histdata[l,m] <<- histdata[l,m] + 1
        
      }
    }
  persp(seq(0+50/division,100,by=100/division), seq(0+50/division,100,by=100/division), histdata, 
  xlim=c(0,100), ylim=c(0,100), xlab="X", ylab="Y",zlab="fraction")
}

#######################################################
###################                            ########
co$ooo<-'\n########     Data processing     ########
'##################                            ######
#######################################################

####### Internal functions #######

#Detemine distance
dist2 <- function(X1,Y1,X2,Y2){
  return((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2))
}

#Calculate angle
costheta <- function(X,Y,X1,Y1,X2,Y2){
  a1 <- c(X1-X, Y1-Y)
  a2 <- c(X2-X, Y2-Y)
  return(sum(a1*a2)/sqrt(sum(a1*a1))/sqrt(sum(a2*a2)))
}

#Calculation of angle
product <- function(X,Y,X1,Y1,X2,Y2){
  return(c( ((X1-X)*(X2-X)+(Y1-Y)*(Y2-Y)), abs((X1-X)*(Y2-Y)-(X2-X)*(Y1-Y))) )
}

####### Coordinate transformation #######

#adjust.position
co$adjust.position<-'
adjust.position()
'
adjust.position<-function(){
  
  # Calculate Transform
  
  Transform <<- list()
  for(plate.i in 1:plate.n){
    if(plate_format=="2spot"){
      if(!exists("spot")){cat("Execute read.spots first.\n");return}
      X1 <- spot$X1[plate.i]; Y1 <- spot$Y1[plate.i] # odor spots
      X2 <- spot$X2[plate.i]; Y2 <- spot$Y2[plate.i] # odor spots
      XY0 <- c((X1+X2)/2, (Y1+Y2)/2)
      tt <- (X1-X2)/(Y1-Y2) #tangent theta
      ct <- sqrt(1/(1+tt^2)) #cosine theta
      st <- ct*tt #sine theta
      A <- matrix(c(ct,-st,st,ct),2,2)
      b <- c(19.46,50)
    }else if(plate_format=="2odornew"){
      if(!exists("markpoints")){cat("No Markpoints properly read.\n");return}
      if(length(markpoints[[plate.i]])!=2){cat("Number of Markpoints is wrong\n");return}
      X1 <- markpoints[[plate.i]][1,1]; Y1 <- markpoints[[plate.i]][1,2] # spot position
      X2 <- markpoints[[plate.i]][2,1]; Y2 <- markpoints[[plate.i]][2,2] # spot position
      XY0 <- c((X1+X2)/2, (Y1+Y2)/2)
      tt <- (X1-X2)/(Y1-Y2) #tangent theta
      ct <- sqrt(1/(1+tt^2)) #cosine theta
      st <- ct*tt #sine theta
      A <- matrix(c(ct,-st,st,ct),2,2)
      b <- c(19.46,50)
    }else if(plate_format=="kunitomo"){
      if(!exists("markpoints")){cat("No Markpoints properly read.\n");return}
      if(length(markpoints[[plate.i]])!=2){cat("Markpointsの数が正しくありません。\n");return}
      X1 <- markpoints[[plate.i]][1,1]; Y1 <- markpoints[[plate.i]][1,2] # left plug position
      X2 <- markpoints[[plate.i]][2,1]; Y2 <- markpoints[[plate.i]][2,2] # right position
      XY0 <- c((X1+X2)/2,(Y1+Y2)/2)
      if(mark_left_to_right){
        r <- sqrt((Y2-Y1)*(Y2-Y1)+(X2-X1)*(X2-X1))
        ct <- (X2-X1)/r #cosine theta; added at ver 5.9.3
        st <- (Y2-Y1)/r #sine theta; added at ver 5.9.3
      }else{
        if(X2==X1){cat("Error in markposition\n")}
        tt <- (Y2-Y1)/(X2-X1) #tangent theta
        ct <- sqrt(1/(1+tt^2)) #cosine theta
        st <- ct*tt #sine theta
      }
      A <- matrix(c(ct,st,-st,ct),2,2)  #5.4beta1
      b <- c(50,50)
    }else if(plate_format=="center-peak"){
      if(!exists("markpoints")){cat("No Markpoints properly read.\n");return}
      if(is.na(markpoints[[plate.i]][1,1]) || is.na(markpoints[[plate.i]][1,2])){cat("No Markpoints found\n");return}
      XY0 <- c(mean(markpoints[[plate.i]][,1]), mean(markpoints[[plate.i]][,2]))
      A <- diag(2)
      b <- c(50,50)
    }else{
      XY0 <- c(0,0)
      A <- diag(2)
      b <- c(0,0)
    }
    
    Transform <<- c(Transform, list(list(mag=mags[plate.i], XY0=XY0, A=A, b=b)))
    
  } # end for(plate.i)
  
  # Coordinate transform
  
  for(plate.i in 1:plate.n){
  for(track.i in plate.track[[plate.i]]){
    Xnew <- 
    cbind(dX[[track.i]]-Transform[[plate.i]]$XY0[1], dY[[track.i]]-Transform[[plate.i]]$XY0[2]) %*%
      Transform[[plate.i]]$A + matrix(Transform[[plate.i]]$b, length(dX[[track.i]]), 2, byrow=T)
    dX[[track.i]] <<- Xnew[,1]
    dY[[track.i]] <<- Xnew[,2]
  }
  }

}

####### Data processing #######

#calc.dL
co$calc.dL<-'
calc.dL(calc.dV=TRUE)
'
calc.dL <- function(calc.dV=TRUE){
  dL <<- list()
  if(calc.dV){dV<<-list()}
  for(track.i in 1:track.n){
    dL <<- c(dL, list(c(sapply(1:(point.n[track.i]-1),function(i) sqrt(dist2(dX[[track.i]][i],dY[[track.i]][i],dX[[track.i]][i+1],dY[[track.i]][i+1]))),NA)))
    if(calc.dV){
    dV <<- c(dV, list(dL[[track.i]]/c(dT[[track.i]][-1]-dT[[track.i]][-point.n[track.i]],NA)))
    }
  }
}

##################################
# Lable Pirouettes and Turns
##################################

#findPir
co$findPir<-'
findPir(from=1, to=track.n)
'
findPir <- function(from=1, to=track.n){
  cat(paste("Total tracks ", track.n, "\nProcessing track no:\n"))
  if(from == 1){
    TurnRun <<- list()
    AvTheta <<- list()
  }
  else{
    if(length(TurnRun) != from-1 || length(AvTheta) != from-1){
      cat("Illegal value of from")
      return
    }
  }
  for(track.i in from:to){
  if(track.i%%100 == 0) cat(paste(" ",track.i,sep=""))
  TurnRun <<- c(TurnRun, list(rep("R",length(dT[[track.i]]))))
  tempAvTheta <- rep(NA, point.n[track.i])
  if(point.n[track.i]>=3){
  for(j in 2:(point.n[track.i]-1)){
    flag1 <- FALSE
    flag2 <- FALSE
    cumL <- 0
    for(j11 in (j-1):1){
      cumL <- cumL + dL[[track.i]][j11]
      if(cumL > gauge) break
    }
    for(j1 in j11:1){
      if(dist2(dX[[track.i]][j],dY[[track.i]][j],dX[[track.i]][j1],dY[[track.i]][j1]) > gaugegauge){
        flag1<-TRUE
	  break
      }
    }
    cumL <- 0
    for(j12 in (j+1):length(dT[[track.i]])){
      cumL <- cumL + dL[[track.i]][j12-1]
      if(cumL > gauge) break
    }
    for(j2 in j12:length(dT[[track.i]])){
      if(dist2(dX[[track.i]][j],dY[[track.i]][j],dX[[track.i]][j2],dY[[track.i]][j2])>gaugegauge){
	  flag2<-TRUE
	  break
      }
    }
    if(flag1 && flag2){
      pro <- product(dX[[track.i]][j],dY[[track.i]][j],dX[[track.i]][j1],dY[[track.i]][j1],dX[[track.i]][j2],dY[[track.i]][j2])
      if((pro[2]!=0 && pro[1]/pro[2] > atanpirangle) || (pro[2]==0 && pro[1]>0)){
        TurnRun[[track.i]][j]<<-"T"
      }

      #
      # Calculation of AvTheta
      #

      AvX <- mean(dX[[track.i]][j1:j2])
      AvY <- mean(dY[[track.i]][j1:j2])
      u <- dX[[track.i]][j1:j2]-AvX
      v <- dY[[track.i]][j1:j2]-AvY
      s2 <- cov(cbind(u,v))
      ev <- eigen(s2)$values[1]
      if(s2[1,2]==0){
        if(s2[1,1]>s2[2,2]){tempAvTheta[j]<-0}
        if(s2[1,1]<s2[2,2]){tempAvTheta[j]<-90}
        if(s2[1,1]==s2[2,2]){tempAvTheta[j]<-NA}
      }else{
        tempAvTheta[j] <- atan(s2[1,2]/(ev-s2[2,2]))/pi*180
      }

      if(s2[1,1]>=s2[2,2]){
        if(u[1]>u[length(u)]){
          if(tempAvTheta[j]>0) tempAvTheta[j] <- tempAvTheta[j]-180
          else tempAvTheta[j] <- tempAvTheta[j]+180
        }
      }else{
        if(v[1]>v[length(v)] && tempAvTheta[j]>0) tempAvTheta[j] <- tempAvTheta[j]-180
        else if(v[1]<v[length(v)] && tempAvTheta[j]<0) tempAvTheta[j] <- tempAvTheta[j]+180
      }
    }
  }
  }
  AvTheta <<- c(AvTheta, list(tempAvTheta))
  }
  cat("\rDone.\n")
}


#findPirA
co$findPirA<-'
findPirA()
'
findPirA <- function(){
  cat(paste("Total tracks ", track.n, "\nProcessing track no:\n"))
  TurnRunA <<- list()
  Theta <<- list()
  for(track.i in 1:track.n){
	  if(track.i%%100 == 0) cat("", track.i)
	  TurnRunA <<- c(TurnRunA, list(rep("R",point.n[track.i])))
	  tempTheta2 <- c(NA)
	  if(point.n[track.i]>=2){
	  for(i in 2:point.n[track.i]){
		ddX <- dX[[track.i]][i]-dX[[track.i]][i-1]
        ddY <- dY[[track.i]][i]-dY[[track.i]][i-1]
        tempTheta <- atan(ddY/ddX)/pi*180
        if(ddX<0 && ddY>=0) tempTheta<-tempTheta+180
        if(ddX<0 && ddY<0) tempTheta<-tempTheta-180
        tempTheta2 <- c(tempTheta2, tempTheta)
      }
      }
      Theta <<- c(Theta, list(tempTheta2))
      if(point.n[track.i]>=3){
	  for(j in 2:(point.n[track.i]-1)){
		angle <- abs(Theta[[track.i]][j+1]-Theta[[track.i]][j])
		if(!is.nan(angle)){
		if(angle>180) angle <- 360-angle
		if(angle>90){
			TurnRunA[[track.i]][j]<<-"T"
		}
		}
	  }
  	  }
  }
  cat("\nDone.\n")
}


#findShortTurn
co$findShortTurn<-'
findShortTurn(from=1, to=track.n)
'
findShortTurn <- function(from=1, to=track.n){
  cat(paste("Total tracks ", track.n, "\nProcessing track no:\n"))
  if(from == 1){
    ShortTurn <<- list()
  }
  else{
    if(length(TurnRun) != from-1){
      cat("Illegal value of from")
      return
    }
  }
  for(track.i in from:to){
  if(track.i%%100 == 0) cat(paste("\r",track.i))
  ShortTurn <<- c(ShortTurn, list(rep("R",length(dT[[track.i]]))))

  if(point.n[track.i]>=3){
  for(j in 2:(point.n[track.i]-1)){
    flag1 <- FALSE
    flag2 <- FALSE
    cumL <- 0
    for(j11 in (j-1):1){
      cumL <- cumL + dL[[track.i]][j11]
      if(cumL > gauge3) break
    }
    for(j1 in j11:1){
      if(dist2(dX[[track.i]][j],dY[[track.i]][j],dX[[track.i]][j1],dY[[track.i]][j1]) > gaugegauge3){
        flag1<-TRUE
	  break
      }
    }
    cumL <- 0
    for(j12 in (j+1):length(dT[[track.i]])){
      cumL <- cumL + dL[[track.i]][j12-1]
      if(cumL > gauge3) break
    }
    for(j2 in j12:length(dT[[track.i]])){
      if(dist2(dX[[track.i]][j],dY[[track.i]][j],dX[[track.i]][j2],dY[[track.i]][j2])>gaugegauge3){
	  flag2<-TRUE
	  break
      }
    }
    if(flag1 && flag2){
      pro <- product(dX[[track.i]][j],dY[[track.i]][j],dX[[track.i]][j1],dY[[track.i]][j1],dX[[track.i]][j2],dY[[track.i]][j2])
      if((pro[2]!=0 && pro[1]/pro[2] > atanpirangle) || (pro[2]==0 && pro[1]>0)){
        ShortTurn[[track.i]][j]<<-"T"
      }
    }
  }
  }
  }
  ShortTurnStart <<- lapply(from:to, function(track.i) ShortTurn[[track.i]]=="T" & c("R",ShortTurn[[track.i]][-point.n[track.i]])=="R")
  cat("\rDone.\n")
}


#calc.PirRun
co$calc.PirRun<-'
calc.PirRun()
'
calc.PirRun <- function(){
  PirRun <<- list()
  Pirsurround <<- list()
  cat("Processing track no\n")
  for(track.i in 1:track.n){
    if(track.i%%100 == 0) cat(paste("\r",track.i))
    tempPirRun <- rep("R", point.n[track.i])
    tempPirRun[TurnRun[[track.i]]=="T"] <- "P"
    if(tempPirRun[1] != "R"){return("error: beginning is not Run")}
    RunStart <- (1:(point.n[track.i]-1))[TurnRun[[track.i]][1:(point.n[track.i]-1)]=="T" & TurnRun[[track.i]][2:point.n[track.i]]=="R"]
    RunEnd <- (2:(point.n[track.i]))[TurnRun[[track.i]][1:(point.n[track.i]-1)]=="R" & TurnRun[[track.i]][2:point.n[track.i]]=="T"]
    if(length(RunEnd)>=2){
      for(i in 1:(length(RunEnd)-1)){
        if(dT[[track.i]][RunEnd[i+1]]-dT[[track.i]][RunStart[i]]<=Tcrit){
          tempPirRun[RunStart[i]:RunEnd[i+1]] <- "P"
        }
      }
    }
    PirRun <<- c(PirRun, list(tempPirRun))
    
    tempsurround <- tempPirRun
    PirStart <- (2:point.n[track.i])[tempPirRun[1:(point.n[track.i]-1)]=="R" & tempPirRun[2:point.n[track.i]]=="P"]
    PirEnd <- (1:(point.n[track.i]-1))[tempPirRun[1:(point.n[track.i]-1)]=="P" & tempPirRun[2:point.n[track.i]]=="R"]
    for(i in PirStart){
      l1 <- 0
      for(i1 in (i-1):1){
        l1 <- l1 + dL[[track.i]][i1]
        if(l1>=gauge+gauge22) break  #0.8mm
        else tempsurround[i1] <- "P"
      }
    }
    for(i in PirEnd){
      l1 <- 0
      for(i1 in (i+1):point.n[track.i]){
        l1 <- l1 + dL[[track.i]][i1-1]
        if(l1>=gauge+gauge22) break  #0.8mm
        else tempsurround[i1] <- "P"
      }
    }
    Pirsurround <<- c(Pirsurround, list(tempsurround))
  }
  calc.StartEnd()
  cat("\rDone.    \n")
}


#calc.PirRunA
co$calc.PirRunA<-'
calc.PirRunA()
'
calc.PirRunA <- function(){
  PirRunA <<- list()
  PirsurroundA <<- list()
  cat("Processing track no\n")
  for(track.i in 1:track.n){
    if(track.i%%100 == 0) cat("",track.i)
    tempPirRunA <- rep("R", point.n[track.i])
    tempPirRunA[TurnRunA[[track.i]]=="T"] <- "P"
    if(tempPirRunA[1] != "R"){return("error: beginning is not Run")}
    RunStartA <- (1:(point.n[track.i]-1))[TurnRunA[[track.i]][1:(point.n[track.i]-1)]=="T" & TurnRunA[[track.i]][2:point.n[track.i]]=="R"]
    RunEndA <- (2:(point.n[track.i]))[TurnRunA[[track.i]][1:(point.n[track.i]-1)]=="R" & TurnRunA[[track.i]][2:point.n[track.i]]=="T"]
    if(length(RunEndA)>=2){
      for(i in 1:(length(RunEndA)-1)){
        if(dT[[track.i]][RunEndA[i+1]]-dT[[track.i]][RunStartA[i]]<=Tcrit){
          tempPirRunA[RunStartA[i]:RunEndA[i+1]] <- "P"
        }
      }
    }
    PirRunA <<- c(PirRunA, list(tempPirRunA))
    
    tempsurroundA <- tempPirRunA
    localPirStartA <- (2:point.n[track.i])[tempPirRunA[1:(point.n[track.i]-1)]=="R" & tempPirRunA[2:point.n[track.i]]=="P"]
    localPirEndA <- (1:(point.n[track.i]-1))[tempPirRunA[1:(point.n[track.i]-1)]=="P" & tempPirRunA[2:point.n[track.i]]=="R"]
    for(i in localPirStartA){
      l1 <- 0
      for(i1 in (i-1):1){
        l1 <- l1 + dL[[track.i]][i1]
        if(l1>=gauge+gauge22) break  #0.8mm
        else tempsurroundA[i1] <- "P"
      }
    }
    for(i in localPirEndA){
      l1 <- 0
      for(i1 in (i+1):point.n[track.i]){
        l1 <- l1 + dL[[track.i]][i1-1]
        if(l1>=gauge+gauge22) break  #0.8mm
        else tempsurroundA[i1] <- "P"
      }
    }
    PirsurroundA <<- c(PirsurroundA, list(tempsurroundA))
  }
  TurnStartA <<- list()
  TurnEndA <<- list()
  PirStartA <<- list()
  PirEndA <<- list()
  for(track.i in 1:track.n){
    tempTurnStartA <- rep(FALSE,point.n[track.i])
    tempTurnStartA[TurnRunA[[track.i]]=="T" & c("R",TurnRunA[[track.i]][-point.n[track.i]])=="R"] <- TRUE
    TurnStartA <<- c(TurnStartA, list(tempTurnStartA))
    tempTurnEndA <- rep(FALSE,point.n[track.i])
    tempTurnEndA[TurnRunA[[track.i]]=="T" & c(TurnRunA[[track.i]][-1],"R")=="R"] <- TRUE
    TurnEndA <<- c(TurnEndA, list(tempTurnEndA))
    tempPirStartA <- rep(FALSE,point.n[track.i])
    tempPirStartA[PirRunA[[track.i]]=="P" & c("R",PirRunA[[track.i]][-point.n[track.i]])=="R"] <- TRUE
    PirStartA <<- c(PirStartA, list(tempPirStartA))
    tempPirEndA <- rep(FALSE,point.n[track.i])
    tempPirEndA[PirRunA[[track.i]]=="P" & c(PirRunA[[track.i]][-1],"R")=="R"] <- TRUE
    PirEndA <<- c(PirEndA, list(tempPirEndA))
  }
  cat("\nDone.    \n")
}


#calc.StartEnd
co$calc.StartEnd<-'
calc.StartEnd()
'
calc.StartEnd <- function(){
  TurnStart <<- list()
  TurnEnd <<- list()
  PirStart <<- list()
  PirEnd <<- list()
  for(track.i in 1:track.n){
    tempTurnStart <- rep(FALSE,point.n[track.i])
    tempTurnStart[TurnRun[[track.i]]=="T" & c("R",TurnRun[[track.i]][-point.n[track.i]])=="R"] <- TRUE
    TurnStart <<- c(TurnStart, list(tempTurnStart))
    tempTurnEnd <- rep(FALSE,point.n[track.i])
    tempTurnEnd[TurnRun[[track.i]]=="T" & c(TurnRun[[track.i]][-1],"R")=="R"] <- TRUE
    TurnEnd <<- c(TurnEnd, list(tempTurnEnd))
    tempPirStart <- rep(FALSE,point.n[track.i])
    tempPirStart[PirRun[[track.i]]=="P" & c("R",PirRun[[track.i]][-point.n[track.i]])=="R"] <- TRUE
    PirStart <<- c(PirStart, list(tempPirStart))
    tempPirEnd <- rep(FALSE,point.n[track.i])
    tempPirEnd[PirRun[[track.i]]=="P" & c(PirRun[[track.i]][-1],"R")=="R"] <- TRUE
    PirEnd <<- c(PirEnd, list(tempPirEnd))
  }
}


#################################
# Calculation of TurnRate
#################################

#calc.TurnRate
co$calc.TurnRate<-'
calc.TurnRate()
'
calc.TurnRate <- function(){
  cat("Processing track no:\n")
  TurnRate <<- list()
  for(track.i in 1:track.n){
    if(track.i%%100 == 0) cat(paste("\r",track.i))
    tempTR <- rep(NA,point.n[track.i])
    if(point.n[track.i]>=3){
    for(i in 2:(point.n[track.i]-1)){
      if(TurnRun[[track.i]][i]!="R") next
      flag1 <- FALSE
       l1 <- 0
      for(i1 in (i-1):1){
        if(TurnRun[[track.i]][i1]!="R") break      #addition for 5.5beta5
        l1 <- l1 + dL[[track.i]][i1] 
        if(l1>=gauge22){
          flag1 <- TRUE
          break
        }
      }
      flag2 <- FALSE
     l2 <- 0
      for(i2 in (i+1):point.n[track.i]){ 
        if(TurnRun[[track.i]][i2]!="R") break      #addition for 5.5β5
        l2 <- l2 + dL[[track.i]][i2-1]
        if(l2>=gauge22){
          flag2 <- TRUE
          break
        }
      }
      if(flag1 && flag2){    #added for wt5.5β5
        #if(!is.na(AvTheta[[track.i]][i1]) && !is.na(AvTheta[[track.i]][i2])){
          tempTR[i] <- AvTheta[[track.i]][i2]-AvTheta[[track.i]][i1]
        #}
        if(!is.na(tempTR[i])){
        if(tempTR[i]>180){tempTR[i] <- tempTR[i]-360}
        if(tempTR[i] <= -180){tempTR[i] <- tempTR[i]+360}
        tempTR[i]<-tempTR[i]/(l1+l2)
        }
       #if(!is.na(tempTR[i])) if(tempTR[i]>100) recover()
      }
    }
    }
   TurnRate <<- c(TurnRate, list(tempTR))
  }
  cat("\rDone.\n")
}

#######################
# Calculation of Bearing
#######################

#calc.Bearing
co$calc.Bearing<-'
calc.Bearing(plate_format, odordirection="closer", suppress.numerical=F)
'
calc.Bearing <- function(plate_format, odordirection="closer", suppress.numerical=F,tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  Theta <<- list()
  Bearing <<- list()
  Dist <<- list()
  cat("Processing track no:\n")
  for(track.i in 1:track.n){
    if(track.i%%100 == 0) cat(paste("\r",track.i))
    tempTheta2 <- c(NA)
    tempBearing <- c(NA)
    tempDist <- c()
    for(i in 1:point.n[track.i]){
      X <- dX[[track.i]][i]
      Y <- dY[[track.i]][i]
     # browser()
      if(plate_format=="12point" || plate_format=="kunitomo" || plate_format=="center-peak" || plate_format=="1point" || (plate_format=="2point" && odordirection == "closer")){
        if(plate_format=="12point"){
          peakX <- floor(X/20)*20+10
          peakY <- floor(Y/20)*20+10
          if(abs(peakX)==50 & abs(peakY)==50){
            peakX <- NA
            peakY <- NA
          }
        }
        
        else if(plate_format=="2point"){
          X1 <- 62.5
          Y1 <- 50
          X2 <- 37.5
          Y2 <- 50
          if(dist2(X,Y,X1,Y1) < dist2(X,Y,X2,Y2))
          {
            peakX <- X1
            peakY <- Y1
          }
          else
          {
            peakX <- X2
            peakY <- Y2
          }
        }
        
        else if(plate_format=="kunitomo"){
          peakY<-50
          if(X<=50){peakX<-20}else{peakX<-80}
        }
        
        else if(plate_format=="center-peak"){
          peakX<-50
          peakY<-50
        }
        
        else if(plate_format=="1point"){
          if(!exists("peakX")||!exists("peakY")){
            cat("\nError: 1point specified even if peakX, peakY are not set\n")
            return()
          }
        }
        
        
        DX <- peakX-X
        DY <- peakY-Y
        peakDir <- atan(DY/DX)/pi*180
        if(!is.na(DX) && !is.na(DY)){
          if(DX<0 & DY>=0) peakDir<-peakDir+180
          if(DX<0 & DY<0) peakDir<-peakDir-180
        }
        Distance <- sqrt(DX^2+DY^2)
      }
      else if(plate_format=="2point" && odordirection=="midline"){
        DX <- c(spot$X1[origfile[track.i]], spot$X2[origfile[track.i]]) - X #vector
        DY <- c(spot$Y1[origfile[track.i]], spot$Y2[origfile[track.i]]) - Y
        peakDirv <- atan(DY/DX)/pi*180
        peakDirv[!is.na(DX) & !is.na(DY) & DX<0 & DY>=0] <- peakDirv[!is.na(DX) & !is.na(DY) & DX<0 & DY>=0]+180
        peakDirv[!is.na(DX) & !is.na(DY) & DX<0 & DY<0] <- peakDirv[!is.na(DX) & !is.na(DY) & DX<0 & DY<0]-180
        peakDir <- sum(peakDirv)/2
        if(abs(peakDirv[1]-peakDirv[2])>180){
          if(peakDir<=0)peakDir<-peakDir+180
          else peakDir<-peakDir-180
        }
        Distance <- sqrt(mean(DX)^2+mean(DY)^2)
      }else if(plate_format=="none"){
	    peakDir <- NA
		Distance <- NA
	  }else {
        print("Illegal parameters plate_format or odordirection")
        return
      }
      tempDist <- c(tempDist, Distance)
      if(gradient=="numerical" && !suppress.numerical){
        peakDir <- atan(dCdY[[track.i]][i]/dCdX[[track.i]][i])/pi*180
        if(!is.na(dCdY[[track.i]][i]) && !is.na(dCdX[[track.i]][i])){
          if(dCdX[[track.i]][i]<0 & dCdY[[track.i]][i]>=0) peakDir<-peakDir+180
          if(dCdX[[track.i]][i]<0 & dCdY[[track.i]][i]<0) peakDir<-peakDir-180
        }
      }
      if(i>1){
        ddX <- X-dX[[track.i]][i-1]
        ddY <- Y-dY[[track.i]][i-1]
        tempTheta <- atan(ddY/ddX)/pi*180
        if(ddX<0 && ddY>=0) tempTheta<-tempTheta+180
        if(ddX<0 && ddY<0) tempTheta<-tempTheta-180
        tempTheta2 <- c(tempTheta2, tempTheta)
        
        B <- peakDir-tempTheta
        if(!is.na(B)){
        if(B > 180) B <- B-360
        else if(B < -180) B <- B+360
        }
        tempBearing <- c(tempBearing, B)
      }
    }
    Theta <<- c(Theta, list(tempTheta2))
    Bearing <<- c(Bearing, list(tempBearing))
    Dist <<- c(Dist, list(tempDist))
  }
    cat("\rDone.\n")
}

##################################################
# Pirouette analysis
##################################################

#before.after
co$before.after<-'
before.after(tracks=1:track.n, division=12, timewindow=c(0,maxT))
'
before.after <- function(tracks=1:track.n, division=12, timewindow=c(0,maxT),tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  beforeTheta <<- c()
  afterTheta <<- c()
  deltaTheta <<- c()
  bearingTheta <<- c()
  bearingThetaEnd <<- c()
  for(track.i in tracks){
    n <- point.n[track.i]
    timebool <-  dT[[track.i]]>=timewindow[1] & dT[[track.i]]<=timewindow[2]
    timeindex <- which(timebool)
    if(length(timeindex)>0){
      from <- timeindex[1]
      to <- timeindex[length(timeindex)]
      if (!all(timebool[from:to])){print("Abnormal time structure. track = ", track.i); return()}
      R <- (Pirsurround[[track.i]] == "R")
      P <- (Pirsurround[[track.i]] == "P")
      start <- which(timebool & R & c(P[2:n],F))
      end <- which(timebool & R & c(F,P[1:(n-1)]))
      if(length(end)>0 && length(start)>0){
      #if(track.i == 161){print(start);print(end)}
      if(end[1]<=start[1]) end <- end[-1]
      if(length(end)>0){
        if(length(start)>length(end)) start <- start[-length(start)]
          if(length(start) != length(end)) {print(paste("Wrong length",track.i));return()}
          if(!all(start < end)) {print(paste("Wrong large/small",track.i));print(start);print(end);return()}
          #if(track.i == 39){print(start);print(end)}
          if(!all(start[-1] >= end[-length(end)])) {print(paste("Wrong large/small order",track.i));return()}
          startTheta <- AvTheta[[track.i]][start]
          bearing <- Bearing[[track.i]][start]
          endTheta <- AvTheta[[track.i]][end]
          bearingEnd <- Bearing[[track.i]][end]
          #if(P[1]){
          #  if(length(endTheta)>1) endTheta <- endTheta[2:length(endTheta)]
          #  else next
          #}
          #if(P[n]){
          #  if(length(startTheta)>1){
          #    startTheta <- startTheta[1:(length(startTheta)-1)]
          #    bearing <- bearing[-length(bearing)]
          #  }
          #  else next
          #}
          temp <- !is.na(startTheta) & !is.na(endTheta)
          startTheta <- startTheta[temp]
          bearing <- bearing[temp]
          endTheta <- endTheta[temp]
          bearingEnd <- bearingEnd[temp]
          if(length(startTheta) != length(endTheta)){
            print("Error")
          }
          diffTheta <- endTheta - startTheta
          diffTheta[diffTheta <= -180] <- diffTheta[diffTheta <= -180] + 360
          diffTheta[diffTheta > 180] <- diffTheta[diffTheta > 180] - 360
          beforeTheta <<- c(beforeTheta, startTheta)
          afterTheta <<- c(afterTheta, endTheta)
          deltaTheta <<- c(deltaTheta, diffTheta)
          bearingTheta <<- c(bearingTheta, bearing)
          bearingThetaEnd <<- c(bearingThetaEnd, bearingEnd)
        }
      }
    }
  }

  BdeltaTheta <<- matrix(0, division, division)
  for(i in 1:length(bearingTheta))
  {
    BdeltaTheta[ceiling((bearingTheta[i]+180)/360*division), ceiling((deltaTheta[i]+180)/360*division)] <<- 
    BdeltaTheta[ceiling((bearingTheta[i]+180)/360*division), ceiling((deltaTheta[i]+180)/360*division)] + 1
  }
  for(i in 1:division)
  {
    sumB <- sum(BdeltaTheta[i,])
    BdeltaTheta[i,] <<- BdeltaTheta[i,]/sumB
  }
  image(seq(-180,180,360/division),seq(-180,180,360/division),BdeltaTheta,col=gray(50:0/50),xlab="Bearing",ylab="deltaTheta", main="Before-After")
}

#addtion at 5.1

##################################################
# Calculation of chemical concentration
##################################################

#calc.C
co$calc.C<-'
calc.C(type="12point"|"plug")
'
calc.C <- function(type="12point",tbl=NULL){ #12point
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  dC   <<- list()
  dCdT <<- list()
  dCdX <<- list()
  dCdY <<- list()
  dCdLat <<- list()
  cat("Processing track no:\n")
  for(track.i in 1:track.n){
    cat(paste("\r",track.i))
    tempC <- c()
    tempdCdT <- c(NA)
    tempdCdX <- c()
    tempdCdY <- c()
    tempdCdLat <- c()
    for(i in 1:point.n[track.i]){
      if(type=="12point"){
        currentC <- 0
        currentdCdX <- 0
        currentdCdY <- 0
        t <- dT[[track.i]][i] + 3600
        for(k in 1:4){
        for(l in 1:4){
        if(abs(k-2.5)+abs(l-2.5)<3){
          peakX <- k*20-50
          peakY <- l*20-50
          DX <- dX[[track.i]][i]-peakX
          DY <- dY[[track.i]][i]-peakY
          Distance2 <- DX^2+DY^2
          C1 <- C0 * exp(-Distance2/400/DiffusionConst/t) / 4 / pi / plate_thickness / DiffusionConst / t
          currentC <- currentC + C1
          currentdCdX <- currentdCdX - DX*C1/20/DiffusionConst/t
          currentdCdY <- currentdCdY - DY*C1/20/DiffusionConst/t
        }
        }
        }
      }
      if(type=="plug"){
        if(!exists("Cxyt") || !exists("dCdXxyt") || !exists("dCdYxyt")){cat("Execute read.C first.\n");return}
        currentC<-get.C(dX[[track.i]][i],dY[[track.i]][i],dT[[track.i]][i])
        currentdCdX<-get.dCdX(dX[[track.i]][i],dY[[track.i]][i],dT[[track.i]][i])
        currentdCdY<-get.dCdY(dX[[track.i]][i],dY[[track.i]][i],dT[[track.i]][i])
      }
      tempC <- c(tempC, currentC)
      tempdCdX <- c(tempdCdX, currentdCdX)
      tempdCdY <- c(tempdCdY, currentdCdY)
      if(i>1){
        tempdCdT <- c(tempdCdT, (tempC[i]-tempC[i-1])/(dT[[track.i]][i]-dT[[track.i]][i-1]))
      }
      currentdCdLat <- - currentdCdX*sin(AvTheta[[track.i]][i]/180*pi) + currentdCdY*cos(AvTheta[[track.i]][i]/180*pi)
      tempdCdLat <- c(tempdCdLat, currentdCdLat)
    }
    dC   <<- c(dC, list(tempC))
    dCdT <<- c(dCdT, list(tempdCdT))
    dCdX <<- c(dCdX, list(tempdCdX))
    dCdY <<- c(dCdY, list(tempdCdY))
    dCdLat <<- c(dCdLat, list(tempdCdLat))
  }
    gradient <<- "numerical"   #5.9.8
    cat("\rDone.\n")
}

#calc.C2
co$calc.C2<-'
calc.C2()
'
calc.C2 <- function(){
  PeakX <<- c(-30,-30,-10,-10,-10,-10,10,10,10,10,30,30)
  PeakY <<- c(-10,10,-30,-10,10,30,-30,-10,10,30,-10,10)
  dC <<- lapply(1:track.n, function(track.i) sapply(1:point.n[track.i], function(i) calc.C2.func1(track.i, i, 1)))
  dCdX <<- lapply(1:track.n, function(track.i) sapply(1:point.n[track.i], function(i) calc.C2.func1(track.i, i, 2)))
  dCdY <<- lapply(1:track.n, function(track.i) sapply(1:point.n[track.i], function(i) calc.C2.func1(track.i, i, 3)))
  dCdT <<- lapply(1:track.n, function(track.i) c(NA,(dC[[track.i]][-1]-dC[[track.i]][-point.n[track.i]])/(dT[[track.i]][-1]-dT[[track.i]][-point.n[track.i]])))
  gradient <<- "numerical"   #5.9.8
}
#For calc.C2
calc.C2.func1 <- function(track.i, i, sel){
  DX <- dX[[track.i]][i]-PeakX
  DY <- dY[[track.i]][i]-PeakY
  t <- dT[[track.i]][i]+3600
  C1 <- C0 * exp(-(DX^2+DY^2)/400/DiffusionConst/t) / 4 / pi / plate_thickness / DiffusionConst / t
  if(sel==1){return(sum(C1))}
  if(sel==2){return(sum(DX*C1/20/DiffusionConst/t))}
  if(sel==3){return(sum(DY*C1/20/DiffusionConst/t))}
}

#get.C
get.C<-function(x,y,t){
  if(!exists("Cxyt")){cat("No Cxyt found.\n");return}
  t <- t/60
  x0 <- floor(x/simulator.unit+0.5)
  x1 <- ceiling(x/simulator.unit+0.5)
  xd <- x/simulator.unit+0.5-x0
  y0 <- floor(y/simulator.unit+0.5)
  y1 <- ceiling(y/simulator.unit+0.5)
  yd <- y/simulator.unit+0.5-y0
  t0 <- floor(t+1)
  t1 <- ceiling(t+1)
  td <- t+1-t0
  d<-dim(Cxyt)
  if(x0<1)    x0 <- 1;    if(x1<1)    x1 <- 1
  if(x0>d[1]) x0 <- d[1]; if(x1>d[1]) x1 <- d[1]
  if(y0<1)    y0 <- 1;    if(y1<1)    y1 <- 1
  if(y0>d[2]) y0 <- d[2]; if(y1>d[2]) y1 <- d[2]
  if(t0<1)    t0 <- 1;    if(t1<1)    t1 <- 1
  if(t0>d[3]) t0 <- d[3]; if(t1>d[3]) t1 <- d[3]
  C0 <- Cxyt[x0,y0,t0]*(1-xd)*(1-yd) + Cxyt[x1,y0,t0]*xd*(1-yd) + Cxyt[x0,y1,t0]*(1-xd)*yd + Cxyt[x1,y1,t0]*xd*yd
  C1 <- Cxyt[x0,y0,t1]*(1-xd)*(1-yd) + Cxyt[x1,y0,t1]*xd*(1-yd) + Cxyt[x0,y1,t1]*(1-xd)*yd + Cxyt[x1,y1,t1]*xd*yd
  return(C0*(1-td)+C1*td)
}

#get.dCdX
get.dCdX<-function(x,y,t){
  if(!exists("dCdXxyt")){cat("dCdXxytがありません。\n");return}
  t <- t/60
  x0 <- floor(x/simulator.unit)
  x1 <- ceiling(x/simulator.unit)
  xd <- x/simulator.unit-x0
  y0 <- floor(y/simulator.unit)
  y1 <- ceiling(y/simulator.unit)
  yd <- y/simulator.unit-y0
  t0 <- floor(t+1)
  t1 <- ceiling(t+1)
  td <- t+1-t0
  d<-dim(dCdXxyt)
  if(x0<1 || x1>d[1] || y0<1 || y1>d[2] || t0<1 || t1>d[3]) {
    return(0)
  }else{
    dCdX0 <- dCdXxyt[x0,y0,t0]*(1-xd)*(1-yd) + dCdXxyt[x1,y0,t0]*xd*(1-yd) + dCdXxyt[x0,y1,t0]*(1-xd)*yd + dCdXxyt[x1,y1,t0]*xd*yd
    dCdX1 <- dCdXxyt[x0,y0,t1]*(1-xd)*(1-yd) + dCdXxyt[x1,y0,t1]*xd*(1-yd) + dCdXxyt[x0,y1,t1]*(1-xd)*yd + dCdXxyt[x1,y1,t1]*xd*yd
    return(dCdX0*(1-td)+dCdX1*td)
  }
}

#get.dCdY
get.dCdY<-function(x,y,t){
  if(!exists("dCdYxyt")){cat("No dCdYxyt found.\n");return}
  t <- t/60
  x0 <- floor(x/simulator.unit)
  x1 <- ceiling(x/simulator.unit)
  xd <- x/simulator.unit-x0
  y0 <- floor(y/simulator.unit)
  y1 <- ceiling(y/simulator.unit)
  yd <- y/simulator.unit-y0
  t0 <- floor(t+1)
  t1 <- ceiling(t+1)
  td <- t+1-t0
  d<-dim(dCdYxyt)
  if(x0<1 || x1>d[1] || y0<1 || y1>d[2] || t0<1 || t1>d[3]) {
    return(0)
  }else{
    dCdY0 <- dCdYxyt[x0,y0,t0]*(1-xd)*(1-yd) + dCdYxyt[x1,y0,t0]*xd*(1-yd) + dCdYxyt[x0,y1,t0]*(1-xd)*yd + dCdYxyt[x1,y1,t0]*xd*yd
    dCdY1 <- dCdYxyt[x0,y0,t1]*(1-xd)*(1-yd) + dCdYxyt[x1,y0,t1]*xd*(1-yd) + dCdYxyt[x0,y1,t1]*(1-xd)*yd + dCdYxyt[x1,y1,t1]*xd*yd
    return(dCdY0*(1-td)+dCdY1*td)
  }
}


#######################################################
###################                           ########
co$oooo<-'\n########     Higher order analyses     ########
'##################                           ########
#######################################################

#classify #5.9
co$classify<-'
generic
classify<-function(x=dT, y=dV, spacer1=NULL, spacer2=NULL, spacer3=NA, spacer4=NULL,
 xmin=0, xbin=100, xmax=maxT, plate=F, PirRunSelect="", timemin=0, timemax=maxT)
'
classify<-function(x=dT, y=dV, spacer1=NULL, spacer2=NULL, spacer3=NA, spacer4=NULL, xmin=0, xbin=100, xmax=maxT, plate=F, PirRunSelect="", timemin=0, timemax=maxT, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  plate.number <- 1;
  if(plate){plate.number <- plate.n}

  ylist <- vector("list", plate.number)
  yn <- matrix(NA, plate.number, ceiling((xmax-xmin)/xbin))
  ymean <- matrix(NA, plate.number, ceiling((xmax-xmin)/xbin))
  ystdev <- matrix(NA, plate.number, ceiling((xmax-xmin)/xbin))
    
  for(plate.i in 1:plate.number){
    if(PirRunSelect == ""){
      x2<-lapply(plate.track[[plate.i]], function(track.i) c(spacer1, x[[track.i]], spacer2)[dT[[track.i]]>=timemin & dT[[track.i]]<=timemax])
      y2<-lapply(plate.track[[plate.i]], function(track.i) c(spacer3, y[[track.i]], spacer4)[dT[[track.i]]>=timemin & dT[[track.i]]<=timemax])
    }else if(PirRunSelect == "R"){
      x2<-lapply(plate.track[[plate.i]], function(track.i) c(spacer1, x[[track.i]], spacer2)[PirRun[[track.i]]=="R" & dT[[track.i]]>=timemin & dT[[track.i]]<=timemax])
      y2<-lapply(plate.track[[plate.i]], function(track.i) c(spacer3, y[[track.i]], spacer4)[PirRun[[track.i]]=="R" & dT[[track.i]]>=timemin & dT[[track.i]]<=timemax])
    }else if(PirRunSelect == "P"){
      x2<-lapply(plate.track[[plate.i]], function(track.i) c(spacer1, x[[track.i]], spacer2)[PirRun[[track.i]]=="P" & dT[[track.i]]>=timemin & dT[[track.i]]<=timemax])
      y2<-lapply(plate.track[[plate.i]], function(track.i) c(spacer3, y[[track.i]], spacer4)[PirRun[[track.i]]=="P" & dT[[track.i]]>=timemin & dT[[track.i]]<=timemax])
    }
    uy2<-unlist(y2)
    ux2<-unlist(x2)
    if(length(ux2) != length(uy2)){cat("Error: Different length of ux2 and uy2.\n");return}
    
    templist <- vector("list", ceiling((xmax-xmin)/xbin))
    for(i in 1:(ceiling((xmax-xmin)/xbin))){
      templist[[i]] <- uy2[ux2>=xmin+(i-1)*xbin & ux2<xmin+i*xbin]
      yn[plate.i,i] <- length(templist[[i]][!is.na(templist[[i]])])
      ymean[plate.i,i] <- mean(templist[[i]], na.rm=T)
      ystdev[plate.i,i] <- sd(templist[[i]], na.rm=T)
    }
    ylist[[plate.i]] <- templist
  }
  list(ylist=ylist, yn=yn, ymean=ymean, ystdev=ystdev)
}


#calc.speed #addition at 5.2
co$calc.speed<-'
'
calc.speed<-function(timespan=100, maxtime=maxT, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  RunV<-lapply(1:track.n, function(track.i) dV[[track.i]][ PirRun[[track.i]][-1]=="R"&PirRun[[track.i]][-point.n[track.i]]=="R" ])
  RunT<-lapply(1:track.n, function(track.i) dT[[track.i]][ c(PirRun[[track.i]][-1]=="R"&PirRun[[track.i]][-point.n[track.i]]=="R",FALSE) ])
  uRunV<-unlist(RunV)
  uRunT<-unlist(RunT)
  meanVT<<-c()
  for(i in 1:(ceiling(maxtime/timespan))){
    meanVT <<- c(meanVT, mean(uRunV[uRunT>=(i-1)*timespan & uRunT<i*timespan ], na.rm=T))
    
  }
  #meanVT.timespan <<- timespan
  worm.speed <<- mean(uRunV)
  plot(seq(timespan/2, (ceiling(maxtime/timespan)-1/2)*timespan, timespan), meanVT, main="Worm Speed", xlab="T (sec)", ylab="average run speed (mm/sec)")
}

#weathervane.graph
co$weathervane.graph<-'
weathervane.graph(xfrom=-2.5,xto=2.5,xby=0.5,plates=1:plate.n, ylim=c(-40,40), Cfrom=NA, Cto=NA, Tfrom=NA, Tto=NA)
'
weathervane.graph<-function(xfrom=-2.5,xto=2.5,xby=0.5,plates=1:plate.n, ylim=c(-40,40), Cfrom=NA, Cto=NA, Tfrom=NA, Tto=NA,tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  xn<-ceiling((xto-xfrom)/xby)
  if(xn<1) {cat("unappropriate xn\n");return}
  br<-seq(xfrom,xto,xby)
  RankedTR<<-array(NA,c(length(plates),xn))
  for(j in 1:xn){
    for(i in 1:length(plates)){
      tempTR <- c()
      for(track.i in plate.track[[plates[i]]]){
        if(is.na(Cfrom)) {Clow <- min(dC[[track.i]],na.rm=T)} else {Clow <- Cfrom}
        if(is.na(Cto)) {Chigh <- max(dC[[track.i]],na.rm=T)} else {Chigh <- Cto}
        if(is.na(Tfrom)) {Tlow <- min(dT[[track.i]],na.rm=T)} else {Tlow <- Tfrom}
        if(is.na(Tto)) {Thigh <- max(dT[[track.i]],na.rm=T)} else {Thigh <- Tto}
        tempTR <- c(tempTR, TurnRate[[track.i]][dCdLat[[track.i]]>=br[j] & dCdLat[[track.i]]<br[j+1] & dC[[track.i]]>=Clow & dC[[track.i]]<=Chigh & dT[[track.i]]>=Tlow & dT[[track.i]]<=Thigh] , na.rm=T)
      }
      RankedTR[i,j] <<- mean(tempTR,na.rm=T)
    }
  }
  RankedTR.mean <<- sapply(1:xn,function(k) mean(RankedTR[,k],na.rm=T))
  RankedTR.sem <<- sapply(1:xn,function(k) sd(RankedTR[,k],na.rm=T))/sqrt(length(plates))
  plot(seq(xfrom+xby/2,xto-xby/2,xby),RankedTR.mean,ylim=ylim,xlab="NaCl gradient (mM/mm)",ylab="Average Curving Rate (deg/mm)",type="l",lwd=2)
  arrows(seq(xfrom+xby/2,xto-xby/2,xby), RankedTR.mean, seq(xfrom+xby/2,xto-xby/2,xby), RankedTR.mean+RankedTR.sem, length=0.05,angle=90)
  arrows(seq(xfrom+xby/2,xto-xby/2,xby), RankedTR.mean, seq(xfrom+xby/2,xto-xby/2,xby), RankedTR.mean-RankedTR.sem, length=0.05,angle=90)
  #print(RankedTR)
}

#pirouette.graph
co$pirouette.graph<-'
pirouette.graph(xfrom=-0.045,xto=0.045,xby=0.01,plates=1:plate.n, ylim=c(0,0.14), Cfrom=NA, Cto=NA, Tfrom=NA, Tto=NA, mincount=10)
'
pirouette.graph<-function(xfrom=-0.045,xto=0.045,xby=0.01,plates=1:plate.n, ylim=c(0,0.14), Cfrom=NA, Cto=NA, Tfrom=NA, Tto=NA, mincount=10, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  xn<-ceiling((xto-xfrom)/xby)
  if(xn<1) {cat("unappropriate xn\n");return}
  br<-seq(xfrom,xto,xby)
  RankedTotal<<-array(0,c(length(plates),xn))
  RankedPircount<<-array(0,c(length(plates),xn))
  RankedPir<<-array(NA,c(length(plates),xn))
  for(i in 1:length(plates)){
    for(j in 1:xn){
      tempTotal <- 0
      tempPircount <- 0
      for(track.i in plate.track[[plates[i]]]){
        if(is.na(Cfrom)) {Clow <- min(dC[[track.i]],na.rm=T)} else {Clow <- Cfrom}
        if(is.na(Cto)) {Chigh <- max(dC[[track.i]],na.rm=T)} else {Chigh <- Cto}
        if(is.na(Tfrom)) {Tlow <- min(dT[[track.i]],na.rm=T)} else {Tlow <- Tfrom}
        if(is.na(Tto)) {Thigh <- max(dT[[track.i]],na.rm=T)} else {Thigh <- Tto}
        Totaltime <- sum((c(dT[[track.i]][-1]-dT[[track.i]][-point.n[track.i]],0))[PirRun[[track.i]]=="R" 
        & !is.na(dCdT[[track.i]]) & dCdT[[track.i]]>=br[j] & dCdT[[track.i]]<br[j+1] & dC[[track.i]]>=Clow & dC[[track.i]]<=Chigh
        & dT[[track.i]]>=Tlow & dT[[track.i]]<=Thigh])
        Pirstartcount <- length((1:point.n[track.i])[PirStart[[track.i]] & dCdT[[track.i]]>=br[j] & dCdT[[track.i]]<br[j+1] & 
        dC[[track.i]]>=Clow & dC[[track.i]]<=Chigh & dT[[track.i]]>=Tlow & dT[[track.i]]<=Thigh])
        tempTotal <- tempTotal + Totaltime
        tempPircount <- tempPircount + Pirstartcount
      }
      RankedTotal[i,j] <<- tempTotal
      RankedPircount[i,j] <<- tempPircount
      if(!is.na(tempTotal) & tempTotal>=mincount){ RankedPir[i,j] <<- tempPircount/tempTotal }
    }
  }

  RankedPir.mean <<- sapply(1:xn,function(k) mean(RankedPir[,k],na.rm=T))
  RankedPir.sem <<- sapply(1:xn,function(k) sd(RankedPir[,k],na.rm=T))/sqrt(length(plates))
  plot(seq(xfrom+xby/2,xto-xby/2,length.out=xn),RankedPir.mean,ylim=ylim,xlab="dC/dT (mM/sec)",ylab="Pirouette frequency (/sec)",type="l",lwd=2)
  isna<- !is.na(RankedPir.sem) & RankedPir.sem!=0
  arrows(seq(xfrom+xby/2,xto-xby/2,length.out=xn)[isna], RankedPir.mean[isna], seq(xfrom+xby/2,xto-xby/2,length.out=xn)[isna], (RankedPir.mean+RankedPir.sem)[isna], length=0.05,angle=90)
  arrows(seq(xfrom+xby/2,xto-xby/2,length.out=xn)[isna], RankedPir.mean[isna], seq(xfrom+xby/2,xto-xby/2,length.out=xn)[isna], (RankedPir.mean-RankedPir.sem)[isna], length=0.05,angle=90)
}

#plotTRB
co$plotTRB<-'
plotTRB(tracks=1:track.n, plates=1:plate.n, division=12, timewindow=c(), type="all")
'
plotTRB <- function(tracks=1:track.n, plates=1:plate.n, division=12, timewindow=c(), type="all", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  B<-360/division*((1:division)-0.5)-180  
  if(type == "all"){
    TRlist<-vector("list",division)
    for(track.i in tracks){
      for(i in 1:point.n[track.i]){
        if(length(timewindow)!=0){
          if(dT[[track.i]][i] < timewindow[1] || dT[[track.i]][i] > timewindow[2]){ next}
        }
        if(!is.na(Bearing[[track.i]][i]) && !is.na(TurnRate[[track.i]][i]) && PirRun[[track.i]][i]=="R"){
          temp <- floor((Bearing[[track.i]][i]+180)/360*division)+1
          TRlist[[temp]] <- c(TRlist[[temp]], TurnRate[[track.i]][i])
        }
      }
    }
    TR <- c()
    for(i in 1:division) {TR<-c(TR, mean(TRlist[[i]]))}
    if(length(timewindow)==0){
    plot(B, TR, xlim=c(-180,180), xlab="Bearing", ylab="TurnRate")
    }else{
    plot(B, TR, xlim=c(-180,180), xlab="Bearing", ylab="TurnRate", main=paste("T=",timewindow[1],"-",timewindow[2]))
    }
    B1<<-B
    TR1<<-TR
    TRlist1<<-TRlist
  }
  if(type == "plate"){
    TR <- matrix(NA,plate.n,division)
    TRlist<-vector("list",division)
    for(plate.i in plates){
      for(track.i in plate.track[[plate.i]]){
        for(i in 1:point.n[track.i]){
          if(length(timewindow)!=0){
            if(dT[[track.i]][i] < timewindow[1] || dT[[track.i]][i] > timewindow[2]){ next}
          }
          if(!is.na(Bearing[[track.i]][i]) && !is.na(TurnRate[[track.i]][i]) && PirRun[[track.i]][i]=="R"){
            temp <- floor((Bearing[[track.i]][i]+180)/360*division)+1
            TRlist[[temp]] <- c(TRlist[[temp]], TurnRate[[track.i]][i])
          }
        }
      }
      for(i in 1:division) {TR[plate.i, i]<-mean(TRlist[[i]])}
    }
    TRmean <- sapply(1:division, function(x) mean(TR[plates,x]))
    #TRerror <- sapply(1:division, function(x) sqrt(sd(TR[plates,x]/(plate.n-1))))
    TRsd <- sapply(1:division, function(x) sqrt(sd(TR[plates,x])))
    if(length(timewindow)==0)
    {
      plot(B, TRmean, type = "b", xlim=c(-180,180), xlab="Bearing", ylab="TurnRate") #5.1 remove ylim=c(-5,8)
    }
    else
    {
      plot(B, TRmean, type = "b", xlim=c(-180,180), ylim=c(-5,8), xlab="Bearing", ylab="TurnRate", main=paste("T=",timewindow[1],"-",timewindow[2]))
    }
    arrows(B, TRmean, B, TRmean+TRsd, length=0)
    arrows(B, TRmean, B, TRmean-TRsd, length=0)
  }
}

#multiTRB
co$multiTRB<-'
multiTRB(step, division, mrow, mcol)
'
multiTRB <- function(step, division, mrow, mcol, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  par(mfrow=c(mrow,mcol), mai=c(0.7,0.7,0.5,0.3))
  for(i in 0:(division-1)){
    plotTRB(type="plate", timewindow=c(i*step,(i+1)*step))
  }
  par(mfrow=c(1,1), mai=c(1,1,1,1))
}

### calc.TRBs ###

co$calc.TRBs<-'
calc.TRBs(time.division=12, dist.division=4, sbear.division=10, tracks=1:track.n, type="plate")
'
calc.TRBs <- function(time.division=12, dist.division=4, sbear.division=10, tracks=1:track.n, type="plate", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  #pirhistc5 is modified
  x <- seq(-1+1/sbear.division, 1, by=2/sbear.division)
  plates=1:plate.n
  if(type=="all")
  {
    TRcount <<- array(0, dim=c(time.division, dist.division, sbear.division))
    TRsum <<- array(0, dim=c(time.division, dist.division, sbear.division)) 
    TR2sum <<- array(0, dim=c(time.division, dist.division, sbear.division)) 
    TRmean <<- array(NA, dim=c(time.division, dist.division, sbear.division)) 
    TRsd <<- array(NA, dim=c(time.division, dist.division, sbear.division)) 
    WVIndex <<- array(NA, dim=c(time.division, dist.division))
    plates <- 1:1
  }
  if(type=="plate") 
  {
    TRcountp <<- array(0, dim=c(time.division, dist.division, sbear.division, plate.n))
    TRsump <<- array(0, dim=c(time.division, dist.division, sbear.division, plate.n))  
    TRmeanp <<- array(NA, dim=c(time.division, dist.division, sbear.division, plate.n)) 
    WVIndexp <<- array(NA, dim=c(time.division, dist.division, plate.n))
  }
  for(plate.i in plates)
  {
    if(type=="plate")
    {
      tracks <- plate.track[[plate.i]]
    }
    
    for(track.i in tracks)
    {
	    for(i in 1:(point.n[track.i]))
	    {
	      if(Pirsurround[[track.i]][i] == "R")
	      {
	        # time rank
	        time.i <- ceiling(dT[[track.i]][i]/maxT*time.division)
	        if(time.i == 0) time.i <- 1
	        if(time.i>time.division) next
	        # dist rank
	        dist.i <- ceiling(Dist[[track.i]][i]/maxdist*dist.division)
	        if(dist.i == 0) dist.i <- 1
	        if(dist.i > dist.division) next
	        # sbear rank
	        if(!is.na(Bearing[[track.i]][i]) && !is.na(TurnRate[[track.i]][i]))
	        {
	          sbear.i <- ceiling((sin(Bearing[[track.i]][i]/180*pi)+1)/2*sbear.division)
	          if(sbear.i == 0) sbear.i <- 1
	          if(type=="all")
	          {
	            TRcount[time.i, dist.i, sbear.i] <<- TRcount[time.i, dist.i, sbear.i] + 1
	            TRsum[time.i, dist.i, sbear.i] <<- TRsum[time.i, dist.i, sbear.i] + TurnRate[[track.i]][i]
	            TR2sum[time.i, dist.i, sbear.i] <<- TR2sum[time.i, dist.i, sbear.i] + TurnRate[[track.i]][i]*TurnRate[[track.i]][i]
	          }
	          if(type=="plate")
	          {
	            TRcountp[time.i, dist.i, sbear.i, plate.i] <<- TRcountp[time.i, dist.i, sbear.i, plate.i] + 1
	            TRsump[time.i, dist.i, sbear.i, plate.i] <<- TRsump[time.i, dist.i, sbear.i, plate.i] + TurnRate[[track.i]][i]
	          }
	        }
	      }
	    }
    }

      TRmean <<- TRsum/TRcount
      TRmeanp[,,,plate.i] <<- TRsump[,,,plate.i]/TRcountp[,,,plate.i]
      for(time.i in 1:time.division)
      {
        for(dist.i in 1:dist.division)
        {
          if(type=="all")
          {
            y <- TRmean[time.i, dist.i,]
            a <- var(x,y,na.rm=T)/var(x[!is.na(y)])
            WVIndex[time.i, dist.i] <<- a
          }
          if(type=="plate")
          {
            y <- TRmeanp[time.i, dist.i,,plate.i]
            a <- var(x,y,na.rm=T)/var(x[!is.na(y)])
            WVIndexp[time.i, dist.i, plate.i] <<- a
          }
        }
      }
  }#
  
  cat("All tracks processed\n")
  if(type=="all"){
    # TRsd is sd
    TRsd[TRcount>=2] <<- sqrt(((TR2sum[TRcount>=2] - TRcount[TRcount>=2]*TRmean[TRcount>=2]*TRmean[TRcount>=2])/(TRcount[TRcount>=2]-1))/(TRcount[TRcount>=2]-1))
  }
  if(type=="all") cat(paste("\n", "2D array WVIndex, 3D arrays TRmean and TRsd etc were created.\n"))
  if(type=="plate") cat(paste("\n", "3D array WVIndexp and 4D arrays TRmeanp and TRsdp etc were created.\n"))
}

#plotTRBS
co$plotTRBS<-'
plotTRBS(time.n=1, dist.n=1, mrow, mcol,type="plate time")
'
plotTRBS <- function(time.n=1, dist.n=1, mrow, mcol, type="time", tbl){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  par(mfrow=c(mrow,mcol), mai=c(0.7,0.7,0.5,0.3))
  if(type=="plate time"){
	if(!exists("TRmeanp")){cat('No TRmeanp. Execute calc.TRBs(type="plate")\n');return()}
    sbear.division <- dim(TRmeanp)[3]
  } else {
    if(!exists("TRmean")){cat('No TRmean. Execute calc.TRBs(type="all")\n');return()}
    sbear.division <- dim(TRmean)[3]
  }
  sbear.mid <- sapply(1:sbear.division, function(i) 2/sbear.division*(i-0.5)-1)
  
  if(type=="time")
  {
    time.n <- dim(TRmean)[1]
    if(dist.n>dim(TRmean)[2]){cat("dist.n is larger than that set by calc.TRBs\n");return()}
    for(time.i in 1:time.n)
    {
      plot(sbear.mid, TRmean[time.i, dist.n, ], type = "b", xlim=c(-1,1), ylim=c(-8,8), xlab="sin Bearing", ylab="TurnRate", main=paste("T=",maxT/time.n*(time.i-1),"-",maxT/time.n*time.i))
      arrows(sbear.mid, TRmean[time.i, dist.n, ], sbear.mid, TRmean[time.i, dist.n, ]+TRsd[time.i, dist.n, ], length=0)
      arrows(sbear.mid, TRmean[time.i, dist.n, ], sbear.mid, TRmean[time.i, dist.n, ]-TRsd[time.i, dist.n, ], length=0)
    }
  }
  if(type=="dist")
  {
    dist.n <- dim(TRmean)[2]
    if(time.n>dim(TRmean)[1]){cat("time.n is larget than set by calc.TRBs\n");return()}
    for(dist.i in 1:dist.n)
    {
      plot(sbear.mid, TRmean[time.n, dist.i, ], type = "b", xlim=c(-1,1), ylim=c(-8,8), xlab="sin Bearing", ylab="TurnRate", main=paste("Distance=",maxdist/dist.n*(dist.i-1),"-",maxdist/dist.n*dist.i))
      arrows(sbear.mid, TRmean[time.n, dist.i, ], sbear.mid, TRmean[time.n, dist.i, ]+TRsd[time.n, dist.i, ], length=0)
      arrows(sbear.mid, TRmean[time.n, dist.i, ], sbear.mid, TRmean[time.n, dist.i, ]-TRsd[time.n, dist.i, ], length=0)
    }
  }
  if(type=="both")
  {
    time.n <- dim(TRmean)[1]
    dist.n <- dim(TRmean)[2]
    for(time.i in 1:time.n)
    {
      plot(c(),c(),xlim=c(-1,1), ylim=c(-8,8), xlab="sin Bearing", ylab="TurnRate", main=paste("T=",maxT/time.n*(time.i-1),"-",maxT/time.n*time.i))
      for(dist.i in 1:dist.n)
      {
        points(sbear.mid, TRmean[time.i, dist.i, ], type = "b", pch=dist.i)
        arrows(sbear.mid, TRmean[time.i, dist.i, ], sbear.mid, TRmean[time.i, dist.i, ]+TRsd[time.i, dist.i, ], length=0)
        arrows(sbear.mid, TRmean[time.i, dist.i, ], sbear.mid, TRmean[time.i, dist.i, ]-TRsd[time.i, dist.i, ], length=0)
      }
    }
  }
	if(type=="plate time")
	{
    time.n <- dim(TRmean)[1]
    if(dist.n>dim(TRmean)[2]){cat("dist.n is larger than set by calc.TRBs\n");return()}
	Mean <- array(NA, sbear.division)
 	Sd <- array(NA, sbear.division)
	for(time.i in 1:time.n)
	{
	for (sbear.i in 1:sbear.division)
	{
		Mean[sbear.i] <- mean(TRmeanp[time.i, dist.n, sbear.i, ])
		Sd[sbear.i] <- sd(TRmeanp[time.i, dist.n, sbear.i, ],na.rm = TRUE)
	}
      plot(sbear.mid, Mean, type = "b", xlim=c(-1,1), ylim=c(-8,8), xlab="sin Bearing", ylab="TurnRate", main=paste("T=",maxT/time.n*(time.i-1),"-",maxT/time.n*time.i))
      arrows(sbear.mid, Mean, sbear.mid, Mean+Sd, length=0)
      arrows(sbear.mid, Mean, sbear.mid, Mean-Sd, length=0)}
    }
  par(mfrow=c(1,1), mai=c(1,1,1,1))
}

#plotTRBcircles
co$plotTRBcircles<-'
plotTRBcircles(plates=1:plate.n)
'
plotTRBcircles <- function(plates=1:plate.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  if(!exists("WVIndexp")){cat('\nNo WVIndexp found. Execute calc.TRBs(type="plate") first.\n');return()}
  time.division=dim(WVIndexp)[1]
  dist.division=dim(WVIndexp)[2]
  WVIndexMean <<- apply(array(WVIndexp[,,plates],c(dim(WVIndexp)[1],dim(WVIndexp)[2],length(plates))), c(1,2), mean, na.rm=T) #130911改変：長さが1だとそのディメンションがなくなってしまうことを防ぐため。
  WVIndexSd <<- apply(array(WVIndexp[,,plates],c(dim(WVIndexp)[1],dim(WVIndexp)[2],length(plates))), c(1,2), sd, na.rm=T)
  WVIndexT <<- apply(array(WVIndexp[,,plates],c(dim(WVIndexp)[1],dim(WVIndexp)[2],length(plates))), c(1,2), function(x) if (length(which(!is.na(x)))>1) t.test(x)$p.value else NA)
  par(mfrow=c(1,1))
  plot(c(),c(),xlim=c(0,maxT),ylim=c(0,ceiling(maxdist/10)*10),xlab="Time(s)",ylab="Distance(mm)",main="Weathervane Index")
  for(time.i in 1:time.division)
  {
  for(dist.i in 1:dist.division)
  {
  if(is.na(WVIndexMean[time.i, dist.i]) || is.na(WVIndexT[time.i, dist.i]) || WVIndexT[time.i, dist.i] > 0.05)
    {
      text((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
      "N",cex=0.7)
    }
    else
    {
      if(WVIndexMean[time.i, dist.i]>0)
      {
        symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
        WVIndexMean[time.i, dist.i]*20,inches=F,add=T)
      }
      else
      {
        symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
        -WVIndexMean[time.i, dist.i]*20,inches=F,add=T,bg="black")
      }
    }
  }
  }
   write.table(as.data.frame(WVIndexMean),file="WVIndexmean.txt")
   write.table(as.data.frame(WVIndexSd),file="WVIndexsd.txt")
   write.table(as.data.frame(WVIndexT),file="WVIndexT.txt")
}


#TRBlist
# Not used
TRBlist <- function(tracks=1:track.n, plates=1:plate.n, division=12, timewindow=c(), type="all"){
B<-360/division*((1:division)-0.5)-180  
  if(type == "plate"){
    TR <- matrix(NA,plate.n,division)
    Blist2 <<- c()
    TRlist2 <<- c()
    TRlist<-vector("list",division)
    for(plate.i in plates){
      for(track.i in plate.track[[plate.i]]){
        for(i in 1:point.n[track.i]){
          if(length(timewindow)!=0){
            if(dT[[track.i]][i] < timewindow[1] || dT[[track.i]][i] > timewindow[2]){ next}
          }
          #if(!is.na(Bearing[[track.i]][i]) && !is.na(TurnRate[[track.i]][i]) && PirRun[[track.i]][i]=="R"){
          #if(PirRun[[track.i]][i]=="R"){
            #temp <- floor((Bearing[[track.i]][i]+180)/360*division)+1
            #TRlist[[temp]] <- c(TRlist[[temp]], TurnRate[[track.i]][i])
            TRlist2 <<- c(TRlist2, TurnRate[[track.i]][i])
            Blist2 <<- c(Blist2, Bearing[[track.i]][i])
          #}
        }
      }
      for(i in 1:division) {TR[plate.i, i]<-mean(TRlist[[i]])}
    }
    TRmean <- sapply(1:division, function(x) mean(TR[,x]))
  }
}

#plotTRAT
co$plotTRAT<-'
plotTRAT(tracks=1:track.n, plates=1:plate.n, division=12, timewindow=c(), type="all")
'
plotTRAT <- function(tracks=1:track.n, plates=1:plate.n, division=12, timewindow=c(), type="all", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  B<-360/division*((1:division)-0.5)-180  
  if(type == "all"){
  TRlist<-vector("list",division)
  for(track.i in tracks){
    for(i in 1:point.n[track.i]){
      if(length(timewindow)!=0){
        if(dT[[track.i]][i] < timewindow[1] || dT[[track.i]][i] > timewindow[2]){ next}
      }
      if(!is.na(AvTheta[[track.i]][i]) && !is.na(TurnRate[[track.i]][i]) && PirRun[[track.i]][i]=="R"){
        temp <- floor((AvTheta[[track.i]][i]+180)/360*division)+1
        TRlist[[temp]] <- c(TRlist[[temp]], TurnRate[[track.i]][i])
      }
    }
  }
  TR <- c()
  for(i in 1:division) {TR<-c(TR, mean(TRlist[[i]]))}
  if(length(timewindow)==0){
  plot(B, TR, xlim=c(-180,180), xlab="AvTheta", ylab="TurnRate")
  }else{
  plot(B, TR, xlim=c(-180,180), xlab="AvTheta", ylab="TurnRate", main=paste("T=",timewindow[1],"-",timewindow[2]))
  }
  B1<<-B
  TR1<<-TR
  TRlist1<<-TRlist
  }
  if(type == "plate"){
    TR <- matrix(NA,plate.n,division)
    TRlist<-vector("list",division)
    for(plate.i in plates){
      for(track.i in plate.track[[plate.i]]){
        for(i in 1:point.n[track.i]){
          if(length(timewindow)!=0){
            if(dT[[track.i]][i] < timewindow[1] || dT[[track.i]][i] > timewindow[2]){ next}
          }
          if(!is.na(AvTheta[[track.i]][i]) && !is.na(TurnRate[[track.i]][i]) && PirRun[[track.i]][i]=="R"){
            temp <- floor((AvTheta[[track.i]][i]+180)/360*division)+1
            TRlist[[temp]] <- c(TRlist[[temp]], TurnRate[[track.i]][i])
          }
        }
      }
      for(i in 1:division) {TR[plate.i, i]<-mean(TRlist[[i]])}
    }
    TRmean <- sapply(1:division, function(x) mean(TR[,x]))
    #TRerror <- sapply(1:division, function(x) sqrt(sd(TR[,x]/(plate.n-1))))
    TRsd <- sapply(1:division, function(x) sqrt(sd(TR[,x])))
    if(length(timewindow)==0){
    plot(B, TRmean, type = "b", xlim=c(-180,180), ylim=c(-5,8), xlab="AvTheta", ylab="TurnRate")
    }else{
    plot(B, TRmean, type = "b", xlim=c(-180,180), ylim=c(-5,8), xlab="AvTheta", ylab="TurnRate", main=paste("T=",timewindow[1],"-",timewindow[2]))
    }
    arrows(B, TRmean, B, TRmean+TRsd, length=0)
    arrows(B, TRmean, B, TRmean-TRsd, length=0)
  }
}

#multiTRAT
co$multiTRAT<-'
multiTRAT(step, division, mrow, mcol)
'
multiTRAT <- function(step, division, mrow, mcol, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  par(mfrow=c(mrow,mcol))
  for(i in 0:(division-1)){
    plotTRAT(type="plate", timewindow=c(i*step,(i+1)*step))
  }
  par(mfrow=c(1,1))
}


#bahist
co$bahist<-'
bahist(division=12)
'
bahist <- function(division=12, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  baThetaHist <<- matrix(0,division, division)
  for(i in 1:length(afterTheta)){
    if(!is.na(beforeTheta[i]) && !is.na(afterTheta[i])){
    b <- ((beforeTheta[i]+180)*division)%/%360
    if(b == division) b <- division-1
    a <- ((afterTheta[i]+180)*division)%/%360
    if(a == division) a <- division-1
    baThetaHist[b+1,a+1] <<- baThetaHist[b+1,a+1]+1
    }
  }
}


#pirhist
co$pirhist<-'
pirhist(timewindow=c(0,maxT), tracks=1:track.n)
'
pirhist <- function(timewindow=c(0,maxT), tracks=1:track.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  BPirStart <<- c()
  BRun <<- c()
  for(track.i in tracks){
    cat(paste("\r", track.i, "      "))
    timebool <- dT[[track.i]] >= timewindow[1] & dT[[track.i]] <= timewindow[2]
    BPirStart <<- c(BPirStart, Bearing[[track.i]][PirRun[[track.i]]=="R" & c(PirRun[[track.i]][-1]=="P",F) & timebool])
    BRun <<- c(BRun, Bearing[[track.i]][PirRun[[track.i]]=="R" & timebool])
  }
  cat(paste("\n", "vectors BPirStart and BRun were created.\n"))
}

#pirhist2
co$pirhist2<-'
pirhist2(timewindow=c(0,3600), tracks=1:track.n)
'
pirhist2 <- function(timewindow=c(0,maxT), tracks=1:track.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  BPirStart <<- c()
  BRun <<- c()
  for(track.i in tracks){
    cat(paste("\r", track.i, "      "))
    timebool <- dT[[track.i]] >= timewindow[1] & dT[[track.i]] <= timewindow[2]
    BPirStart <<- c(BPirStart, Bearing2[[track.i]][PirRun[[track.i]]=="R" & c(PirRun[[track.i]][-1]=="P",F) & timebool])
    BRun <<- c(BRun, Bearing2[[track.i]][PirRun[[track.i]]=="R" & timebool])
  }
  cat(paste("\n", "vectors BPirStart and BRun were created.\n"))
}


#pirhist3
co$pirhist3<-'
pirhist3(dist.division=4, timewindow=c(0,maxT), tracks=1:track.n)
'
pirhist3 <- function(dist.division=4, timewindow=c(0,maxT), tracks=1:track.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
 distbreaks <- (1:dist.division)*maxdist/dist.division
 BRund <<- list(); for(i in 1:(length(distbreaks)+1)){BRund <<- c(BRund, list(c()))}
 BPirStartd <<- list(); for(i in 1:(length(distbreaks)+1)){BPirStartd <<- c(BPirStartd, list(c()))} 
  for(track.i in tracks){
    cat(paste("\r", track.i, "      "))
    for(i in 1:(point.n[track.i]-1)){
      if(dT[[track.i]][i] >= timewindow[1] && dT[[track.i]][i] <= timewindow[2] && PirRun[[track.i]][i]=="R"){
      rank <- which(c(distbreaks > Dist[[track.i]][i], T))[1]
      BRund[[rank]] <<- c(BRund[[rank]], Bearing[[track.i]][i])
      if(PirRun[[track.i]][i+1]=="P"){
        BPirStartd[[rank]] <<- c(BPirStartd[[rank]], Bearing[[track.i]][i])
      }
    }
    }
  }
  cat(paste("\n", "lists BPirStartd and BRund were created.\n"))
}


#BPirplot
co$BPirplot<-'
BPirplot(maxprobab=0.05)
'
BPirplot<-function(maxprobab=0.05, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
   if(!exists("BPirStartd") || !exists("BRund")){
     cat("No BPirStartd, BRund pirhist3\n");return();
   }
   dist.division <- length(BPirStartd)-1
   par(mfrow=c(ceiling(sqrt(dist.division)),ceiling(sqrt(dist.division))), mai=c(0.7,0.7,0.4,0.2))
   distbreaks <- (1:dist.division)*maxdist/dist.division
   title <- paste("<",distbreaks[1],"mm",sep="")
   for(i in 2:length(distbreaks)) title<-c(title, paste(distbreaks[i-1],"mm-",distbreaks[i],"mm",sep=""))
   title <- c(title, paste(">",distbreaks[length(distbreaks)],"mm",sep=""))
   for(i in 1:(length(distbreaks)+1)){
   if(length(BPirStartd[[i]]) != 0 && length(BRund[[i]]) != 0){
     BP <- hist(BPirStartd[[i]], breaks=seq(-180,180,by=30), plot=F)$counts
     br <- hist(BPirStartd[[i]], breaks=seq(-180,180,by=30), plot=F)$mids
     BR <- hist(BRund[[i]], breaks=seq(-180,180,by=30), plot=F)$counts

     plot(br, BP/BR, ylim=c(0,maxprobab), xlim=c(-180,180), xlab="Bearing", 
     ylab="Probability of Pirouettes", main=title[i])
     }
   }
   par(mfrow=c(1,1), mai=c(1,1,1,1))
}


#BPirplotc2d
co$BPirplotc2d<-'
BPirplotc2d(maxprobab=0.05)
'
BPirplotc2d<-function(maxprobab=0.05, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
   if(!exists("BPirStartd") || !exists("BRund")){cat("No BPirStartd, BRund found. Execute pirhist3\n");return()}
   dist.division <- length(BPirStartd)-1
   par(mfrow=c(ceiling(sqrt(dist.division)),ceiling(sqrt(dist.division))), mai=c(0.8,0.8,0.5,0.3))
   pi <- acos(-1)
   cBPirStartd <- list()
   cBRund <- list()
   for(i in 1:dist.division){
     cBPirStartd <- c(cBPirStartd,list(cos(BPirStartd[[i]]/180*pi)))
     cBRund <- c(cBRund, list(cos(BRund[[i]]/180*pi)))
   }
   title<-c()
   for(i in 1:dist.division) title<-c(title, paste(maxdist/dist.division*(i-1),"mm-",maxdist/dist.division*i,"mm",sep=""))
   for(i in 1:dist.division){
   if(length(cBPirStartd[[i]]) != 0 && length(BRund[[i]]) != 0){
     BP <- hist(cBPirStartd[[i]], breaks=seq(-1,1,by=0.2), plot=F)$counts
     br <- hist(cBPirStartd[[i]], breaks=seq(-1,1,by=0.2), plot=F)$mids
     BR <- hist(cBRund[[i]], breaks=seq(-1,1,by=0.2), plot=F)$counts

     plot(br, BP/BR, ylim=c(0,maxprobab), xlim=c(-1,1), xlab="cos Bearing", 
     ylab="Probability of Pirouettes", main=title[i])
     }
   }
   par(mfrow=c(1,1), mai=c(1,1,1,1))
}


#pirhist4
co$pirhist4<-'
pirhist4(time.division=12, distance=c(0,maxdist), tracks=1:track.n)
'
pirhist4 <- function(time.division=12, distance=c(0,maxdist), tracks=1:track.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
 BRunt <<- list(); for(i in 1:time.division){BRunt <<- c(BRunt, list(c()))}
 BPirStartt <<- list(); for(i in 1:time.division){BPirStartt <<- c(BPirStartt, list(c()))} 
  for(track.i in tracks){
    cat(paste("\r", track.i, "      "))
    for(i in 1:(point.n[track.i]-1)){
      if(Dist[[track.i]][i] >= distance[1] && Dist[[track.i]][i] <= distance[2] && PirRun[[track.i]][i]=="R"){
      rank <- ceiling(dT[[track.i]][i]/maxT*time.division)
      if(rank == 0) rank<-1
      if(rank < 1 || rank>time.division) {print(paste("rank=",rank)); return()}
      BRunt[[rank]] <<- c(BRunt[[rank]], Bearing[[track.i]][i])
      if(PirRun[[track.i]][i+1]=="P"){
        BPirStartt[[rank]] <<- c(BPirStartt[[rank]], Bearing[[track.i]][i])
      }
    }
    }
  }
  cat(paste("\n", "lists BPirStartt and BRunt were created.\n"))
}


#BPirplot2
co$BPirplot2<-'
BPirplot2(maxprobab=0.05)
'
BPirplot2<-function(maxprobab=0.05, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
   if(!exists("BPirStartt") || !exists("BRunt")){cat("No BPirStartt, BRunt found. Execute pirhist4\n");return()}
   time.division=length(BPirStartt)
   par(mfrow=c(3,4), mai=c(0.6,0.6,0.3,0.1))
   title<-c()
   for(i in 1:time.division) title<-c(title, paste(maxT/time.division*(i-1),"s-",maxT/time.division*i,"s",sep=""))
   for(i in 1:time.division){
   if(length(BPirStartt[[i]]) != 0 && length(BRunt[[i]]) != 0){
     BP <- hist(BPirStartt[[i]], breaks=seq(-180,180,by=30), plot=F)$counts
     br <- hist(BPirStartt[[i]], breaks=seq(-180,180,by=30), plot=F)$mids
     BR <- hist(BRunt[[i]], breaks=seq(-180,180,by=30), plot=F)$counts

     plot(br, BP/BR, ylim=c(0,maxprobab), xlim=c(-180,180), xlab="Bearing", 
     ylab="Probability of Pirouettes", main=title[i])
     }
   }
   par(mfrow=c(1,1), mai=c(1,1,1,1))
}


#BPirplotc2t
co$BPirplotc2t<-'
BPirplotc2t(maxprobab=0.05)
'
BPirplotc2t<-function(maxprobab=0.05, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
   if(!exists("BPirStartt") || !exists("BRunt")){cat("No BPirStartt, BRunt found. Execute pirhist4\n");return()}
   time.division <- length(BPirStartt)
   par(mfrow=c(ceiling(sqrt(time.division)),ceiling(sqrt(time.division))), mai=c(0.6,0.6,0.3,0.1))
   pi <- acos(-1)
   cBPirStartt <- list()
   cBRunt <- list()
   for(i in 1:time.division){
     cBPirStartt <- c(cBPirStartt,list(cos(BPirStartt[[i]]/180*pi)))
     cBRunt <- c(cBRunt, list(cos(BRunt[[i]]/180*pi)))
   }
   title<-c()
   for(i in 1:time.division) title<-c(title, paste(maxT/time.division*(i-1),"s-",maxT/time.division*i,"s",sep=""))
   for(i in 1:time.division){
   if(length(cBPirStartt[[i]]) != 0 && length(BRunt[[i]]) != 0){
     BP <- hist(cBPirStartt[[i]], breaks=seq(-1,1,by=0.2), plot=F)$counts
     br <- hist(cBPirStartt[[i]], breaks=seq(-1,1,by=0.2), plot=F)$mids
     BR <- hist(cBRunt[[i]], breaks=seq(-1,1,by=0.2), plot=F)$counts

     plot(br, BP/BR, ylim=c(0,maxprobab), xlim=c(-1,1), xlab="cos Bearing", 
     ylab="Probability of Pirouettes", main=title[i])
     }
   }
   par(mfrow=c(1,1), mai=c(1,1,1,1))
}


#pirhist5
co$pirhist5<-'
pirhist5(time.division=12, dist.division=4, bear.division=12, tracks=1:track.n)
'
pirhist5 <- function(time.division=12, dist.division=4, bear.division=12, tracks=1:track.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
 Runcount <<- array(0, dim=c(time.division, dist.division, bear.division))
 Pircount <<- array(0, dim=c(time.division, dist.division, bear.division)) 
  over_maxdist<-0
  for(track.i in tracks){
    cat(paste("\r", track.i, "      "))
    for(i in 1:(point.n[track.i]-1)){
      if(PirRun[[track.i]][i]=="R"){
      # time rank
      time.i <- ceiling(dT[[track.i]][i]/maxT*time.division)
      if(time.i == 0) time.i<-1
      if(time.i < 1 || time.i>time.division) {print(paste("time.i=",time.i)); return()}
      # dist rank
      dist.i <- ceiling(Dist[[track.i]][i]/maxdist*dist.division)
      if(dist.i == 0) dist.i<-1
      if(dist.i < 1 || dist.i>dist.division){
        over_maxdist <- over_maxdist+1;
      }else{
	      # bear rank
	      if(!is.na(Bearing[[track.i]][i])){
	        bear.i <- ceiling((Bearing[[track.i]][i]+180)/360*bear.division)
	        if(bear.i == 0) bear.i<-1
	        if(bear.i < 1 || bear.i>bear.division) {print(paste("bear.i=",bear.i)); return()}
	        Runcount[time.i, dist.i, bear.i] <<- Runcount[time.i, dist.i, bear.i] +  (dT[[track.i]][i+1]-dT[[track.i]][i])
	        if(PirRun[[track.i]][i+1]=="P"){
	          Pircount[time.i, dist.i, bear.i] <<- Pircount[time.i, dist.i, bear.i] + 1
	        }
	      }
      }
    }
    }
  }
  cat(paste("\n", "Arrays Pircount and PirRun were created.\n"))
  if(over_maxdist>0){cat(paste(over_maxdist, " data points had distances over maxdist=", maxdist, "\n", sep=""))}
}


#BPirplot3
co$BPirplot3<-'
BPirplot3(maxprobab=0.05, mincount=50)
'
BPirplot3<-function(maxprobab=0.05, mincount=50, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
   if(!exists("Pircount")||!exists("Runcount")){cat("No Execute Pircount, Runcount found. Execute pirhist5");return()}
   time.division <- dim(Runcount)[1]
   dist.division <- dim(Runcount)[2]
   bear.division <- dim(Runcount)[3]
   par(mfrow=c(3,4), mai=c(0.6,0.6,0.3,0.1))
   title<-c()
   for(time.i in 1:time.division) title<-c(title, paste(maxT/time.division*(time.i-1),"s-",maxT/time.division*time.i,"s",sep=""))
   bear.mid <- sapply(1:bear.division, function(i) 360/bear.division*(i-0.5)-180)
   for(time.i in 1:time.division)
   {
     
     plot(c(),c(), ylim=c(0,maxprobab), xlim=c(-180,180), xlab="Bearing", 
     ylab="Probability of Pirouettes", main=title[time.i], type="b")
       for(dist.i in 1:dist.division)
       {
         if(min(Runcount[time.i,dist.i,]>=mincount)) points(bear.mid, Pircount[time.i,dist.i,]/Runcount[time.i,dist.i,], type="b", pch=dist.i)
       }
   }
   par(mfrow=c(1,1), mai=c(1,1,1,1))
}


#pirhistc5
co$pirhistc5<-'
pirhistc5(time.division=12, dist.division=4, cbear.division=10, tracks=1:track.n, plates=1:plate.n, type="all")
'
pirhistc5 <- function(time.division=12, dist.division=4, cbear.division=10, tracks=1:track.n, plates=1:plate.n, type="all", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
 if(type=="all"){
 Runcountc <<- array(0, dim=c(time.division, dist.division, cbear.division))
 Pircountc <<- array(0, dim=c(time.division, dist.division, cbear.division)) 
 plates <- 1:1
 }
 if(type=="plate"){
 Runcountcp <<- array(0, dim=c(time.division, dist.division, cbear.division, plate.n))
 Pircountcp <<- array(0, dim=c(time.division, dist.division, cbear.division, plate.n)) 
 }
  for(plate.i in plates){
  if(type=="plate") tracks <- plate.track[[plate.i]]
  for(track.i in tracks){
    cat(paste("\r", track.i, "      "))
    for(i in 1:(point.n[track.i]-1)){
      if(PirRun[[track.i]][i]=="R"){
      # time rank
      time.i <- ceiling(dT[[track.i]][i]/maxT*time.division)
      if(time.i == 0) time.i<-1
      if(time.i < 1 || time.i>time.division) {next}
      # dist rank
      dist.i <- ceiling(Dist[[track.i]][i]/maxdist*dist.division)
      if(dist.i == 0) dist.i<-1
      if(dist.i < 1 || dist.i>dist.division) {next}
      # cbear rank
      if(!is.na(Bearing[[track.i]][i])){
        cbear.i <- ceiling((cos(Bearing[[track.i]][i]/180*pi)+1)/2*cbear.division)
        if(cbear.i == 0) cbear.i<-1
        if(cbear.i < 1 || cbear.i>cbear.division) {print(paste("cbear.i=",cbear.i)); return()}
        if(type=="all"){
          Runcountc[time.i, dist.i, cbear.i] <<- Runcountc[time.i, dist.i, cbear.i] + (dT[[track.i]][i+1]-dT[[track.i]][i])
          if(PirRun[[track.i]][i+1]=="P"){
            Pircountc[time.i, dist.i, cbear.i] <<- Pircountc[time.i, dist.i, cbear.i] + 1
          }
        }
        else if(type=="plate")
        {
          Runcountcp[time.i, dist.i, cbear.i, plate.i] <<- Runcountcp[time.i, dist.i, cbear.i, plate.i] +  (dT[[track.i]][i+1]-dT[[track.i]][i])
          if(PirRun[[track.i]][i+1]=="P"){
            Pircountcp[time.i, dist.i, cbear.i, plate.i] <<- Pircountcp[time.i, dist.i, cbear.i, plate.i] + 1
          }
        }
      }
    }
    }
  }
  }
  if(type=="all") cat(paste("\n", "3D arrays Pircountc and Runcountc were created.\n"))
  if(type=="plate") cat(paste("\n", "4D arrays Pircountcp and Runcountcp were created.\n"))
}


#BPirplotc3
co$BPirplotc3<-'
BPirplotc3(maxprobab=0.05, mincount=50)
'
BPirplotc3<-function(maxprobab=0.05, mincount=50, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
   if(!exists("Pircountc")||!exists("Runcountc")){cat('No Pircountc, Runcountc found. Execute pirhistc5()\n');return()}
   time.division <- dim(Runcountc)[1]
   dist.division <- dim(Runcountc)[2]
   cbear.division <- dim(Runcountc)[3]
   par(mfrow=c(ceiling(sqrt(time.division)),ceiling(sqrt(time.division))), mai=c(0.6,0.6,0.3,0.1))
   title<-c()
   for(time.i in 1:time.division) title<-c(title, paste(maxT/time.division*(time.i-1),"s-",maxT/time.division*time.i,"s",sep=""))
   cbear.mid <- sapply(1:cbear.division, function(i) 2/cbear.division*(i-0.5)-1)
   for(time.i in 1:time.division)
   {
     
     plot(c(),c(), ylim=c(0,maxprobab), xlim=c(-1,1), xlab="cos Bearing", 
     ylab="Probability of Pirouettes", main=title[time.i], type="b")
       for(dist.i in 1:dist.division)
       {
         if(min(Runcountc[time.i,dist.i,]>=mincount)) points(cbear.mid, Pircountc[time.i,dist.i,]/Runcountc[time.i,dist.i,], type="b", pch=dist.i)
       }
   }
   par(mfrow=c(1,1), mai=c(1,1,1,1))
}


#BPirplotc3p
co$BPirplotc3p<-'
BPirplotc3p(maxprobab=0.05)
'
BPirplotc3p<-function(maxprobab=0.05, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
   if(!exists("Pircountcp")||!exists("Runcountcp")){cat('No Pircountcp, Runcountcp found. Execute pirhistc5(type="plates")\n');return()}
   time.division <- dim(Runcountcp)[1]
   dist.division <- dim(Runcountcp)[2]
   cbear.division <- dim(Runcountcp)[3]
   par(mfrow=c(ceiling(sqrt(time.division)),ceiling(sqrt(time.division))), mai=c(0.6,0.6,0.3,0.1))
   title<-c()
   PdivR <- array(NA, c(cbear.division, dist.division, plate.n))
   Mean <- array(NA, c(cbear.division, dist.division))
   Sd <- array(NA, c(cbear.division, dist.division))
   for(time.i in 1:time.division) title<-c(title, paste(maxT/time.division*(time.i-1),"s-",maxT/time.division*time.i,"s",sep=""))
   cbear.mid <- sapply(1:cbear.division, function(i) 2/cbear.division*(i-0.5)-1)
   for(time.i in 1:time.division)
   {
     for(dist.i in 1:dist.division){
		for(cbear.i in 1:cbear.division){
               PdivR[cbear.i, dist.i, ] <- Pircountcp[time.i, dist.i, cbear.i, ]/Runcountcp[time.i, dist.i, cbear.i, ]	
		       Mean[cbear.i, dist.i] <- mean(PdivR[cbear.i, dist.i, ])
               Sd[cbear.i, dist.i] <- sd(PdivR[cbear.i, dist.i,  ])
        }
     }
     plot(c(),c(), ylim=c(0,maxprobab), xlim=c(-1,1), xlab="cos Bearing", 
     ylab="Probability of Pirouettes", main=title[time.i], type="b")
     
       for(dist.i in 1:dist.division)
       {
         points(cbear.mid, Mean[,dist.i], type="b", pch=dist.i)
            arrows(cbear.mid, Mean[,dist.i],cbear.mid, Mean[,dist.i]+Sd[,dist.i], length=0)
            arrows(cbear.mid, Mean[,dist.i],cbear.mid, Mean[,dist.i]-Sd[,dist.i], length=0)
       }
   }
   par(mfrow=c(1,1), mai=c(1,1,1,1))
}

#calc.PirIndex
co$calc.PirIndex<-'
calc.PirIndex(plates=1:plate.n, type="all", chart="pirindex"))
'
calc.PirIndex<-function(plates=1:plate.n, type="all", chart="pirindex", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
 if(type=="all")
 {
  if(!exists("Pircountc")||!exists("Runcountc")){cat('No Pircountc, Runcountc found. Execute pirhistc5(type="all")\n');return()}
  time.division <- dim(Runcountc)[1]
  dist.division <- dim(Runcountc)[2]
  cbear.division <- dim(Runcountc)[3]
  PirIndex <<- matrix(NA, time.division, dist.division)
  BasalPir <<- matrix(NA, time.division, dist.division)
  x <- seq(-1+1/cbear.division, 1, by=2/cbear.division)
  for(time.i in 1:time.division)
  {
  for(dist.i in 1:dist.division)
  {
    y <- Pircountc[time.i, dist.i, ]/Runcountc[time.i, dist.i, ]
    a <- var(x,y,na.rm=T)/var(x[!is.na(y)])
    PirIndex[time.i, dist.i] <<- -a
    BasalPir[time.i, dist.i] <<- mean(y,na.rm=T)+a*mean(x[!is.na(y)])
  }
  }
  plot(c(),c(),xlim=c(0,maxT),ylim=c(0,ceiling(maxdist/10)*10),xlab="Time(s)",ylab="Distance(mm)",main="Pirouette Index")
  for(time.i in 1:time.division)
  {
  for(dist.i in 1:dist.division)
  {
    if(!is.na(PirIndex[time.i, dist.i]) && min(Runcountc[time.i, dist.i,])>=100)
    {
      if(PirIndex[time.i, dist.i]>0)
      {
        symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
        PirIndex[time.i, dist.i]*10000,inches=F,add=T)
      }
      else
      {
        symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
        -PirIndex[time.i, dist.i]*10000,inches=F,add=T,bg="black")
      }
    }
    else
    {
      text((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division,labels="+",cex=0.5)
    }
  }
  }
 }
 if(type=="plate")
 {
  if(!exists("Pircountcp")||!exists("Runcountcp")){cat('No Pircountcp, Runcountcpfound. Execute pirhistc5(type="plate")\n');return()}
  time.division <- dim(Runcountcp)[1]
  dist.division <- dim(Runcountcp)[2]
  cbear.division <- dim(Runcountcp)[3]
  PirIndexp <<- array(NA, c(time.division, dist.division, plate.n))
  BasalPirp <<- array(NA, c(time.division, dist.division, plate.n))
  for(plate.i in plates)
  {
  x <- seq(-1+1/cbear.division, 1, by=2/cbear.division)
  for(time.i in 1:time.division)
  {
  for(dist.i in 1:dist.division)
  {
    y <- Pircountcp[time.i, dist.i,,plate.i]/Runcountcp[time.i, dist.i,,plate.i]
    a <- var(x,y,na.rm=T)/var(x[!is.na(y)])
    PirIndexp[time.i, dist.i, plate.i] <<- -a
    BasalPirp[time.i, dist.i, plate.i] <<- mean(y,na.rm=T)+a*mean(x[!is.na(y)])
  }
  }
  }
  PirIndexMean <<- apply(array(PirIndexp[,,plates],c(dim(PirIndexp)[1],dim(PirIndexp)[2],length(plates))), c(1,2), mean, na.rm=T)
  PirIndexSd <<- apply(array(PirIndexp[,,plates],c(dim(PirIndexp)[1],dim(PirIndexp)[2],length(plates))), c(1,2), sd, na.rm=T)
  PirIndexT <<- apply(array(PirIndexp[,,plates],c(dim(PirIndexp)[1],dim(PirIndexp)[2],length(plates))), c(1,2), function(x) if (length(which(!is.na(x)))>1) t.test(x)$p.value else NA)
  BasalPirMean <<- apply(array(BasalPirp[,,plates],c(dim(BasalPirp)[1],dim(BasalPirp)[2],length(plates))), c(1,2), mean, na.rm=T)
  BasalPirSd <<- apply(array(BasalPirp[,,plates],c(dim(BasalPirp)[1],dim(BasalPirp)[2],length(plates))), c(1,2), sd, na.rm=T)
  BasalPirT <<- apply(array(BasalPirp[,,plates],c(dim(BasalPirp)[1],dim(BasalPirp)[2],length(plates))), c(1,2), function(x) if (length(which(!is.na(x)))>1) t.test(x)$p.value else NA)
  
   if(chart=="pirindex") plot(c(),c(),xlim=c(0,maxT),ylim=c(0,ceiling(maxdist/10)*10),xlab="Time(s)",ylab="Distance(mm)",main="Pirouette Index")
   if(chart=="basalpir") plot(c(),c(),xlim=c(0,maxT),ylim=c(0,ceiling(maxdist/10)*10),xlab="Time(s)",ylab="Distance(mm)",main="Basal Pirouette")
   for(time.i in 1:time.division)
   {
   for(dist.i in 1:dist.division)
   {
     if(chart=="pirindex")
     {
       if(is.na(PirIndexMean[time.i, dist.i]) || is.na(PirIndexT[time.i, dist.i]) || PirIndexT[time.i, dist.i] > 0.05)
       {
         symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
         0,inches=F,add=T,bg="black")
       }
       else
       {
         if(PirIndexMean[time.i, dist.i]>0)
         {
            #text((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division,PirIndexT[time.i, dist.i],cex=0.5) 
           symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
           PirIndexMean[time.i, dist.i]*10000,inches=F,add=T)
         }
         else
         {
           symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
           -PirIndexMean[time.i, dist.i]*10000,inches=F,add=T,bg="black")
         }
       }
     }
     if(chart=="basalpir")
     {
       if(is.na(BasalPirMean[time.i, dist.i]) || is.na(BasalPirT[time.i, dist.i]) || BasalPirT[time.i, dist.i] > 0.05)
       {
         symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
         0,inches=F,add=T,bg="black")
       }
       else
       {
         if(BasalPirMean[time.i, dist.i]>0)
         {
            #text((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division,BasalPirT[time.i, dist.i],cex=0.5) 
           symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
           BasalPirMean[time.i, dist.i]*4000,inches=F,add=T)
         }
         else
         {
           symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
           -BasalPirMean[time.i, dist.i]*4000,inches=F,add=T,bg="black")
         }
       }
     }
   }
   }
 
 write.table(as.data.frame(PirIndexMean),file="PirIndexMean.txt")
 write.table(as.data.frame(PirIndexSd),file="PirIndexSd.txt")
 write.table(as.data.frame(PirIndexT),file="PirIndexT.txt")
 write.table(as.data.frame(BasalPirMean),file="BasalPirMean.txt")
 write.table(as.data.frame(BasalPirSd),file="BasalPirSd.txt")
 write.table(as.data.frame(BasalPirT),file="BasalPirT.txt")
 }
}


#draw.PirIndex
co$draw.PirIndex<-'
draw.PirIndex(tracks=1:track.n, type="all", chart="pirindex"))
'
draw.PirIndex<-function(tracks=1:track.n, plates=1:plate.n, type="all", chart="pirindex", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
 if(type=="all")
 {
  time.division <- dim(PirIndex)[1]
  dist.division <- dim(PirIndex)[2]
  plot(c(),c(),xlim=c(0,maxT),ylim=c(0,ceiling(maxdist/10)*10),xlab="Time(s)",ylab="Distance(mm)",main="Pirouette Index")
  for(time.i in 1:time.division)
  {
  for(dist.i in 1:dist.division)
  {
    if(!is.na(PirIndex[time.i, dist.i]) && min(Runcountc[time.i, dist.i,])>=100)
    {
      if(PirIndex[time.i, dist.i]>0)
      {
        symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
        PirIndex[time.i, dist.i]*10000,inches=F,add=T)
      }
      else
      {
        symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
        -PirIndex[time.i, dist.i]*10000,inches=F,add=T,bg="black")
      }
    }
    else
    {
      text((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division,labels="+",cex=0.5)
    }
  }
  }
 }
 if(type=="plate")
 {
  
   if(chart=="pirindex") plot(c(),c(),xlim=c(0,maxT),ylim=c(0,ceiling(maxdist/10)*10),xlab="Time(s)",ylab="Distance(mm)",main="Pirouette Index")
   if(chart=="basalpir") plot(c(),c(),xlim=c(0,maxT),ylim=c(0,ceiling(maxdist/10)*10),xlab="Time(s)",ylab="Distance(mm)",main="Basal Pirouette")
   time.division <- dim(PirIndexMean)[1]
   dist.division <- dim(PirIndexMean)[2]
   for(time.i in 1:time.division)
   {
   for(dist.i in 1:dist.division)
   {
     if(chart=="pirindex")
     {
       if(is.na(PirIndexMean[time.i, dist.i]) || is.na(PirIndexT[time.i, dist.i]) || PirIndexT[time.i, dist.i] > 0.05)
       {
         symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
         0,inches=F,add=T,bg="black")
       }
       else
       {
         if(PirIndexMean[time.i, dist.i]>0)
         {
            #text((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division,PirIndexT[time.i, dist.i],cex=0.5) 
           symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
           PirIndexMean[time.i, dist.i]*10000,inches=F,add=T)
         }
         else
         {
           symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
           -PirIndexMean[time.i, dist.i]*10000,inches=F,add=T,bg="black")
         }
       }
     }
     if(chart=="basalpir")
     {
       if(is.na(BasalPirMean[time.i, dist.i]) || is.na(BasalPirT[time.i, dist.i]) || BasalPirT[time.i, dist.i] > 0.05)
       {
         symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
         0,inches=F,add=T,bg="black")
       }
       else
       {
         if(BasalPirMean[time.i, dist.i]>0)
         {
            #text((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division,BasalPirT[time.i, dist.i],cex=0.5) 
           symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
           BasalPirMean[time.i, dist.i]*4000,inches=F,add=T)
         }
         else
         {
           symbols((time.i-1/2)*maxT/time.division, (dist.i-1/2)*maxdist/dist.division, 
           -BasalPirMean[time.i, dist.i]*4000,inches=F,add=T,bg="black")
         }
       }
     }
   }
   }
 }
 
}

#plot.PirdCdT
co$plot.PirdCdT<-'
plot.PirdCdT(from=-0.1, to=0.1, by=0.02, Cfrom=NA, Cto=NA, ylim=NA)
'
plot.PirdCdT <- function(from=-0.1, to=0.1, by=0.02, Cfrom=NA, Cto=NA, ylim=NA, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  if(is.na(Cfrom) && is.na(Cto)){main=""} else{main<-paste("C= ",Cfrom,"-",Cto," mM",sep="")}
  if(is.na(ylim[1])){
  plot(seq(from+by/2,to-by/2,by), plot.PirdCdT.func1(from,to,by,i,Cfrom,Cto), type="b", xlim=c(from,to), xlab="dC/dT (mM/sec)", ylab="Piruette probability", main=main)
  }
  else{
  plot(seq(from+by/2,to-by/2,by), plot.PirdCdT.func1(from,to,by,i,Cfrom,Cto), type="b", xlim=c(from,to), xlab="dC/dT (mM/sec)", ylab="Piruette probability", ylim=ylim, main=main)
  }
}
#For plot.PirdCdT
plot.PirdCdT.func1<-function(from,to,by,i,Cfrom,Cto){
  unlistdCdT <- unlist(dCdT)
  if(is.na(Cfrom) && is.na(Cto)){
  total <- hist(unlistdCdT[unlist(PirRun)=="R" & unlistdCdT>=from & unlistdCdT<=to], seq(from,to,by),plot=F)$counts
  pirstartdCdT <- unlist(lapply(1:track.n, function(track.i) dCdT[[track.i]][PirRun[[track.i]][-1]=="P" & PirRun[[track.i]][-point.n[track.i]]!="P"]))
  pir <- hist(pirstartdCdT[pirstartdCdT>=from & pirstartdCdT<=to],seq(from,to,by),plot=F)$counts
  pir/total
  }
  else if(!is.na(Cfrom) && is.na(Cto)){
  total <- hist(unlistdCdT[unlist(PirRun)=="R" & unlistdCdT>=from & unlistdCdT<=to & unlist(dC)>=Cfrom], seq(from,to,by),plot=F)$counts
  pirstartdCdT <- unlist(lapply(1:track.n, function(track.i) dCdT[[track.i]][PirRun[[track.i]][-1]=="P" & PirRun[[track.i]][-point.n[track.i]]!="P" & dC[[track.i]][-point.n[track.i]]>=Cfrom]))
  pir <- hist(pirstartdCdT[pirstartdCdT>=from & pirstartdCdT<=to],seq(from,to,by),plot=F)$counts
  pir/total
  }
  else if(is.na(Cfrom) && !is.na(Cto)){
  total <- hist(unlistdCdT[unlist(PirRun)=="R" & unlistdCdT>=from & unlistdCdT<=to & unlist(dC)<=Cto], seq(from,to,by),plot=F)$counts
  pirstartdCdT <- unlist(lapply(1:track.n, function(track.i) dCdT[[track.i]][PirRun[[track.i]][-1]=="P" & PirRun[[track.i]][-point.n[track.i]]!="P" & dC[[track.i]][-point.n[track.i]]<=Cto]))
  pir <- hist(pirstartdCdT[pirstartdCdT>=from & pirstartdCdT<=to],seq(from,to,by),plot=F)$counts
  pir/total
  }
  else if(!is.na(Cfrom) && !is.na(Cto)){
  total <- hist(unlistdCdT[unlist(PirRun)=="R" & unlistdCdT>=from & unlistdCdT<=to & unlist(dC)>=Cfrom & unlist(dC)<=Cto], seq(from,to,by),plot=F)$counts
  pirstartdCdT <- unlist(lapply(1:track.n, function(track.i) dCdT[[track.i]][PirRun[[track.i]][-1]=="P" & PirRun[[track.i]][-point.n[track.i]]!="P" & dC[[track.i]][-point.n[track.i]]>=Cfrom & dC[[track.i]][-point.n[track.i]]<=Cto]))
  pir <- hist(pirstartdCdT[pirstartdCdT>=from & pirstartdCdT<=to],seq(from,to,by),plot=F)$counts
  pir/total
  }
}

#plot.PirdCdTMulti
co$plot.PirdCdTMulti<-'
plot.PirdCdTMulti(from=-0.1, to=0.1, by=0.02, Cfrom, Cto, Cstep, ylim=NA)
'
plot.PirdCdTMulti<-function(from=-0.1, to=0.1, by=0.02, Cfrom, Cto, Cstep, ylim=NA, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  n<-ceiling((Cto-Cfrom)/Cstep)
  col<-ceiling(sqrt(n))
  par(mfrow=c(ceiling(n/col),col))
  for(Cf in seq(Cfrom,Cto-Cstep,by=Cstep)){
    plot.PirdCdT(from, to, by, Cf, Cf+Cstep, ylim)
  }
  par(mfrow=c(1,1))
}

#plot.PirCdCdT
co$plot.PirCdCdT<-'
plot.PirCdCdT(xfrom=30, xto=100, xby=2.5, yfrom=-0.6, yto=0.6, yby=0.025, maxprobab=0.1, cutoff=20, 
persp=FALSE, Tfrom=0, Tto=maxT, tracks=1:track.n, legend = TRUE)
'
plot.PirCdCdT <- function(xfrom=30, xto=100, xby=2.5, yfrom=-0.6, yto=0.6, yby=0.025, 
                 maxprobab=0.1, cutoff=20, persp=FALSE, Tfrom=0, Tto=maxT, tracks=1:track.n, legend = TRUE, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }

  xn <- ceiling((xto-xfrom)/xby)
  yn <- ceiling((yto-yfrom)/yby)
  total <<- matrix(0, xn, yn)
  pir <<- matrix(0, xn, yn)
  for(track.i in tracks){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      xi <- ceiling((dC[[track.i]][point.i]-xfrom)/xby)
      yi <- ceiling((dCdT[[track.i]][point.i]-yfrom)/yby)
      if(xi>=1 && xi<=xn && yi>=1 && yi<=yn && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){
        if(PirRun[[track.i]][point.i]=="R"){
          total[xi,yi]<<-total[xi,yi]+(dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
        }
        if(PirRun[[track.i]][point.i]=="P" && PirRun[[track.i]][point.i-1]!="P"){
          total[xi,yi]<<-total[xi,yi]+(dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          pir[xi,yi]<<-pir[xi,yi]+1
        }
      }
    }
    }
  }
  probabCdCdT <<- pir/total
  probabCdCdT[total<cutoff]<<-NA
  probabCdCdT.xfrom<<-xfrom
  probabCdCdT.xto<<-xto
  probabCdCdT.xby<<-xby
  probabCdCdT.yfrom<<-yfrom
  probabCdCdT.yto<<-yto
  probabCdCdT.yby<<-yby
  probabCdCdT.mean<<-mean(probabCdCdT,na.rm=T)

  #if(maxprobab<=0){maxprobab <- max(probab,na.rm=T)}
  power <- floor(log10(maxprobab))
  maxprobab <- ceiling(maxprobab/10^power)*10^power
  probab <- probabCdCdT/maxprobab
  probab[probab>1]<-1
  cx=1
  if(persp) {
  par(mfrow=c(2,2))
  persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),total,main="Total Run Points",xlab="C (mM)", ylab="dC/dT (mM/sec)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),pir,main="Pirouette Occurence",xlab="C (mM)", ylab="dC/dT (mM/sec)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  cx=0.5
  }
  #palette(rainbow(20,start=0.67,end=1.0))
  palette(rev(rainbow(20,start=1.0,end=0.67)))
  #image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), total, main="total", zlim=c(1,10000), xlab="C (mM)", ylab="dC/dT (mM/sec)", col=rainbow(20,start=0.67,end=1.0))#, col=gray(seq(1,0,by=-0.05)))
  #image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), pir, main="pir", zlim=c(1,100), xlab="C (mM)", ylab="dC/dT (mM/sec)", col=1:20) #col=rainbow(20,start=0.67,end=1.0))#, col=gray(seq(1,0,by=-0.05)))
  par(mar=c(5,5.5,1.5,2))
  image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), probab, 
  zlim=c(0,1), xlab="C (mM)", ylab="dC/dT (mM/sec)", col=1:20, font.lab=2, cex.axis=2, cex.lab=2)
  if(legend){
  legend(xto, yto-(yto-yfrom)/20, c(paste(maxprobab,"/sec",sep=""),rep("",18),0), xjust=1, x.intersp=0.5, fill=20:1, bty="n",y.intersp=0.5,border=20:1,cex=cx)
  #legend(xto, yto+(yto-yfrom)/40, c(paste(maxprobab,"/sec",sep=""),rep("",18),0), xjust=1, x.intersp=0.5, fill=20:1, bty="n",y.intersp=0.5,border=20:1,cex=2)
  }
  par(mfrow=c(1,1))
}


#calc.PirCdCdT
co$calc.PirCdCdT<-'
calc.PirCdCdT(Cfrom=20, Cto=100, yfrom=-0.6, yto=0.6, yby=0.025, 
Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="")
'
calc.PirCdCdT <- function(Cfrom=20, Cto=100, yfrom=-0.6, yto=0.6, yby=0.025, 
                 Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  plate.number <- 1
  if(plate){plate.number <- plate.n}

  yn <- ceiling((yto-yfrom)/yby)
  
  total <- matrix(0, plate.number, yn)
  pir <- matrix(0, plate.number, yn)
  count <- matrix(0, plate.number, yn)
  
  for(plate.i in 1:plate.number){
  for(track.i in plate.track[[plate.i]]){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      if(dC[[track.i]][point.i]>=Cfrom && dC[[track.i]][point.i]<=Cto){
      yi <- ceiling((dCdT[[track.i]][point.i]-yfrom)/yby)
      if(yi>=1 && yi<=yn && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){
        if(PirRun[[track.i]][point.i]=="R"){
          total[plate.i,yi]<-total[plate.i,yi]+(dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i,yi]<-count[plate.i,yi]+1
        }
        if(PirRun[[track.i]][point.i]=="P" && PirRun[[track.i]][point.i-1]!="P"){
          total[plate.i,yi]<-total[plate.i,yi]+(dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i,yi]<-count[plate.i,yi]+1
          pir[plate.i,yi]<-pir[plate.i,yi]+1
        }
      }
      }
    }
    }
  }
  }
  colnames(total) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(pir) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(count) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  if(fileout != ""){
    write.csv(file=paste(fileout,"_total.csv",sep=""), total, row.names=F)
    write.csv(file=paste(fileout,"_pir.csv",sep=""), pir, row.names=F)
    write.csv(file=paste(fileout,"_count.csv",sep=""), count, row.names=F)
  }
  list(total=total, pir=pir, count=count)
}


#calc.PirCdCdl
co$calc.PirCdCdl<-'
calc.PirCdCdl(Cfrom=20, Cto=100, yfrom=-2, yto=2, yby=0.5, 
Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="")
'
calc.PirCdCdl <- function(Cfrom=20, Cto=100, yfrom=-2, yto=2, yby=0.5, 
                 Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  plate.number <- 1
  if(plate){plate.number <- plate.n}

  yn <- ceiling((yto-yfrom)/yby)
  
  total <- matrix(0, plate.number, yn)
  pir <- matrix(0, plate.number, yn)
  count <- matrix(0, plate.number, yn)
  
  for(plate.i in 1:plate.number){
  for(track.i in plate.track[[plate.i]]){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      if(dC[[track.i]][point.i]>=Cfrom && dC[[track.i]][point.i]<=Cto
      && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){ # prerequisite
        yi <- ceiling(((dC[[track.i]][point.i]-dC[[track.i]][point.i-1])/dL[[track.i]][point.i-1]-yfrom)/yby+0.5)
        if(!is.nan(yi) && !is.na(yi) && yi>=1 && yi<=yn){
        if(PirRun[[track.i]][point.i]=="R"){
          total[plate.i, yi] <- total[plate.i, yi] + (dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i, yi] <- count[plate.i, yi] + 1
        }
        if(PirStart[[track.i]][point.i]){
          total[plate.i, yi] <- total[plate.i, yi] + (dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i, yi] <- count[plate.i, yi] + 1
          pir[plate.i, yi] <- pir[plate.i, yi] + 1
        }
        }
      } # end of if dC
    }
    }
  }
  }
  colnames(total) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(pir) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(count) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  if(fileout != ""){
    write.csv(file=paste(fileout,"_total.csv",sep=""), total, row.names=F)
    write.csv(file=paste(fileout,"_pir.csv",sep=""), pir, row.names=F)
    write.csv(file=paste(fileout,"_count.csv",sep=""), count, row.names=F)
  }
  list(total=total, pir=pir, count=count)
}


#calc.TurnCdCdl
co$calc.TurnCdCdl<-'
calc.TurnCdCdl(Cfrom=20, Cto=100, yfrom=-2, yto=2, yby=0.5, 
Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="")
'
calc.TurnCdCdl <- function(Cfrom=20, Cto=100, yfrom=-2, yto=2, yby=0.5, 
                 Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  plate.number <- 1
  if(plate){plate.number <- plate.n}

  yn <- ceiling((yto-yfrom)/yby)
  
  total <- matrix(0, plate.number, yn)
  turn <- matrix(0, plate.number, yn)
  count <- matrix(0, plate.number, yn)
  
  for(plate.i in 1:plate.number){
  for(track.i in plate.track[[plate.i]]){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      if(dC[[track.i]][point.i]>=Cfrom && dC[[track.i]][point.i]<=Cto
      && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){ # prerequisite
        yi <- ceiling(((dC[[track.i]][point.i]-dC[[track.i]][point.i-1])/dL[[track.i]][point.i-1]-yfrom)/yby+0.5)
        if(!is.nan(yi) && !is.na(yi) && yi>=1 && yi<=yn){
        if(TurnRun[[track.i]][point.i]=="R"){
          total[plate.i, yi] <- total[plate.i, yi] + (dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i, yi] <- count[plate.i, yi] + 1
        }
        if(TurnRun[[track.i]][point.i]=="T" && TurnRun[[track.i]][point.i-1]=="R"){
          total[plate.i, yi] <- total[plate.i, yi] + (dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i, yi] <- count[plate.i, yi] + 1
          turn[plate.i, yi] <- turn[plate.i, yi] + 1
        }
        }
      } # end of if dC
    }
    }
  }
  }
  colnames(total) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(turn) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(count) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  if(fileout != ""){
    write.csv(file=paste(fileout,"_total.csv",sep=""), total, row.names=F)
    write.csv(file=paste(fileout,"_turn.csv",sep=""), turn, row.names=F)
    write.csv(file=paste(fileout,"_count.csv",sep=""), count, row.names=F)
  }
  list(total=total, turn=turn, count=count)
}


#calc.PirACdCdl
co$calc.PirACdCdl<-'
calc.PirACdCdl(Cfrom=20, Cto=100, yfrom=-2, yto=2, yby=0.5, 
Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="")
'
calc.PirACdCdl <- function(Cfrom=20, Cto=100, yfrom=-2, yto=2, yby=0.5, 
                 Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  plate.number <- 1
  if(plate){plate.number <- plate.n}

  yn <- ceiling((yto-yfrom)/yby)
  
  total <- matrix(0, plate.number, yn)
  pir <- matrix(0, plate.number, yn)
  count <- matrix(0, plate.number, yn)
  
  for(plate.i in 1:plate.number){
  for(track.i in plate.track[[plate.i]]){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      if(dC[[track.i]][point.i]>=Cfrom && dC[[track.i]][point.i]<=Cto
      && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){ # prerequisite
        yi <- ceiling(((dC[[track.i]][point.i]-dC[[track.i]][point.i-1])/dL[[track.i]][point.i-1]-yfrom)/yby+0.5)
        if(!is.nan(yi) && !is.na(yi) && yi>=1 && yi<=yn){
        if(PirRunA[[track.i]][point.i]=="R"){
          total[plate.i, yi] <- total[plate.i, yi] + (dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i, yi] <- count[plate.i, yi] + 1
        }
        if(PirStartA[[track.i]][point.i]){
          total[plate.i, yi] <- total[plate.i, yi] + (dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i, yi] <- count[plate.i, yi] + 1
          pir[plate.i, yi] <- pir[plate.i, yi] + 1
        }
        }
      } # end of if dC
    }
    }
  }
  }
  colnames(total) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(pir) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(count) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  if(fileout != ""){
    write.csv(file=paste(fileout,"_total.csv",sep=""), total, row.names=F)
    write.csv(file=paste(fileout,"_pir.csv",sep=""), pir, row.names=F)
    write.csv(file=paste(fileout,"_count.csv",sep=""), count, row.names=F)
  }
  list(total=total, pir=pir, count=count)
}


#calc.TurnACdCdl
co$calc.TurnACdCdl<-'
calc.TurnACdCdl(Cfrom=20, Cto=100, yfrom=-2, yto=2, yby=0.5, 
Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="")
'
calc.TurnACdCdl <- function(Cfrom=20, Cto=100, yfrom=-2, yto=2, yby=0.5, 
                 Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, fileout="", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  plate.number <- 1
  if(plate){plate.number <- plate.n}

  yn <- ceiling((yto-yfrom)/yby)
  
  total <- matrix(0, plate.number, yn)
  turn <- matrix(0, plate.number, yn)
  count <- matrix(0, plate.number, yn)
  
  for(plate.i in 1:plate.number){
  for(track.i in plate.track[[plate.i]]){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      if(dC[[track.i]][point.i]>=Cfrom && dC[[track.i]][point.i]<=Cto
      && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){ # prerequisite
        yi <- ceiling(((dC[[track.i]][point.i]-dC[[track.i]][point.i-1])/dL[[track.i]][point.i-1]-yfrom)/yby+0.5)
        if(!is.nan(yi) && !is.na(yi) && yi>=1 && yi<=yn){
        if(TurnRunA[[track.i]][point.i]=="R"){
          total[plate.i, yi] <- total[plate.i, yi] + (dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i, yi] <- count[plate.i, yi] + 1
        }
        if(TurnRunA[[track.i]][point.i]=="T" && TurnRunA[[track.i]][point.i-1]=="R"){
          total[plate.i, yi] <- total[plate.i, yi] + (dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          count[plate.i, yi] <- count[plate.i, yi] + 1
          turn[plate.i, yi] <- turn[plate.i, yi] + 1
        }
        }
      } # end of if dC
    }
    }
  }
  }
  colnames(total) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(turn) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  colnames(count) <- seq(yfrom+yby/2, by=yby, length.out=yn)
  if(fileout != ""){
    write.csv(file=paste(fileout,"_total.csv",sep=""), total, row.names=F)
    write.csv(file=paste(fileout,"_turn.csv",sep=""), turn, row.names=F)
    write.csv(file=paste(fileout,"_count.csv",sep=""), count, row.names=F)
  }
  list(total=total, turn=turn, count=count)
}


#calc.TRCdCdLat
co$calc.TRCdCdLat<-'
calc.TRCdCdLat(Cfrom=20, Cto=100, yfrom=-3, yto=3, yby=0.2, Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T)
'
calc.TRCdCdLat <- function(Cfrom=20, Cto=100, yfrom=-3, yto=3, yby=0.2, 
                  Tfrom=0, Tto=maxT, tracks=1:track.n, plate=T, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  plate.number <- 1
  if(plate){plate.number <- plate.n}
  
  yn <- ceiling((yto-yfrom)/yby)
  TRsum <<- matrix(0, plate.number, yn)
  TRcount <<- matrix(0, plate.number, yn)
  for(plate.i in 1:plate.number){
  for(track.i in plate.track[[plate.i]]){
    for(point.i in 1:point.n[track.i]){
      if(dC[[track.i]][point.i]>=Cfrom && dC[[track.i]][point.i]<=Cto){
      yi <- ceiling((dCdLat[[track.i]][point.i]-yfrom)/yby)
      if(!is.na(TurnRate[[track.i]][point.i]) && !is.na(dCdLat[[track.i]][point.i]) 
         && yi>=1 && yi<=yn && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){
        TRcount[plate.i,yi]<<-TRcount[plate.i,yi]+1
        TRsum[plate.i,yi]<<-TRsum[plate.i,yi]+TurnRate[[track.i]][point.i]
      }
      }
    }
  }
  }
list(TRcount=TRcount, TRsum=TRsum)
}



#plot.PirCdCdTMulti
co$plot.PirCdCdTMulti<-'
plot.PirCdCdTMulti(xfrom=30, xto=100, xby=2.5, yfrom=-0.6, yto=0.6, yby=0.025, maxprobab=0.1, cutoff=20, 
persp=FALSE, Tfrom=0, Tto=maxT, Tspan=600, saveplot=TRUE, tracks=1:track.n)
'
plot.PirCdCdTMulti <- function(xfrom=30, xto=100, xby=2.5, yfrom=-0.6, yto=0.6, yby=0.025, 
                 maxprobab=0.1, cutoff=20, persp=FALSE, Tfrom=0, Tto=maxT, Tspan=600, saveplot=TRUE, tracks=1:track.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  Tn <- ceiling((Tto-Tfrom)/Tspan)
  xn <- ceiling((xto-xfrom)/xby)
  yn <- ceiling((yto-yfrom)/yby)
  probabCdCdT.Tfrom <<- Tfrom
  probabCdCdT.Tto <<- Tto
  probabCdCdT.Tspan <<- Tspan
  probabCdCdTT <<- array(NA, c(xn, yn, Tn))
  probabCdCdTT.mean <<- rep(NA, Tn)
  for(Ti in 1:Tn){
    plot.PirCdCdT(xfrom=xfrom, xto=xto, xby=xby, yfrom=yfrom, yto=yto, yby=yby, 
                  maxprobab=maxprobab, cutoff=cutoff, persp=persp, Tfrom=(Ti-1)*Tspan, Tto=min(Ti*Tspan, Tto), tracks)
    probabCdCdTT[,,Ti] <<- probabCdCdT
    probabCdCdTT.mean[Ti] <<- probabCdCdT.mean
    if(saveplot) savePlot(paste("plot.PirCdCdT(T_", (Ti-1)*Tspan, "-", min(Ti*Tspan, Tto), ").tiff", sep=""),"tiff")
  }
}

#clip.probabCdCdTT
co$clip.probabCdCdTT<-'
clip.probabCdCdTT(title, replace=NA, xfrom=NA, xto=NA, yfrom=NA, yto=NA, Tfrom=NA, Tto=NA
                   maxprobab=0.1, cutoff=20, Tfrom=0, saveplot=TRUE)
'
clip.probabCdCdTT<-function(title, replace=NA, xfrom=NA, xto=NA, yfrom=NA, yto=NA, Tfrom=NA, Tto=NA,
                   maxprobab=0.1, cutoff=20, Tspan=600, saveplot=TRUE, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  if(is.na(xfrom)){
    ifrom <- 1
  }else{
    ifrom <- floor((xfrom-probabCdCdT.xfrom)/probabCdCdT.xby + 1)
  }
  if(ifrom<1) ifrom<-1
  if(is.na(xto)){
    ito <- ceiling((probabCdCdT.xto-probabCdCdT.xfrom)/probabCdCdT.xby)
  }else{
    ito <- ceiling((xto-probabCdCdT.xfrom)/probabCdCdT.xby)
  }
  if(ito>ceiling((probabCdCdT.xto-probabCdCdT.xfrom)/probabCdCdT.xby)) ito<-ceiling((probabCdCdT.xto-probabCdCdT.xfrom)/probabCdCdT.xby)

  if(is.na(yfrom)){
    jfrom <- 1
  }else{
    jfrom <- floor((yfrom-probabCdCdT.yfrom)/probabCdCdT.yby + 1)
  }
  if(jfrom<1) jfrom<-1
  if(is.na(yto)){
    jto <- ceiling((probabCdCdT.yto-probabCdCdT.yfrom)/probabCdCdT.yby)
  }else{
    jto <- ceiling((yto-probabCdCdT.yfrom)/probabCdCdT.yby)
  }
  if(jto>ceiling((probabCdCdT.yto-probabCdCdT.yfrom)/probabCdCdT.yby)) jto<-ceiling((probabCdCdT.yto-probabCdCdT.yfrom)/probabCdCdT.yby)

  if(is.na(Tfrom)) Tfrom<-1
  if(is.na(Tto)) Tto<-dim(probabCdCdTT)[3]
  for(Ti in Tfrom:Tto){
    if(is.na(replace)){
      probabCdCdTT[ifrom:ito,jfrom:jto,Ti][!is.na(probabCdCdTT[ifrom:ito,jfrom:jto,Ti])]<<-probabCdCdTT.mean[Ti][!is.na(probabCdCdTT[ifrom:ito,jfrom:jto,Ti])]
    }else{
      probabCdCdTT[ifrom:ito,jfrom:jto,Ti][!is.na(probabCdCdTT[ifrom:ito,jfrom:jto,Ti])]<<-replace
    }
  }
  #if(is.na(replace)){
  #  probabCdCdT<-probabCdCdT.mean
  #}else{
  #  probabCdCdT<-replace
  #}
  power <- floor(log10(maxprobab))
  maxprobab <- ceiling(maxprobab/10^power)*10^power
  cx=1
  palette(rev(rainbow(20,start=1.0,end=0.67)))
  for(Ti in 1:dim(probabCdCdTT)[3]){
    probab <- probabCdCdTT[,,Ti]/maxprobab
    probab[probab>1]<-1
    image(seq(probabCdCdT.xfrom,by=probabCdCdT.xby,length.out=ceiling((probabCdCdT.xto-probabCdCdT.xfrom)/probabCdCdT.xby)+1), seq(probabCdCdT.yfrom,by=probabCdCdT.yby,length.out=ceiling((probabCdCdT.yto-probabCdCdT.yfrom)/probabCdCdT.yby)+1), main="Pirouette Probability", probab, 
    zlim=c(0,1), xlab="C (mM)", ylab="dC/dT (mM/sec)", col=1:20, font.lab=2)
    legend(probabCdCdT.xto, probabCdCdT.yto-(probabCdCdT.yto-probabCdCdT.yfrom)/20, c(maxprobab,rep("",18),0), xjust=1, x.intersp=0.5, fill=20:1, bty="n",y.intersp=0.5,border=20:1,cex=cx)
    if(saveplot) savePlot(paste(title, "(T_", (Ti-1)*Tspan, "-", min(Ti*Tspan, maxT), ").tiff", sep=""),"tiff")
  }

}


#plot.PirTdCdT
co$plot.PirTdCdT<-'
plot.PirTdCdT(xfrom=0, xto=NA, xby=NA, yfrom=-0.6, yto=0.6, yby=0.025, maxprobab=0.1, cutoff=20, persp=FALSE)
'
plot.PirTdCdT <- function(xfrom=0, xto=NA, xby=NA, yfrom=-0.6, yto=0.6, yby=0.025, maxprobab=0.1, cutoff=20, persp=FALSE, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  if(is.na(xto)) xto<-maxT
  if(is.na(xby)) xby<-ceiling(maxT/240)*10
  xn <- ceiling((xto-xfrom)/xby)
  yn <- ceiling((yto-yfrom)/yby)
  total <<- matrix(0, xn, yn)
  pir <<- matrix(0, xn, yn)
  for(track.i in 1:track.n){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      xi <- ceiling((dT[[track.i]][point.i]-xfrom)/xby)
      yi <- ceiling((dCdT[[track.i]][point.i]-yfrom)/yby)
      if(xi>=1 && xi<=xn && yi>=1 && yi<=yn){
        if(PirRun[[track.i]][point.i]=="R"){
          total[xi,yi]<<-total[xi,yi]+1
        }
        if(PirRun[[track.i]][point.i]=="P" && PirRun[[track.i]][point.i-1]!="P"){
          pir[xi,yi]<<-pir[xi,yi]+1
        }
      }
    }
    }
  }
  probabTdCdT <<- pir/total
  probabTdCdT[total<cutoff]<<-NA
  probabTdCdT.xfrom<<-xfrom
  probabTdCdT.xto<<-xto
  probabTdCdT.xby<<-xby
  probabTdCdT.yfrom<<-yfrom
  probabTdCdT.yto<<-yto
  probabTdCdT.yby<<-yby
  probabTdCdT.mean<<-mean(probabTdCdT,na.rm=T)

  #if(maxprobab<=0){maxprobab <- max(probab,na.rm=T)}
  power <- floor(log10(maxprobab))
  maxprobab <- ceiling(maxprobab/10^power)*10^power
  probab <- probabTdCdT/maxprobab
  probab[probab>1]<-1
  cx=1
  if(persp) {
  par(mfrow=c(2,2))
  persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),total,main="Total Run Points",xlab="T (sec)", ylab="dC/dT (mM/sec)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),pir,main="Pirouette Occurence",xlab="T (sec)", ylab="dC/dT (mM/sec)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  cx=0.5
  }
  #palette(rainbow(20,start=0.67,end=1.0))
  palette(rev(rainbow(20,start=1.0,end=0.67)))
  #image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), total, main="total", zlim=c(1,10000), xlab="T (sec)", ylab="dC/dT (mM/sec)", col=rainbow(20,start=0.67,end=1.0))#, col=gray(seq(1,0,by=-0.05)))
  #image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), pir, main="pir", zlim=c(1,100), xlab="T (sec)", ylab="dC/dT (mM/sec)", col=1:20) #col=rainbow(20,start=0.67,end=1.0))#, col=gray(seq(1,0,by=-0.05)))
  image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), main="Pirouette Probability", probab, 
  zlim=c(0,1), xlab="T (sec)", ylab="dC/dT (mM/sec)", col=1:20, font.lab=2)
  legend(xto, yto-(yto-yfrom)/20, c(maxprobab,rep("",18),0), xjust=1, x.intersp=0.5, fill=20:1, bty="n",y.intersp=0.5,border=20:1,cex=cx)
  par(mfrow=c(1,1))
}

#plot.PirTC
co$plot.PirTC<-'
plot.PirTC(xfrom=0, xto=NA, xby=NA, yfrom=30, yto=100, yby=2.5, dCdTfrom=-0.6, 
     dCdTto=0.6, maxprobab=0.1, cutoff=20, persp=FALSE, tracks=1:track.n)
'
plot.PirTC <- function(xfrom=0, xto=NA, xby=NA, yfrom=30, yto=100, yby=2.5, dCdTfrom=-0.6, dCdTto=0.6, maxprobab=0.1, cutoff=20, persp=FALSE, tracks=1:track.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  if(is.na(xto)) xto<-maxT
  if(is.na(xby)) xby<-ceiling(maxT/240)*10
  xn <- ceiling((xto-xfrom)/xby)
  yn <- ceiling((yto-yfrom)/yby)
  total <<- matrix(0, xn, yn)
  pir <<- matrix(0, xn, yn)
  for(track.i in tracks){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      if(dCdT[[track.i]][point.i]>=dCdTfrom && dCdT[[track.i]][point.i]<=dCdTto){
      xi <- ceiling((dT[[track.i]][point.i]-xfrom)/xby)
      yi <- ceiling((dC[[track.i]][point.i]-yfrom)/yby)
      if(xi>=1 && xi<=xn && yi>=1 && yi<=yn){
        if(PirRun[[track.i]][point.i]=="R"){
          total[xi,yi]<<-total[xi,yi]+1
        }
        if(PirRun[[track.i]][point.i]=="P" && PirRun[[track.i]][point.i-1]!="P"){
          pir[xi,yi]<<-pir[xi,yi]+1
        }
      }
      }
    }
    }
  }
  probabTC <<- pir/total
  probabTC[total<cutoff]<<-NA
  probabTC.xfrom<<-xfrom
  probabTC.xto<<-xto
  probabTC.xby<<-xby
  probabTC.yfrom<<-yfrom
  probabTC.yto<<-yto
  probabTC.yby<<-yby
  probabTC.mean<<-mean(probabTC,na.rm=T)

  #if(maxprobab<=0){maxprobab <- max(probab,na.rm=T)}
  power <- floor(log10(maxprobab))
  maxprobab <- ceiling(maxprobab/10^power)*10^power
  probab <- probabTC/maxprobab
  probab[probab>1]<-1
  cx=1
  if(persp) {
  par(mfrow=c(2,2))
  persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),total,main="Total Run Points",xlab="T (sec)", ylab="C (mM)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),pir,main="Pirouette Occurence",xlab="T (sec)", ylab="C (mM)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  cx=0.5
  }
  #palette(rainbow(20,start=0.67,end=1.0))
  palette(rev(rainbow(20,start=1.0,end=0.67)))
  #image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), total, main="total", zlim=c(1,10000), xlab="T (sec)", ylab="C (mM)", col=rainbow(20,start=0.67,end=1.0))#, col=gray(seq(1,0,by=-0.05)))
  #image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), pir, main="pir", zlim=c(1,100), xlab="T (sec)", ylab="C (mM)", col=1:20) #col=rainbow(20,start=0.67,end=1.0))#, col=gray(seq(1,0,by=-0.05)))
  image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), main="Pirouette Probability", probab, 
  zlim=c(0,1), xlab="T (sec)", ylab="C (mM)", col=1:20, font.lab=2)
  legend(xto, yto-(yto-yfrom)/20, c(maxprobab,rep("",18),0), xjust=1, x.intersp=0.5, fill=20:1, bty="n",y.intersp=0.5,border=20:1,cex=cx)
  par(mfrow=c(1,1))
}

#calc.PirCIndex
co$calc.PirCIndex<-'
calc.PirCIndex(xfrom=30, xto=100, xby=2.5, yfrom=-0.6, yto=0.6, yby=0.025, 
maxprobab=0.1, cutoff=20, , persp=FALSE, Tfrom=0, Tto=maxT, tracks=1:track.n)
'
calc.PirCIndex <- function(xfrom=30, xto=100, xby=2.5, yfrom=-0.6, yto=0.6, yby=0.025, 
                 maxprobab=0.1, cutoff=20, persp=FALSE, Tfrom=0, Tto=maxT, tracks=1:track.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  xn <- ceiling((xto-xfrom)/xby)
  yn <- ceiling((yto-yfrom)/yby)
  BasalPir <<- rep(NA, xn)
  PirIndex <<- rep(NA, xn)
  total <- matrix(0, xn, yn)
  pir <- matrix(0, xn, yn)
  for(track.i in tracks){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      xi <- ceiling((dC[[track.i]][point.i]-xfrom)/xby)
      yi <- ceiling((dCdT[[track.i]][point.i]-yfrom)/yby)
      if(xi>=1 && xi<=xn && yi>=1 && yi<=yn && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){
        if(PirRun[[track.i]][point.i]=="R"){
          total[xi,yi]<-total[xi,yi]+(dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
        }
        if(PirRun[[track.i]][point.i]=="P" && PirRun[[track.i]][point.i-1]!="P"){
          total[xi,yi]<-total[xi,yi]+(dT[[track.i]][point.i]-dT[[track.i]][point.i-1])
          pir[xi,yi]<-pir[xi,yi]+1
        }
      }
    }
    }
  }
  probabCdCdT <- pir/total
  probabCdCdT[total<cutoff]<-NA
  palette(rainbow(xn, start=0, end=0.6))
  plot(c(), c(), xlim=c(yfrom, yto), ylim=c(0, maxprobab), xlab="dC/dT (mM/sec)", ylab="Probability")
  
  for(xi in 1:xn){
    Dataset <- data.frame(x=seq(yfrom+yby/2, yto-yby/2, yby), y=probabCdCdT[xi,], weight=total[xi, ]/probabCdCdT[xi,]/(1-probabCdCdT[xi,]))
    range <- (1:yn)[!is.na(Dataset$weight) & pir[xi,]>0]
    lines(seq(yfrom+yby/2, yto-yby/2, yby), probabCdCdT[xi,], type="o", col=xi, cex=0.5)
    if(length((1:yn)[range])>=3){
      Model <- lm(y~x,data=Dataset[range,],weight=Dataset$weight[range])
      BasalPir[xi] <<- summary(Model)$coefficients[1,1]
      PirIndex[xi] <<- summary(Model)$coefficients[2,1]
      abline(Model, col=xi, lty=2)
    }
  }
  
  legend("topright", as.character(seq(xfrom+xby/2, xto-xby/2, xby)), pch=1, lty=1, col=1:xn, cex=0.5)

}


#plot.BPirTC
co$plot.BPirTC<-'
plot.BPirTC(Tfrom=0, Tto=2000, Tby=180, Cfrom=20, Cto=100, Cby=2.5, maxdCdT=0.02, maxprobab=0.05, cutoff=50)
'
plot.BPirTC <- function(Tfrom=0, Tto=2000, Tby=180, Cfrom=20, Cto=100, Cby=2.5, maxdCdT=0.02, maxprobab=0.05, cutoff=50, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  xn <- ceiling((Tto-Tfrom)/Tby)
  yn <- ceiling((Cto-Cfrom)/Cby)
  totalTC <<- matrix(0, xn, yn)
  pirTC <<- matrix(0, xn, yn)
  for(track.i in 1:track.n){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      xi <- ceiling((dT[[track.i]][point.i]-Tfrom)/Tby)
      yi <- ceiling((dC[[track.i]][point.i]-Cfrom)/Cby)
      if(xi>=1 && xi<=xn && yi>=1 && yi<=yn && abs(dCdT[[track.i]][point.i])<=maxdCdT){
        if(PirRun[[track.i]][point.i]=="R"){
          totalTC[xi,yi]<<-totalTC[xi,yi]+1
        }
        if(PirRun[[track.i]][point.i]=="P" && PirRun[[track.i]][point.i-1]!="P"){
          pirTC[xi,yi]<<-pirTC[xi,yi]+1
        }
      }
    }
    }
  }
  BPirTC <<- pirTC/totalTC
  BPirT <<- sapply(1:xn, function(xi) sum(pirTC[xi,]))/sapply(1:xn, function(xi) sum(totalTC[xi,]))
  BPirTCplot <- BPirTC
  BPirTCplot[totalTC<cutoff]<-NA

  power <- floor(log10(maxprobab))
  maxprobab <- ceiling(maxprobab/10^power)*10^power
  BPirTCplot <- BPirTCplot/maxprobab
  BPirTCplot[BPirTCplot>1]<-1
  cx=1
  #if(persp) {
  #par(mfrow=c(2,2))
  #persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),total,main="Total Run Points",xlab="C (mM)", ylab="dC/dT (mM/sec)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  #persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),pir,main="Pirouette Occurence",xlab="C (mM)", ylab="dC/dT (mM/sec)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  #cx=0.5
  #}
  #palette(rainbow(20,start=0.67,end=1.0))
  par(mfrow=c(1,1))
  palette(rev(rainbow(20,start=1.0,end=0.67)))
  #image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), total, main="total", zlim=c(1,10000), xlab="C (mM)", ylab="dC/dT (mM/sec)", col=rainbow(20,start=0.67,end=1.0))#, col=gray(seq(1,0,by=-0.05)))
  #image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), pir, main="pir", zlim=c(1,100), xlab="C (mM)", ylab="dC/dT (mM/sec)", col=1:20) #col=rainbow(20,start=0.67,end=1.0))#, col=gray(seq(1,0,by=-0.05)))
  image(seq(Tfrom,by=Tby,length.out=xn+1), seq(Cfrom,by=Cby,length.out=yn+1), main="Basal Pirouette Probability", BPirTCplot, 
  zlim=c(0,1), xlab="T (sec)", ylab="C (mM)", col=1:20, font.lab=2)
  legend(Tto, Cto-(Cto-Cfrom)/20, c(maxprobab,rep("",18),0), xjust=1, x.intersp=0.5, fill=20:1, bty="n",y.intersp=0.5,border=20:1,cex=cx)
}

#plot.TRdCdLat
co$plot.TRdCdLat<-'
plot.TRdCdLat(from=-3, to=3, by=0.5, Cfrom=NA, Cto=NA, ylim=NA)
'
plot.TRdCdLat <- function(from=-3, to=3, by=0.5, Cfrom=NA, Cto=NA, ylim=NA, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  n <- floor((to-from)/by)
  if(is.na(Cfrom) && is.na(Cto)){main=""} else{main<-paste("C= ",Cfrom,"-",Cto," mM",sep="")}
  if(is.na(ylim[1])){
  plot(seq(from+by/2,to-by/2,by=by), sapply(1:n, function(i) plot.TRdCdLat.func1(from,to,by,i,Cfrom,Cto))
  , xlab="dCdLat (mM/mm)", ylab="Curving Rate (deg/mm)",type="b", pch=16, font.lab=2, main=main)
  }
  else{
  plot(seq(from+by/2,to-by/2,by=by), sapply(1:n, function(i) plot.TRdCdLat.func1(from,to,by,i,Cfrom,Cto))
  , xlab="dCdLat (mM/mm)", ylab="Curving Rate (deg/mm)",type="b", pch=16, font.lab=2, ylim=ylim, main=main)
  }
}
#plot.TRdCdLat用の
plot.TRdCdLat.func1<-function(from,to,by,i,Cfrom,Cto){
  if(is.na(Cfrom) && is.na(Cto)){
  mean(unlist(TurnRate)[ceiling((unlist(dCdLat)-from)/by)==i],na.rm=T)
  }
  else if(!is.na(Cfrom) && is.na(Cto)){
  mean(unlist(TurnRate)[ceiling((unlist(dCdLat)-from)/by)==i & unlist(dC)>=Cfrom],na.rm=T)
  }
  else if(is.na(Cfrom) && !is.na(Cto)){
  mean(unlist(TurnRate)[ceiling((unlist(dCdLat)-from)/by)==i & unlist(dC)<=Cto],na.rm=T)
  }
  else if(!is.na(Cfrom) && !is.na(Cto)){
  mean(unlist(TurnRate)[ceiling((unlist(dCdLat)-from)/by)==i & unlist(dC)>=Cfrom & unlist(dC)<=Cto],na.rm=T)
  }
}

#plot.TRdCdLatMulti
co$plot.TRdCdLatMulti<-'
plot.TRdCdLatMulti(from=-3, to=3, by=0.5, Cfrom, Cto, Cstep, ylim=NA)
'
plot.TRdCdLatMulti<-function(from=-3, to=3, by=0.5, Cfrom, Cto, Cstep, ylim=NA, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  n<-ceiling((Cto-Cfrom)/Cstep)
  col<-ceiling(sqrt(n))
  par(mfrow=c(ceiling(n/col),col))
  for(Cf in seq(Cfrom,Cto-Cstep,by=Cstep)){
    plot.TRdCdLat(from, to, by, Cf, Cf+Cstep, ylim)
  }
  par(mfrow=c(1,1))
}


#plot.TRCdCdLat
co$plot.TRCdCdLat<-'
plot.TRCdCdLat(xfrom=30, xto=100, xby=2.5, yfrom=-3, yto=3, yby=0.2, minTR=-20, maxTR=20, cutoff=5, Tfrom=0, Tto=maxT, tracks=1:track.n, legend=TRUE, legend = "Y")
'
plot.TRCdCdLat <- function(xfrom=30, xto=100, xby=2.5, yfrom=-3, yto=3, yby=0.2, 
                  minTR=-20, maxTR=20, cutoff=5, Tfrom=0, Tto=maxT, tracks=1:track.n, legend=TRUE, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  xn <- ceiling((xto-xfrom)/xby)
  yn <- ceiling((yto-yfrom)/yby)
  TRsum <<- matrix(0, xn, yn)
  TRcount <<- matrix(0, xn, yn)
  for(track.i in tracks){
    for(point.i in 1:point.n[track.i]){
      xi <- ceiling((dC[[track.i]][point.i]-xfrom)/xby)
      yi <- ceiling((dCdLat[[track.i]][point.i]-yfrom)/yby)
      if(!is.na(TurnRate[[track.i]][point.i]) && !is.na(dCdLat[[track.i]][point.i]) 
         && xi>=1 && xi<=xn && yi>=1 && yi<=yn && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){
        TRcount[xi,yi]<<-TRcount[xi,yi]+1
        TRsum[xi,yi]<<-TRsum[xi,yi]+TurnRate[[track.i]][point.i]
      }
    }
  }
  TRCdCdL <<- TRsum/TRcount
  TRCdCdL[TRcount<cutoff]<<-NA
  TRCdCdL.xfrom<<-xfrom
  TRCdCdL.xto<<-xto
  TRCdCdL.xby<<-xby
  TRCdCdL.yfrom<<-yfrom
  TRCdCdL.yto<<-yto
  TRCdCdL.yby<<-yby
  TRCdCdL.mean<<-mean(TRCdCdL,na.rm=T)

  TR <- (TRCdCdL-minTR)/(maxTR-minTR)
  TR[TR>1]<-1
  TR[TR<0]<-0
  par(mfrow=c(1,1))
  par(mar=c(5,5.5,1.5,2))
  image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), TR, zlim=c(0,1), 
  xlab="C (mM)", ylab="dC/dl (mM/mm)", col=rev(rainbow(20,start=1.0,end=0.67)), font.lab=2, cex.axis=2, cex.lab=2)#col=rainbow(20,start=0.67,end=1.0), font.lab=2)
  #palette(rev(rainbow(20,start=0.67,end=1.0)))
  palette(rainbow(20,start=1.0,end=0.67))
  if(legend){
  text(xto,yto-(yto-yfrom)/40,"(deg/mm)",pos=2)
  legend(xto, yto-(yto-yfrom)/20, c(maxTR,rep("",18),minTR), xjust=1, x.intersp=0.5, fill=1:20, bty="n",y.intersp=0.5,border=1:20)
  #text(xto,yto-(yto-yfrom)/20,"(deg/mm)",pos=2,cex=2)
  #legend(xto, yto-(yto-yfrom)/20, c(maxTR,rep("",18),minTR), xjust=1, x.intersp=0.5, fill=1:20, bty="n",y.intersp=0.5,border=1:20,cex=2)
  }
  #par(mfrow=c(1,1))
}

#plot.TRCdCdLatMulti
co$plot.TRCdCdLatMulti<-'
plot.TRCdCdLatMulti(xfrom=30, xto=100, xby=2.5, yfrom=-3, yto=3, yby=0.2, 
                  minTR=-20, maxTR=20, cutoff=5, Tfrom=0, Tto=maxT, Tspan=600, saveplot=TRUE, tracks=1:track.n)
'
plot.TRCdCdLatMulti <- function(xfrom=30, xto=100, xby=2.5, yfrom=-3, yto=3, yby=0.2, 
                  minTR=-20, maxTR=20, cutoff=5, Tfrom=0, Tto=maxT, Tspan=600, saveplot=TRUE, tracks=1:track.n, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  Tn <- ceiling((Tto-Tfrom)/Tspan)
  xn <- ceiling((xto-xfrom)/xby)
  yn <- ceiling((yto-yfrom)/yby)
  TRCdCdL.Tfrom <<- Tfrom
  TRCdCdL.Tto <<- Tto
  TRCdCdL.Tspan <<- Tspan
  TRCdCdLT <<- array(NA, c(xn, yn, Tn))
  TRCdCdLT.mean <<- rep(NA, Tn)
  for(Ti in 1:Tn){
    plot.TRCdCdLat(xfrom=xfrom, xto=xto, xby=xby, yfrom=yfrom, yto=yto, yby=yby, 
                  minTR=minTR, maxTR=maxTR, cutoff=cutoff, Tfrom=(Ti-1)*Tspan, Tto=min(Ti*Tspan, Tto), tracks)
    TRCdCdLT[,,Ti] <<- TRCdCdL
    TRCdCdLT.mean[Ti] <<- TRCdCdL.mean
    if(saveplot) savePlot(paste("plot.TRCdCdLat(T_", (Ti-1)*Tspan, "-", min(Ti*Tspan, Tto), ").tiff", sep=""),"tiff")
  }
}

#clip.TRCdCdLT
co$clip.TRCdCdLT<-'
clip.TRCdCdLT(title, replace=NA, xfrom=NA, xto=NA, yfrom=NA, yto=NA, Tfrom=NA, Tto=NA
                   minTR=-20, maxTR=20, cutoff=5, saveplot=TRUE)
'
clip.TRCdCdLT<-function(title, replace=NA, xfrom=NA, xto=NA, yfrom=NA, yto=NA, Tfrom=NA, Tto=NA,
                   minTR=-20, maxTR=20, cutoff=5, Tspan=600, saveplot=TRUE, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
                   
  if(is.na(xfrom)){
    ifrom <- 1
  }else{
    ifrom <- floor((xfrom-TRCdCdL.xfrom)/TRCdCdL.xby + 1)
  }
  if(ifrom<1) ifrom<-1
  if(is.na(xto)){
    ito <- ceiling((TRCdCdL.xto-TRCdCdL.xfrom)/TRCdCdL.xby)
  }else{
    ito <- ceiling((xto-TRCdCdL.xfrom)/TRCdCdL.xby)
  }
  if(ito>ceiling((TRCdCdL.xto-TRCdCdL.xfrom)/TRCdCdL.xby)) ito<-ceiling((TRCdCdL.xto-TRCdCdL.xfrom)/TRCdCdL.xby)

  if(is.na(yfrom)){
    jfrom <- 1
  }else{
    jfrom <- floor((yfrom-TRCdCdL.yfrom)/TRCdCdL.yby + 1)
  }
  if(jfrom<1) jfrom<-1
  if(is.na(yto)){
    jto <- ceiling((TRCdCdL.yto-TRCdCdL.yfrom)/TRCdCdL.yby)
  }else{
    jto <- ceiling((yto-TRCdCdL.yfrom)/TRCdCdL.yby)
  }
  if(jto>ceiling((TRCdCdL.yto-TRCdCdL.yfrom)/TRCdCdL.yby)) jto<-ceiling((TRCdCdL.yto-TRCdCdL.yfrom)/TRCdCdL.yby)

  if(is.na(Tfrom)) Tfrom<-1
  if(is.na(Tto)) Tto<-dim(TRCdCdLT)[3]
  for(Ti in Tfrom:Tto){
    if(is.na(replace)){
      TRCdCdLT[ifrom:ito,jfrom:jto,Ti][!is.na(TRCdCdLT[ifrom:ito,jfrom:jto,Ti])]<<-TRCdCdLT.mean[Ti][!is.na(TRCdCdLT[ifrom:ito,jfrom:jto,Ti])]
    }else{
      TRCdCdLT[ifrom:ito,jfrom:jto,Ti][!is.na(TRCdCdLT[ifrom:ito,jfrom:jto,Ti])]<<-replace
    }
  }
  for(Ti in 1:dim(TRCdCdLT)[3]){
    TR <- (TRCdCdLT[,,Ti]-minTR)/(maxTR-minTR)
    TR[TR>1]<-1
    TR[TR<0]<-0
    par(mfrow=c(1,1))
    image(seq(TRCdCdL.xfrom,by=TRCdCdL.xby,length.out=ceiling((TRCdCdL.xto-TRCdCdL.xfrom)/TRCdCdL.xby)+1), seq(TRCdCdL.yfrom,by=TRCdCdL.yby,length.out=ceiling((TRCdCdL.yto-TRCdCdL.yfrom)/TRCdCdL.yby)+1), main="Curving Rate", TR, zlim=c(0,1), 
    xlab="C (mM)", ylab="dC/dl (mM/mm)", col=rev(rainbow(20,start=1.0,end=0.67)), font.lab=2)#col=rainbow(20,start=0.67,end=1.0), font.lab=2)
    palette(rainbow(20,start=1.0,end=0.67))
    text(TRCdCdL.xto,TRCdCdL.yto-(TRCdCdL.yto-TRCdCdL.yfrom)/40,"(deg/mm)",pos=2)
    legend(TRCdCdL.xto, TRCdCdL.yto-(TRCdCdL.yto-TRCdCdL.yfrom)/20, c(maxTR,rep("",18),minTR), xjust=1, x.intersp=0.5, fill=1:20, bty="n",y.intersp=0.5,border=1:20)
    if(saveplot) savePlot(paste(title,"(T_", (Ti-1)*Tspan, "-", min(Ti*Tspan, maxT), ").tiff", sep=""),"tiff")
  }

}

#chemotaxis.index
co$chemotaxis.index<-'
chemotaxis.index(tpoints = seq(0,round(maxT/60)*60,by=120), plates=1:plate.n, left.low=T)
'
chemotaxis.index <- function(tpoints = seq(0,round(maxT/60)*60,by=120), plates=1:plate.n, left.low=T,tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  if(plate_format!="kunitomo"){
    cat('plate_format is not "kunitomo". Sorry.\n')
    return()
  }
  cat("Warning: we assume that adjust.position() has been executed.\n")
  # peak positions are (20,50) and (80,50)
  
  CI <- matrix(NA, length(plates), length(tpoints))
  Acount <- matrix(NA, length(plates), length(tpoints))
  Bcount <- matrix(NA, length(plates), length(tpoints))
  A1count <- matrix(NA, length(plates), length(tpoints))
  B1count <- matrix(NA, length(plates), length(tpoints))
  Ocount <- matrix(NA, length(plates), length(tpoints))
  
  tpfrom <- tpoints-30 # one minute time span (vector)
  tpto <- tpoints+30
  tpto[tpfrom<0] <- 60
  tpfrom[tpfrom<0] <- 0
  tpfrom[tpto>maxT] <- maxT-60
  tpto[tpto>maxT] <- maxT
  for(platei in 1:length(plates)){ # process each plate
    plate <- plates[platei]
    tracks <- plate.track[[plate]]
    Tvec <- unlist(dT[tracks])
    Xvec <- unlist(dX[tracks])
    Yvec <- unlist(dY[tracks])
    for(ti in 1:length(tpoints)){ # process each timespan and find worms in this timespan
      ind <- Tvec>=tpfrom[ti] & Tvec<=tpto[ti]
      Xi <- Xvec[ind]
      Yi <- Yvec[ind]
      Oind <- (Xi-50)^2 + (Yi-50)^2 < 10^2 # within 10mm radius from (50,50)
      O1ind <- !Oind
      Aind <- Xi<50 & !Oind
      Bind <- Xi>50 & !Oind
      A1ind <- (Xi-20)^2 + (Yi-50)^2 > 20^2 # outside 20mm radius from (20,50)
      B1ind <- (Xi-80)^2 + (Yi-50)^2 > 20^2 # outside 20mm radius from (80,50)
      Aind <- Aind & !A1ind
      Bind <- Bind & !B1ind
      Acount[platei,ti] <- sum(Aind)
      Bcount[platei,ti] <- sum(Bind)
      A1count[platei,ti] <- sum(A1ind)
      B1count[platei,ti] <- sum(B1ind)
      Ocount[platei,ti] <- sum(Oind)
    }
  }
  if(left.low){
    CI <- (Bcount-Acount)/(Bcount+Acount+B1count+A1count)
  }else{
    CI <- (Acount-Bcount)/(Bcount+Acount+B1count+A1count)
  }
  CImean <- colMeans(CI)
  CIsd <- apply(CI,2,sd)
  CIsem <- CIsd/sqrt(nrow(CI))
  tiff("chemotaxis.index.tiff")
  plot(tpoints, CImean, xlab="time(s)", ylim=c(-1,1), ylab="Chemotaxis Index")
  arrows(tpoints, CImean, tpoints, CImean+CIsem, angle=90, length=0.05)
  arrows(tpoints, CImean, tpoints, CImean-CIsem, angle=90, length=0.05)
  dev.off()
  for(ti in 1:length(tpoints)){
    outtable <- data.frame(plate=plates,A=Acount[,ti],A1=A1count[,ti],
    O=Ocount[,ti],B1=B1count[,ti],B=Bcount[,ti],CI=CI[,ti])
    write.table(file=paste0("worm_counts_t",tpoints[ti],".csv"), 
    outtable,sep=",",row.names=F)
  }
  rownames(CI) <- plates
  outmatrix <- rbind(CImean, CIsd, CIsem, CI)
  colnames(outmatrix) <- tpoints
  write.csv(file="chemotaxis.index.csv", outmatrix)
}


#plot.TRTdCdLat
co$plot.TRTdCdLat<-'
plot.TRTdCdLat(xfrom=0, xto=NA, xby=NA, yfrom=-0.6, yto=0.6, yby=0.025, maxprobab=0.1, cutoff=20, persp=FALSE)
'
plot.TRTdCdLat <- function(xfrom=0, xto=NA, xby=NA, yfrom=-3, yto=3, yby=0.2, 
                  minTR=-20, maxTR=20, cutoff=5, Tfrom=0, Tto=maxT, tracks=1:track.n, persp=FALSE, tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  if(is.na(xto)) xto<-maxT
  if(is.na(xby)) xby<-ceiling(maxT/240)*10
  xn <- ceiling((xto-xfrom)/xby)
  yn <- ceiling((yto-yfrom)/yby)
  turnsum <<- matrix(0, xn, yn)
  turncount <<- matrix(0, xn, yn)
  for(track.i in tracks){
    if(point.n[track.i]>=2){
    for(point.i in 2:point.n[track.i]){
      xi <- ceiling((dT[[track.i]][point.i]-xfrom)/xby)
      yi <- ceiling((dCdLat[[track.i]][point.i]-yfrom)/yby)
      if(!is.na(TurnRate[[track.i]][point.i]) && !is.na(dCdLat[[track.i]][point.i]) 
         && xi>=1 && xi<=xn && yi>=1 && yi<=yn && dT[[track.i]][point.i]>=Tfrom && dT[[track.i]][point.i]<=Tto){
        turncount[xi,yi]<<-turncount[xi,yi]+1
        turnsum[xi,yi]<<-turnsum[xi,yi]+TurnRate[[track.i]][point.i]
      }
    }
    }
  }
  TRTdCdL <<- turnsum/turncount
  TRTdCdL[turncount<cutoff]<<-NA
  TRTdCdL.xfrom<<-xfrom
  TRTdCdL.xto<<-xto
  TRTdCdL.xby<<-xby
  TRTdCdL.yfrom<<-yfrom
  TRTdCdL.yto<<-yto
  TRTdCdL.yby<<-yby
  TRTdCdL.mean<<-mean(TRTdCdL,na.rm=T)

  TRTdCdL <- (TRTdCdL-minTR)/(maxTR-minTR)
  TRTdCdL[TRTdCdL>1]<-1
  TRTdCdL[TRTdCdL<0]<-0
  cx=1
  if(persp) {
  par(mfrow=c(2,2))
  persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),turncount,main="Total Run Points",xlab="T (sec)", ylab="dC/dl (mM/mm)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  persp(x=seq(xfrom,xto,length.out=xn),y=seq(yfrom,yto,length.out=yn),TRTdCdL,main="Curving Rate",xlab="T (sec)", ylab="Curving Rate (deg/mm)",border=NA,shade=0.5,theta=-30,phi=30,expand=0.5, ticktype="detailed")
  cx=0.5
  }
  palette(rev(rainbow(20,start=1.0,end=0.67)))
  image(seq(xfrom,by=xby,length.out=xn+1), seq(yfrom,by=yby,length.out=yn+1), main="Curving Rate", TRTdCdL, 
  zlim=c(0,1), xlab="T (sec)", ylab="dC/dl (mM/mm)", col=1:20, font.lab=2)
  legend(xto, yto-(yto-yfrom)/20, c(maxTR,rep("",18),minTR), xjust=1, x.intersp=0.5, fill=1:20, bty="n",y.intersp=0.5,border=1:20)
  par(mfrow=c(1,1))
}


#quantify.pirouette
co$quantify.pirouette<-'
quantify.pirouette()
'
quantify.pirouette<-function(){

  cat("Pirouette initiation frequency:",length((1:sum(point.n))[unlist(PirStart)])/sum(point.n),"\n",sep="")
  cat("Turn initiation frequency:",length((1:sum(point.n))[unlist(TurnStart)])/sum(point.n),"\n",sep="")
  
  Pirduration<-c()
  Turnduration<-c()
  Turncount <<- c()
  
  for(track.i in 1:track.n){
    starts <- grep(TRUE,PirStart[[track.i]])
    ends <- grep(TRUE,PirEnd[[track.i]])
    if(starts[1]<=ends[1]){Pirduration<-c(Pirduration, ends-starts)}
        else{cat("error1\n");recover()}
    for(i in 1:length(starts)){
      Turncount<<-c(Turncount, length(grep(TRUE, TurnStart[[track.i]][starts[i]:ends[i]])))
    }
    turnstarts <- grep(TRUE,TurnStart[[track.i]])
    turnends <- grep(TRUE,TurnEnd[[track.i]])
    if(turnstarts[1]<=turnends[1]){Turnduration<-c(Turnduration, turnends-turnstarts)}
        else{cat("error2\n");recover()}
  }
  par(mfrow=c(1,1))
  hist(main="Turn count distribution",Turncount,breaks=0:15)
  print(hist(main="Turn count distribution",Turncount,breaks=0:15)$count)
  #par(mfrow=c(1,2))
  #hist(main="Pirouette duration",Pirduration, breaks=seq(0,210,30), freq=F)
  #hist(main="Turn duration",Turnduration, breaks=seq(0,105,15), freq=F)
}

#calc.CI
co$calc.CI<-'
calc.CI()
'
calc.CI<-function(time=maxT,type="plate", tbl=NULL){
  if(!is.null(tbl)){
  for(i in 1:nrow(tbl)){
    eval(parse(text=paste(svalue(tbl[i,1]),"<-" ,svalue(tbl[i,2]))))
  }
  }
  CI<<-c()
  for(plate.i in 1:plate.n){
    ABC<-rep(0,10)
    for(track.i in plate.track[plate.i]){
    if(plate_format=="kunitomo"){
    }
    }
  }
}

#migration.bias
co$migration.bias<-'
migration.bias()
'
migration.bias <- function(){
	templist <- list()
	for(track.i in 1:track.n){
		dCdX2 <- (dCdX[[track.i]][-1]+dCdX[[track.i]][-point.n[track.i]])/2
		dCdY2 <- (dCdY[[track.i]][-1]+dCdY[[track.i]][-point.n[track.i]])/2
		ddX <- dX[[track.i]][-1] - dX[[track.i]][-point.n[track.i]]
		ddY <- dY[[track.i]][-1] - dY[[track.i]][-point.n[track.i]]
		temp.migbias <- (dCdX2*ddX + dCdY2*ddY)/(sqrt(dCdX2*dCdX2+dCdY2*dCdY2)+sqrt(ddX*ddX+ddY*ddY))
		templist <- c(templist, list(temp.migbias))
	}
	migbias <<- templist
}


#######################################################
###################                            ########
co$ooooo<-'\n########     Simulation      ########
'##################                            ######
#######################################################


#Function backup
co$backup<-'
backup()
backup lost in simulate()
dT.back <<- dT
dX.back <<- dX
dY.back <<- dY
track.n.back <<- track.n
point.n.back <<- point.n
maxT.back <<- maxT
Executed
'
backup <- function(){
  dT.back <<- dT
  dX.back <<- dX
  dY.back <<- dY
  track.n.back <<- track.n
  point.n.back <<- point.n
  maxT.back <<- maxT
}

#Function backup.reverse
co$backup.reverse<-'
backup.reverse()
Return backup.
  dT <<- dT.back
  dX <<- dX.back
  dY <<- dY.back
  track.n <<- track.n.back
  point.n <<- point.n.back
  maxT <<- maxT.back
Executed
'
backup.reverse <- function(){
  dT <<- dT.back
  dX <<- dX.back
  dY <<- dY.back
  track.n <<- track.n.back
  point.n <<- point.n.back
  maxT <<- maxT.back
}
            
#Function simulate
co$simulate<-'
simulate(repeat.n=1, type="RW"/"JN", pir=T, wv=T ,maxtime=NA, basal.pirouette=NA, 
            variable.speed=T, worm.speed=worm.speed, Tstep=0.5, initialX=50, initialY=50, initialR=10,
            plate.centerX=50, plate.centerY=50, plate.radius=40, no.asking=F, timeseries=F)
Perform simulation.
'
simulate <- function(repeat.n=1, type="RW", pir=T, wv=T ,maxtime=NA, basal.pirouette=NA, 
            variable.speed=T, worm.speed2=worm.speed, Tstep=0.5, initialX=50, initialY=50, initialR=10,
            plate.centerX=50, plate.centerY=50, plate.radius=40, no.asking=F, timeseries=F){

  #X1<-19.46; Y1<-37.5; X2<-19.46; Y2<-62.5 # 50-sqrt(33^2-12.5^2)=19.46
  
  if(!no.asking){
    cat("dT, dX, dY, point.n, track.n, maxT substituted. backup()Backup?([Y]/N)")
    input <- readLines(n=1)
    if(input!="" && input!="y" && input!="Y") return()
  }
  if(variable.speed && exists("meanVT") && exists("meanVT.timespan")) cat("worm.speed used\n")
  else{cat("Fixed worm.speed used\n")}
  
  if(is.na(maxtime)) {maxtime<-maxT; if(!is.numeric(maxT)) {cat("maxtime need to be set\n"); return()}}
  
  if(!exists("BdeltaTheta")){
    if(file.exists("bearing before pirouette delta bearing.txt")){
    tempBD <- read.table("bearing before pirouette delta bearing.txt", header=T, sep="\t")
    BdeltaTheta <<- matrix(0,12,12)
    for(i in 1:length(tempBD[[1]]))
    {
      if(!is.na(tempBD[[1]][i]) && !is.na(tempBD[[2]][i]))
      {
        k<-ceiling((tempBD[[1]][i]+180)/30)
        l<-ceiling((tempBD[[2]][i]+180)/30)
        if(k==0) k<- 1
        if(l==0) l<- 1
        BdeltaTheta[k,l] <<- BdeltaTheta[k,l] + 1
      }
    }
    for(i in 1:12)
    {
      tempsum <- sum(BdeltaTheta[,i])
      BdeltaTheta[i,] <<- BdeltaTheta[i,] / tempsum
    }
    }else{
    cat("No BdeltaTheta. before.after()execute or bearing before pirouette delta bearing.txt prepare file\n")
    }
  }
  
  if(type == "RW")
  {
  sigmaPsi <- 11.6 #ξcurving rate SD 32.3°/mm。and alpha
  # sigma = sart(1-alpha^2)SD = 11.6
  alpha <- 0.933^(Tstep*2)  #decay const  0.5s in 0.933. FigS2
  }

  dT <<- list()
  dX <<- list()
  dY <<- list()
  point.n <<- c()
  
  #Trials
  for(repeat.i in 1:repeat.n)
  {
    T <- 0
    repeat{
      rX<-runif(1,min=-1,max=1)
      rY<-runif(1,min=-1,max=1)  
      if(rX^2+rY^2<=1) break
    }
    X <- rX*initialR+initialX #Start
    Y <- rY*initialR+initialY
    Theta <- runif(1, min=-360, max=360)

    tempT <- c(T)
    tempX <- c(X)
    tempY <- c(Y)
    tempTheta <<- c(Theta)
    Psi <- 0
    tempPsi<- c(Psi)
    tempPhi<- c(Psi)
    prevPsi <- 0
    nextPsi <- rnorm(1,0,32.3)
    i <- 1
    while(1)
    {
      #Time proceeds
      i <- i + 1
      T <- T + Tstep
      if(T > maxtime) break
      if(variable.speed && exists("meanVT") && exists("meanVT.timespan") && ceiling(T/meanVT.timespan)<=length(meanVT)){
        speed <- meanVT[ceiling(T/meanVT.timespan)]
      }else{
        speed <- worm.speed2
      }
      Theta <- tempTheta[i-1]+tempPhi[i-1]*Tstep*speed
      if(Theta > 180) Theta <- Theta - 360
      else if(Theta <= -180) Theta <- Theta + 360
      X <- tempX[i-1]+speed*Tstep*cos(Theta/180*pi)
      Y <- tempY[i-1]+speed*Tstep*sin(Theta/180*pi)
      if(sqrt((X-plate.centerX)^2 + (Y-plate.centerY)^2 ) > plate.radius) break
      
      #####Bearing calculation####
      
      #Nearest peak peakX, peakY
      mindist2<-10000
      for(tempi in 1:length(peak.positionX)){
      tempdist2 <- dist2(X,Y,peak.positionX[tempi],peak.positionY[tempi])
      if(tempdist2 < mindist2)
      {
        peakX <- peak.positionX[tempi]
        peakY <- peak.positionY[tempi]
        mindist2 <- tempdist2
      }
      }

      #Peak direction
      DX <- peakX-X
      DY <- peakY-Y
      peakDir <- atan(DY/DX)/pi*180
      if(!is.na(DX) && !is.na(DY)){
        if(DX<0 & DY>=0) peakDir<-peakDir+180
        if(DX<0 & DY<0) peakDir<-peakDir-180
      }
      Dist <- sqrt(DX^2+DY^2)
      
      if(gradient=="numerical"){
        peakDir <- atan(get.dCdY(X,Y,T)/get.dCdX(X,Y,T))/pi*180
        if(get.dCdX(X,Y,T)<0 & get.dCdY(X,Y,T)>=0) peakDir<-peakDir+180
        if(get.dCdX(X,Y,T)<0 & get.dCdY(X,Y,T)<0) peakDir<-peakDir-180
      }
      
      
      #Bearing
      Bearing <- peakDir-Theta
      if(!is.na(Bearing)){
        if(Bearing > 180) Bearing <- Bearing-360
        else if(Bearing < -180) Bearing <- Bearing+360
      }

      if(type=="RW") Psi <- tempPsi[i-1]*alpha + rnorm(1,0,sigmaPsi)[1]
      if(type=="JN")
      {
        if(i%%24 == 0)
        {
          Psi <- nextPsi
          prevPsi <- nextPsi
          nextPsi <- rnorm(1,0,32.3)
        }
        else
        {
          Psi <- prevPsi + (nextPsi-prevPsi)/24*(i%%24)
        }
      }
      
      #Weathervane
      if(wv) Phi <- Psi + WVturnrate(T, X, Y, Dist, Bearing, Theta, timeseries)
      else Phi <- Psi
      
      #Pir mechanism
      if(source=="salt" && gradient=="numerical") dCdT <- (get.C(X,Y,T) - get.C(tempX[i-1],tempY[i-1],tempT[i-1]))/(T-tempT[i-1])
      if(runif(1)[1] < Pirprobab(T, X, Y, Dist, Bearing, dCdT, timeseries, pir, basal.pirouette)*(T-tempT[i-1])) #probab in time
      {
      #Pir occurs
        Theta <- Theta + DTheta(Bearing)
        if(Theta > 180) Theta <- Theta - 360
        else if(Theta <= -180) Theta <- Theta + 360
      }
      tempT <- c(tempT, T)
      tempX <- c(tempX, X)
      tempY <- c(tempY, Y)
      tempTheta <<- c(tempTheta, Theta)
      tempPsi <- c(tempPsi, Psi)
      tempPhi <- c(tempPhi, Phi)
    }

    dT <<- c(dT, list(tempT))
    dX <<- c(dX, list(tempX))
    dY <<- c(dY, list(tempY))
    point.n <<- c(point.n, i-1)
  }
  track.n <<- repeat.n
  maxT <<- maxtime
}

#For simulate, WVturnrate
  WVturnrate <- function(T, X, Y, Dist, Bearing, Theta, timeseries){

  if(source=="odor"){
    if(!exists("WVIndexMean")){cat("No WVturnrate: WVIndexMean found\n")}
    #return(30*sin(Bearing/180*pi))
    trank <- ceiling(T/300)
    drank <- ceiling(Dist/15)
    # added
    #if(trank <= 2) trank <- 2 else trank <- 5
    #drank <- 2
        recover()
    if(drank>4) index <- 0
    else if(WVIndexT[trank, drank] > 0.05) index <- 0 else index <- WVIndexMean[trank, drank]
    return(index*sin(Bearing/180*pi))
  }
  
  if(source=="salt"){
      if(gradient=="numerical"){ #numerical以外は未対応
      C <- get.C(X,Y,T)
      dCdX <- get.dCdX(X,Y,T)
      dCdY <- get.dCdY(X,Y,T)
      dCdLat <- - dCdX*sin(Theta/180*pi) + dCdY*cos(Theta/180*pi)
      crank <- ceiling((C - TRCdCdL.xfrom)/TRCdCdL.xby)
      clrank <- ceiling((dCdLat - TRCdCdL.yfrom)/TRCdCdL.yby)
      if(!timeseries){
        if(crank>dim(TRCdCdL)[1] || crank<=0 || clrank>dim(TRCdCdL)[2] || clrank<=0){ TR<-NA
        }else{ TR<-TRCdCdL[crank,clrank]}
        if(is.na(TR)) TR<-TRCdCdL.mean
      }else{
        trank <- ceiling((T-probabCdCdT.Tfrom)/probabCdCdT.Tspan)
        if(crank>dim(TRCdCdLT)[1] || crank<=0 || clrank>dim(TRCdCdLT)[2] || clrank<=0 || trank>dim(TRCdCdLT)[3] || trank<=0){ TR<-NA
        }else{ TR<-TRCdCdLT[crank,clrank,trank]}
        if(is.na(TR)) TR<-TRCdCdLT.mean[trank]
      }
      return(TR)
      }
  }
  
  }
#For simulate, Pirprobab
  Pirprobab <- function(T, X, Y, Dist, Bearing, dCdT, timeseries, pir, basal.pirouette){
  
    if(source=="odor"){
      if(!exists("PirIndexT")){cat("No Pirprobab: PirIndexT found\n")}
      if(!exists("BasalPirT")){cat("No Pirprobab: BasalPirT found\n")}
      trank <- ceiling(T/300)
      drank <- ceiling(Dist/15)
      ##added
      if(trank <= 3) trank <- 1 else trank <- 6
      drank <- 1 
      if(drank>4) {index <- 0; bindex <- 0}
      else
      {
        if(PirIndexT[trank, drank] > 0.05) index <- 0 else index <- PirIndexMean[trank, drank]
        if(BasalPirT[trank, drank] > 0.05) bindex <- 0 else bindex <- BasalPirMean[trank, drank]
      }
      if(pir){return(bindex - index*cos(Bearing/180*pi))
      }else{ return(basal.pirouette) }
    }
    
    if(source=="salt"){
      if(!pir && !is.na(basal.pirouette)) {return(basal.pirouette)
      }else{
        if(gradient=="numerical"){
          C <- get.C(X,Y,T)
          crank <- ceiling((C - probabCdCdT.xfrom)/probabCdCdT.xby)
          ctrank <- ceiling((dCdT - probabCdCdT.yfrom)/probabCdCdT.yby)
          if(!timeseries){
            if(crank>dim(probabCdCdT)[1] || crank<=0 || ctrank>dim(probabCdCdT)[2] || ctrank<=0){probab<-NA
            }else{probab<-probabCdCdT[crank,ctrank]}
            if(is.na(probab) || (!pir && is.na(basal.pirouette)) ) probab<-probabCdCdT.mean
          }else{
            trank <- ceiling((T-probabCdCdT.Tfrom)/probabCdCdT.Tspan)
            if(crank>dim(probabCdCdTT)[1] || crank<=0 || ctrank>dim(probabCdCdTT)[2] || ctrank<=0 || trank>dim(probabCdCdTT)[3] || trank<=0){
              probab<-NA
            }else{
              probab<-probabCdCdTT[crank,ctrank,trank]
            }
            if(is.na(probab) || (!pir && is.na(basal.pirouette)) ) probab<-probabCdCdTT.mean[trank]
          }
          return(probab)
        }
      }
    }
    
  }
#DTheta for simulate  #30deg each
  DTheta <- function(Bearing){
    P <- runif(1)
    sumP <- 0
    if(!is.nan(Bearing)){
      for(i in 1:12)
      {
        sumP <- sumP + BdeltaTheta[ceiling((Bearing+180)/30), i]
        if(sumP >= P) return(i*30-195)
      } 
    }else{  #No gradient and numeric then Bearing=NaN
      for(i in 1:12)
      {
        sumP <- sumP + mean(BdeltaTheta[ ,i])
        if(sumP >= P) return(i*30-195)
      }
    }
  }

#Function routine.simulation
co$routine.simulation<-'
routine.simulation()
Batch execute the following
backup()
save.image()
simulate(100,"RW",T,T,Tstep=1,initialX=50,initialY=70)
multiplotxy(division.t=9)
savePlot("simulate(100,RW,T,T,Tstep=1,initialX=50,initialY=70)multiplotxy.tiff","tiff")
multiplotxy(division.t=9,type="density")
savePlot("simulate(100,RW,T,T,Tstep=1,initialX=50,initialY=70)multiplotxy_density.tiff","tiff")
'
routine.simulation <- function(){
backup()
save.image()
simulate(100,"RW",T,T,Tstep=1,initialX=50,initialY=70)
multiplotxy(division.t=9)
savePlot("simulate(100,RW,T,T,Tstep=1,initialX=50,initialY=70)multiplotxy.tiff","tiff")
multiplotxy(division.t=9,type="density")
savePlot("simulate(100,RW,T,T,Tstep=1,initialX=50,initialY=70)multiplotxy_density.tiff","tiff")
}

#######################################################
###################                            ########
co$oooooo<-'\n########     Batch execution      ########
'##################                            ######
#######################################################

#Function routine.kunitomo
co$routine.kunitomo<-'
routine.kunitomo()
Batch execute following
setdir("./data/")
read.files(type="multi")
adjust.position(type="kunitomo")
calc.dL()
findPir()
calc.PirRun()
calc.TurnRate()
calc.C(type="plug")
calc.Bearing(plate_format="kunitomo")
before.after()
savePlot("before.after.tiff","tiff")
plot.PirCdCdT(30,100,2.5,-0.6,0.6,0.025,0.1,20,persp=F)
savePlot("plot.PirCdCdT(30,100,2.5,-0.6,0.6,0.025,0.1,20,persp=F).tiff","tiff")
calc.speed()
savePlot("worm.speed.tiff","tiff")
save.image("result.RData")
plot.TRCdCdLat(30,100,2.5,-3,3,0.2,-20,20)
savePlot("plot.TRCdCdLat(30,100,2.5,-3,3,0.2,-20,20).tiff","tiff")
maxT
maxT<-ceiling(maxT/100)*100
multiplotxy(division.t=9)
savePlot("multiplotxy.tiff","tiff")
multiplotxy(division.t=9,type="density")
savePlot("multiplotxy_density.tiff","tiff")
'

routine.kunitomo <- function(){
setdir("./data/")
read.files(datatype="multi")
adjust.position(plate_format="kunitomo")
calc.dL()
findPir()
calc.PirRun()
calc.TurnRate()
calc.C(type="plug")
calc.Bearing(plate_format="kunitomo")
before.after()
savePlot("before.after.tiff","tiff")
plot.PirCdCdT(30,100,2.5,-0.6,0.6,0.025,0.1,20,persp=F)
savePlot("plot.PirCdCdT(30,100,2.5,-0.6,0.6,0.025,0.1,20,persp=F).tiff","tiff")
calc.speed()
savePlot("worm.speed.tiff","tiff")
save.image("result.RData")
plot.TRCdCdLat(30,100,2.5,-3,3,0.2,-20,20)
savePlot("plot.TRCdCdLat(30,100,2.5,-3,3,0.2,-20,20).tiff","tiff")
maxT
maxT<-ceiling(maxT/100)*100
multiplotxy(division.t=9)
savePlot("multiplotxy.tiff","tiff")
multiplotxy(division.t=9,type="density")
savePlot("multiplotxy_density.tiff","tiff")
}


########################
##################
# main
##################
########################

cat('＜',version,'＞\n')
show.message <- TRUE
