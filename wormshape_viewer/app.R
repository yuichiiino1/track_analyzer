library(shiny)
library(colorRamps)

#load("revomega.RData")
#infile <- file.choose()
#infile <- "working/revomega.RData"
#load(infile)

#load("Contour.RData")
#infile <- file.choose()
#infile <- "Contour.RData"
#load(infile)

if(exists("Contour")){
  show.contour <- T
}else{
  show.contour <- F
  cat("Contour plot is not shown because contour data was not found.")
}


#cat("Need to preload revomega.RData and Contour.RData\n")

# (track:turn.track[i], points:turn.point[i]) of i'th turn is pre-given
# rv$plotstart:rv$plotend is determined from turn.point[i]


# start of TurnA is listed by i.

default_pointsbefore = 20
default_pointsafter = 30
default_sigma = 0.65
tracks = 1:track.n
#gauges <- c(0.10, 0.15, 0.20, 0.25, 0.3)

rainbow=TRUE; txt="i"; xlim=c(); ylim=c(); cex=0; cex2=0; cextxt=0; type="o"; 
main=""; Pircolor="green"; Turncolor="red"; TurnAcolor="blue"; additional=15

#mag = 0.04600 # mm/pixel

track.plate <- rep(NA, track.n)
cdT <- list()
for(plate.i in 1:plate.n){
  track.plate[ plate.track[[plate.i]] ] <- plate.i
  cdT[[plate.i]] <- sort(unique(unlist(dT[ plate.track[[plate.i]] ])))
}

##############################################################################
#####################                        #################################                                  
#####################        show GUI        #################################
#####################                        #################################
##############################################################################

ui <- fluidPage(

  # Sidebar panel for inputs ----
  #sidebarPanel(
  column(width=6,
         
         h3("Wormtrack viewer"),
         fluidRow(
           column(width=4,
           textInput(inputId="turn_number",label="Turn no:",value=1),
           uiOutput('ntracks')
           ),
           column(width=8,
           actionButton(inputId="random", label="random",width='30%'),
           actionButton(inputId="previous", label="previous",width='40%'),
           actionButton(inputId="following", label="next",width='20%')
           )
         ),
         fluidRow(
           column(width=6, numericInput(inputId = "pointsbefore", label = "Points before:", value = default_pointsbefore)),
           column(width=6, numericInput(inputId = "pointsafter", label = "Points after:", value = default_pointsafter))
         ),
         fluidRow(
           #column(width=6, checkboxInput(inputId="T1", label="T1 only", value=TRUE)),
           column(width=6, actionButton(inputId="reset_span", label="Reset"))
         ),
         fluidRow(
           column(width=12,sliderInput(inputId = "sigma", label="sigma:", min=1e-5, max=5, value=default_sigma))
         ),
         fluidRow(
           column(width=12,selectInput(inputId = "used_gauge", label = "Use gauge:", choices = gauges, selected = 1))
         ),         
         plotOutput(outputId = "contourPlot", width="250pt",height="250pt"),
         fluidRow(
           column(width=4, sliderInput(inputId = "frame", label="frame:", min=0, max=1, value=0,
             animate = animationOptions(interval=500, loop = TRUE, playButton = "F",  pauseButton = "P"))),
           column(width=2, actionButton(inputId="prev_frame", label="Prev")),
           column(width=2, actionButton(inputId="next_frame", label="Next"))
         ),
         
         fluidRow(
           actionButton(inputId="rev_omega", label="Rev-Omega",width='45%'),
           actionButton(inputId="rev_forward", label="Rev-Forward",width='45%'),
           actionButton(inputId="omega", label="Omega",width='45%'),
           actionButton(inputId="omega_post", label="Omega-postRev",width='45%'),
           actionButton(inputId="other", label="Other/Complexed",width='45%'),
           actionButton(inputId="unknown", label="Unclear",width='45%')
         ),
         fluidRow(
           column(width=6,numericInput(inputId = "turnpoint", label="Turn point", value=1)),
           column(width=6,numericInput(inputId = "omegapoint", label="Omega point", value=2)),
           column(width=6,numericInput(inputId = "distance", label="Distance", value=1)),
           column(width=6,actionButton(inputId = "save", label="Save data"))
         ),
         fluidRow(
           textInput(inputId="message_out", label="Message", value="")
         )
         
  ),
  
  # Main panel for displaying outputs ----
  #mainPanel(
  column(width=6,
         # Output: Histogram ----
         plotOutput(outputId = "trackPlot", width="300pt",height="300pt"),
         plotOutput(outputId = "circularityPlot", height="200pt"),
         plotOutput(outputId = "anglePlot", height="200pt")
  )
  
)

##############################################################################
######################                                  ######################
######################  Server for realtime processing  ######################
######################                                  ######################
##############################################################################


server <- function(input, output, session) {
  
  rv <- reactiveValues()
  #i <- sample(length(turn.track),1)
  #i <- which(turn.Tn==1)[sample(sum(turn.Tn==1),1)]
  #i <- which(turn.Tn==1)[sample(100,1)]
  i <- 1 #sample(length(turn.track),1)
  rv$i <- i  # you cannot get value from a reactive value but can just put a value
  fillen <- default_pointsbefore + default_pointsafter
  xs <- ((-fillen):fillen)/default_sigma
  rv$gaussfilter <- exp(-1/2*xs*xs)
  
  rv$omegapoints <- rep(NA, length(gauges))
  rv$used_m <- 1
  LTrange <- 1
  
  output$ntracks <- renderText({
    paste0('/', length(turn.track), ' turns')
  })
  
  observeEvent(input$sigma, {
    rv$fillen <- input$pointsbefore + input$pointsafter
    xs <- ((-rv$fillen):rv$fillen)/input$sigma
    rv$gaussfilter <- exp(-1/2*xs*xs)
  })
  
  observeEvent(c(input$pointsbefore, input$pointsafter), {
    rv$plotstart <- max(1, turn.point[rv$i]-input$pointsbefore)
    rv$plotend <- min(turn.point[rv$i]+input$pointsafter, point.n[turn.track[rv$i]])
    if((input$pointsbefore + input$pointsafter)*2 +1 > length(rv$gaussfilter)){
      # remake filter
      rv$fillen <- input$pointsbefore + input$pointsafter
      xs <- ((-rv$fillen):rv$fillen)/input$sigma
      rv$gaussfilter <- exp(-1/2*xs*xs)
    }
  })
  
  rv$plotstart <- max(1,turn.point[i]-default_pointsbefore)
  rv$plotend <- min(point.n[turn.track[i]],turn.point[i]+default_pointsafter)
  
  observeEvent(input$turn_number,{
    if(!is.na(suppressWarnings(as.numeric(input$turn_number)))){
      rv$i <- as.numeric(input$turn_number)
      rv$plotstart <- max(1,turn.point[rv$i]-input$pointsbefore)
      rv$plotend <- min(point.n[turn.track[rv$i]],turn.point[rv$i]+input$pointsafter)
      updateSliderInput(session, "frame", value = 0)
      rv$ti <- 1
    }
  })
  
  observeEvent(input$random,{
    #if(input$T1){
    #rv$i <- which(turn.Tn==1)[sample(sum(turn.Tn==1),1)]
    #rv$i <- which(turn.Tn==1)[sample(100,1)]
    #}else{
    rv$i <- sample(length(turn.track),1)
    #}
    rv$plotstart <- max(1,turn.point[rv$i]-input$pointsbefore)
    rv$plotend <- min(point.n[turn.track[rv$i]],turn.point[rv$i]+input$pointsafter)
    updateSliderInput(session, "frame", value = 0)
    rv$ti <- 1
    updateNumericInput(session, "turn_number", value = rv$i)
  })
  
  observeEvent(input$previous,{
    if(rv$i>1) rv$i <- rv$i - 1
    #while(input$T1 && turn.Tn[rv$i]!=1 && rv$i>1) rv$i <- rv$i - 1
    rv$plotstart <- max(1,turn.point[rv$i]-input$pointsbefore)
    rv$plotend <- min(point.n[turn.track[rv$i]],turn.point[rv$i]+input$pointsafter)
    updateSliderInput(session, "frame", value = 0)
    rv$ti <- 1
    updateNumericInput(session, "turn_number", value = rv$i)
  })
  
  observeEvent(input$following,{
    if(rv$i<length(turn.point)) rv$i <- rv$i + 1
    #while(input$T1 && turn.Tn[rv$i]!=1 && rv$i<length(turn.point)) rv$i <- rv$i + 1
    rv$plotstart <- max(1,turn.point[rv$i]-input$pointsbefore)
    rv$plotend <- min(point.n[turn.track[rv$i]],turn.point[rv$i]+input$pointsafter)
    updateSliderInput(session, "frame", value = 0)
    rv$ti <- 1
    updateNumericInput(session, "turn_number", value = rv$i)
  })
  
  observeEvent(input$used_gauge,{
    rv$used_m <- which(gauges==input$used_gauge)
  })
  
  observeEvent(input$reset_span, {
    updateNumericInput(session, "pointsbefore", value = default_pointsbefore)
    updateNumericInput(session, "pointsafter", value = default_pointsafter)
  })

  observeEvent(input$frame, {
    rv$ti <- round((rv$plotend - rv$plotstart) * input$frame) + 1
  })
  observeEvent(input$prev_frame, {
    rv$ti <- rv$ti-1
    if(rv$ti<1) rv$ti <- 1
    updateSliderInput(session, "frame", value = (rv$ti-1)/(rv$plotend - rv$plotstart))
  })
  observeEvent(input$next_frame, {
    rv$ti <- rv$ti+1
    if(rv$ti>rv$plotend-rv$plotstart+1) rv$ti <- rv$plotend-rv$plotstart+1
    updateSliderInput(session, "frame", value = (rv$ti-1)/(rv$plotend - rv$plotstart))
  })
  
  observeEvent(input$save, {
    filename <- paste0("revomega_",format(Sys.time(), "%Y%m%d_%H%M%S"),".RData")
    
    save("dX","dY","TurnStartA","TurnRunA","TurnAngle","gauges", "track.n","point.n",
         "turn.track","turn.point", file=filename)
    cat("Saved data in \"",filename,"\".\n",sep="")
    updateNumericInput(session, "message_out", value = paste0("Saved data in \"",filename,"\".\n"))
  })
  
  
  observe({
    updateNumericInput(session, "turnpoint", value = turn.point[rv$i])
  })
  
  observe({
    updateNumericInput(session, "omegapoint", value = rv$omegapoints[rv$used_m])
  })
  
  observe({
    updateNumericInput(session, "distance", value = input$omegapoint - input$turnpoint)
  })
  
  ########################
  ###  plot the track  ###
  ########################
  
  output$trackPlot <- renderPlot({
    
    #plotstart <- rv$plotstart
    #plotend <- rv$plotend
    #print(rv$i)
    i <- rv$i # track turn.track[i], points rv$plotstart:rv$plotend of i'th pirouette
    
    X <- dX[[turn.track[i]]][rv$plotstart:rv$plotend]
    Y <- dY[[turn.track[i]]][rv$plotstart:rv$plotend]
    
    maxX <- max(X)
    minX <- min(X)
    meanX <- mean(X)
    midX <- (maxX+minX)/2
    maxY <- max(Y)
    minY <- min(Y)
    meanY <- mean(Y)
    midY <- (maxY+minY)/2
    span2 <- max(maxX-minX,maxY-minY)/2
    
    if(length(xlim) == 0) rv$plotxlim <- c(midX-span2,midX+span2)
    if(length(ylim) == 0) rv$plotylim <- c(midY-span2,midY+span2)
    if(cex==0) cex<-1.2
    if(cex2==0) cex2<-0.8
    if(cextxt==0) cextxt<-0.8
    
    par(mar=c(3,4,2,1))
    par(mgp=c(2,1,0))
    
    plot(X,Y, xlab="X", xlim=rv$plotxlim, ylim=rv$plotylim, ylab="Y", asp=1, cex=cex, type=type, 
         main=paste("turn ",i, " = track ", turn.track[i],", point ",turn.point[i], " (", rv$plotstart,"-",rv$plotend,")",sep=""))
    
    # show salt gradient direction
    tpx <- X[turn.point[i]-rv$plotstart+1]
    tpy <- Y[turn.point[i]-rv$plotstart+1]
    dcx <- dCdX[[turn.track[i]]][turn.point[i]] # determined at the initial potision 
    dcy <- dCdY[[turn.track[i]]][turn.point[i]]
    arrowx <- dcx/(sqrt(dcx^2+dcy^2))/4
    arrowy <- dcy/(sqrt(dcx^2+dcy^2))/4
    arrows(tpx,tpy,tpx+arrowx,tpy+arrowy,length=0.1,angle=45, lwd=2, col="lightblue")
    
    if(rainbow){
      for(j in 1:(length(X)-1)){
        lines(X[j:(j+1)],Y[j:(j+1)], col=green2red(length(X)-1)[j])
      }
    }
    #if(Pircolor != "") points(X[PirRun[[turn.track[i]]][rv$plotstart:rv$plotend]=="P"],Y[PirRun[[turn.track[i]]][rv$plotstart:rv$plotend]=="P"],col=Pircolor,pch=16,cex=cex2)
    #if(Turncolor != "") points(X[TurnRun[[turn.track[i]]][rv$plotstart:rv$plotend]=="T"],Y[TurnRun[[turn.track[i]]][rv$plotstart:rv$plotend]=="T"],col=Turncolor,pch=16,cex=cex2)
    if(TurnAcolor != "") points(X[TurnRunA[[turn.track[i]]][rv$plotstart:rv$plotend]=="T"],Y[TurnRunA[[turn.track[i]]][rv$plotstart:rv$plotend]=="T"],col=TurnAcolor,pch=16,cex=cex2)
    #points(X[PirStart[[turn.track[i]]][rv$plotstart:rv$plotend]],Y[PirStart[[turn.track[i]]][rv$plotstart:rv$plotend]],col="black",pch=16,cex=cex2)
    #points(X[PirEnd[[turn.track[i]]][rv$plotstart:rv$plotend]],Y[PirEnd[[turn.track[i]]][rv$plotstart:rv$plotend]],col="black",pch=16,cex=cex2)
    if(turn.point[i]>=rv$plotstart && turn.point[i]<=rv$plotend){
      points(X[turn.point[i]-rv$plotstart+1],Y[turn.point[i]-rv$plotstart+1],pch=3,col="red",cex=cex*1.5)
    }
    if(!is.na(rv$omegapoints[rv$used_m])){
    if(rv$omegapoints[rv$used_m]>=rv$plotstart && rv$omegapoints[rv$used_m]<=rv$plotend){
      points(X[rv$omegapoints[rv$used_m]-rv$plotstart+1],Y[rv$omegapoints[rv$used_m]-rv$plotstart+1],pch=3,col="blue",cex=cex*1.5)
    }
    }
    if(txt=="i"){  
      text(X,Y,rv$plotstart:rv$plotend,pos=4,cex=cextxt)
    }
    
    
    #if(txt=="TR"){
    #  text(X,Y,TurnRate[[turn.track[i]]][rv$plotstart:rv$plotend],pos=4,cex=cextxt)
    #}
  })
  
if(show.contour){
  ##########################
  ###  plot the contour  ###
  ##########################
  library(EBImage)
  freeman_code <- matrix(c(1,0, 1,-1, 0,-1, -1,-1, -1,0, -1,1, 0,1, 1,1),8,2,byrow=T)
  freeman_code_x <- freeman_code[,1]
  freeman_code_y <- freeman_code[,2]
    
  freeman2contour <- function(freeman1, origin.x=0, origin.y=0){
    # freeman1 = freeman+1
    # calculate contour as a series of (x,y) in pixel coordinates
    freemanx <- freeman_code_x[freeman1]
    freemany <- freeman_code_y[freeman1]
    contourx <- origin.x + cumsum(freemanx)
    contoury <- origin.y + cumsum(freemany)
    return(cbind(contourx,contoury))
  }

  get.worm <- function(contour.worm){
    freeman <- as.integer(contour.worm[[4]])+1
    origin.x <- contour.worm[[1]]
    origin.y <- contour.worm[[2]]
    total <- contour.worm[[3]]
    cXY <- c();contourxy <- c()
    if(total > 0 && max(freeman)<=8){
      contourxy <- freeman2contour(freeman, origin.x, origin.y)
      cx <- contourxy[,1,drop=F]*Transform[[track.plate[turn.track[rv$i]]]]$mag[[1]]
      cy <- contourxy[,2,drop=F]*Transform[[track.plate[turn.track[rv$i]]]]$mag[[1]]
      XY0 <- Transform[[track.plate[turn.track[rv$i]]]]$XY0
      cXY <- cbind(cx-XY0[1], cy-XY0[2]) %*% 
        Transform[[track.plate[turn.track[rv$i]]]]$A +
        matrix(Transform[[track.plate[turn.track[rv$i]]]]$b, nrow(cx), 2, byrow=T)
    }
    return(list(cXY=cXY, contourxy=contourxy))
  }

  output$contourPlot <- renderPlot({
    
    #tic <- proc.time();cat("start time\n")  ##########
    
    i <- rv$i
    Trange <- dT[[turn.track[i]]][rv$plotstart:rv$plotend]
    LTrange <<- length(Trange)
    t <- Trange[rv$ti]
    par(mar=c(3,4,2,1))
    par(mgp=c(2,1,0))
    plot(c(),c(), xlab="X", xlim=rv$plotxlim, ylim=rv$plotylim, ylab="Y", asp=1, 
         main=paste("turn ",i,"= track ", turn.track[i],", point ",turn.point[i], " (", rv$plotstart+rv$ti-1,")",sep=""))
    
    #toc <- proc.time();print(toc-tic);tic<-toc ###############
    
    plate.i <- track.plate[turn.track[i]]
    contour.plate <- Contour[[plate.i]]
    
    #toc <- proc.time();print(toc-tic);tic<-toc ###############
    
    # determine centroids
    Tvec <- cdT[[plate.i]]
    cti <- which.min(abs(Tvec-t)) # find index of cdT nearst to real time dT
    contour.t <- contour.plate[[cti]]
    centX <- rep(NA, length(contour.t))
    centY <- rep(NA, length(contour.t))
    for(worm.i in 1:length(contour.t)){
      cXY <- get.worm(contour.t[[worm.i]])$cXY           ################################
      if(length(cXY)>0){
      centX[worm.i] <- mean(cXY[,1])
      centY[worm.i] <- mean(cXY[,2])
      }
    }
    
    #toc <- proc.time();print(toc-tic);tic<-toc ###############
    
    # find closest contour
    matched.worm <- which.min(
      (centX-dX[[turn.track[i]]][rv$plotstart+rv$ti-1])^2 +
      (centY-dY[[turn.track[i]]][rv$plotstart+rv$ti-1])^2)
    gw <- get.worm(contour.t[[matched.worm[1] ]])
    cXY <- gw$cXY
    lines(c(cXY[,1],cXY[1,1]),c(cXY[,2],cXY[1,2]), type="l", col="blue")
    
    contour <- gw$contourxy
    xmin1 <- min(contour[,1])-1
    ymin1 <- min(contour[,2])-1
    contx <- contour[,1] - xmin1
    conty <- contour[,2] - ymin1  # (contx,conty) is contour points shifted to bottomleft
    nx <- max(contx)
    ny <- max(conty)
    border <- matrix(0, nx, ny)
    for(i in 1:length(contx)) border[contx[i],conty[i]] <- 1
    realworm <- fillHull(border)
    
    #toc <- proc.time();print(toc-tic);tic<-toc ###############
    
    wfts <- computeFeatures.shape(realworm)
    wftm <- computeFeatures.moment(realworm)
    
    text(mean(cXY[,1]),mean(cXY[,2]),labels=rv$plotstart+rv$ti-1)
    
    # draw nearby worms
    nearby.worm <- which(
      centX >= rv$plotxlim[1] & centX <= rv$plotxlim[2] &
      centY >= rv$plotylim[1] & centY <= rv$plotylim[2])
    for(wormi in nearby.worm){
      if(wormi != matched.worm){
        gw <- get.worm(contour.t[[wormi]])
        cXY <- gw$cXY
        lines(c(cXY[,1],cXY[1,1]),c(cXY[,2],cXY[1,2]), type="l", col="gray")
      }
    }

    #toc <- proc.time();print(toc-tic);tic<-toc ###############
    
  })
}
  
  #############################
  ### show circularity plot ###
  #############################
  
  output$circularityPlot <- renderPlot({
  
    i <- rv$i
    plate.i <- track.plate[turn.track[i]]
    Tvec <- cdT[[plate.i]]
    mag <- mags[[plate.i]]
    contour.plate <- Contour[[plate.i]]
    
    areavec <- c()
    roundnessvec <- c()
    lengthvec <- c()
    eccentricityvec <- c()
    
    for(point.i in rv$plotstart:rv$plotend){
      #cat("point.i=",point.i,"\n")
      #cat(length(dT[[turn.track[i]]]),"\n")
      # find closest contour
      t <- dT[[turn.track[i]]][point.i]
      #print(Tvec)
      #print(t)
      if(is.na(t)){
        browser()
      }
      cti <- which.min(abs(Tvec-t)) # find index of cdT nearst real time dT
      #print(cti)
      contour.t <- contour.plate[[cti]]
      # all worms at this time point
      centX <- rep(NA, length(contour.t))
      centY <- rep(NA, length(contour.t))
      for(worm.i in 1:length(contour.t)){
        cXY <- get.worm(contour.t[[worm.i]])$cXY           ################################
        if(length(cXY)>0){
        centX[worm.i] <- mean(cXY[,1])
        centY[worm.i] <- mean(cXY[,2])
        }
      }
            
      # find closest worm to track centroid
      matched.worm <- which.min(
        (centX-dX[[turn.track[i]]][point.i])^2 +
        (centY-dY[[turn.track[i]]][point.i])^2)
      gw <- get.worm(contour.t[[matched.worm[1] ]])
      cXY <- gw$cXY
      #cat("matched.worm=",matched.worm,"\n")
      # calculate shape
      contour <- gw$contourxy
      xmin1 <- min(contour[,1])-1
      ymin1 <- min(contour[,2])-1
      contx <- contour[,1] - xmin1
      conty <- contour[,2] - ymin1  # (contx,conty) is contour points shifted to bottomleft
      nx <- max(contx)
      ny <- max(conty)
      border <- matrix(0, nx, ny)
      for(tempi in 1:length(contx)) border[contx[tempi],conty[tempi]] <- 1
      realworm <- fillHull(border)
      wfts <- computeFeatures.shape(realworm)
      wftm <- computeFeatures.moment(realworm)
            
      areavec <- c(areavec, wfts[1,"s.area"]*mag^2)
      roundnessvec <- c(roundnessvec, (wfts[1,"s.perimeter"]^2)/(4*pi*wfts[1,"s.area"]))
      lengthvec <- c(lengthvec, wfts[1,"s.radius.max"]) # wfts[1,"s.radius.max"]/wfts[1,"s.radius.min"]
      eccentricityvec <- c(eccentricityvec, wftm[1,"m.eccentricity"])
    }
    par(mar=c(4,4,1,8))
    par(mgp=c(2,1,0))
    par(xpd=T)
    matplot(rv$plotstart:rv$plotend, cbind(areavec*10, roundnessvec, lengthvec/5, eccentricityvec*2), #aspectvec/50, 
            type="l",lty=1, ylab="value", xlab="timepoints", col=1:4)
        legend(par()$usr[2]+0.5, par()$usr[4],lty=1,
           col=1:4,legend=c("area*10","roundness", "length/5", "eccentricity*2"))
  })
  
  #######################
  ### show angle plot ###
  #######################
  
  output$anglePlot <- renderPlot({
    

    i <- rv$i
    
    plotd <- abs(TurnAngle[[turn.track[i]]][,rv$plotstart:rv$plotend])
    M <- dim(plotd)[1]
    N <- dim(plotd)[2]
    plotdn <- plotd
    plotdn[is.na(plotd)] <- 0
    #print(dim(plotdn))
    #print((rv$plotend-rv$plotstart)+1)
    #print(length(rv$gaussfilter))
    #print(length(rv$gaussfilter[(rv$fillen+2-(rv$plotend-rv$plotstart+1)):(rv$fillen+2-(rv$plotend-rv$plotstart+1)+rv$plotend-rv$plotstart)]))
    smplotd <- sapply(1:(rv$plotend-rv$plotstart+1), function(x) plotdn %*% 
                        rv$gaussfilter[(rv$fillen+2-x):(rv$fillen+2-x+rv$plotend-rv$plotstart)]/
                        rowSums(matrix(rv$gaussfilter[(rv$fillen+2-x):(rv$fillen+2-x+rv$plotend-rv$plotstart)],M,N,byrow=T)*(!is.na(plotd)))
    )

    # search omegapoint candidate
    
    omegapoints <- rep(NA, M)
    for(m in 1:M){
      downflag <- F
      upflag <- F
      peakflag <- F
      #current <- smplotd[m,turn.point[i]-rv$plotstart+1]
      if(turn.point[i]-rv$plotstart+1 >=1 && turn.point[i]-rv$plotstart+2 <= N){
        for(j in (turn.point[i]-rv$plotstart+2):N){
          if(!downflag){
            if(smplotd[m,j]<smplotd[m,j-1]) downflag <- T # need to go down first
          }else{
            if(!upflag){ # then need to go up
              if(smplotd[m,j]>smplotd[m,j-1]) upflag <- T
            }else{ # finally go down after the peak
              if(smplotd[m,j]<smplotd[m,j-1]){
                peakflag <- T # omega peak
                omegapoints[m] <- j+rv$plotstart-2
                break
              }
            }
          }
        }
      }
    } # m loop
    rv$omegapoints <- omegapoints
    
    par(mar=c(4,4,1,8))
    par(mgp=c(2,1,0))
    par(xpd=T)
    matplot(rv$plotstart:rv$plotend, t(plotd),
            type="p", pch=1, ylab="abs(TurnAngle)", xlab="timepoints",
            col=rainbow(M,start=0.25,end=0.1))
    for(m in 1:M){
      lines(rv$plotstart:rv$plotend, smplotd[m,], col=rainbow(M,start=0.25,end=0.1)[m])
      if(!is.na(rv$omegapoints[m])){
        points(rv$omegapoints[m], smplotd[m,rv$omegapoints[m]-rv$plotstart+1], pch=16, col=rainbow(M,start=0.25,end=0.1)[m])
      }
    }
    
    legend(par()$usr[2]+0.5, par()$usr[4],lty=1,pch=16,
           col=rainbow(M,start=0.25,end=0.1),legend=paste("g=",gauges,sep=""))
    abline(v=turn.point[i],col="black")
    
    #print(smplotd) 
    
  })
  
}

### show app ###
shinyApp(ui = ui, server = server)


