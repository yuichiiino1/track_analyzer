bearing.mean.timecourse <- function(plates=1:length(plate.track), tracks=1:track.n, minturns=1, maxturns=Inf, 
                            initbearspan=180, nangles=8, timebin=1, timerepeats=50, plottimespan=50, plotprevtime=15){
  
  # Contour必要。
  # ピルエット中最初のTurnA("T1")のあとのオメガターン（roundness < roundess.rhresholdの最初の点）をtime0として、以下時間を追って平均（ベクトル平均）のBearingをプロット。
  # T1直後のBearingが一定範囲ごとのピルエットだけ選んでそれぞれ追跡、平均する。
  # トラック範囲tracksを指定したり、ターン回数がminturnsからmaxturnsのピルエットのみ選択できる（デフォールトではすべて）。
  # 
  
  area.threshold <- 2.5
  roundness.threshold <- 1.2
  
  #library(colorRamps)
  if(!exists("dCdX") || !exists("dCdY")){
    cat("Either dCdX or dCdY are missing\n")
    return("")
  }
  if(!exists("Contour")){
    cat("Contour is missing\n")
    return("")
  }  

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
  
  get.worm <- function(contour.worm, plate.i){
    freeman <- as.integer(contour.worm[[4]])+1
    origin.x <- contour.worm[[1]]
    origin.y <- contour.worm[[2]]
    total <- contour.worm[[3]]
    cXY <- c();contourxy <- c()
    if(total > 0 && max(freeman)<=8){
      contourxy <- freeman2contour(freeman, origin.x, origin.y)
      cx <- contourxy[,1,drop=F]*Transform[[plate.i]]$mag[[1]]
      cy <- contourxy[,2,drop=F]*Transform[[plate.i]]$mag[[1]]
      XY0 <- Transform[[plate.i]]$XY0
      cXY <- cbind(cx-XY0[1], cy-XY0[2]) %*% 
        Transform[[plate.i]]$A +
        matrix(Transform[[plate.i]]$b, nrow(cx), 2, byrow=T)
    }
    return(list(cXY=cXY, contourxy=contourxy))
  }
  
  get.shape.descriptors <- function(contourxy,plate.i){
    
    # calculate shape

    xmin1 <- min(contourxy[,1])-1
    ymin1 <- min(contourxy[,2])-1
    contx <- contourxy[,1] - xmin1
    conty <- contourxy[,2] - ymin1  # (contx,conty) is contour points shifted to bottomleft
    nx <- max(contx)
    ny <- max(conty)
    border <- matrix(0, nx, ny)
    for(tempi in 1:length(contx)) border[contx[tempi],conty[tempi]] <- 1
    realworm <- fillHull(border)
    wfts <- computeFeatures.shape(realworm)
    wftm <- computeFeatures.moment(realworm)
    
    area <- wfts[1,"s.area"]*Transform[[plate.i]]$mag[[1]]^2
    roundness <- (wfts[1,"s.perimeter"]^2)/(4*pi*wfts[1,"s.area"])
    return(list(area=area,roundness=roundness))
  }
  
  cdT <- list()
  for(plate.i in plates){
    cdT[[plate.i]] <- sort(unique(unlist(dT[ plate.track[[plate.i]] ])))
  }
  
  # data collection
  
  data.collection <- function(){
    bT1After <- c()
    Us <- c()
    Vs <- c()
    for(plate.i in plates){
      cat("plate",plate.i,"\n")
    for(track.i in plate.track[[plate.i]]){
      n <- point.n[track.i]
      R <- (Pirsurround[[track.i]] == "R")
      P <- (Pirsurround[[track.i]] == "P")
      start <- which(R & c(P[2:n],F)) #ピルエット開始点ひとつ前の添え字の列
      end <- which(R & c(F,P[1:(n-1)])) #ピルエット終了点のひとつ後の添え字の列
      if(length(end)>0 && length(start)>0){
        if(end[1]<=start[1]) end <- end[-1]
        if(length(end)>0)
        {
          
          if(length(start)>length(end)) start <- start[-length(start)]
          startTheta <- AvTheta[[track.i]][start]
          endTheta <- AvTheta[[track.i]][end]
          temp <- !is.na(startTheta) & !is.na(endTheta)  # boolean vector of usable pirouettes
          if(any(temp)){
            # pcount: 各有効ピルエット中のターンの数
            
            for(j in 1:length(which(temp))){
              
              # (start[temp][j]+1):(end[temp][j]-1) : j番目の有効のピルエットの期間の添字の列
              turnstarts <- which(TurnStart[[track.i]][(start[temp][j]+1):(end[temp][j]-1)])+start[temp][j]
              turnends <- which(TurnEnd[[track.i]][(start[temp][j]+1):(end[temp][j]-1)])+start[temp][j]
              pcount <- length(turnstarts)
              if(pcount>=minturns && pcount<=maxturns){
                
                if(turnstarts[1]>turnends[1]){cat("wrong turnstartsends\n");browser()}
                mainturn <- which(TurnRunA[[track.i]][turnstarts[1]:turnends[1]] == "T")
                
                if(length(mainturn)>=1){
                  # mainturn後はじめてTurnAでないものが次。
                  aftermainturn <- min(which(TurnRunA[[track.i]][(mainturn[1]+turnstarts[1]):point.n[track.i]]=="R"))+mainturn[1]+turnstarts[1]-1
                  bT1After = c(bT1After, Bearing[[track.i]][aftermainturn])
                  
                  #####  analyze contour  #####
                  #stn <- Sys.time();print(stn-syst);syst<-stn   ### 1
                  # T1 is at mainturn[1]+turnstarts[1]-1
                  
                  Tvec <- cdT[[plate.i]]
                  contour.plate <- Contour[[plate.i]]
                  
                  omega.start <- NA
                  omega.end <- point.n[track.i]
                  for(point.i in (turnstarts[1]+mainturn[1]-1):point.n[track.i]){
                     ## this loop takes time
                    
                    ###  find closest contour  ###
                    
                    #stn <- Sys.time();cat("1.1 ");print(stn-syst);syst<-stn  ### 1.1
                    
                    t <- dT[[track.i]][point.i]
                    cti <- which.min(abs(Tvec-t)) # find index of cdT nearst real time dT
                    contour.t <- contour.plate[[cti]] # all worms at this time point
                    
                    cent.t <- Contour.centroid[[plate.i]][[cti]]
                    if(cent.t$flag){ # if centX, centY data already exist
                      centX <- cent.t$centX # centroids of worms at this time point
                      centY <- cent.t$centY
                      #stn <- Sys.time();cat("1.11 ");print(stn-syst);syst<-stn  ### takes time
                    }else{ # if newly generate
                      centX <- rep(NA, length(contour.t))
                      centY <- rep(NA, length(contour.t))
                      for(worm.i in 1:length(contour.t)){
                        cXY <- get.worm(contour.t[[worm.i]], plate.i)$cXY
                        if(length(cXY)>0){
                          centX[worm.i] <- mean(cXY[,1])
                          centY[worm.i] <- mean(cXY[,2])
                        }
                      }
                      Contour.centroid[[plate.i]][[cti]] <<- list(flag=TRUE, centX=centX, centY=centY) # record
                      #stn <- Sys.time();cat("1.12 ");print(stn-syst);syst<-stn  ### takes time
                    }
                    #stn <- Sys.time();cat("1.2 ");print(stn-syst);syst<-stn  ### takes time
                    # find closest worm to track centroid
                    matched.worm <- which.min(
                      (centX-dX[[track.i]][point.i])^2 +
                        (centY-dY[[track.i]][point.i])^2)
                    gw <- get.worm(contour.t[[matched.worm[1] ]], plate.i)
  
                    shape <- get.shape.descriptors(gw$contourxy, plate.i) # call the function
                    area <- shape$area
                    roundness <- shape$roundness
                    
                    if(area > area.threshold){ # fused two worms
                      if(point.i == turnstarts[1]+mainturn[1]-1){ # at the beginning
                        break # omega.start remains as NA
                      }
                    }else{ # single worm
                      if(roundness < roundness.threshold){ # omega shape
                        if(is.na(omega.start)){ # first omega
                          omega.start <- point.i
                        }
                      }else{ # not omega shape
                        if(!is.na(omega.start)){ # already omega ocurred
                          omega.end <- point.i - 1 # this is the end of omega
                          break
                        }
                      }
                    }
                    #stn <- Sys.time();cat("1.3 ");print(stn-syst);syst<-stn  ###
                  } # end for(point.i)
                  
                  #### align at omegapoint  ####
                  #stn <- Sys.time();cat("2 ");print(stn-syst);syst<-stn  ###  2
                  endpoint <- point.n[track.i]
                  if(is.na(omega.start)){
                    tempX <- rep(NA, plotprevtime+1)
                    tempY <- rep(NA, plotprevtime+1)
                  }else{
                    omegapoint <- floor((omega.start+omega.end)/2) # position of omega turn midpoint
                    startpoint <- max(mainturn[1]+turnstarts[1]-1, 1, omegapoint-plotprevtime)
                    dummypoint <-  plotprevtime - (omegapoint - startpoint)
                    if(dummypoint > 0){
                      tempX <- c(rep(NA,dummypoint),dX[[track.i]][startpoint:endpoint])
                      tempY <- c(rep(NA,dummypoint),dY[[track.i]][startpoint:endpoint])
                    }else{
                      tempX <- dX[[track.i]][startpoint:endpoint]
                      tempY <- dY[[track.i]][startpoint:endpoint]
                    }
                  }
                  
                  ### rotate
                  #stn <- Sys.time();print(stn-syst);syst<-stn  ###  3
                  tempXY <- cbind(tempX, tempY)
                  dcx <- dCdX[[track.i]][mainturn[1]+turnstarts[1]-1] # note that in this version the peak direction is determined at the initial potision 
                  dcy <- dCdY[[track.i]][mainturn[1]+turnstarts[1]-1]
                  R <- matrix(c(dcx,dcy,-dcy,dcx),2,2)/sqrt(dcx^2+dcy^2)
                  UV <- tempXY %*% R
                  #cat(dcx, ",",dcy,"\n")
                  U <- UV[,1]
                  U <- U-U[plotprevtime+1] # start from (0,0)
                  V <- UV[,2]
                  V <- V-V[plotprevtime+1]
                  if(plottimespan+plotprevtime<=length(U)){
                    Us <- rbind(Us, U[1:(plottimespan+plotprevtime)])
                    Vs <- rbind(Vs, V[1:(plottimespan+plotprevtime)])
                  }else{
                    Us <- rbind(Us, c(U,rep(NA,plottimespan+plotprevtime-length(U))))
                    Vs <- rbind(Vs, c(V,rep(NA,plottimespan+plotprevtime-length(U))))
                  }
                  
                }else{
                  bT1After = c(bT1After, NA)
                  Us = rbind(Us, rep(NA,plottimespan+plotprevtime))
                  Vs = rbind(Vs, rep(NA,plottimespan+plotprevtime))
                }
                #stn <- Sys.time();print(stn-syst);syst<-stn ### 4
              }
            }
          }
        }
      }
      if(track.i%%1000==0){
        cat("track",track.i,"\n",sep="")
      }
    } # end for(track.i)
    } # end of for(plate.i)
    
    return(list(bT1After=bT1After, Us=Us, Vs=Vs))
  }
  
  dc <- data.collection()
  bT1After <- dc$bT1After
  Us <- dc$Us
  Vs <- dc$Vs
  
  radius <- 1 #0.5
  radius2 <- 0.35
  
  ########## classify to angle bins  ###########
  
  classify.k <- function(){
    
    meanUs <- matrix(NA, nangles, plottimespan+plotprevtime)
    meanVs <- matrix(NA, nangles, plottimespan+plotprevtime)
    countk <- rep(NA, nangles)
    
    for(k in 1:nangles){
      cat("k=",k,"\n",sep="")
      selectlow = -initbearspan + initbearspan*2/nangles*(k-1)
      selecthigh = -initbearspan + initbearspan*2/nangles*k
      
      selected <- bT1After>=selectlow & bT1After<=selecthigh
      
      meanUs[k,] = colMeans(Us[selected,],na.rm=T) + radius*cos((selectlow+selecthigh)/2*pi/180)
      meanVs[k,] = colMeans(Vs[selected,],na.rm=T) - radius*sin((selectlow+selecthigh)/2*pi/180)
      
      countk[k] <- sum(!apply(is.na(Us[selected,]), 1, all))
      
    } # end for(k)
  
    return(list(meanUs=meanUs,meanVs=meanVs,countk=countk))
  }
  
  ck <- classify.k()
  meanUs <- ck$meanUs
  meanVs <- ck$meanVs
  countk <- ck$countk
  
  ##########   plot   ###########
  
  dev.new()
  fileheader2 <- paste0("mean_track_postomega_",format(Sys.time(), "%Y%m%d_%H%M%S"))
  matplot(t(meanUs),t(meanVs),type="o",col=rainbow(nangles,start=0.25,end=0.1), pch=16,cex=0.3,asp=1,main="Averaged positions after initial turn", xlab="Towards peak (mm)", ylab="Parallel to peak (mm)",cex.lab=1.2)
  for(k in 1:nangles){
    u1 <- meanUs[k,1]
    u2 <- meanUs[k,2]
    v1 <- meanVs[k,1]
    v2 <- meanVs[k,2]
    lng <- 0.3
    l <- sqrt((u2-u1)^2+(v2-v1)^2)
    #arrows(u1-(u2-u1)*lng/l,v1-(v2-v1)*lng/l,u1,v1,length=lng/5,angle=40,lwd=2,col=rainbow(nangles,start=0.25,end=0.1)[k])
    text(radius2*cos((-initbearspan*2 + initbearspan*4/nangles*(k-1/2))/2*pi/180),
         -radius2*sin((-initbearspan*2 + initbearspan*4/nangles*(k-1/2))/2*pi/180),
         labels=countk[k],col="black",cex=0.6)
    points(meanUs[,plotprevtime+1],meanVs[,plotprevtime+1],pch=3,col="red",cex=0.5)
    #print(-radius*0.7*sin((-initbearspan + initbearspan*2/nangles*(k-1/2))/2*pi/180))
    #print(countk[k])
    #cat("done\n")
  }
  savePlot(paste0(fileheader2,".pdf"), type="pdf")
  savePlot(paste0(fileheader2,".tiff"), type="tiff")
  cat("All finished\n")
  #par(default.par)
  #browser()
}

reset.Contour.centroid <- function(){
  Contour.centroid <<- list()
  for(plate.i in 1:length(plate.track)){
    cdT <- list()
    for(plate.i in 1:length(plate.track)){
      cdT[[plate.i]] <- sort(unique(unlist(dT[ plate.track[[plate.i]] ])))
    }
    Contour.centroid.plate <- list()
    for(time in cdT[[plate.i]]){
      Contour.centroid.t <- list(flag=FALSE,centX=NA,centY=NA)
      Contour.centroid.plate <- c(Contour.centroid.plate, list(Contour.centroid.t)) 
    }
    Contour.centroid <<- c(Contour.centroid, list(Contour.centroid.plate))
  }
}