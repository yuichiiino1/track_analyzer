bearing.mean.timecourse <- function(tracks=1:track.n, minturns=1, maxturns=Inf, sharponly=F,
                            initbearspan=180, nangles=8, timebin=1, timerepeats=50, plottimespan=50, plotprevtime=15, termination="", pironly=F){
  
  # ピルエット中最初のTurnA("T1")直後（TurnA="R"の最初の点）をtime0として、以下時間を追って平均（ベクトル平均）のBearingをプロット。
  # T1直後のBearingが一定範囲ごとのピルエットだけ選んでそれぞれ追跡、平均する。
  # terminationがトラックの選択条件。"" はすべて。"Turn","TurnA","both"はこれらのターンをしたら打ち切る。
  # "TurnOnly","TurnAOnly","either"は最初以外はTurn後、TurnA後、どちらか後のみ。
  # ピルエット中のトラックしか追跡しない場合はpironly=TRUE
  # トラック範囲tracksを指定したり、ターン回数がminturnsからmaxturnsのピルエットのみ選択できる（デフォールトではすべて）。
  # 
  
  library(circular)
  alpha <- 0.1
  #library(colorRamps)
  if(!exists("dCdX") || !exists("dCdY")){
    cat("Either dCdX or dCdY are missing\n")
    return("")
  }
  
  # data collection
  
  pir.track = c()
  pir.from = c()
  pir.to = c()
  pir.afterBearing = c()
  pir.mainturn = c()
  bAfter <- c()
  bBefore <- c()
  dTheta <- c()
  bT1After <- c()
  bT1Before <- c()
  Us <- c()
  Vs <- c()
  for(track.i in tracks){
    n <- point.n[track.i]
    #R <- (PirRun[[track.i]] == "R")
    #P <- (PirRun[[track.i]] == "P")
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
            if(sharponly){
              if(length(turnstarts) != length(turnends)) browser()
              pcount <- sum(sapply(1:length(turnstarts), function(x) any(TurnRunA[[track.i]][turnstarts[x]:turnends[x]]=="T")))
            }else{
              pcount <- length(turnstarts)
            }
            if(pcount>=minturns && pcount<=maxturns){
              if(turnstarts[1]>turnends[1]){cat("wrong turnstartsends\n");browser()}
              mainturn <- which(TurnRunA[[track.i]][turnstarts[1]:turnends[1]] == "T")
              if(length(mainturn)>=1){
                pir.mainturn = c(pir.mainturn, mainturn[1]+turnstarts[1]-1)
              }else{
                pir.mainturn = c(pir.mainturn, NA)
              }
              pir.track = c(pir.track, track.i)
              pir.from = c(pir.from, start[temp][j])
              pir.to = c(pir.to, end[temp][j])
              pir.afterBearing = c(pir.afterBearing, Bearing[[track.i]][end[temp][j]])
              bAfter = c(bAfter, Bearing[[track.i]][end[temp][j]])
              bBefore = c(bBefore, Bearing[[track.i]][start[temp][j]])
              #dTheta = c(dTheta, ((endTheta-startTheta)+180)%%360-180)
              #dTheta = c(dTheta, ((AvTheta[[track.i]][end[temp][j]]-AvTheta[[track.i]][start[temp][j]])+180)%%360-180)
              
              if(length(mainturn)>=1){
                # mainturn後はじめてTurnAでないものが次。
                aftermainturn <- min(which(TurnRunA[[track.i]][(mainturn[1]+turnstarts[1]):point.n[track.i]]=="R"))+mainturn[1]+turnstarts[1]-1
                # mainturn後はじめてTurnでないものが次。
                aftermainturn2 <- min(which(TurnRun[[track.i]][(mainturn[1]+turnstarts[1]):point.n[track.i]]=="R"))+mainturn[1]+turnstarts[1]-1
                bT1Before = c(bT1Before, Bearing[[track.i]][mainturn[1]+turnstarts[1]-1])
                bT1After = c(bT1After, Bearing[[track.i]][aftermainturn])
                #cat("mainturn[1]+turnstarts[1]-1 = ",mainturn[1]+turnstarts[1]-1,"; aftermainturn = ",aftermainturn,", bT1Before=",bT1Before[length(bT1Before)],", bT1After=",bT1After[length(bT1After)],", bBefore=",bBefore[length(bBefore)],"\n",sep="")
                
                # XYposition
                endpoint <- point.n[track.i]
                if(pironly) endpoint <- pir.to[length(pir.to)]
                Turns <- which(TurnRun[[track.i]][aftermainturn2:point.n[track.i]]=="T")
                if(length(Turns)>0){
                  TurnPoint <- min(Turns)+aftermainturn2-2
                }else{
                  TurnPoint <- point.n[track.i]
                }
                TurnAs <- which(TurnRunA[[track.i]][aftermainturn:point.n[track.i]]=="T")
                if(length(TurnAs)>0){
                  TurnAPoint <- min(TurnAs)+aftermainturn-2
                }else{
                  TurnAPoint <- point.n[track.i]
                }
                if(termination=="Turn") endpoint <- min(endpoint,TurnPoint)
                if(termination=="TurnA") endpoint <- min(endpoint,TurnAPoint)
                if(termination=="both") endpoint <- min(endpoint,TurnPoint,TurnAPoint)
                startpoint <- mainturn[1]+turnstarts[1]-1 - plotprevtime
                dummypoint <- 0
                if(startpoint<1){
                  dummypoint <- 1-startpoint
                  startpoint <- 1
                }
                
                #rotate
                tempX <- c(rep(NA,dummypoint),dX[[track.i]][startpoint:endpoint])
                tempY <- c(rep(NA,dummypoint),dY[[track.i]][startpoint:endpoint])
                
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
                bT1Before = c(bT1Before, NA)
                bT1After = c(bT1After, NA)
                Us = rbind(Us, rep(NA,plottimespan+plotprevtime))
                Vs = rbind(Vs, rep(NA,plottimespan+plotprevtime))
              }
              
            }
          }
        }
      }
    }
    if(track.i%%1000==0){
      cat("track",track.i,"\n",sep="")
    }
  }


  meanbearings = matrix(NA,timerepeats,nangles)
  
  # plot timeseries histograms for selected data
  
  meanUs <- matrix(NA, nangles, plottimespan+plotprevtime)
  meanVs <- matrix(NA, nangles, plottimespan+plotprevtime)
  for(k in 1:nangles){
    cat("k=",k,"\n",sep="")
    selectlow = -initbearspan + initbearspan*2/nangles*(k-1)
    selecthigh = -initbearspan + initbearspan*2/nangles*k
    
    deltaAngles <- c()
    selected <- bT1After>=selectlow & bT1After<=selecthigh
    
    radius <- 0.5
    meanUs[k,] = colMeans(Us[selected,],na.rm=T) + radius*cos((selectlow+selecthigh)/2*pi/180)
    meanVs[k,] = colMeans(Vs[selected,],na.rm=T) - radius*sin((selectlow+selecthigh)/2*pi/180)
    
    dead <- rep(FALSE, length(pir.track))
    duringpir <- rep(TRUE, length(pir.track))
    for(j in 1:timerepeats){
      
      timeBearing <- c()
      #deadBearing <- c()
      endBearing <- c()
      TurnBearing <- c()
      TurnABearing <- c()
      TurnNow <- rep(FALSE, length(pir.track))
      TurnANow <- rep(FALSE, length(pir.track)) 
      #deadnow <- rep(FALSE, length(pir.track))
      
      sumsin <- 0
      sumcos <- 0
      count <- 0
      cumB <- c()
      for(i in which(selected)){ # follow selected pirouettes
        if(!is.na(pir.mainturn[i])){
          # mainturen後はじめてTurnAでないものが次。
          aftermainturn <- min(which(TurnRunA[[pir.track[i]]][(pir.mainturn[i]+1):point.n[pir.track[i]]]=="R"))+pir.mainturn[i]
          # mainturn後はじめてTurnでないものが次。
          aftermainturn2 <- min(which(TurnRun[[pir.track[i]]][(pir.mainturn[i]+1):point.n[pir.track[i]]]=="R"))+pir.mainturn[i]
          TurnNow[i] <- any(TurnRun[[pir.track[i]]][max(aftermainturn2,aftermainturn+timebin*(j-1)+1):max(aftermainturn2,aftermainturn+timebin*j)]=="T",na.rm=T)
          TurnANow[i] <- any(TurnRunA[[pir.track[i]]][(aftermainturn+timebin*(j-1)+1):(aftermainturn+timebin*j)]=="T",na.rm=T)
          duringpir[i] <- aftermainturn+timebin*(j-1) <= pir.to[i]
          if(!pironly || duringpir[i]){
            if(!dead[i]){ # if this piroutte is still followed
              B <- Bearing[[pir.track[i]]][aftermainturn+timebin*(j-1)]
              timeBearing <- c(timeBearing, B)
              endBearing <- c(endBearing, Bearing[[pir.track[i]]][pir.to[i]])
              if(!is.na(B)){
                sumcos <- sumcos + cos(B/180*pi)
                sumsin <- sumsin + sin(B/180*pi)
                cumB <- c(cumB, B)
                count <- count + 1
              }
              #if(deadnow[i]){
              #  deadBearing <- c(deadBearing, Bearing[[pir.track[i]]][aftermainturn+timebin*(j-1)])
              #}
              if(TurnNow[i]){
                TurnBearing <- c(TurnBearing, Bearing[[pir.track[i]]][aftermainturn+timebin*(j-1)])
              }
              if(TurnANow[i]){
                TurnABearing <- c(TurnABearing, Bearing[[pir.track[i]]][aftermainturn+timebin*(j-1)])
              }
            }
          }
        }
      }
      
      meanangle <- atan(sumsin/sumcos)/pi*180
      if(sumcos<0 && sumsin>=0) meanangle <- meanangle+180
      if(sumcos<0 && sumsin<0) meanangle <- meanangle-180
      
      meanabs <- sqrt((sumsin*sumsin+sumcos*sumcos)/count)
      #cat(meanangle,"\n")
      
      # statistical test   # added 2018/10/6
      Bc <- as.circular(cumB, units="degrees", type="angles",template="none", modulo='asis',zero=0, rotation='counter')
      #dev.new()
      #plot.circular(Bc)
      #cat("k=",k," j=",j,'length cumB =',length(cumB),"\n")
      Rtest <- rayleigh.test(Bc)
      if(Rtest$p.value > alpha) meanangle <- NA
      #browser()
    
      meanbearings[j,k] = meanangle
      
      if(termination=="Turn") dead <- dead | TurnNow
      if(termination=="TurnA") dead <- dead | TurnANow
      if(termination=="both") dead <- dead | TurnNow | TurnANow
      if(termination=="TurnOnly"){
        if(j==1) dead <- !TurnNow else dead <- dead & !TurnNow
      }
      if(termination=="TurnAOnly"){
        if(j==1) dead <- !TurnANow else dead <- dead & !TurnANow
      }
      if(termination=="either"){
        if(j==1) dead <- !TurnNow & !TurnANow else dead <- dead & !TurnNow & !TurnANow
      }
    }
    # amend jump points
    for(j in 2:timerepeats){
      if(!is.na(meanbearings[j,k]) && !is.na(meanbearings[j-1,k])){
      if(meanbearings[j,k]-meanbearings[j-1,k]>180){
        meanbearings[j,k] = meanbearings[j,k]-360
      }else if(meanbearings[j,k]-meanbearings[j-1,k]< -180){
        meanbearings[j,k] = meanbearings[j,k]+360
      }
      }
    }
    meanbearings[,k][!is.na(meanbearings[,k]) && meanbearings[,k]< -360] <- meanbearings[,k][!is.na(meanbearings[,k]) && meanbearings[,k]< -360] + 360
    meanbearings[,k][!is.na(meanbearings[,k]) && meanbearings[,k]> 360] <- meanbearings[,k][!is.na(meanbearings[,k]) && meanbearings[,k]> 360] - 360
  }
  
  dev.new() # necessary for  RStudio
  par(mgp=c(2,0.5,0))
  matplot(meanbearings, type="o", pch=16, cex=0.5, ylab="Average bearing (deg)", xlab="Time after initial reversal")
  fileheader <- paste0("bearing_mean_timecourse_",format(Sys.time(), "%Y%m%d_%H%M%S"))
  savePlot(paste0(fileheader,"_o.pdf"), type="pdf")
  savePlot(paste0(fileheader,"_o.tiff"), type="tiff")

  dev.new() # necessary for  RStudio
  par(mgp=c(2,0.5,0))
  matplot(meanbearings, type="p", pch=16, cex=0.8, ylab="Average bearing (deg)", xlab="Time after initial reversal (")
  fileheader <- paste0("bearing_mean_timecourse_",format(Sys.time(), "%Y%m%d_%H%M%S"))
  savePlot(paste0(fileheader,"_p.pdf"), type="pdf")
  savePlot(paste0(fileheader,"_p.tiff"), type="tiff")
  
  dev.new()
  fileheader2 <- paste0("mean_track_postturn_",format(Sys.time(), "%Y%m%d_%H%M%S"))
  matplot(t(meanUs),t(meanVs),type="o",col=rainbow(nangles,start=0.25,end=0.1), pch=16,cex=0.3,asp=1,main="Averaged positions after initial turn", xlab="Towards peak (mm)", ylab="Parallel to peak (mm)",cex.lab=1.2)
  for(k in 1:nangles){
    u1 <- meanUs[k,1]
    u2 <- meanUs[k,2]
    v1 <- meanVs[k,1]
    v2 <- meanVs[k,2]
    lng <- 0.3
    l <- sqrt((u2-u1)^2+(v2-v1)^2)
    arrows(u1-(u2-u1)*lng/l,v1-(v2-v1)*lng/l,u1,v1,length=lng/5,angle=40,lwd=2,col=rainbow(nangles,start=0.25,end=0.1)[k])
    #cat("done\n")
  }
  savePlot(paste0(fileheader2,".pdf"), type="pdf")
  savePlot(paste0(fileheader2,".tiff"), type="tiff")
  cat("All finished\n")
  #par(default.par)
  #browser()
}
