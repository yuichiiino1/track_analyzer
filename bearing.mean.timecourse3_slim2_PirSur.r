bearing.mean.timecourse <- function(tracks=1:track.n, minturns=1, maxturns=Inf, 
                            initbearspan=180, nangles=8, timebin=1, timerepeats=50, plottimespan=50, plotprevtime=15){
  
  # ピルエット中最初のTurnA("T1")直後（TurnA="R"の最初の点）をtime0として、以下時間を追って平均（ベクトル平均）のBearingをプロット。
  # T1直後のBearingが一定範囲ごとのピルエットだけ選んでそれぞれ追跡、平均する。
  # トラック範囲tracksを指定したり、ターン回数がminturnsからmaxturnsのピルエットのみ選択できる（デフォールトではすべて）。
  
  # bearing.mean.timecourse3_slim2_Pir.r: 表示範囲をPirsurroundの範囲に限る。
  
  library(circular)
  #library(colorRamps)
  if(!exists("dCdX") || !exists("dCdY")){
    cat("Either dCdX or dCdY are missing\n")
    return("")
  }
  
  # data collection
  
  bT1After <- c()
  Us <- c()
  Vs <- c()
  for(track.i in tracks){
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
                
                # XYposition
                endpoint <- point.n[track.i]   # Pirouette(Pirsurround)範囲に限定 end[1]  #
                startpoint <- mainturn[1]+turnstarts[1]-1 - plotprevtime
                dummypoint <- 0
                if(startpoint<1){
                  dummypoint <- 1-startpoint
                  startpoint <- 1
                }
                
                #rotate
                dXPir <- dX[[track.i]][startpoint:endpoint]
                dXPir[Pirsurround[[track.i]][startpoint:endpoint] == "R" & c(rep(F,plotprevtime-dummypoint),rep(T,(endpoint-startpoint)-(plotprevtime-dummypoint)+1))] <- NA
                dYPir <- dY[[track.i]][startpoint:endpoint]
                dYPir[Pirsurround[[track.i]][startpoint:endpoint] == "R" & c(rep(F,plotprevtime-dummypoint),rep(T,(endpoint-startpoint)-(plotprevtime-dummypoint)+1))] <- NA
                tempX <- c(rep(NA,dummypoint),dXPir)
                tempY <- c(rep(NA,dummypoint),dYPir) # dY[[track.i]][startpoint:endpoint]
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
              
            }
          }
        }
      }
    }
    if(track.i%%1000==0){
      cat("track",track.i,"\n",sep="")
    }
  } # end for(track.i)

  
  ########## classify to angle bins  ###########
  
  meanUs <- matrix(NA, nangles, plottimespan+plotprevtime)
  meanVs <- matrix(NA, nangles, plottimespan+plotprevtime)
  countk <- rep(NA, nangles)
  
  for(k in 1:nangles){
    cat("k=",k,"\n",sep="")
    selectlow = -initbearspan + initbearspan*2/nangles*(k-1)
    selecthigh = -initbearspan + initbearspan*2/nangles*k
    
    selected <- bT1After>=selectlow & bT1After<=selecthigh
    countk[k] <- sum(selected,na.rm=T)
    
    radius <- 0.5
    meanUs[k,] = colMeans(Us[selected,],na.rm=T) + radius*cos((selectlow+selecthigh)/2*pi/180)
    meanVs[k,] = colMeans(Vs[selected,],na.rm=T) - radius*sin((selectlow+selecthigh)/2*pi/180)
    
  } # end for(k)

  
  ##########   plot   ###########
  
  dev.new()
  fileheader2 <- paste0("mean_track_postturn_PirSur",format(Sys.time(), "%Y%m%d_%H%M%S"))
  matplot(t(meanUs),t(meanVs),type="o",col=rainbow(nangles,start=0.25,end=0.1), pch=16,cex=0.3,asp=1,main="Averaged positions after initial turn", xlab="Towards peak (mm)", ylab="Parallel to peak (mm)",cex.lab=1.2)
  for(k in 1:nangles){
    u1 <- meanUs[k,1]
    u2 <- meanUs[k,2]
    v1 <- meanVs[k,1]
    v2 <- meanVs[k,2]
    lng <- 0.3
    l <- sqrt((u2-u1)^2+(v2-v1)^2)
    arrows(u1-(u2-u1)*lng/l,v1-(v2-v1)*lng/l,u1,v1,length=lng/5,angle=40,lwd=2,col=rainbow(nangles,start=0.25,end=0.1)[k])
    text(radius*0.7*cos((-initbearspan*2 + initbearspan*4/nangles*(k-1/2))/2*pi/180),
         -radius*0.7*sin((-initbearspan*2 + initbearspan*4/nangles*(k-1/2))/2*pi/180),
         labels=countk[k],col="black",cex=0.6)
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
