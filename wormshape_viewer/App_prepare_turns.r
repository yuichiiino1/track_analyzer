App_prepare_turns <- function(from=1, to=track.n, turn.select="T1", nangles=8,
   anglebin=0){

# this is a function.
# Execute by for example App_prepare_turns(turn.select="all",anglebin=2)
# turn.select = "all"/"T1", selectbin = 1 to nangles (only T1after angles in this bin are selected)
# requirements: dX, dY, dL, TurnStartA(from calc.PirRunA), Pirsurround(from calc.PirRun)
#      TurnStart, TurnEnd, TurnRunA
# turns.RData is generated, which can later be loaded for App.r

gauges <<- seq(0.1,0.3,by=0.05) ### this may have to be changed.###

initbearspan <- 180
selectlow = -initbearspan + initbearspan*2/nangles*(anglebin-1)
selecthigh = -initbearspan + initbearspan*2/nangles*anglebin


#距離の算出（距離の２乗）
dist2 <- function(X1,Y1,X2,Y2){
  return((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2))
}

#角度の算出（(X,Y)を頂点とした(X1,Y1)→(X,Y)と(X,Y)→(X2,Y2)へのベクトルについて、c(内積, 外積)）
product2 <- function(X,Y,X1,Y1,X2,Y2){
  return(c( ((X-X1)*(X2-X)+(Y-Y1)*(Y2-Y)), ((X-X1)*(Y2-Y)-(X2-X)*(Y-Y1))) )
}

############  calculate TurnAngle  ##########


  cat(paste("Total tracks ", track.n, "\nProcessing track no:\n"))
  M <- length(gauges)
  tracks <- 1:to

  TurnAngle <<- c()
  for(track.i in tracks){
  if(track.i%%100 == 0) cat(paste("\r",track.i,sep=""))
  TurnAngle <<- c(TurnAngle, list(matrix(NA,M,point.n[track.i])))

  if(point.n[track.i]>=3){
  for(m in 1:M){
    gauge <- gauges[m]
    gaugegauge <- gauges[m]^2
    for(j in 2:(point.n[track.i]-1)){
      flag1 <- FALSE
      flag2 <- FALSE
      cumL <- 0
      for(j11 in (j-1):1){
        cumL <- cumL + dL[[track.i]][j11] #高速化のためまずは積算距離で計算し次に直線距離を計算。
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
        pro <- product2(dX[[track.i]][j],dY[[track.i]][j],dX[[track.i]][j1],dY[[track.i]][j1],dX[[track.i]][j2],dY[[track.i]][j2])
        if(pro[1]==0){
          ang = 90
        }else{
          ang <- atan(abs(pro[2])/pro[1])*180/pi
          if(ang<0){
            ang <- ang + 180
          }
          ang <- ang * sign(pro[2])
        }
        TurnAngle[[track.i]][m,j] <<- ang
      }
    }# j loop
    }# m loop
  }# if(point.n[track.i]>=3)
  }# track.i loop
  cat("\nDone.\n")


########  generate vector representation of turns #########

cat(paste("Second step: Processing track no:\n"))

bT1A <<- c()
turn.track <<- c()
turn.point <<- c()
for(track.i in tracks){
  if(track.i%%100 == 0) cat(paste("\r",track.i,sep=""))
  n <- point.n[track.i]
  R <- (Pirsurround[[track.i]] == "R")
  P <- (Pirsurround[[track.i]] == "P")
  start <- which(R & c(P[2:n],F)) #ピルエット開始点ひとつ前の添え字の列
  end <- which(R & c(F,P[1:(n-1)])) #ピルエット終了点のひとつ後の添え字の列
  if(length(end)>0 && length(start)>0){
  if(end[1]<=start[1]) end <- end[-1]
  if(length(end)>0){
  if(length(start)>length(end)) start <- start[-length(start)]
    startTheta <- AvTheta[[track.i]][start]
    endTheta <- AvTheta[[track.i]][end]
    # boolean vector of usable pirouettes
    temp <- !is.na(startTheta) & !is.na(endTheta)
    if(any(temp)){
    
    for(j in 1:length(which(temp))){ # 有効ピルエット順
      turnstarts <- which(TurnStart[[track.i]][(start[temp][j]+1):(end[temp][j]-1)])+start[temp][j]
      turnends <- which(TurnEnd[[track.i]][(start[temp][j]+1):(end[temp][j]-1)])+start[temp][j]

      mainturn <- which(TurnRunA[[track.i]][turnstarts[1]:turnends[1]] == "T")
      if(length(mainturn)>=1){
        # T1後はじめてTurnAでないもの
        aftermainturn <- min(which(TurnRunA[[track.i]][(mainturn[1]+turnstarts[1]):point.n[track.i]]=="R"))+mainturn[1]+turnstarts[1]-1
        if(turn.select=="T1"){
        bT1After1 <- Bearing[[track.i]][aftermainturn]
        bT1A <<- c(bT1A, bT1After1)
        if(anglebin==0 || (!is.na(bT1After1) && bT1After1>=selectlow && bT1After1<=selecthigh) ){
          # register this T1
          turn.track <<- c(turn.track, track.i)
          turn.point <<- c(turn.point, turnstarts[1]+mainturn[1]-1)
        }
        }else if(turn.select=="all"){
          temp2 <- which(TurnStartA[[track.i]][start[j]:end[j]])
          if(length(temp2)>0){
          temp2 <- temp2 + start[j]-1
          for(turn.i in 1:length(temp2)){
            # register all turns
            turn.track <<- c(turn.track, track.i)
            turn.point <<- c(turn.point, temp2[turn.i])
          }
          }
        }
      }
    
    }
    }

  }
  }
}

save("TurnAngle","turn.track","turn.point","gauges", file="turns.RData")
cat("\nDone.\n Saved to turns.RData\n")
}

cat('Command: App_prepare_turns(from=1, to=track.n, turn.select="T1", nangles=8,
   anglebin=0)\nwhere turn.select="T1"/"all", anglebin=1 to nangles\n')
