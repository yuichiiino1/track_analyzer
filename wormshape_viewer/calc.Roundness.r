calc.Contour.centroid <- function(){
  Contour.centroid <<- list()
  for(plate.i in 1:length(plate.track)){
    contour.plate <- Contour[[plate.i]]
    cat("plate", plate.i,"\n")
    #cdT <- list()
    #for(plate.i in 1:length(plate.track)){
    #  cdT[[plate.i]] <- sort(unique(unlist(dT[ plate.track[[plate.i]] ])))
    #}
    Contour.centroid.plate <- list()
    for(time in 1:length(contour.plate)){
      contour.t <- contour.plate[[time]]
      centX <- rep(NA, length(contour.t))
      centY <- rep(NA, length(contour.t))
      for(worm.i in 1:length(contour.t)){
        cXY <- get.worm(contour.t[[worm.i]], plate.i)$cXY
        if(length(cXY)>0){
          centX[worm.i] <- mean(cXY[,1])
          centY[worm.i] <- mean(cXY[,2])
        }
      }
      Contour.centroid.t <- list(flag=TRUE, centX=centX, centY=centY) # record
      Contour.centroid.plate <- c(Contour.centroid.plate, list(Contour.centroid.t)) 
    }
    Contour.centroid <<- c(Contour.centroid, list(Contour.centroid.plate))
  }
}

calc.Roundness <- function(){

  library(EBImage)
  
  if(!exists("Contour.centroid")){
    calc.Contour.centroid()
  }
  
  requirements <- c("point.n","Contour","dX","dY","Contour.centroid")
  for(obj in requirements){
  if(!exists(obj)){
    cat(obj,"is missing\n")
    return("")
  }
  }
  
  Roundness <<- list()
  Area <<- list()
  cdT <- list()
  for(plate.i in 1:length(plate.track)){
    cdT[[plate.i]] <- sort(unique(unlist(dT[ plate.track[[plate.i]] ])))
  }
  for(plate.i in 1:length(plate.track)){
    cat("plate", plate.i,"\n")
    contour.plate <- Contour[[plate.i]]
    cat("track ")
    for(track.i in plate.track[[plate.i]]){
      if(track.i %% 100 == 0)cat(paste0(track.i," "))
      roundnessvec <- rep(NA, point.n[track.i])
      areavec <- rep(NA, point.n[track.i])
      for(point.i in 1:point.n[track.i]){
        time <- which.min(abs(dT[[track.i]][point.i] - cdT[[plate.i]]))
        contour.t <- contour.plate[[time]]
        worm.i <- which.min(
           (dX[[track.i]][point.i] - Contour.centroid[[plate.i]][[time]]$centX)^2 + 
           (dY[[track.i]][point.i] - Contour.centroid[[plate.i]][[time]]$centY)^2 )
          
          gw <- get.worm(contour.t[[worm.i]], plate.i)
          contour <- gw$contourxy
          if(length(contour)>0){
            
            xmin1 <- min(contour[,1])-1
            ymin1 <- min(contour[,2])-1
            contx <- contour[,1] - xmin1
            conty <- contour[,2] - ymin1  # (contx,conty) is contour points shifted to bottomleft
            nx <- max(contx)
            ny <- max(conty)
            border <- matrix(0, nx, ny)
            for(i in 1:length(contx)) border[contx[i],conty[i]] <- 1
            realworm <- fillHull(border)
            wfts <- computeFeatures.shape(realworm)
            wftm <- computeFeatures.moment(realworm)
            area <- wfts[1,"s.area"]*Transform[[plate.i]]$mag[[1]]^2
            areavec[point.i] <- area
            roundness <- (wfts[1,"s.perimeter"]^2)/(4*pi*wfts[1,"s.area"])
            roundnessvec[point.i] <- roundness
          } # end if(length(contour))
      } # end for(point.i)
      Roundness <<- c(Roundness, list(roundnessvec))
      Area <<- c(Area, list(areavec))
    } # end for(track.i)
    cat("\n")
  } # end for(plate.i)
  cat("Saving Roundness and Area to Roundness.RData\n")
  save(Roundness, Area, file="Roundness.RData")
} # end function

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
freeman2contour <- function(freeman1, origin.x=0, origin.y=0){
  freeman_code <- matrix(c(1,0, 1,-1, 0,-1, -1,-1, -1,0, -1,1, 0,1, 1,1),8,2,byrow=T)
  freeman_code_x <- freeman_code[,1]
  freeman_code_y <- freeman_code[,2]
  # freeman1 = freeman+1
  # calculate contour as a series of (x,y) in pixel coordinates
  freemanx <- freeman_code_x[freeman1]
  freemany <- freeman_code_y[freeman1]
  contourx <- origin.x + cumsum(freemanx)
  contoury <- origin.y + cumsum(freemany)
  return(cbind(contourx,contoury))
}

cat("Usage: calc.Roundness()\n")