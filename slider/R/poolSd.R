poolSd <-
function(x,y,gSD){
    x<- as.vector(x)
    y<- as.vector(y)
    nx <- sum(!is.na(x))
    ny <- sum(!is.na(y))

    if(nx >1 && ny >1){
        vx <- var(x,na.rm=T)
        vy <- var(y,na.rm=T)
        stderrx <- sqrt(vx/nx)
        stderry <- sqrt(vy/ny)
        stderr <- sqrt(stderrx^2 + stderry^2)
        df <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
    }
    else if(nx >1){
        stderr<-sd(x,na.rm=T)
    }
    else if(ny >1){
        stderr<-sd(y,na.rm=T)
        
    }
    else{
        stderr<-gSD
    }
    if(stderr ==0){
        stderr <- gSD
    }
    return (stderr)
}

