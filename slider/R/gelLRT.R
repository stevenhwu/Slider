gelLRT <-
function(allData, group, th=NA, tMax=log2(100)){

    if(length(group)!=2){
        stop("Please make sure there are exactly two groups")
    }
    if(!is.matrix(allData)){
        stop("Please make sure allData is a matrix")
    }
    if( (length(group[[1]])+length(group[[2]])) != NCOL(allData)   ){
        stop("Please check the dimensions of the matrix")
    }
    if( max(allData, na.rm=T) > tMax   ){
        stop( paste("Maximum value in the dataset \"", max(allData, na.rm=T) , "\" in greater then tMax (",tMax,")", sep="")  )
    }
    if(is.na(th)){
        th=min(as.vector(allData),na.rm=T)
    }
    LRTStat<-2*( gelML(allData,group,th,2,theoryMax=tMax) - gelML(allData,group,th,1,theoryMax=tMax) )
    return(LRTStat)
}

