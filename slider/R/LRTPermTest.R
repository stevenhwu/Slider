LRTPermTest<- function(LRTStat, data, noP = 1000, th=NA, tMax=log2(100)){

    if( max(data, na.rm=T) > tMax   ){
        stop( paste("Maximum value in the dataset \"", max(data, na.rm=T) , "\" in greater then tMax (",tMax,")", sep="")  )
    }
    if(is.na(th)){
        th=min(as.vector(data),na.rm=T)
    }
	
	if(length(LRTStat) != NROW(data)){
		stop("length of LRTStat does not equal to the number of row in data")
	}
	noSample<- NCOL(data)
	halfSample<- noSample/2
	vNoSample<- 1:noSample
	group<- vector(length=2, mode="list")
	
		perStat<- matrix(nrow=length(LRTStat), ncol=noP)
		for(p in 1:noP){
			group[[1]]<- sample(noSample, halfSample)
			group[[2]]<- vNoSample[-group[[1]]]
			perStat[,p]<- gelLRT(data,group, th, tMax)
		}

	perPvalue<- vector(length=length(LRTStat))
	for(i in 1:length(LRTStat)){
		perPvalue[i]<- sum( LRTStat[i] < perStat[i,] )/noP
	}
	return(perPvalue)
}


