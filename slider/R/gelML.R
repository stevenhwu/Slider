gelML <-
function(allMatrix,group,th,model=1,theoryMax=log2(100)){

allMatrix<-as.matrix(allMatrix)
nrow<-dim(allMatrix)[1]
allSD<-vector(length=nrow)
gSD<-sd(as.vector(allMatrix),na.rm=T)
for(i in 1:nrow){
    allSD[i]<-poolSd(allMatrix[i,group[[1]]],allMatrix[i,group[[2]]],gSD=gSD)
}
gMean<-mean(as.vector(allMatrix),na.rm=T)
matrix<-cbind(allMatrix,allSD)
result<-0

for(i in 1:model){
    if(model!=1){
        matrix<-cbind(allMatrix[,group[[i]]],allSD)
    }

    ml<-apply(matrix,1,function(x){
        x<-unlist(x)     
        lx<-length(x)

        pstart<-sum(!is.na(x[1:(lx-1)]))/length(x[1:(lx-1)])
        if(pstart==0) pstart=0.0001
        if(pstart==1) pstart=0.9999
    
        intM<-mean(x[1:(lx-1)],na.rm=T)
        if(is.na(intM)){ intM<- gMean}

        op<-optim(c(intM,pstart),calLogLikeli,method="L-BFGS-B",lower=c(th,pstart),upper=c(theoryMax,0.9999999999),spot=x[1:(lx-1)],th=th,gsd=x[lx],control=list(fnscale=-1))

        likeli<-op$value
    })

    result<-result+ml
}
return(result)

}

