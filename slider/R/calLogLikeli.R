calLogLikeli <-
function(par,spot,th,gsd) {

    m<-par[1]
    p<-par[2]
    nbar<-sum(is.na(spot))
    size<-length(spot)
    n<-size-nbar
    pbar<- 1-p

    if(nbar==0 | n==0){
        lp<-0
    }
    else{
        lp<-log(p)
    }
    if( pnorm(th,mean=m,sd=gsd) == 0 & pbar ==0){ #when th is too low, and all expressed
        nonExpress <- 0
    }
    else{
        nonExpress <- nbar*(log(
                        pbar+p*pnorm(th,mean=m,sd=gsd)
                       ))
    }

    sum(dnorm(spot,mean=m,sd=gsd,log=T),na.rm=T)+n*lp+ nonExpress

}

