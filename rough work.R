# greeks
greeks_options <- function(S,K,T,r,sigma,type){
  d1<-(log(S/K)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2<-d1-sigma*sqrt(T)
  if (type == 'Call') {
    delta<- pnorm(d1,mean=0,sd=1)
    gamma<- dnorm(d1,mean=0,sd=1)/ (So*price*sqrt(TTM))
    vega<- price*dnorm(d1,mean=0,sd=1)*sqrt(TTM)
    theta<- -((price*vol*dnorm(d1,mean=0,sd=1))/(2*sqrt(TTM))) - (rf*greeks.call$Strike*exp(-rf*TTM)*pnorm(d2,mean=0,sd=1))
    rho<- greeks.call$Strike*TTM*exp(-rf*TTM)*pnorm(d2,mean=0,sd=1)
  }
  if (type == 'Put') {
    delta<- pnorm(d1,mean=0,sd=1)-1
    gamma<- dnorm(d1,mean=0,sd=1)/ (price*vol*sqrt(TTM))
    vega<- price*dnorm(d1,mean=0,sd=1)*sqrt(TTM)
    theta<- -((price*vol*dnorm(d1,mean=0,sd=1))/(2*sqrt(TTM))) + (rf*greeks.put$Strike*exp(-rf*TTM)*pnorm(nd2,mean=0,sd=1))
    rho<- -greeks.put$Strike*TTM*exp(-rf*TTM)*pnorm(nd2,mean=0,sd=1)
  }
}


Binomial_OPM <- function(S,K,T,r,sigma,n,type) {
  x=NA
  if (type=="call") x=1
  if (type=="put") x=-1
  if (is.na(x)) stop("Option Type can only be call or put")
  dt=T/ n
  u=exp(sigma*sqrt(dt))
  d=1/ u
  p=((1+r*dt)-d)/ (u-d)
  disc<- (1+r*dt)
  OptVal<- x*(S*u^(0:n)*d^(n:0)-K)
  OptVal=ifelse(OptVal<0,0,OptVal)
  for (j in seq(from=n-1,to=0,by=-1))
    for (i in 0:j)
      OptVal[i+1]=(p*OptVal[i+2]+(1-p)*OptVal[i+1])/disc
  value=OptVal[1]
  results<- rbind(u,d,p,value)
  results
}

stock_price <- stock_price['2015/', ]

# to improve image quality
# {r img-setup, include=FALSE, cache=FALSE}
out.format <- knitr::opts_knit$get("out.format")
img_template <- switch( out.format,
                        word = list("img-params"=list(fig.width=6,
                                                      fig.height=6,
                                                      dpi=150)),
                        {
                          # default
                          list("img-params"=list( dpi=150,
                                                  fig.width=6,
                                                  fig.height=6,
                                                  out.width="504px",
                                                  out.height="504px"))
                        } )

knitr::opts_template$set( img_template )



