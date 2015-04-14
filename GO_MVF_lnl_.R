#Mean Value Function
GO_BM_FT_MVF <- function(aMLE,bMLE,data){		return(aMLE*(1-exp(-bMLE*data)))}

#log-likelihood function
GO_BM_FT_LLF <- function(aMLE,bMLE,data){	return( -aMLE*(1-exp(-bMLE*tn))+n*log(aMLE)+n*log(bMLE)-bMLE*sum(data))}

#Estimated number of faults remaining

GO_BM_FT_FaultsRemaining <- function(aMLE,n){return( abs(aMLE-n))}

#Reliability
GO_BM_FT_Reliability <- function(bMLE,tn){return( exp(-bMLE*tn))}

#Mean-time-to-failure (MTTF)
GO_BM_FT_MTTF <- function(bMLE){return( 1/bMLE)}
