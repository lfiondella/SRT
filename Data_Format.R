#The purpose of these fuctions are to convert between failure times and interfailure times
failureT_to_interF <- function(failure_T)
{

interfailure <- c()
interfailure[1] = failure_T[1] #initial value for failure time

n = 2

while(n<=length(failure_T))
{
  interfailure[n] = failure_T[n] - failure_T[n-1]
   
   n = n+1
}

return(interfailure)#return failure times(failure_T)
}




#################################



 interF_to_failureT <- function(interfailure) #interfailure to failure times
{


   failure_T <- c()

   failure_T[1] = interfailure[1] #initial value for failure time

n = 2

while(n<=length(interfailure))
{
  failure_T[n] = interfailure[n] + failure_T[n-1]
  
  n = n + 1
}


return(failure_T)#return interfailure times(interfailure)
}





#################################




failureC_to_failureT <- function(initial,final,num_count) #failure count to failure time
{

 
  
failure_T <- c()

failure_Interval = final - initial #times interval

change = failure_Interval / num_count #offset to space out failure times

i = 0
j = 1

while(i < num_count)
{
  failure_T[j] = initial + (( 0.5 + i) * change) #sets failure times 
  
  i = i + 1
  j = j + 1
}





return(failure_T)#return failure times(failure_t)
}


