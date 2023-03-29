#This function computes the functional, subsystem, and system failure probabilities
#from the basic failure mode probabilities
#failure_modes: A vector of failure probabilities for the base level failure modes

system_fails_from_failure_modes<-function(failure_modes){
  function_failure_probs<-c(1-prod(1-failure_modes[1:2]),
                            1-prod(1-failure_modes[3:4]),
                            1-prod(1-failure_modes[5:6]),
                            1-prod(1-failure_modes[7:8]),
                            1-prod(1-failure_modes[9:10]),
                            1-prod(1-failure_modes[11:12]),
                            1-prod(1-failure_modes[13:14]))
  
  subsystem<-c(1-prod(1-function_failure_probs[1:3]),
               1-prod(1-function_failure_probs[4:5]),
               1-prod(1-function_failure_probs[6:7]))
  
  system=1-prod(1-subsystem)
  
  return(c(function_failure_probs,subsystem,system))
}