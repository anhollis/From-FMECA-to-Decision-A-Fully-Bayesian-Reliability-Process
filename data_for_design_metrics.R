source('~/USMA Project/MORS_paper/code/compute_red_yellow_green.R')
source('~/USMA Project/MORS_paper/code/system_fails_from_fail_modes.R')

get_red_probs<-function(failure_mode_probs){
  col_probs<-compute_p_ryg_system_comp(failure_mode_probs)
  red_function<-as.numeric(col_probs$function_ryg[,3])
  red_sub<-as.numeric(col_probs$sub_ryg[,3])
  red_system<-as.numeric(col_probs$system_ryg[3])
  return(c(red_function,red_sub,red_system))
}

system_probs<-function(failure_modes){
  names<-c("PropellantDoesNotIgnite","PropellantSnuffsOut",
           "Blowby_projectile","Blowby_breech",
           "NotEnoughPressure","TooMuchPressure",
           "FailToInitiatePower","InsufficientBatterySupply",
           "MotorDamage","InadequateTorque",
           "GEUCommandNotRecieved","CapacitorNotCharged",
           "CommandSentEarly","CommandSentLate",
           "IgnitePropellant","SealGas","GeneratePressure",
           "InitiateInflightPower","ModifyAttitude",
           "ArmWarheadFuze","SendFireCommand",
           "Launch","BallisticFlight","TerminalEffects", "System")
  red_names<-c("red_IgnitePropellant","red_SealGas","red_GeneratePressure",
               "red_InitiateInflightPower","red_ModifyAttitude",
               "red_ArmWarheadFuze","red_SendFireCommand",
               "red_Launch","red_BallisticFlight","red_TerminalEffects", "red_System")
  red_probs=t(apply(failure_modes,1,get_red_probs))
  overall_probs=t(apply(failure_modes,1,system_fails_from_failure_modes))
  col_names<-c(names,red_names)
  dat<-cbind(failure_modes,overall_probs,red_probs)
  colnames(dat)=col_names
  return(dat)
}
