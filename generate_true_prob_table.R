#True Data Simulator
failure_modes_ids<-c("PropellantDoesNotIgnite","PropellantSnuffsOut",
                     "Blowby_projectile","Blowby_breech",
                     "NotEnoughPressure","TooMuchPressure",
                     "FailToInitiatePower","InsufficientBatterySupply",
                     "MotorDamage","InadequateTorque",
                     "GEUCommandNotRecieved","CapacitorNotCharged",
                     "CommandSentEarly","CommandSentLate")
function_ids<-c("IgnitePropellant","SealGas","GeneratePressure",
                "InitiateInflightPower","ModifyAttitude",
                "ArmWarheadFuze","SendFireCommand")
sub_ids<-c("Launch","BallisticFlight","TerminalEffects")

#failure_mode_risk<-c(low_probability,low_probability,low_probability,
#                     low_probability,medium_probability,low_probability,
#                     medium_probability,low_probability,low_probability,
#                     low_probability,low_probability,low_probability,
#                     low_probability,low_probability)

failure_mode_risk=c(0.04,1.200477e-05,0.42,0.07,0.003,0.05,0.007,0.0009,0.31,0.37,0.45,0.23,0.0002,0.003)

failure_mode_sev<-c(1,9,3,9,9,9,3,3,9,9,1,1,1,1)
fail_function<-c("IgnitePropellant","IgnitePropellant",
                 "SealGas","SealGase",
                 "GeneratePressure","GeneratePressure",
                 "InitiateInflightPower","InitiateInflightPower",
                 "ModifyAttitude","ModifyAttitude",
                 "ArmWarheadFuze","ArmWarheadFuze",
                 "SendFireCommand","SendFireCommand")

failure_mode_data<-data.frame(failure_mode=failure_modes_ids,OCC=failure_mode_risk,
           SEV=failure_mode_sev,fail_function=fail_function)

function_probs=lower_prob_to_upper_prop(function_ids,failure_mode_data$fail_function,
                                        failure_mode_data$OCC)
function_sub<-c("Launch","Launch","Launch",
                "BallisticFlight","BallisticFlight",
                "TerminalEffects","TerminalEffects")

function_data<-data.frame(func=function_ids,OCC=function_probs,
                          function_sub=function_sub)

sub_probs<-lower_prob_to_upper_prop(sub_ids,function_data$function_sub,
                                    function_data$OCC)

system_probs=1-prod(1-sub_probs)

prob_table<-data.frame(ID=c(as.character(failure_mode_data$failure_mode),
                            as.character(function_data$func),
                            sub_ids,"System"),probs=c(failure_mode_data$OCC,
                                                      function_data$OCC,sub_probs,
                                                      system_probs))

save(prob_table,file="C:\\Users\\anhol\\Documents\\USMA Project\\MORS_paper\\code\\prob_table")

lower_prob_to_upper_prop<-function(upper_id,map,lower_probs){
  upper_prob<-rep(0,length(upper_id))
  for(i in 1:length(upper_id)){
    idx<-which(map==upper_id[i])
    upper_prob[i]=1-prod(1-lower_probs[idx])
  }
  return(upper_prob)
}

