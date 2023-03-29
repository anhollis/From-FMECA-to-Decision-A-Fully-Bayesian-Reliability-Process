#example run_spec: run_spec=data.frame(name=c("InitiateInflightPower","ModifyAttitude","ArmWarheadFuze","SendFireCommand"),n=c(15,11,6,7))

#This function is used to generate test data from a system with true prespecified failure probabilities.
#run_spec: dataframe with two columns: 1st columns has names of system components for which we want to generate test data
#second column contains the number of runs for each component
#prob_table: dataframe with two columns: first column contains names of all system components, second column contains list of all associated true failure probabilities.
simulate_true<-function(run_spec,prob_table=NULL){
  load("C:\\Users\\anhol\\Documents\\USMA Project\\MORS_paper\\code\\true_prob_table")
  comp_names<-run_spec$name
  probs<-prob_table$probs[which(prob_table$ID%in%comp_names)]
  n=run_spec$n
  sims=rep(0,length(n))
  for(i in 1:length(n)){
    sims[i]=rbinom(1,size=n[i],prob=probs[i])
  }
  sims_data<-data.frame(name=comp_names,n_fails=sims,n_total=n)
  return(sims_data)
}


