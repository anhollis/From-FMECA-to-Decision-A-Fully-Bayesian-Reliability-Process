simulate_true<-function(run_spec,prob_table=NULL){
  load("prob_table")
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


