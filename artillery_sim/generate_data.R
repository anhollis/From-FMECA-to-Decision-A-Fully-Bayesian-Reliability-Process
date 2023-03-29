source('~/USMA Project/MORS_paper/code/true_sim.R')
generate_data<-function(test_design){
  non_zero<-test_design>0
  dat_names<-names(test_design)[non_zero]
  dat_counts<-as.numeric(test_design[non_zero])
  runspec<-data.frame(name=dat_names,n=dat_counts)
  simdat<-simulate_true(runspec)
  return(simdat)
}