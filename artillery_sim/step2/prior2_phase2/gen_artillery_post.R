dat<- read.csv("collected_data.csv")
source('mh_sampler.R')
sample_res<-mh_sampler(data=dat,log_prior=log_prior,log_lik=log_lik,N=2500000,burn=1500000)
save(sample_res,file="posterior_samples")