source('~/USMA Project/MORS_paper/code/eval.R')
source('~/USMA Project/MORS_paper/code/R_GA_functions.R')
# Our implementation of the GA function assumes that
# the user is specifying the members of the initial 
# population and that those allocations have been 
# evaluated using the method of Chapman et al. (2012)

#This is a function for generating candidate designs using a genetic algorithm.
#Inputs:
#mcmcoutput: prior or posterior samples of the failure probabilities and 
#red (severe) failure probabilties for different components of the system
#node_names: list of names of all parameters of interest (failure probabilities) in the model,
#this should just be the set of column names from mcmcoutput
#crit_names: names of columns in mcmcoutput corresponding to criterion used to evaluate design
#(e.g. System and red_System). 
#startgen: starting generation of designs
#start_crit: design criterion values for starting generation
#weights: vector of weights used to combine criterion to create fitness functino
#cost: vector of cost of performing test of each component of the system
#the length of the cost vector should equal the number of system components
#bud: Overall budget constraint of the test plan
#constraints: a vector specifying the maximum possible number of tests for each component of the system
#nDen: Number of equal spaced points used to estimate densities in the CW approximation, this is the fidelity of the density estimation
#nOut: Number of datasets to draw from the design in order to estimate the CW for the design. We have to integrate out the variation due to dataset
#so we generate several and take the average. 

# GA function
ga_allR <- function(mcmcoutput,node_names,crit_names,startgen, start_crit, weights, cost, bud, constraints, nOut = 200, nDen=512,directory)
{
  #directory<-'C:\\Users\\anhol\\Documents\\USMA Project\\MORS_paper\\code\\experimental\\prior2_phase1\\GA_res\\'
	genstarttime=Sys.time()
	
	ncomps=ncol(mcmcoutput)	# identify number of nodes in the system
	ndraws=nrow(mcmcoutput) # identify number of posterior draws
		
	if (ncomps != length(node_names))
	{
		print("Number of names specified does not match number of columns in 'mcmcout'. Exiting GA function and returning NULL value.")
		return(NULL)
	}
	
	IDs = which(node_names %in% crit_names) # identify in which column nodes of interest are located
	ncrit=length(crit_names)	# identify number of nodes in which we are interested
	if(ncrit>ncol(start_crit)){
	  print("Starting generation values not provided for all criteria.")
	  return(NULL)
	}
	# assuming current pop
	# has been evaluated once and those values will be 
	# passed in to the GA function
	current_pop=startgen
	M = nrow(current_pop)
	nexps=nrow(current_pop)

	# Want all GAs to use the same expected credible interval
	# widths for the allocations in the initial generation
	# so these are passed to the function rather than re-evaluating
	# them and possibly getting something different.
	crit=start_crit
	colnames(crit)=crit_names
	
	# assuming no repeats in current generation
	unique_cand = cbind(current_pop, crit)
	colnames(unique_cand) = c(node_names,crit_names)
	
	# attach generation number to designs
	current_pop=cbind(generation=rep(0,M),current_pop)
	
	popnames=c("Gen")
	popnames=c(popnames,node_names)
	colnames(current_pop)=popnames
	
	# calculate desirability function for specified weight
	destemp = t(apply(crit,1,mult,weights))	# multiply each criteria by its weight
	des = 0
	for (i in 1:length(crit_names))
	{
		des = des + destemp[,i]   # additive desirability
	}	

	ord = order(des)				# identify order of des (from best to worst)
	current_pop = current_pop[ord[1:M],]  # order the current pop  (from best to worst)
	des = des[ord[1:M]]			# order the current pop  (from best to worst)
	crit = crit[ord[1:M],]				# order the current pop  (from best to worst)
	
	evaltime = difftime(Sys.time(),genstarttime,units='mins')
	startsearch = Sys.time()
	
	# identify which of the current candidates could potentially be on PF
	PFcandsCrit=crit[1,]
	PFcands=current_pop[1,]

	for (i in 2:nrow(crit))
	{
		temp = checkon(crit[i,],current_pop[i,], PFcandsCrit, PFcands)
		PFcandsCrit = temp[[1]]
		PFcands = temp[[2]]
	}
	
	AF=cbind(rep(0,nrow(PFcands)),PFcands,PFcandsCrit)
	namesAF=c("EndOfGen","Gen")
	namesAF=c(namesAF,node_names,crit_names)
	colnames(AF)=namesAF
	
	write.table(AF,paste(directory,"AllFronts.txt",sep=""),row.names=F,quote=F,sep=" ")
	
	searchtime = difftime(Sys.time(),startsearch,units='mins')
	evalANDsearch = difftime(Sys.time(),genstarttime,units='mins')
	
	times=t(c(0,format(evaltime),format(searchtime),format(evalANDsearch)))
	tnames=c("gen","evaltime","searchtime","evalANDsearch")
	names(times)=tnames
	write.table(times,paste(directory,"timeInfo.txt",sep=""),row.names=F,col.names=tnames,quote=T,sep=" ")
	
	write.table(cbind(current_pop,crit),paste(directory,"AllCandidates.txt",sep=""),row.names=F,col.names=namesAF[-1],quote=F,sep=" ")
	prev_max<-max(des)
	conv_crit=100
	gen=1
	while (conv_crit>1e-10)
	{
	  print(max(des))
	  print(gen)
		genstarttime=Sys.time()
		
		offspring=NULL
		critoff = NULL

		for (i in 1:M) # create crossover solutions
		{
			newC = crossover(current_pop[,-1],des)
			#newC=repairUP(newC,cost,bud)
			newC=repairDown(newC,cost,bud)
			offspring = rbind(offspring, as.data.frame(t(newC)))
			Cseen = FALSE
			for (j in 1:nrow(unique_cand))
			{
			    if (sum(unique_cand[j,1:ncomps]==newC)==ncomps)
				{
					Cseen = TRUE			# if the candidate has been evaluated already, don't re-evaluate
					critoff = rbind(critoff,unique_cand[j,(ncomps+1):(ncomps+ncrit)])
					break
				}
			}
			if (Cseen == FALSE)
			{
				temp_cw = round(eval_CW(mcmcoutput,node_names,t(newC),crit_names,nOut=nOut,nDen=nDen),6)
				temp<-temp_cw
				critoff = rbind(critoff,temp)
				temp2 = cbind(as.data.frame(t(newC)),temp)
				dimnames(temp2)[[2]] = dimnames(unique_cand)[[2]]
				unique_cand = rbind(unique_cand,temp2)
			}
		}
		
		for (i in 1:M) # create mutation solutions
		{
			newM = mutate(current_pop[i,-1], cost, bud, gen,constraints)
			#newM = repairUP(newM,cost,bud)
			newM = repairDown(newM,cost,bud)
			offspring = rbind(offspring, as.data.frame(t(newM)))
			Mseen = FALSE
			for (j in 1:nrow(unique_cand))
			{
			    if (sum(unique_cand[j,1:ncomps]==newM)==ncomps)
				{
					Mseen = TRUE		# if the candidate has been evaluated already, don't re-evaluate
					critoff = rbind(critoff,unique_cand[j,(ncomps+1):(ncomps+ncrit)])
					break
				}
			}
			if (Mseen == FALSE)
			{
			  temp_cw = round(eval_CW(mcmcoutput,node_names,t(newM),crit_names,nOut=nOut,nDen=nDen),6)
			  temp<-temp_cw
				critoff = rbind(critoff,temp)
				temp2 = cbind(as.data.frame(t(newM)),temp)
				dimnames(temp2)[[2]] = dimnames(unique_cand)[[2]]
				unique_cand = rbind(unique_cand,temp2)
			}			
		}

		# attach generation number to designs
		offspring = as.data.frame(cbind(generation=rep(gen,nrow(offspring)),offspring),row.names=1:(2*M))
		colnames(offspring)=popnames
		critoff=as.data.frame(critoff,row.names=1:(2*M))
		dimnames(critoff)[[2]]=dimnames(crit)[[2]]
				

		# calculate desirability function for specified weight
		desofftemp = t(apply(critoff,1,mult,weights))		# multiply each criteria by its weight
		desoff = 0
		for (i in 1:length(crit_names))
		{
			desoff = desoff + desofftemp[,i]   # additive desirability
		}
		current_pop = rbind(current_pop, offspring)
		colnames(current_pop)=popnames
		des = c(des,desoff)
		crit=rbind(crit,critoff)
		ord = order(des)				# identify order of des (from best to worst)
		current_pop = current_pop[ord[1:M],]  # keep the M best from the current pop
		des = des[ord[1:M]]			# keep the M best from the current pop
		crit = crit[ord[1:M],]				# keep the M best from the current pop
		
		evaltime = difftime(Sys.time(),genstarttime,units='mins')
		startsearch = Sys.time()

		write.table(critoff,paste(directory,"critoff.txt",sep=""),row.names=F,col.names=F,quote=T,sep=" ",append=T)
		write.table(offspring,paste(directory,"offspring.txt",sep=""),row.names=F,col.names=F,quote=T,sep=" ",append=T)

		for (i in 1:nrow(critoff))
		{
			temp = checkon(critoff[i,],offspring[i,], PFcandsCrit, PFcands)
			PFcandsCrit = temp[[1]]
			PFcands = temp[[2]]
		}
		AF=cbind(rep(gen,nrow(PFcands)),PFcands,PFcandsCrit)
		colnames(AF)=namesAF
		write.table(AF,paste(directory,"AllFronts.txt",sep=""),row.names=F,col.names=F,quote=F,sep=" ",append=T)
	
		searchtime = difftime(Sys.time(),startsearch,units='mins')
		evalANDsearch = difftime(Sys.time(),genstarttime,units='mins')
		
		times=t(c(gen,format(evaltime),format(searchtime),format(evalANDsearch)))
		write.table(times,paste(directory,"timeInfo.txt",sep=""),row.names=F,col.names=F,quote=T,sep=" ",append=T)
		
		write.table(cbind(offspring,critoff),paste(directory,"AllCandidates.txt",sep=""),
		            row.names=F,col.names=F,quote=F,sep=" ", append=T)
		cur_max=max(des)
		conv_crit=abs(cur_max-prev_max)
		prev_max=cur_max
		gen=gen+1
	} # end generation loop

	write.table(cbind(current_pop,crit,des),paste(directory,"final_pop.txt",sep=""),
	            row.names=F,col.names=c(namesAF[-1],"des"))
	write.table(cbind(PFcands,PFcandsCrit),paste(directory,"PFcandidates.txt",sep=""),
	            row.names=F,col.names=namesAF[-1],quote=F,append = T)
	write.table(unique_cand,paste(directory,"Unique.txt",sep=""),
	            row.names=F,col.names=T,quote=F,append=T)
} 

