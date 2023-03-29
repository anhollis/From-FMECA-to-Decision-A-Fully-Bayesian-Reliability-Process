eval_CW <- function(mcmcout,node_names, allocs, crit_names, nOut = 200, nDen=512)
{
	# R function to evaluate the expected credible interval width
	# for the nodes indicated in "crit_names"

	# mcmcout = matrix containing posterior draws for system nodes
	#		columns should correspond to nodes while each
	# 		row represents a draw from the joint node reliability
	#		posterior distribution

	# node_names = vector indicating names for the nodes (this correspond
	# 			to the columns of mcmcout (indicates which column
	#			corresponds to each node)

	# allocs = matrix containing allocations to be evaluated
	#		columns correspond to nodes and rows are allocations
	# 		indicating the numbers of tests allotted to each node

	# crit = vector indicating which nodes (via names used in "node_names") to
	#		compute expected credible interval widths for

	# nOut = number of hypothetical outcomes to generate for each allocation;
	#		by default this is specified to be 200 randomly generated outcomes
	# 		Increasing this number (to something like 1000) will result in less variability when the
	#		same allocation is evaluated multiple times (results will stay stable
	#		for several significant digits); this will slow run time

	# nDen = number of points at which to evaluate density estimates (note that
	#		512 is the default in R, and we prefer to use that)

	nsys = ncol(mcmcout) 		# identify number of nodes
	
	nMCMC = nrow(mcmcout)		# identify number of posterior draws
	
	if (nsys != length(node_names))
	{
		print("Number of names specified does not match number of columns in 'mcmcout'. Returning NULL value.")
		return(NULL)
	}

	nAlloc = nrow(allocs)		# identify number of allocations
	if (ncol(allocs) != nsys)
	{
		print("Number of nodes in allocation does not match number of nodes in MCMC output. Returning NULL value.")
		return(NULL)
	}

	nCrit = length(crit_names)		# identify number of criteria
	crit_nums = which(node_names %in% crit_names)
	
	densities = NULL
	# obtain density estimates for the marginal posterior distributions for the nodes of interest
	# note that the density estimate should be obtained over the entire interval 0 to 1
	# This is the approximation of f(theta | bold_x)
	for (i in crit_nums)		 
	{
		d = density(mcmcout[,i],from=0,to=1,n=nDen)
		densities=cbind(densities,d$x,d$y)
	}
		
	# remaining code is needed for approximation of f(boldx_new | theta) and f(boldx_new | bold_x)

	# compute the increment between successive grid values for each node
	increment = densities[2,seq(1,(2*nCrit-1),2)]-densities[1,seq(1,(2*nCrit-1),2)]
	halfinc = increment/2
	lo = densities[1,seq(1,(2*nCrit-1),2)] - halfinc
	hi = densities[nDen,seq(1,(2*nCrit-1),2)] + halfinc

	# define bins in terms of breaks (as in a histogram) that cover interval 0 and 1 and count how many
	# posterior draws are in each bin. Also identify in which bin each posterior draw falls.
	#breaks=NULL		# will be a matrix with a row for each criterion being estimated
	#binSizes=NULL	# will be a matrix with a row for each criterion being estimated
	#binID=NULL		# rows = criteria, columns = posterior draws
				# keeps track of in which bin each draw falls
	
	whichBin<-function(val,breaks)
	{
		return(max(which(val>breaks)))
	}
	
	breaks = vector("list",nCrit)
	binSizes = vector("list",nCrit)
	binID = vector("list",nCrit)
	for (i in 1:nCrit)
	{
		breaks[[i]] = seq(lo[i],hi[i],by=increment[i])
		binSizes[[i]] = hist(mcmcout[,crit_nums[i]],breaks=breaks[[i]],plot=FALSE)$counts
		binID[[i]] = as.numeric(apply(as.matrix(mcmcout[,crit_nums[i]]),1,whichBin,breaks[[i]]))
	}
	
	outcomeProb <- function(p,x,n)
	{
		indprob = dbinom(as.numeric(x),as.numeric(n),as.numeric(p))
		return(prod(indprob))
	}

	expCW=NULL 			# will store the expected credible interval widths for the nodes of interest (for all allocations)
	
	for (m in 1:nAlloc)	# loop through all allocations
	{
			
		expCWm = rep(0,nCrit)

		for (k in 1:nOut)
		{
			rrow = sample(1:nMCMC,1)		# sample a row from MCMC output
			Outcome = NULL
			for (i in 1:nsys)	
			{
				Outcome = cbind(Outcome, rbinom(1,allocs[m,i],mcmcout[rrow,i]))	# use row to generate an outcome
			}

			# Find the probability of (joint) outcome k as product of individual probabilities
			Pout = apply(mcmcout,1,outcomeProb,Outcome,allocs[m,])
			
			pxnewgivenxold = mean(Pout)	# average over all posterior draws for f(boldx_new | bold_x)

			# finally approximate f(boldx_new | theta) for each criterion
			for (n in 1:nCrit)
			{
				pxnewgiventheta = NULL
				
				for (i in 1:nDen)
				{
					# identify which posterior draws have theta in the ith bin

					if (binSizes[[n]][i]==0)
					{
						pxnewgiventheta = cbind(pxnewgiventheta,0)
					}
					else
					{
						pxnewgiventheta = cbind(pxnewgiventheta,mean(Pout[binID[[n]]==i]))
					}
				}

				# Chapman et al. (2012) note that in some cases, particularly when f(boldx_new | bold_x) is very small,
				# the above approximation is poor and recommend using weighted regression
				# to approximate f(boldx_new | theta) for each criterion. See Chapman et al. (2012)

				if (pxnewgivenxold < 1e-12)
				{
					XX = as.matrix(cbind(rep(1,nMCMC),mcmcout[,crit_nums[n]],mcmcout[,crit_nums[n]]^2))
						# define X matrix for weighted regression with posterior theta draws as predictor (using a quadratic model)
					W = diag(binSizes[[n]][(binID[[n]])])
						# define weight matrix (weight for each observation is number of posterior draws in the bin in which that
						# observation appears
	
					Y = Pout + runif(nMCMC,1e-100,1e-50) # add a small amount of random noise to outcome probabilities (to transfrom for response)
					Y = as.matrix(log(Y/(1-Y)))
					
					res = solve(t(XX)%*%W%*%XX)%*%t(XX)%*%W%*%Y		#(X'WX)^(-1)X'WY

					pxnewgiventheta = exp(res[1,1]+res[2,1]*densities[,(2*n-1)]+res[3,1]*densities[,(2*n-1)]^2)/
								(1+exp(res[1,1]+res[2,1]*densities[,(2*n-1)]+res[3,1]*densities[,(2*n-1)]^2))
				} # end weighted regression

			# approximate updated posterior distribution
			updatedpost = densities[,2*n]*pxnewgiventheta/pxnewgivenxold
			updatedpost = updatedpost/sum(updatedpost*increment[n])	# scale so a valid density
			
			# identify in which "bin" the 0.025 and 0.975 quantiles occur
			lb = densities[min(which(cumsum(updatedpost*increment[n])>0.025)),(2*n-1)]	# lower bound on credible interval
			ub = densities[min(which(cumsum(updatedpost*increment[n])>0.975)),(2*n-1)]	# upper bound on credible interval

			expCWm[n] = expCWm[n] + (ub-lb)/nOut	# expected CW for allocation is the average cw for the nOut hypothetical outcomes

			} # end criteria loop
		} # end outcome loop 
		
		expCW = rbind(expCW,expCWm)
	} # end allocation loop
	
	dimnames(expCW)[[2]]=crit_names

	return(expCW)
}



eval_weightedEnt<-function(mcmcout,allocs,q=1){
  mcmcout<-as.matrix(mcmcout)
  #w<-t(mcmcout)
  w<-as.numeric(apply(mcmcout,2,mean))
  w<-q*w+(1-q)
  allocs<-as.matrix(allocs)
  allocs<-t(apply(allocs,1,function(x){return(x/sum(x))}))+1e-14
  entrop<-allocs*(-log(allocs))
  crits<-as.vector(entrop%*%w)
  #mean_crit<-apply(crits,1,mean)
  #return(mean_crit)
  return(crits)
}





