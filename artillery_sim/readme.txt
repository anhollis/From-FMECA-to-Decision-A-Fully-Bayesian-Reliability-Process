GA_R.R contains code that takes in prior or posterior draws of component failure probabilitities, initial designs and associated design criteria, as well as design constraints to produce candidate designs that are optimal according to a convex combination of the design criteria using a genetic algorithm.

R_GA_functions.R contains helper functions for the main genetic algorthim design generator in GA_R.R

eval.R contains functions for approximating the posterior credible interval widths used as design criterion using the computational approach from the Chapman paper.

generate_data.R used to generate data from a spcific test design

generate_post.R used to generate posterior sample of failure mode probabilities given data and prior

example_run_through.R demonstrates the code needed to go from a prior/posterior sample of failure probability parameters to a posterior sample of failure probability parameters given data from a test design chosen using the method in the paper.

step1 contains results and code specific to the initial tests using all combinations of phase (phase 1 or 2) and prior (prior 1 or 2)
step2 contains results and code specific to the subsequents tests using all combinations of phase (phase 1 or 2) and prior (prior 1 or 2)

