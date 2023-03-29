GA_R.R contains code that takes in prior or posterior draws of component failure probabilitities, initial designs and associated design criteria, as well as design constraints to produce candidate designs that are optimal according to a convex combination of the design criteria using a genetic algorithm.

R_GA_functions.R contains helper functions for the main genetic algorthim design generator in GA_R.R

eval.R contains functions for approximating the posterior credible interval widths used as design criterion using the computational approach from the Chapman paper.

true_sim.R contains functions for generating data from true system (true_prob_table contains the associated true system component failure probabilities).

process_pareto.R does a final check of Pareto candidates (in PFCandidates or pareto.txt) and outputs pareto.csv a final set of true pareto designs.

prior_options.R contains the details of the different priors used in experiments

plot_designs.R contains functions for plotting visuals of different test designs

commanders_intent_metrics.R contains functions for computing entropy, cost, weighted entropy and other commander's intent metrics for final design selection.

system_fails_from_fail_modes.R contains functions for computing the functional, subsytem, and system failure probabilities given the failure mode probabilities.

compute_red_yellow_green.R contains functions for computing the function, subsytem, and system level probabilities for green, yellow, and red failure from the failure mode probabilties. 

data_for_design_metrics.R contains functions for computing the function, subsystem, and system level failure and red failure probabilities from the failure mode probabilities.