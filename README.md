# room-orientation_behavior

lambda.csv
----------
This contains the posterior transfer efficiencies from DOI: 10.1098/rsif.2020.0121

movsdf.rbind_orientationcorrected.csv
-------------------------------------
This contains contact behaviour data used to inform the Markov chains utilised in the exposure model

particle_comparison.csv
----------------------
This provides deposition information from the CFD modeling, where orientation 1 corresponds to right-facing and orientation 2 corresponds to left-facing.

meshing_sensitivity analysis_v2.R
---------------------------------
This is the code used for the element size sensitivity analysis and utilizes files "orientation2_rake2_0.027.csv", "orientation2_rake2_0.03_2.csv", and "orientation2_rake2_0.04.csv".

source_code.R
-------------------------------
The main scenarios can be run from this code.

exposure_sim_and_figures.R
-------------------------------
This code is for running scenarios and creating figures in the manuscript.

sim_function.R
--------------------------------
This code is for creating the simulation function.

markov_chain.R
---------------------------------
This code is for creating discrete Markov chains necessary for the simulation model.

particle_tracking_analysis.R
----------------------------------
This code is for creating concentrations on surfaces informed by CFD results.

sensitivity_analysis.R
------------------------
This code is for investigating relationships between model inputs and outputs.
