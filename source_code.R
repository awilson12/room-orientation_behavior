
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#set up Markov chains
suppressMessages(suppressWarnings(source("markov_chain.R")))

#set up particle-tracking import
suppressMessages(suppressWarnings(source("particle_tracking_analysis.R")))

#set up exposure sim function
suppressMessages(suppressWarnings(source("sim_function.R")))

#run sims and analysis for figure/results generation
suppressMessages(suppressWarnings(source("exposure_sim_and_figures.R")))


