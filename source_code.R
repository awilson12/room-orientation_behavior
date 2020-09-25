
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if("readr" %in% rownames(installed.packages())==FALSE){install.packages("readr"); require(readr)}else{require(readr)}
if("plyr" %in% rownames(installed.packages())==FALSE){install.packages("plyr"); require(plyr)}else{require(plyr)}
if("markovchain" %in% rownames(installed.packages())==FALSE){install.packages("markovchain"); require(markovchain)}else{require(markovchain)}
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}

#set up Markov chains
suppressMessages(suppressWarnings(source("markov_chain.R")))

#set up particle-tracking import
suppressMessages(suppressWarnings(source("particle_tracking_analysis.R")))

#set up exposure sim function
suppressMessages(suppressWarnings(source("sim_function.R")))

#run sims and analysis for figure/results generation
suppressMessages(suppressWarnings(source("exposure_sim_and_figures.R")))


