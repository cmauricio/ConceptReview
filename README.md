# Concept Review cluster analysis
R code used in the cluster analysis for the concept review on Scientific Reasoning and Scientific Thinking
By Carlos Mauricio Castaño Díaz

# List of files (in alphabetic order)

 - ChiAnalyses.R
 - DescriptiveStatistics.R
 - DichotomicVarsCreation.R
 - IgraphFilePreparationFunctions.R
 - NetworkAnalyses.R

# Description of the files

#ChiAnalyses.R#

This piece of code uses the package 'gmodels' to carry chi analysis for comparing the differences between different definitions found in the variables analysed for the concept review.

#DescriptiveStatistics.R#

This code uses the package Plyr to generate the desciptive statistics for the sample of texts according to the database generated for the analysis.

#DichotomicVarsCreation.R#

This code uses the packade 'plyr' to automatic generate dichotomic variablesfrom the categorical data found in the concept review database.

#IgraphFilePreparationFunctions.R#

This file develops a series of functions to prepare files for igraph.
 - Function to transform non-squared edge matrixes into bipartite igraph matrixes.
 - Function to create codes for the EDGE matrix.
 - Function for association of codes to the NODE matrix.

#NetworkAnalyses.R#

This code uses igraph to create the network analyses for the different variables of the concept analyses. Characteristics, Teleologies, and Associated Concepts.