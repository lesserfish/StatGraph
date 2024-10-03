#!/usr/bin/Rscript
# This Rscript builds, test and constructs the documentation for statGraph
# It exists with 1 in care of error, and 0 in case of success
# It is invoked by GithubActions for automatic testing

if (!require("devtools")) install.packages("devtools")
if (!require("igraph")) install.packages("igraph")
if (!require("rARPACK")) install.packages("rARPACK")
if (!require("foreach")) install.packages("foreach")
if (!require("doParallel")) install.packages("doParallel")
if (!require("ks")) install.packages("ks")
