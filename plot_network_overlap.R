rm(list=ls())


# Load libraries ---------------------------

library(here)
library(dplyr)
library(network)
library(sna)


# Setup ---------------------------

par(mar=c(1,1,1,1)) #for figures
ls()

### 


# Set seed ---------------------------

set.seed(1234567)


# Load Objects ---------------------------

## rds object
eda_env <- readRDS("eda_objects.rds")
names(eda_env)

## networks
ct_net_unique <- eda_env$ct_net_unique
phylo_net <- eda_env$phylo_net

## edge lists
ct_el <- eda_env$ct_el 
ct_el_unique <- eda_env$ct_el_unique 
phylo_el <- eda_env$phylo_el 

## common edge lists
common_links <- eda_env$common_links

## query objects
ct_net_unique
phylo_net

head(ct_el_unique)
head(phylo_el)


# Plot the two networks ---------------------------

#gplot(ct_net_unique, usearrows = FALSE)
#gplot(phylo_net, usearrows = FALSE)


# Compute overlap between nodes present in both networks ---------------------------

ct_net_ids <- ct_net_unique %v% "vertex.names"
phylo_net_ids <- phylo_net %v% "vertex.names"

common_node_ids <- intersect(ct_net_ids, phylo_net_ids)
length(common_node_ids)

unique_ct_net_ids <- setdiff(ct_net_ids, phylo_net_ids)
length(unique_ct_net_ids)

unique_phylo_net_ids <- setdiff(phylo_net_ids, ct_net_ids)
length(unique_phylo_net_ids)


# Load overlap between nodes present in both networks ---------------------------

el_ct_net_unique <- as.data.frame(as.edgelist(ct_net_unique))
el_ct_net_unique_named <- apply(el_ct_net_unique, 2, function(x) ct_net_ids[x])

el_phylo_net <- as.data.frame(as.edgelist(phylo_net))
el_phylo_net_named <- apply(el_phylo_net, 2, function(x) phylo_net_ids[x])


head(el_ct_net_unique_named)
head(el_phylo_net_named)
common_links





