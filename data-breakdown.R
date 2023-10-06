rm(list=ls())
library(data.table)
library(dplyr)
library(stringi)
library(network)

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D51_20230512_unified"
list.files(path=data_dir)
net_dt_old <- as.data.table(read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))) 
net_dt <- as.data.table(read.csv("/gpfs/data/rkantor/rtp/shared_dir/ContactTracingNetwork20230726.csv"))
individuals_dt <- as.data.table(read.csv(paste0(data_dir, "/Individuals.csv")))

# Identify all unique StudyIDFroms 
length(unique(net_dt$StudyIDFrom))

# Identify all unique StudyIDTos 
length(unique(net_dt$StudyIDTo))

# Identify the union of the two above
length(union(unique(net_dt$StudyIDFrom), unique(net_dt$StudyIDTo)))

# Identify their intersection
length(intersect(unique(net_dt$StudyIDFrom), unique(net_dt$StudyIDTo)))

# How many unique clusters are the individuals in?
length(unique(individuals_dt$ClusteredPhyloAny)) 

# How many individuals are *not* clustered?
length(which(is.na(individuals_dt$ClusteredPhyloAny)))

# How many individuals are clustered?
length(unique(individuals_dt$StudyID)) - length(which(is.na(individuals_dt$ClusteredPhyloAny)))

# How many are in the intersection of Partner DB and Molecular DB?
individuals_in_net_dt_studyidfrom_or_to <- 
  which(individuals_dt$StudyID %in% c(net_dt$StudyIDFrom, net_dt$StudyIDTo))
id_individuals_in_net_dt_studyidfrom_or_to <- individuals_dt$StudyID[individuals_in_net_dt_studyidfrom_or_to]
length(individuals_in_net_dt_studyidfrom_or_to)
length(unique(individuals_in_net_dt_studyidfrom_or_to))

# How many individuals in the Genomic DB are in the named partners?
individuals_in_net_dt_studyidto <- which(individuals_dt$StudyID %in% net_dt$StudyIDTo)
length(individuals_in_net_dt_studyidto)

# HOw many named partnerships in the partner database?
net_dt_without_empty_StudyIDTo <- net_dt[StudyIDTo != ""]
nrow(net_dt_without_empty_StudyIDTo)

# How many index cases appearing the partner DB named partners?
individuals_in_net_dt_studyidfrom_wo_emptyStudyIDTo <- 
  which(individuals_dt$StudyID %in% net_dt_without_empty_StudyIDTo$StudyIDFrom)
length(individuals_in_net_dt_studyidfrom_wo_emptyStudyIDTo)

# How many named partners of index cases in the partner DB who named someone?
length(unique(net_dt_without_empty_StudyIDTo$StudyIDTo))