rm(list=ls())


# Load libraries ---------------------------
library(sna)
library(network)
library(magrittr)



# Read data ---------------------------

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D30_20211013_V1"
list.files(path=data_dir)
individuals_dt <- read.csv(paste0(data_dir, "/Individuals.csv"))

el_matrix_005 <- readRDS("el_matrix_005.RDS")


# Plot network ---------------------------

mol_net_005 <- network.initialize(n=length(individuals_dt$StudyID), 
                                  directed = FALSE, 
                                  loops = FALSE, 
                                  bipartite = NULL
                                  )

mol_net_005 %v% "vertex.names" <- as.character(individuals_dt$StudyID)
vnames <- mol_net_005 %v% "vertex.names" 

el_matrix_005_by_vid <-
  #create edgelist by vertex ID to preserve ordering
  cbind(match(el_matrix_005[,1], vnames), 
      match(el_matrix_005[,2], vnames)
)
  
add.edges(mol_net_005, el_matrix_005_by_vid[,1], el_matrix_005_by_vid[,2])

mol_net_005
isolates(mol_net_005)

mol_net_005 %v% "vertex.names"

vertex.sides = rep(8, length(individuals_dt$StudyID))
vertex.col = "white"

gplot(mol_net_005,             
      usearrows=FALSE,
      edge.lwd=2,
      edge.lty=1,
      #usecurve=TRUE,
      edge.col="black",
      displayisolates = FALSE,
      vertex.cex = 2,
      vertex.col = vertex.col
      )


# Node highlighting, etc ---------------------------

## Identity: RiskMSM, RiskHRH, RiskIDU
## Race
## Current Gender

sort(colnames(individuals_dt))






