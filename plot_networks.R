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

dim(individuals_dt)
dim(el_matrix_005)

# Filter individuals_full_dt to only include individuals sequenced in HIVTRACE005 individuals ---------------------------

individuals_dt_005 <- 
  individuals_dt %>%
  filter(ClusteredHIVTrace005 != "")

dim(individuals_dt_005)

# Plot network ---------------------------

mol_net_005 <- network.initialize(n=length(individuals_dt_005$StudyID), 
                                  directed = FALSE, 
                                  loops = FALSE, 
                                  bipartite = NULL
                                  )

mol_net_005 %v% "vertex.names" <- as.character(individuals_dt_005$StudyID)
vnames <- mol_net_005 %v% "vertex.names" 

el_matrix_005_by_vid <-
  #create edgelist by vertex ID to preserve ordering
  cbind(match(el_matrix_005[,1], vnames), 
      match(el_matrix_005[,2], vnames)
)
dim(el_matrix_005_by_vid)  

add.edges(mol_net_005, el_matrix_005_by_vid[,1], el_matrix_005_by_vid[,2])
mol_net_005
isolates(mol_net_005)

mol_net_005 %v% "vertex.names"


# Basic network plot ---------------------------



fix_coord <- 
  gplot(mol_net_005,             
      usearrows=FALSE,
      edge.lwd=2,
      edge.lty=1,
      #edge.col="black",
      displayisolates = TRUE,
      vertex.cex = 2,
      #vertex.col = vertex.col,
      #vertex.border = vertex.border
      )


# Highlight by race ---------------------------

sort(colnames(individuals_dt_005))
table(individuals_dt_005$DemoRace)

vertex.col = rep("gray", nrow(individuals_dt_005))
vertex.border = rep("black", nrow(individuals_dt_005))
edge.col = rep("black", nrow(el_matrix_005))


#vertex.col[individuals_dt$DemoRace == ""] <- "white"
  
vertex.sides = rep(8, nrow(individuals_dt_005))
vertex.sides[individuals_dt_005$DemoRace == "Black"] <- 3
vertex.sides[individuals_dt_005$DemoRace == "Asian"] <- 7
vertex.sides[individuals_dt_005$DemoRace == "White"] <- 4
vertex.sides[individuals_dt_005$DemoRace == ""] <- 8

#vertex.border[individuals_dt_005$DemoRace == ""] <- "blue"

gplot(mol_net_005,             
      usearrows=FALSE,
      edge.lwd=0.5,
      edge.lty=3,
      #usecurve=TRUE,
      displayisolates = TRUE,
      vertex.cex = 1.5,
      vertex.col = vertex.col,
      vertex.border = vertex.border,
      vertex.sides = vertex.sides,
      edge.col = edge.col,
      coord = fix_coord
)

legend("bottomright", c("Black", "White", "Not Reported"),
       pch=c(24, 22, 21),
       title = "Race",
       col = "gray",
       pt.bg = c("gray"),
       pt.lwd = 1
       )



# Highlight by current gender ---------------------------

table(individuals_dt_005$DemoGender)
vertex.sides = rep(8, nrow(individuals_dt_005))
vertex.sides[individuals_dt_005$DemoGender == "Male"] <- 3
vertex.sides[individuals_dt_005$DemoGender == "Female"] <- 4
gplot(mol_net_005,             
      usearrows=FALSE,
      edge.lwd=0.5,
      edge.lty=3,
      #usecurve=TRUE,
      displayisolates = TRUE,
      vertex.cex = 1.5,
      vertex.col = vertex.col,
      vertex.border = vertex.border,
      vertex.sides = vertex.sides,
      edge.col = edge.col,
      coord = fix_coord
)
legend("bottomright", c("Male", "Female", "NB/Not Reported"),
       pch=c(24, 22, 21),
       title = "Current Gender",
       col = "gray",
       pt.bg = c("gray"),
       pt.lwd = 1
)


# Highlight by sex at birth ---------------------------

table(individuals_dt_005$DemoBirthSex)
vertex.sides = rep(8, nrow(individuals_dt_005))
vertex.sides[individuals_dt_005$DemoBirthSex == "Male"] <- 3
vertex.sides[individuals_dt_005$DemoBirthSex == "Female"] <- 4
gplot(mol_net_005,             
      usearrows=FALSE,
      edge.lwd=0.1,
      edge.lty=3,
      #usecurve=TRUE,
      displayisolates = TRUE,
      vertex.cex = 1.5,
      vertex.col = vertex.col,
      vertex.border = vertex.border,
      vertex.sides = vertex.sides,
      edge.col = edge.col,
      coord = fix_coord
)
legend("bottomright", c("Male", "Female", "NB/Not Reported"),
       pch=c(24, 22, 21),
       title = "Sex Assigned at Birth",
       col = "gray",
       pt.bg = c("gray"),
       pt.lwd = 1
)


# Highlight by behavior category ---------------------------

table(individuals_dt_005$RiskMSM, exclude = NULL)
table(individuals_dt_005$RiskHRH, exclude = NULL)
table(individuals_dt_005$RiskIDU, exclude = NULL)

vertex.col = rep("gray", nrow(individuals_dt_005))
vertex.col[individuals_dt_005$RiskMSM == "True"] <- "red"

vertex.sides = rep(8, nrow(individuals_dt_005))
vertex.sides[individuals_dt_005$RiskHRH == "True"] <- 4

label <- rep(NA, nrow(individuals_dt_005))
label[individuals_dt_005$RiskIDU == "True"] <- "D"

gplot(mol_net_005,             
      usearrows=FALSE,
      edge.lwd=0.5,
      edge.lty=3,
      #usecurve=TRUE,
      displayisolates = TRUE,
      vertex.cex = 1.5,
      vertex.col = vertex.col,
      vertex.border = vertex.border,
      vertex.sides = vertex.sides,
      edge.col = edge.col,
      label = label,
      label.cex = 0.5,
      label.pos = 4,
      coord = fix_coord
)
legend("bottomright", c("MSM", "Non-MSM", "HRH"), 
       pch=c(21, 21, 22),
       title = "Behavior Category",
       col = c("red", "gray", "gray"),
       pt.bg = c("red", "gray", "gray"),
       pt.lwd = 1
)

