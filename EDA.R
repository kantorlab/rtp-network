# read data

setwd("/gpfs/data/rkantor/rtp/datasets/D30_20211013_V1")
ls()
list.files()

net <- read.csv("ContactTracingNetwork.csv")
dim(net)
str(net)
View(net)

table(net$InterviewRank, exclude = NULL)
