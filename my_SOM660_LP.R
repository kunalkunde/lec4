setwd("C:/Users/Kunal Kunde/OneDrive/Documents/SOM 660/Lec 4")
library(lpSolve)
library(readr)
direction <- read_tsv("direction.txt",col_names = FALSE)
constraintsMatrix <- read_tsv("constraintsMatrix.txt",col_names = FALSE)
rightHandSide <- read_tsv("rightHandSide.txt",col_names = FALSE)
objectiveFunctionCoefficients <- read_tsv("objectiveFunctionCoefficients.txt",col_names = FALSE)
my_SOM660_LP <- lp(direction = "max", objective.in = objectiveFunctionCoefficients, const.mat = constraintsMatrix, const.dir = direction, const.rhs = rightHandSide, transpose.constraints = TRUE)
my_SOM660_LP$solution