#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=9
#SBATCH --nodes=1

R CMD BATCH --no-save R/parallel.R parallel.Rout
