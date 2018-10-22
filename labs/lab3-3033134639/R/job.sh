#!/bin/bash
#SBATCH -- cpus-per-task 2

R CMD BATCH --no-save parallel.R parallel.Rout
