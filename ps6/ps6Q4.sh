#!/bin/bash
# Job name:
#SBATCH --job-name=ps6Q4
#
# Account:
#SBATCH --account=ic_stat243
#
# Partition:
#SBATCH --partition=savio
#
# Wall clock limit (30 seconds here):
#SBATCH --time=02:00:00
#
## Command(s) to run:
module load r/3.2.5
module load foreach doParallel stringr
R CMD BATCH --no-save ps6Q4.R ps6Q4.out