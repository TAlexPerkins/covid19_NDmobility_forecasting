#!/bin/bash
#$ -q *@@perkins
#$ -N mcmc
#$ -t 1-486:1

module load R/3.6.2/gcc gcc
export R_LIBS=${HOME}/myRlibs-3.6.2

Rscript script_forecast_simple.R ${SGE_TASK_ID}
