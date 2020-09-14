#!/bin/bash
#$ -q *@@perkins
#$ -N mcmc
#$ -t 1-486:1

Rscript script_forecast_simple_training.R ${SGE_TASK_ID}
