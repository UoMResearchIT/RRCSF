#!/bin/bash --login
#$ -cwd
#$ -pe smp.pe 4
#$ -o job.log
#$ -e job.log

module load apps/gcc/R/4.2.2

R CMD BATCH --no-save --no-restore parallel.R --iter 32 --sleep 0.1
