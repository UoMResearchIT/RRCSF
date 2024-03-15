#!/bin/bash --login
#$ -cwd
#$ -pe smp.pe 16
#$ -l mem256
#$ -o CSF_ModelD_2.log
#$ -e CSF_ModelD_2.log

module load apps/gcc/R/4.2.2

R CMD BATCH --no-restore CSF_ModelD_2.R CSF_ModelD_2.log
