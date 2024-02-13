#!/bin/bash

#SBATCH --qos=1day
#SBATCH --time=1-00:00:00
#SBATCH --mem=240g
#SBATCH --output=run_bac.out
#SBATCH --error=run_bac.error
#SBATCH --job-name=Bac_ASV_clustering
#SBATCH --cpus-per-task=16
#SBATCH --mail-user=jan.waelchli@unibas.ch
#SBATCH --mail-type=ALL

#load R module
module load foss/2018b
module load R/4.0.0-foss-2018b

srun ./02.1_bacteria_ASV_clustering.R
