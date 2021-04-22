#!/bin/bash
#SBATCH --partition=brc
#SBATCH --tasks=8
#SBATCH --mem=8000
#SBATCH --job-name=array
#SBATCH --array=1-36
#SBATCH --output=/scratch/users/%u/check-longitudinal/analysis/rosalind/logs/%a.out
#SBATCH --time=0-96:00
module load apps/singularity
cd /scratch/users/k1644956/check-longitudinal/analysis/rosalind/fits
singularity exec ../../../containers/mplus.simg mplus $SLURM_ARRAY_TASK_ID
