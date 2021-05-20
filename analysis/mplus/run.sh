#!/bin/bash
#SBATCH --partition=brc
#SBATCH --tasks=4
#SBATCH --mem=4000
#SBATCH --job-name=array
#SBATCH --array=1-62
#SBATCH --output=/scratch/users/%u/check-longitudinal/analysis/mplus/logs/%a.out
#SBATCH --time=0-96:00
module load apps/singularity
cd /scratch/users/k1644956/check-longitudinal/analysis/mplus/input_files
inp=$(sed "$SLURM_ARRAY_TASK_ID q;d" ../data/index)
singularity exec ../../../containers/mplus.simg mplus $inp
