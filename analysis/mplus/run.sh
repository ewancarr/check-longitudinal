#!/bin/bash
#SBATCH --partition=brc
#SBATCH --tasks=8
#SBATCH --mem=8000
#SBATCH --job-name=ch_long
#SBATCH --array=0-92
#SBATCH --output=/scratch/users/%u/check-longitudinal/analysis/mplus/logs/%a.out
#SBATCH --time=0-99:00
module load apps/singularity
MPLUS='singularity exec /scratch/users/k1644956/check-longitudinal/containers/mplus.simg mplus'
cd /scratch/users/k1644956/check-longitudinal/analysis/mplus
INP=input_files/$(sed "$SLURM_ARRAY_TASK_ID q;d" data/index)
FOLDER=$(echo $INP | cut -f1 -d ' ')
FILE=$(echo $INP | cut -f2 -d ' ')
cd $FOLDER
$MPLUS $FILE
