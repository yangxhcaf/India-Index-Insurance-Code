#!/bin/bash

# set output and error output filenames, %j will be replaced by Slurm with the jobid
#SBATCH -o testing%j.out
#SBATCH -e testing%j.err 

# single node in the "short or defq" partition
#SBATCH -N 1
#SBATCH -p defq,short
#SBATCH -D /groups/manngroup/India_Index/India-Index-Insurance-Code
# half hour timelimit
#SBATCH -t 20:00:00
#SBATCH --mail-user=mmann1123@gwu.edu
#SBATCH --mail-type=ALL

# Run the following in bash before starting R
module load proj.4/4.8.0
module load gdal/gcc/1.11
# module load R/3.0.2
module load R
module load gcc/4.9.0


srun R CMD BATCH ./script.R




