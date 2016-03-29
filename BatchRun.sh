#!/bin/bash

# set output and error output filenames, %j will be replaced by Slurm with the jobid
#SBATCH -o testing%j.out
#SBATCH -e testing%j.err 

# single node in the "short" partition
#SBATCH -N 1
#SBATCH -p short
#SBATCH -D /groups/manngroup/India_Index/India-Index-Insurance-Code
# half hour timelimit
#SBATCH -t 8:00:00


# Run the following in bash before starting R
module load proj.4/4.8.0
module load gdal/gcc/1.11
# module load R/3.0.2
module load R
module load gcc/4.9.0


srun R CMD BATCH ./script.R




