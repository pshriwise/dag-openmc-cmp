#!/bin/sh
# this file is called submit-script.sh
#SBATCH --partition=univ		# default "univ", if not specified
#SBATCH --time=1-00:00:00		# run time in days-hh:mm:ss
#SBATCH --nodes=4			# require 2 nodes
#SBATCH --ntasks-per-node=16            # (by default, "ntasks"="cpus")
#SBATCH --mem-per-cpu=4000		# RAM per CPU core, in MB (default 4 GB/core)
#SBATCH --error=job.%J.err
#SBATCH --output=job.%J.out
#Make sure to change the above two lines to reflect your appropriate
# file locations for standard error and output

#Now list your executable command (or a string of them).
# Example for non-SLURM-compiled code:
#module load mpi/gcc/openmpi-1.6.4
#mpirun -n 32 /home/username/mpiprogram
cd $HOME/opt/dag-openmc-cmp/FNG/openmc_run
mpiexec -n 64 openmc 
