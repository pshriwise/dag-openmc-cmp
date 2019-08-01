

# set number of threads for run here
export OMP_NUM_THREADS=6

# OpenMC run
cd openmc_run && python input.py -g csg && openmc

# back to top level dir
cd ..

# DagOpenMC run
cd dagopenmc_run && python input.py && openmc

# back to top dir
cd ..

# Run comparison script
#python compare.py
