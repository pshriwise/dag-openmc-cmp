set -e

# Collimator model
wget https://anl.box.com/shared/static/isn5mn1y5qf2jjwr4ofnrghf11m6ep3r.h5m -O dagmc.h5m
mv dagmc.h5m Collimator/dagopenmc_run

# FNG model
wget https://anl.box.com/shared/static/2nmrf3vpu1n8bpcadjqaf884vc65y8h5.h5m -O dagmc.h5m
mv dagmc.h5m FNG/dagopenmc_run

# ATR model
wget https://anl.box.com/shared/static/w57ambhcijysdns573jnor4215zg28mh.h5m -O dagmc.h5m
mv dagmc.h5m ATR/dagopenmc_run
