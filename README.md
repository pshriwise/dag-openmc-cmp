
This repository contains scripts intended for comparisons of DAG-OpenMC (OpenMC using DagMC geometry) to DAG-MCNP (MCNP using DagMC geometry).

For more information about these projects please see:

  - [OpenMC](https://openmc.readthedocs.io/en/stable/)
  - [MCNP](https://mcnp.lanl.gov/)
  - [DagMC](https://svalinn.github.io/DAGMC/)


*NOTE: These models rely OpenMC's ENDFB-VIII.0 data [found here](https://openmc.org/lanl-data-libraries/)*

## Retrieving the DAGMC Models

To run each model they should be downloaded from the following links and placed in their respective folders:

  - [Collimator](https://anl.box.com/shared/static/isn5mn1y5qf2jjwr4ofnrghf11m6ep3r.h5m)
  - [Frascati Neutron Generator (FNG)](https://anl.box.com/shared/static/2nmrf3vpu1n8bpcadjqaf884vc65y8h5.h5m)

The `get_dagmc_models.sh` script can be used to download these models and place them in the correct locations.

## Running a comparison

  1. Enter the directory of a model (e.g. FNG)
  2. Run `python run.py` to run both versions of the geometry and compare the results

Use `python run.py -h` to see various options availble when performing the comparisons.
