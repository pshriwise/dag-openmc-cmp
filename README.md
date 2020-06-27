
This repository contains scripts intended for comparisons of DAG-OpenMC (OpenMC using DagMC geometry) to DAG-MCNP (MCNP using DagMC geometry).

For more information about these projects please see:

  - [OpenMC](https://openmc.readthedocs.io/en/stable/)
  - [MCNP](https://mcnp.lanl.gov/)
  - [DagMC](https://svalinn.github.io/DAGMC/)


*NOTE: These models rely OpenMC's ENDFB-VIII.0 data [found here](https://openmc.org/lanl-data-libraries/)*


## Running a comparison

  1. Enter the directory of a model (e.g. FNG)
  2. Run `python run.py` to run both versions of the geometry and compare the results

Use `python run.py -h` to see various options availble when performing the comparisons.
