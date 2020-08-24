#! /usr/bin/env python
import os
from argparse import ArgumentParser

import openmc

from create_input import create_input
from compare import perform_comparison

def main(batches, particles, run, plot, vol, n_threads):

    # move to the new dir
    os.chdir("openmc_run")

    # create inputs for the run
    create_input("csg", batches, particles, plot, vol)

    if run:
        openmc.run(threads=n_threads)

    # move to the dagmc run dir
    os.chdir("../dagopenmc_run")

    # create inputs for the run
    create_input("dagmc", batches, particles, plot, vol)

    if run:
        openmc.run(threads=n_threads)

    os.chdir("..")

    tally_id = 1

    # move to results dir so images are placed there
    if not os.path.isdir("results"):
        os.mkdir("results")
    os.chdir("results")
    if run:
        model_name = os.path.abspath('.').split('/')[-1]
        perform_comparison(model_name,
                           "../openmc_run/statepoint.{}.h5".format(batches),
                           "../dagopenmc_run/statepoint.{}.h5".format(batches))


if __name__ == "__main__":

    ap = ArgumentParser("DAG-OpenMC to DAG-MCNP comparison geometry")

    ap.add_argument('-t', '--n-threads', type=int, default=1,
                    help="Number of threads used in the run")

    ap.add_argument('-r', '--run', default=True, action='store_false',
                    help="If present, run OpenMC after creating the model")

    ap.add_argument('-b', '--batches', type=int, default=10,
                    help="Number of batches to run")

    ap.add_argument('-n', '--particles', type=int, default=10000,
                    help="Number of particles per batch")

    ap.add_argument('-v', '--vol', default=False, action="store_true",
                     help="Perform a volume calculation")

    ap.add_argument('-p', '--plot', default=False, action="store_true",
                     help="Plot the 2D slice of geometry")

    args = ap.parse_args()

    main(args.batches, args.particles, args.run, args.plot, args.vol, args.n_threads)
