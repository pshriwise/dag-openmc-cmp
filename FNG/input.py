import os
import sys
from argparse import ArgumentParser

import numpy as np
from shutil import copyfile
import openmc
import openmc.capi
from openmc.stats import Point, Box

def main(geom_type, nps, run=False, plot=False, vol_calc=False):
    #
    model = openmc.model.Model()

    # settings
    model.settings.batches = 5
    model.settings.inactive = 0
    model.settings.particles = nps # particle per batch
    model.settings.run_mode = 'fixed source'
    model.settings.output = {'tallies':True, 'summary':True}
    model.settings.survival_biasing = True
    model.settings.sourcepoint = {'write':False}


    # iso-mesh source covers the geometry
    source = openmc.Source()
    source.space = Box(lower_left=(-49.5, 5.6, -49.2), upper_right=(49.5, 77.43, 49.2))
    source.energy = openmc.stats.Discrete([14.0*1e6], [1.0])
    model.settings.source = source

    # choose geometry and material file to use
    dag_file = os.path.join(os.getcwd(),"..", "geometry", "dagopenmc", "dagmc.h5m")
    geom_file = os.path.join(os.getcwd(), "..", "geometry", "openmc", "geometry.xml")
    mat_file = os.path.join(os.getcwd(), "..", "geometry", "openmc", "materials.xml")
    if geom_type == 'dagmc':
        # set model.settings.dagmc to be true, the default 'dagmc.h5m' will be used
        model.settings.dagmc = True
        copyfile(dag_file, "dagmc.h5m")
    elif geom_type == 'csg':
        copyfile(geom_file, "geometry.xml")
        copyfile(mat_file, "materials.xml")

    # stochastic volume calculation
    if vol_calc:
        model.settings.run_mode = 'volume'
        lower_left = (-50.0, 0.0, -50.0)
        upper_right = (50.0, 100, 50.0)
        cells = [ cell for cell in geom.root_universe.cells.values() ]
        volume_calc = openmc.VolumeCalculation(cells, 10000000, lower_left, upper_right)
        model.settings.volume_calculations = [volume_calc]

    model.settings.export_to_xml(path='settings.xml')

    # tally
    tally = openmc.Tally()
    # define score
    tally.scores = ['flux']
    tally.tally_id = 1
    # define filter
    # 175 energy group, energy unit of openmc is eV
    energy_bins = np.array([0.0,
                            1.00001E-007, 4.13994E-007, 5.31579E-007, 6.82560E-007, 8.76425E-007,
                            1.12535E-006, 1.44498E-006, 1.85539E-006, 2.38237E-006, 3.05902E-006,
                            3.92786E-006, 5.04348E-006, 6.47595E-006, 8.31529E-006, 1.06770E-005,
                            1.37096E-005, 1.76035E-005, 2.26033E-005, 2.90232E-005, 3.72665E-005,
                            4.78512E-005, 6.14421E-005, 7.88932E-005, 1.01301E-004, 1.30073E-004,
                            1.67017E-004, 2.14454E-004, 2.75364E-004, 3.53575E-004, 4.53999E-004,
                            5.82947E-004, 7.48518E-004, 9.61117E-004, 1.23410E-003, 1.58461E-003,
                            2.03468E-003, 2.24867E-003, 2.48517E-003, 2.61259E-003, 2.74654E-003,
                            3.03539E-003, 3.35463E-003, 3.70744E-003, 4.30742E-003, 5.53084E-003,
                            7.10174E-003, 9.11882E-003, 1.05946E-002, 1.17088E-002, 1.50344E-002,
                            1.93045E-002, 2.18749E-002, 2.35786E-002, 2.41755E-002, 2.47875E-002,
                            2.60584E-002, 2.70001E-002, 2.85011E-002, 3.18278E-002, 3.43067E-002,
                            4.08677E-002, 4.63092E-002, 5.24752E-002, 5.65622E-002, 6.73795E-002,
                            7.20245E-002, 7.94987E-002, 8.25034E-002, 8.65170E-002, 9.80365E-002,
                            1.11090E-001, 1.16786E-001, 1.22773E-001, 1.29068E-001, 1.35686E-001,
                            1.42642E-001, 1.49956E-001, 1.57644E-001, 1.65727E-001, 1.74224E-001,
                            1.83156E-001, 1.92547E-001, 2.02419E-001, 2.12797E-001, 2.23708E-001,
                            2.35177E-001, 2.47235E-001, 2.73237E-001, 2.87246E-001, 2.94518E-001,
                            2.97211E-001, 2.98491E-001, 3.01974E-001, 3.33733E-001, 3.68832E-001,
                            3.87742E-001, 4.07622E-001, 4.50492E-001, 4.97871E-001, 5.23397E-001,
                            5.50232E-001, 5.78443E-001, 6.08101E-001, 6.39279E-001, 6.72055E-001,
                            7.06512E-001, 7.42736E-001, 7.80817E-001, 8.20850E-001, 8.62936E-001,
                            9.07180E-001, 9.61672E-001, 1.00259E+000, 1.10803E+000, 1.16484E+000,
                            1.22456E+000, 1.28735E+000, 1.35335E+000, 1.42274E+000, 1.49569E+000,
                            1.57237E+000, 1.65299E+000, 1.73774E+000, 1.82684E+000, 1.92050E+000,
                            2.01897E+000, 2.12248E+000, 2.23130E+000, 2.30693E+000, 2.34570E+000,
                            2.36533E+000, 2.38513E+000, 2.46597E+000, 2.59240E+000, 2.72532E+000,
                            2.86505E+000, 3.01194E+000, 3.16637E+000, 3.32871E+000, 3.67879E+000,
                            4.06570E+000, 4.49329E+000, 4.72367E+000, 4.96585E+000, 5.22046E+000,
                            5.48812E+000, 5.76950E+000, 6.06531E+000, 6.37628E+000, 6.59241E+000,
                            6.70320E+000, 7.04688E+000, 7.40818E+000, 7.78801E+000, 8.18731E+000,
                            8.60708E+000, 9.04837E+000, 9.51229E+000, 1.00000E+001, 1.05127E+001,
                            1.10517E+001, 1.16183E+001, 1.22140E+001, 1.25232E+001, 1.28403E+001,
                            1.34986E+001, 1.38403E+001, 1.41907E+001, 1.45499E+001, 1.49182E+001,
                            1.56831E+001, 1.64872E+001, 1.69046E+001, 1.73325E+001, 1.96403E+001])
    energy_filter = openmc.EnergyFilter(energy_bins*1e6)
    tally.filters.append(energy_filter)
    # mesh filter
    mesh = openmc.Mesh(mesh_id=1, name="n_flux")
    mesh.dimension= [10, 10, 10]
    mesh.lower_left = (-49.5, 5.6, -49.2)
    mesh.upper_right = (49.5, 77.43, 49.2)
    mesh_filter = openmc.MeshFilter(mesh)
    tally.filters.append(mesh_filter)
    # add the tally to tallies list
    tallies = openmc.Tallies()
    tallies.append(tally)
    tallies.export_to_xml()


    # plot 2D slice
    if plot:
        plot1 = openmc.Plot()
        plot1.basis = 'yz'
        plot1.origin = (0, 30, 0)
        plot1.width = (130, 130)
        plot1.color_by = 'material'
        plot1.pixels = (390, 390)
        plots = openmc.Plots()
        plots.append(plot1)
        plots.export_to_xml()


    # run the problem
    #openmc.run(mpi_args=['mpiexec', '-n', '4'])
    if run:
        openmc.run()

    # plot the 2D slice
    if plot:
        openmc.plot_geometry(output=True, openmc_exec='openmc', cwd='.')

if __name__ == "__main__":

    ap = ArgumentParser("DAG-OpenMC to DAG-MCNP comparison geometry")

    ap.add_argument('-g', '--geom', type=str, default="dagmc",
                    choices=('dagmc', 'csg'),
                    help="Geometry type to use")

    ap.add_argument('-r', '--run', default=False, action='store_true',
                    help="If present, run OpenMC after creating the model")

    ap.add_argument('-n', '--nps', type=int, default=10000,
                    help="Number of particles per batch (5 batches)")

    ap.add_argument('-v', '--vol', default=False, action="store_true",
                     help="Perform a volume calculation")

    ap.add_argument('-p', '--plot', default=False, action="store_true",
                     help="Plot the 2D slice of geometry")

    args = ap.parse_args()

    main(args.geom, args.nps, args.run, args.plot, args.vol)
