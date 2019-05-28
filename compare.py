#!/usr/bin/env python3

import os
import numpy as np
from math import*
import openmc
import matplotlib.pyplot as plt

num_e_groups = 175
num_ves = 80
e_bins =np.array(
        [1.00001E-007, 4.13994E-007, 5.31579E-007, 6.82560E-007, 8.76425E-007,
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
 
 
def square_rooted(x):
 
    return sqrt(sum([a*a for a in x]))
 
def cosine_similarity(x,y):
    numerator = sum(a*b for a,b in zip(x,y))
    denominator = square_rooted(x)*square_rooted(y)
    return numerator/float(denominator)

def mean_squared_error(x, y):
    if len(x) != len(y):
        raise ValueError("arrays have different length")
    numerator = sum((a-b)*(a-b) for a, b in zip(x, y))
    denominator = len(x)
    return numerator/denominator

def plot_n_flux(n_flux1, n_flux2, x_bins=e_bins, label1='DagOpenMC', label2='OpenMC',
        figname='n_flux.png', figtitle="Compare of DagOpenMC and OpenMC"):
    """
    Plot the neutron flux for a single mesh element.
    """
    ax1 = plt.subplot(211)
    plt.title(figtitle)
    plt.step(x_bins, n_flux1, label=label1, where='mid', color='r')
    plt.step(x_bins, n_flux2, label=label2, where='mid', color='g')
    ax1.set_xscale('log')
    ax1.set_yscale('log')
    ax1.set_ylabel('neutron flux ')
    plt.legend()
    ax2 = plt.subplot(212, sharex=ax1)
    plt.step(x_bins, np.divide(n_flux1-n_flux2, n_flux2),
            label=''.join([label1, '/', label2]), color='orange')
    ax2.set_xscale('log')
    ax2.set_yscale('linear')
    ax2.set_xlabel('Energy (MeV)')
    ax2.set_ylabel('DagOpenMC/OpenMC')
    plt.legend()
    plt.savefig(figname, dpi=300)
    plt.close()

def histogram_indicator(indicator, label='', figname="similarity.png"):
    plt.hist(indicator, bins=20)
    plt.xlabel(label)
    plt.savefig(figname, dpi=300)
    plt.close()


def remove_nan(n_flux):
    """
    Set nan to zero.
    """
    for v in range(n_flux.shape[0]):
        for e in range(n_flux.shape[1]):
            if isnan(n_flux[v][e]):
                n_flux[v][e] = 0.0
    return n_flux

def n_flux_difference_analysis(n_flux1, n_flux2, item='flux'):
    """
    n_flux1 is the dagopenmc,
    n_flux2 is the openmc
    item could be 'flux' or 'flux_rel_err'
    """
    # draw flux compare of first and last mesh element
    for i in range(num_ves):
        figname = ''.join(['n_', item, '_m', str(i), '.png'])
        figtitle = ''.join(["Comparison of neutron ",  item, " of mesh ", str(i)])
        plot_n_flux(n_flux1[i], n_flux2[i], figname=figname, figtitle=figtitle)
    if item == 'flux':
        # calculate cosine similarity for each volume element
        similarities = np.zeros(num_ves)
        mse = np.zeros(num_ves)
        for i in range(num_ves):
            similarities[i] = cosine_similarity(n_flux1[i],
                    n_flux2[i])
            mse[i] = mean_squared_error(n_flux1[i], n_flux2[i])
        # 
        histogram_indicator(similarities, label='Cosine similarity')
        histogram_indicator(mse, label='Mesn squared error', figname="mse.png")


def get_flux_error_from_sp(filename, num_e_groups, num_ves):
    sp = openmc.StatePoint(filename)
    tally = sp.get_tally(scores=['flux'])
    flux = np.reshape(tally.mean[:], newshape=(num_e_groups, num_ves)).transpose()
    flux = remove_nan(flux)
    std_dev = np.reshape(tally.std_dev[:], newshape=(num_e_groups, num_ves)).transpose()
    std_dev = remove_nan(std_dev)
    # calculate rel_err
    rel_err = np.zeros_like(std_dev)
    for i in range(std_dev.shape[0]):
        for j in range(std_dev.shape[1]):
            if flux[i][j] > 0:
                rel_err[i][j] = std_dev[i][j] / flux[i][j]
    return flux, rel_err

if __name__ == '__main__':
    # read dagopenmc sp
    dagopenmc_sp_filename = os.path.join(os.getcwd(), "dagopenmc_run", "statepoint.5.h5")
    n_flux_dagopenmc, n_flux_rel_err_dagopenmc = get_flux_error_from_sp(dagopenmc_sp_filename, num_e_groups, num_ves)
    # read openmc sp, reference.
    openmc_sp_filename = os.path.join(os.getcwd(), "openmc_run", "statepoint.5.h5")
    n_flux_openmc, n_flux_rel_err_openmc = get_flux_error_from_sp(openmc_sp_filename, num_e_groups, num_ves)
    # compare n_flux
    n_flux_difference_analysis(n_flux_dagopenmc, n_flux_openmc)
    n_flux_difference_analysis(n_flux_rel_err_dagopenmc, n_flux_rel_err_openmc, item='flux_rel_err')
    # compare n_flux_total
    n_flux_total_dagopenmc = np.sum(n_flux_dagopenmc, axis=1)
    n_flux_total_openmc = np.sum(n_flux_openmc, axis=1)
    plot_n_flux(n_flux_total_dagopenmc, n_flux_total_openmc, x_bins=np.array(range(0, 80)),
        figname="n_flux_total.png", figtitle="Total neutron flux comparision")

     
    

