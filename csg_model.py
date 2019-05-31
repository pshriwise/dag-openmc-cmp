
import openmc
from functools import reduce
import operator

def create_openmc_geom():

    # make materials
    water = openmc.Material(name="water")
    water.add_nuclide('H1', 0.111915, 'wo')
    water.add_nuclide('O16', 0.888085, 'wo')
    water.set_density('g/cc', 1.0)

    air = openmc.Material(name="air")
    air.add_element('C', 0.000124, 'wo')
    air.add_element('N', 0.755268, 'wo')
    air.add_element('O', 0.231781, 'wo')
    air.add_element('Ar', 0.012827, 'wo')
    air.set_density('g/cc', 0.001205)

    lead = openmc.Material(name="lead")
    lead.add_element('Pb', 1.0, 'wo')
    lead.set_density('g/cc', 11.35)

    mats = openmc.Materials([water, air, lead])

    # bounding region
    xy_bounds = openmc.get_rectangular_prism(width=80.0,
                                             height=25.0)
    zneg_bound = openmc.ZPlane(z0=-2.5)
    zpos_bound = openmc.ZPlane(z0=2.5)

    bbox = xy_bounds & +zneg_bound & -zpos_bound

    # divisions in x
    x1 = openmc.XPlane(x0=-20.0)
    x2 = openmc.XPlane(x0=-10.0)
    x3 = openmc.XPlane(x0=0.0)

    # divisions in y
    y1 = openmc.YPlane(y0=-7.5)
    y2 = openmc.YPlane(y0=-2.5)
    y3 = openmc.YPlane(y0=2.5)
    y4 = openmc.YPlane(y0=7.5)

    cells = []

    # source block
    src_blk = openmc.Cell(cell_id=3, name="source_block")
    src_blk.region = -x1 & bbox
    cells.append(src_blk)

    # upper src shield
    shield1 = openmc.Cell(cell_id=5, name="upper source shield")
    shield1.region = +x1 & -x2 & +y3 & -y4 & bbox
    cells.append(shield1)

    # lower src shield
    shield2 = openmc.Cell(cell_id=7, name="lower source shield")
    shield2.region = +x1 & -x2 & +y1 & -y2 & bbox
    cells.append(shield2)

    # upper collimator block
    coll1 = openmc.Cell(cell_id=9, name="upper collimator block")
    coll1.region = +x3 & +y3 & bbox
    cells.append(coll1)
    # lower collimator block
    coll2 = openmc.Cell(cell_id=11, name="lower collimator block")
    coll2.region = +x3 & -y2 & bbox
    cells.append(coll2)

    # fill all cells up to this point with lead
    for cell in cells:
        cell.fill = lead

    # beamline
    beamline = openmc.Cell(cell_id=1, name="beamline air gap")
    # complement of all current cells and inside bbox
    regions = [~c.region for c in cells] + [bbox]
    beamline.region = reduce(operator.and_, regions)
    beamline.fill = air
    cells.append(beamline)

    # external water vol
    inner_box_prism = openmc.get_rectangular_prism(width=100.0,
                                                   height=100.0)
    inner_box_zneg = openmc.ZPlane(z0=-50.0)
    inner_box_zpos = openmc.ZPlane(z0=50.0)

    outer_box_prism = openmc.get_rectangular_prism(width=110.0,
                                                   height=110.0,
                                                   boundary_type='vacuum')
    outer_box_zneg = openmc.ZPlane(z0=-55.0, boundary_type='vacuum')
    outer_box_zpos = openmc.ZPlane(z0=55.0, boundary_type='vacuum')

    inner_box_region = inner_box_prism & +inner_box_zneg & -inner_box_zpos
    outer_box_region = outer_box_prism & +outer_box_zneg & -outer_box_zpos

    shell_region = ~inner_box_region & outer_box_region

    water_shell = openmc.Cell(cell_id=17, name="water shell")
    water_shell.region = shell_region
    water_shell.fill = water
    cells.append(water_shell)

    # space between collimator and water vol
    air_gap = openmc.Cell(cell_id=14)
    air_gap.region = ~bbox & inner_box_region
    air_gap.fill = air
    cells.append(air_gap)

    root_universe = openmc.Universe()
    root_universe.add_cells(cells)

    geom = openmc.Geometry(root_universe)

    return geom, mats
