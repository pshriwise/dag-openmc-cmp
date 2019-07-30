from pyne.material import Material
from pyne import nucname

def matlib_entry(mat):
    matlib = "mat_{0}    {1: 1.6E}    {2}\n".format(mat.metadata['mat_number'], mat.density, len(mat.comp)) 
    for nuc, comp in mat.comp.iteritems(): 
        matlib += "{0}    {1: 1.6E} {2}\n".format(nucname.alara(nuc), comp*100.0, nucname.znum(nuc)) 
    matlib += "\n"
    return matlib

s = "#This matlib created with make_matlib.py\n\n"
###############################################################################
# Mat 1
# "Stainless steel (SS316) for all layers except n.15"
# density is 7.954 g/cm .
# make a nucvec out of all the composition given WITHOUT < signs:
nucvec = {
 "Fe": 68.32,
 "Cr": 16.8,
 "Ni": 10.7,
 "Mo": 2.12, 
 "Mn": 1.14, 
 "Si": 0.45, 
 "Cu": 0.09, 
 "Co": 0.14, 
 "V" : 0.16,    
 "C" : 0.04,    
 "P" : 0.022,  
 "Sn": 0.004, 
 "Pb": 0.001, 
 "B" : 0.0035
}
# The remaining nuclide is "<0.006 S", using 0.006 brings the percentage to 99.9965, 
# which is negligibly close to 100
nucvec["S"] = 0.006
# now make a material object and print an alara definition:
mat = Material(nucvec=nucvec, density=7.954, metadata={'mat_number':1})
s += matlib_entry(mat)

###############################################################################
# Mat 10
# Stainless steel (SS316) for layer n.15 - It is a AISI 316 type steel; 
# the density is 7.954 g/cm 3 
nucvec = {
"Fe": 66.22,  
"Cr":  17.8,   
"Ni":  11.3,   
"Mo":  2.00,  
"Mn":  1.64,   
"Si":  0.69,   
"Cu":  0.09,   
"Co":  0.07,   
"V" :  0.12,   
"C" :  0.03,   
"P" : 0.021,  
"B" : 0.005
}
# These compositions add up to 99.986
# The remaining compositions:  < 0.01 Nb, < 0.01 S, < 0.01 Sn, < 0.01 Pb, 
# < 0.01 Ti, < 0.03 Zr, < 0.01 As, < 0.03 Al.
# These add up to 0.12
# 99.986 + 0.12 = 1.0106
nucvec["Nb"] = 0.01
nucvec["S"]  = 0.01 
nucvec["Sn"] = 0.01  
nucvec["Pb"] = 0.01  
nucvec["Ti"] = 0.01  
nucvec["Zr"] = 0.03  
nucvec["As"] = 0.01  
nucvec["Al"] = 0.03 
mat = Material(nucvec=nucvec, density=7.954, metadata={'mat_number':10})
s += matlib_entry(mat)

 
###############################################################################
# Mat 9
# Stainless steel (SS316) for box - It is a AISI 316 type steel; 
# the density is 7.954 g/cm 3 . 
nucvec={
"Fe": 68.102,  
"Cr": 17.2,    
"Ni": 9.9,     
"Mo": 2.07,   
"Mn": 1.58,    
"Si": 0.49,    
"Cu": 0.42,    
"Nb": 0.011,   
"Co": 0.10,    
"V" : 0.04,    
"C" : 0.023,   
"P" : 0.039,  
"Sn": 0.016,   
"B" : 0.002
}   
# These add up to 99.993 
# The remaining are: < 0.01 S, < 0.01 Pb, < 0.005 Ti, 
# < 0.03 Zr, < 0.01 As, < 0.03 Al.
# which add up to 0.095.
# 99.993 + 0.095 = 100.088
nucvec["S"] = 0.01   
nucvec["Pb"] = 0.01   
nucvec["Ti"] = 0.005  
nucvec["Zr"] = 0.03   
nucvec["As"] = 0.01   
nucvec["Al"] = 0.03  
mat = Material(nucvec=nucvec, density=7.954, metadata={'mat_number':9})
s += matlib_entry(mat)

# Mat 5
# Perspex (Polymethylmetacrilate) - The composition is (C 5 O 2 H 8 ) n and the density is
# 1.18 g/cm . 
# (Si, S ~ 4 appm; Ca ~ 2.6 appm; Mg, Ti, Fe, K, Na, Al < 1 appm).

mat = Material(density=1.18, metadata={'mat_number':5})
nucvec={
"C" :5,
"O" :2,
"H" :8,
"Si":4E-6,
"S" :4E-6,
"Ca":2.6E-6,
"Mg":1E-6,
"Ti":1E-6,
"Fe":1E-6,
"K" :1E-6,
"Na":1E-6,
"Al":1E-6,
}
mat.from_atom_frac(nucvec) 
s += matlib_entry(mat)

# remaining materials

# void: a pseudovoid material
s +="""
mat_0     1.000000E-40    1
he:4     1.000000E+02    2
"""

# copper, from MCNP, the comp does not appear to be exactly natural,
s +="""
mat_3     8.940000E+00    2
cu:63     6.832743E+01    29
cu:65     3.167257E+01    29
"""

# water, from MCNP
s +="""
mat_2     1.000000E+00    2
h     1.116172E+01    1
o     8.883828E+01    8
"""

# Air DENSITY SET TO 0 becuase for proper source.F90 void rejection
s +="""
mat_4     1.000000E-40    2
n     7.659051E+01    7
o     2.340949E+01    8
"""

with open("matlib_actmats", 'w') as f:
    f.write(s)
