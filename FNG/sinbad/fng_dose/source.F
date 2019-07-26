!#ident source.F from deck so                                                   !/*so          1*/
      subroutine source                                                         !/*so          2*/
!        dummy subroutine.  aborts job if source subroutine is missing.         !/*so          3*/
!        if nsr=0, subroutine source must be furnished by the user.             !/*so          4*/
!        at entrance, a random set of uuu,vvv,www has been defined.  the        !/*so          5*/
!        following variables must be defined within the subroutine:             !/*so          6*/
!        xxx,yyy,zzz,icl,jsu,erg,wgt,tme and possibly ipt,uuu,vvv,www.          !/*so          7*/
!        subroutine srcdx may also be needed.                                   !/*so          8*/
      use global_data                                                           !/*sox         1*/
!                                                                               !/*sox         2*/
      implicit double precision (a-h,o-z)                                       !/*sox         3*/
      implicit integer(kindi) (i-n)                                             !/*sox         4*/
!                                                                               !/*sox         5*/
!                                                                               !/*so         10*/
!      call expirx(0,'source','you need a source subroutine.')                   !/*so         11*/
!      return                                                                    !/*so         12*/
!      end                                                                       !/*so         13*/
!                                                                       
! **********************************************************************
!                                                                       
!        this source is for D-T/D-D source anisotropy interaction       
!        includes ion s c a t t e r i n g                               
!        37 angles                                                      
!        38 energy points                                               
!        minimum beam energy  10 keV                                    
!        maximun energy 10 MeV (NOT BREAK-UP)                           
!        following variables must be defined within the subroutine:     
!        xxx,yyy,zzz,icl,jsu,erg,wgt,tme and possibly ipt,uuu,vvv,www.  
!        subroutine srcdx may also be needed.                           
!                                                                       
!        INPUT PARAMETERS ON RDUM CARD:                                 
!        rdum(1)=deuteron beam energy                                   
!        rdum(2)=Tritium/Titanium or Deuterium/Copper atomic ratio      
!        rdum(3)= x coordinate                                          
!        rdum(4)= y coordinate                                          
!        rdum(5)= z coordinate                                          
!        rdum(6)=deuteron beam width                                    
!        rdum(7)=flag, 1 for D-T reaction, 2 for D-D reaction           
!        rdum(8)=starting cell                                           
!
!        comments: rdum(4) should be positive
!                  D-D not tested! 
! **********************************************************************
!
      common /mario/ coslab(38,37),sinlab(38,37),pe1(38,37),dedx(38),
     &emin,eb,uione,vione,wione,etb,dsub,ean,iflag
      dimension th(38,37),coscm(38,37),sincm(38,37), 
     &xscman(38,37),xslaban(38,37),pe(38,37),
     &cdedx(38,2),tdedx(38,2),ed2c(38,2),
     &ed(38),
     &coeff(10,38,2), 
     &thcm(37),relj(37),p1(37),e1(37),
     &PL(11),
     &stoich(2),z(3),a2(3,3),ec(3,3),f(3,3),am(3)
      double precision m1,m2,ma,mb,m2v(2),mbv(2),m1m2,m2m1(3,3),q(2)
!    masses from NIST Reference on Constants, Units and Uncertainties Web site
      data ma/1875.612793d0/                                            
      data mbv/2808.920906d0,1875.612793d0/                             
      data m1/939.565346d0/                                             
      data m2v/3727.379109d0,2808.391383d0/                             
!    q-values from Q-value Calculator (QCalc) at NNDC Web site          
      data q/17.5892930d0,3.2688570d0/                                  
!     energy grid
      data npts1/38/
      data  ed/0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10, 
     &0.108,0.12,0.14,0.16,0.18,0.20,0.22,0.24,0.26,0.28,0.30,0.32, 
     &0.34,0.36,0.38,0.40,0.50,0.75,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0, 
     &9.0,10.0/
!     angular grid (angles calculated internally)
      data npts2/37/
!     Legendre coefficients from ENDF/B-VI.8 (MF=6,MT=50)
!     d-t
      data (((coeff(i,j,k),i=1,10),j=1,38),k=1,2)/ 
     &7.3277e-5,1.6082e-4,-7.4731e-7,-4.2038e-8,5.1108e-10, 
     &1.4578e-11,1.1512e-13,-7.9561e-15,1.0885e-14,-8.5751e-15, 
     &2.9841e-4,1.65360e-4,-8.80790e-7,-5.93130e-8,1.05720e-9, 
     &3.9690e-11,3.4502e-13,0.00000,8.4592e-15,-6.5403e-15, 
     &4.68050e-4,1.51900e-4,-8.58900e-7,-6.66850e-8,2.00150e-9, 
     &7.5034e-11,7.0314e-13,1.4751e-14,4.4103e-15,-3.9607e-15, 
     &6.11570e-4,1.25890e-4,-7.12170e-7,-6.47240e-8,3.35990e-9,  
     &1.1800e-10,1.1357e-12,3.8035e-14,6.9189e-16,-4.1733e-16, 
     &7.41860e-4,8.96880e-5,-4.54010e-7,-5.31940e-8,5.15960e-9, 
     &1.6755e-10,1.6002e-12,7.2114e-14,-3.5370e-15,2.9819e-15, 
     &8.65980e-4,4.46760e-5,-9.22400e-8,-3.13960e-8,7.47830e-9, 
     &2.2623e-10,2.1409e-12,1.2015e-13,-8.9366e-15,7.5123e-15, 
     &9.88170e-4,-8.17340e-6,3.68160e-7,1.73420e-9,1.04600e-8, 
     &3.0180e-10,2.9482e-12,1.8684e-13,-1.4010e-14,1.2206e-14, 
     &1.11110e-3,-6.81190e-5,9.24010e-7,4.75760e-8,1.43240e-8, 
     &4.0844e-10,4.4484e-12,2.8604e-13,-2.0152e-14,1.6856e-14, 
     &1.23650e-3,-1.34560e-4,1.57350e-6,1.07780e-7,1.93660e-8, 
     &5.6780e-10,7.3590e-12,4.3410e-13,-2.4795e-14,2.3273e-14, 
     &1.36560e-3,-2.07000e-4,2.31570e-6,1.84260e-7,2.59610e-8, 
     &8.0983e-10,1.2771e-11,6.6372e-13,-3.0193e-14,2.9470e-14, 
     &1.47200e-3,-2.68960e-4,2.97620e-6,2.58570e-7,3.26560e-8, 
     &1.08880e-9,1.9909e-11,9.3489e-13,-3.3857e-14,3.4184e-14, 
     &1.63730e-3,-3.68140e-4,4.07880e-6,3.94670e-7,4.56940e-8, 
     &1.70710e-9,3.7694e-11,1.5596e-12,-3.6738e-14,4.2148e-14, 
     &1.92960e-3,-5.48630e-4,6.21980e-6,6.97920e-7,7.80520e-8, 
     &3.52850e-9,9.7788e-11,3.5524e-12,-3.2391e-14,5.8519e-14, 
     &2.24350e-3,-7.46040e-4,8.75170e-6,1.11560e-6,1.28730e-7, 
     &6.86840e-9,2.2154e-10,7.6189e-12,-3.6236e-16,7.6244e-14, 
     &2.57880e-3,-9.58360e-4,1.16920e-5,1.67140e-6,2.04540e-7, 
     &1.24970e-8,4.4916e-10,1.5236e-11,8.0806e-14,9.9773e-14, 
     &2.93490e-3,-1.18380e-3,1.50610e-5,2.39040e-6,3.13330e-7, 
     &2.13680e-8,8.3424e-10,2.8519e-11,2.4686e-13,1.3054e-13, 
     &3.31090e-3,-1.42090e-3,1.88820e-5,3.29920e-6,4.63920e-7, 
     &3.46230e-8,1.44530e-9,5.0342e-11,5.4900e-13,1.7165e-13, 
     &3.70560e-3,-1.66820e-3,2.31780e-5,4.42550e-6,6.66040e-7, 
     &5.35950e-8,2.36710e-9,8.4466e-11,1.0562e-12,2.2951e-13, 
     &4.11790e-3,-1.92440e-3,2.79750e-5,5.79760e-6,9.30290e-7, 
     &7.98120e-8,3.70220e-9,1.3568e-10,1.8631e-12,3.1268e-13, 
     &4.5466e-3,-2.1884e-3,3.3298e-5,7.4450e-6,1.2681e-6, 
     &1.1500e-7,5.5719e-9,2.0991e-10,3.0904e-12,4.3089e-13, 
     &4.9907e-3,-2.45910e-3,3.91750e-5,9.39730e-6,1.69150e-6, 
     &1.6107e-7,8.11770e-9,3.1438e-10,4.8903e-12,6.0075e-13, 
     &5.4491e-3,-2.7356e-3,4.5633e-5,1.16850e-5,2.21340e-6, 
     &2.2013e-7,1.15020e-8,4.5772e-10,7.4552e-12,8.3958e-13, 
     &5.9207e-3,-3.0169e-3,5.2700e-5,1.43380e-5,2.84720e-6, 
     &2.9450e-7,1.59090e-8,6.5013e-10,1.1017e-11,1.1727e-12, 
     &6.4046e-3,-3.3021e-3,6.0403e-5,1.73890e-5,3.60710e-6, 
     &3.8668e-7,2.15470e-8,9.0345e-10,1.5855e-11,1.6295e-12, 
     &6.8998e-3,-3.5904e-3,6.8773e-5,2.08680e-5,4.50760e-6, 
     &4.9934e-7,2.86460e-8,1.2313e-9,2.23040e-11,2.2482e-12, 
     &7.4056e-3,-3.8811e-3,7.7838e-5,2.48060e-5,5.56390e-6, 
     &6.3537e-7,3.74610e-8,1.6494e-9,3.07550e-11,3.0730e-12, 
     &1.0066e-2,-5.3445e-3,1.3466e-4,5.24880e-5,1.37410e-5, 
     &1.7793e-6,1.18030e-7,5.7882e-9,1.21570e-10,1.2566e-11, 
     &1.7286e-2,-8.6696e-3,3.8250e-4,2.00770e-4,6.70220e-5,  
     &1.0742e-5,8.82290e-7,5.3030e-8,1.36820e-9,1.70200e-10, 
     &2.4742e-2,-1.0763e-2,8.3554e-4,5.06290e-4,1.95450e-4, 
     &3.6307e-5,3.47940e-6,2.4216e-7,7.21790e-9,1.05310e-9, 
     &5.1734e-2,4.23070e-3,5.7343e-3,3.87870e-3,2.02060e-3, 
     &5.3676e-4,7.65120e-5,7.5328e-6,3.16560e-7,7.12120e-8, 
     &7.3569e-2,6.05590e-2,1.5189e-2,9.43340e-3,6.51810e-3, 
     &2.1053e-3,3.91640e-4,4.6653e-5,2.38360e-6,7.22910e-7, 
     &9.5466e-2,1.26570e-1,2.4911e-2,1.34700e-2,1.37310e-2, 
     &4.8687e-3,1.10260e-3,1.5491e-4,9.30690e-6,3.53380e-6, 
     &1.1011e-1,1.54590e-1,3.1490e-2,1.78230e-2,2.42140e-2, 
     &8.8437e-3,2.24950e-3,3.9116e-4,2.83570e-5,1.25420e-5, 
     &1.1966e-1,1.45170e-1,3.1554e-2,2.92710e-2,3.85150e-2, 
     &1.4519e-2,3.92540e-3,8.6660e-4,7.65960e-5,3.81450e-5, 
     &1.1676e-1,1.30900e-1,2.1444e-2,4.42730e-2,5.33720e-2, 
     &2.1923e-2,6.18870e-3,1.7243e-3,1.83950e-4,1.03150e-4, 
     &1.0158e-1,1.31350e-1,8.7716e-3,5.61570e-2,6.39660e-2, 
     &3.0878e-2,8.77530e-3,3.0791e-3,6.46330e-4,3.05180e-4, 
     &9.0511e-2,1.32630e-1,-1.7248e-3,6.0773e-2,7.00810e-2, 
     &4.0266e-2,1.06340e-2,4.86180e-3,1.1046e-3,6.65610e-4, 
     &8.8311e-2,1.30800e-1,-1.1633e-2,6.1327e-2,7.39040e-2, 
     &5.0169e-2,1.21210e-2,6.80300e-3,1.5050e-3,1.06160e-3, 
!    d-d
     &0,4.5426e-2,0,7.0971e-5,0,3.5316e-9,0,0,0,0, 
     &0,5.2119e-2,0,1.2521e-4,0,9.5527e-9,0,0,0,0, 
     &0,5.8303e-2,0,1.8575e-4,0,1.9418e-8,0,0,0,0, 
     &0,6.4068e-2,0,2.5274e-4,0,3.3838e-8,0,0,0,0, 
     &0,6.9460e-2,0,3.2584e-4,0,5.3474e-8,0,0,0,0, 
     &0,7.4508e-2,0,4.0466e-4,0,7.8944e-8,0,0,0,0, 
     &0,7.9237e-2,0,4.8880e-4,0,1.1082e-7,0,0,0,0, 
     &0,8.3670e-2,0,5.7785e-4,0,1.4965e-7,0,0,0,0, 
     &0,8.7829e-2,0,6.7147e-4,0,1.9592e-7,0,0,0,0, 
     &0,9.1731e-2,0,7.6933e-4,0,2.5013e-7,0,0,0,0, 
     &0,9.4337e-2,0,8.5637e-4,0,3.1499e-7,0,0,0,0, 
     &0,9.8247e-2,0,9.8692e-4,0,4.1229e-7,0,0,0,0, 
     &0,1.0476e-1,0,1.2045e-3,0,5.7444e-7,0,0,0,0, 
     &0,1.1046e-1,0,1.4371e-3,0,7.8882e-7,0,0,0,0, 
     &0,1.1534e-1,0,1.6846e-3,0,1.0554e-6,0,0,0,0, 
     &0,1.2022e-1,0,1.9322e-3,0,1.3220e-6,0,0,0,0, 
     &0,1.2397e-1,0,2.2043e-3,0,1.7122e-6,0,0,0,0, 
     &0,1.2772e-1,0,2.4764e-3,0,2.1025e-6,0,0,0,0, 
     &0,1.3107e-1,0,2.7595e-3,0,2.5642e-6,0,0,0,0, 
     &0,1.3402e-1,0,3.0534e-3,0,3.0973e-6,0,0,0,0, 
     &0,1.3697e-1,0,3.3473e-3,0,3.6305e-6,0,0,0,0, 
     &0,1.3934e-1,0,3.6616e-3,0,4.3265e-6,0,0,0,0, 
     &0,1.4171e-1,0,3.9759e-3,0,5.0224e-6,0,0,0,0, 
     &0,1.4386e-1,0,4.3001e-3,0,5.8102e-6,0,0,0,0, 
     &0,1.4580e-1,0,4.6342e-3,0,6.6899e-6,0,0,0,0, 
     &0,1.4774e-1,0,4.9682e-3,0,7.5696e-6,0,0,0,0, 
     &0,1.5513e-1,0,6.7831e-3,0,1.3570e-5,0,0,0,0, 
     &0,1.6587e-1,0,1.2154e-2,0,4.0835e-5,0,0,0,0, 
     &0,1.7109e-1,0,1.8687e-2,0,9.2837e-5,0,0,0,0, 
     &0,1.7641e-1,0,5.4104e-2,0,7.7734e-4,0,0,0,0, 
     &0,1.8281e-1,0,9.6315e-2,0,2.8960e-3,0,0,0,0, 
     &0,1.9856e-1,0,1.3677e-1,0,7.4280e-3,0,0,0,0, 
     &0,2.2182e-1,0,1.7078e-1,0,1.5224e-2,0,0,0,0, 
     &0,2.4748e-1,0,1.9501e-1,0,2.6667e-2,0,0,0,0, 
     &0,2.6829e-1,0,2.0589e-1,0,4.1448e-2,0,0,0,0, 
     &0,2.7499e-1,0,1.9917e-1,0,5.8470e-2,0,0,0,0, 
     &0,2.5869e-1,0,1.7305e-1,0,7.5678e-2,0,0,0,0, 
     &0,2.1701e-1,0,1.3419e-1,0,9.0279e-2,0,0,0,0/
!    Stopping force from SRIM2006 (kev/mg/cm)
!    d-ti
      data ((cdedx(i,j),i=1,38),j=1,2)/ 
     &143.4,195.8,234.2,264.5,289.1,309.4,326.2,340.1, 
     &351.7,361.1,368.8,375.0,383.5,387.9,389.3,388.4,385.5, 
     &381.5,376.6,371.1,365.2,358.7,352.2,345.7,339.1, 
     &332.5,302.0,244.8,207.6,139.9,109.2,91.0,78.7,69.7,62.8,
     &57.3,52.7,49.0, 
!    d-ti also for Ti-D target, otherwise d-cu below 
     &143.4,195.8,234.2,264.5,289.1,309.4,326.2,340.1, 
     &351.7,361.1,368.8,375.0,383.5,387.9,389.3,388.4,385.5, 
     &381.5,376.6,371.1,365.2,358.7,352.2,345.7,339.1, 
     &332.5,302.0,244.8,207.6,139.9,109.2,91.0,78.7,69.7,62.8,
     &57.3,52.7,49.0/
!    d-cu
!     &86.403,115.32,135.72,150.89, 162.8,172.28,180.11,186.58,192.07,196.67, 
!     &199.81,204.03,209.32,213.13,215.86, 217.6,218.59,219.05,219.09,218.78, 
!     &218.11,217.21,216.13,214.91,213.57,212.11,203.94, 182.9,164.69,118.91, 
!     &94.497, 79.76, 69.56,61.992,56.097,51.353, 47.45,44.167/
!    d-t
      data ((tdedx(i,j),i=1,38),j=1,2)/ 
     &516.7,707.4,841.0,940.1,1014.2,1069.9,1109.6,1137.5,1155.3, 
     &1165.2,1167.1,1165.0,1147.9,1118.8,1084.7,1046.7,1007.6,968.8,
     &930.9, 
     &894.3,859.3,826.6,795.7,766.5,739.1,713.4,607.5,447.8,357.8,
     &212.9, 
     &170.1,134.8,111.6,95.5,83.7,74.7,67.6,61.8, 
!    d-d
     &779.1,1062,1261.2,1409.7,1520.7,1603.1,1663.6,1705.2,1730.9,
     &1745.6, 
     &1748.7,1745.3,1719,1676.8,1624.6,1567.5,1508.9,1451,1394.6,1339.9, 
     &1287,1238.2,1191.7,1148.1,1107.2,1068.8,909.86,670.66,535.86,
     &318.89,  
     &254.63, 201.9,167.18,143.07,125.36,111.85,101.15,92.495/   
!    Integral xs from ENDF/B-VI.8 (MF=3, MT=50)
!    d-t
      data ((ed2c(i,j),i=1,38),j=1,2)/ 
     &1.73260e-3,5.96780e-2,0.28096,0.71636,1.3702,2.2021, 
     &3.1134,3.9571, 
     &4.590200,4.937,5.0142,4.8769,4.3027,3.6475,3.0723,2.6051,2.2332, 
     &1.9369,1.6985,1.5046,1.3448,1.2116,1.0994,1.0039,9.2182e-1, 
     &8.5074e-1,6.0485e-1,3.3778e-1,2.3158e-1,1.1320e-1,8.9992e-2, 
     &8.4461e-2,8.0582e-2,7.4348e-2,6.7979e-2,6.1234e-2,5.5680e-2, 
     &5.1372e-2, 
!    d-d
     &8.8216e-6,2.7734e-4,1.1747e-3,2.6785e-3,4.6105e-3,6.8050e-3, 
     &9.1418e-3,1.1541e-2,1.3950e-2,1.6339e-2,1.8124e-2,2.0801e-2, 
     &2.5264e-2,2.9419e-2,3.3266e-2,3.7113e-2,4.0411e-2,4.3709e-2, 
     &4.6779e-2,4.9621e-2,5.2463e-2,5.4925e-2,5.7387e-2,5.9688e-2, 
     &6.1829e-2,6.3970e-2,7.2709e-2,8.6567e-2,9.3566e-2,1.0425e-1, 
     &1.0388e-1,1.0046e-1,9.6621e-2,9.2854e-2,8.9473e-2,8.5752e-2, 
     &8.3193e-2,8.2559e-2/
      save
!
!     parameters setting                                                
      if(fuzz.gt.0) go to 1111                                          
      eb=rdum(1)                                                        
      xt=rdum(2)                                                        
      xx=rdum(3)                                                        
      yy=rdum(4)                                                        
      zz=rdum(5)                                                        
      ry=rdum(6)                                                        
      iflag=rdum(7)
      alicl=rdum(8)
      emin=0.01                                                         
      tmin=5.0                                                          
      mb=mbv(iflag)
      m2=m2v(iflag)
      do 50 j=1,npts1
      do 50 n=1,npts2
      th(j,n)=0.0
      coscm(j,n)=0.0
      sincm(j,n)=0.0
      coslab(j,n)=0.0
      sinlab(j,n)=0.0
      xscman(j,n)=0.0
      xslaban(j,n)=0.0      
      pe(j,n)=0.0
      pe1(j,n)=0.0
   50 continue
!        t target                                                    
      if(iflag.eq.1) then
      z(1)=1.                                                           
      z(2)=22.                                                          
      z(3)=1.                                                           
      am(1)=2.0141010d0  
      am(2)=47.867d0                                                   
      am(3)=3.016049d0                                                
      rhod=4.51d0                                                       
      else
!      d target (same as above)
      z(1)=1.
      z(2)=22.
!     z(2)=29.
      z(3)=1.
      am(1)=2.0141010d0
      am(2)=47.867d0 
!      am(2)=63.54
      am(3)=2.0141010d0
      rhod=4.51d0
!      rhod=8.96
      endif
      stoich(1)=1./(1+xt)                                               
      stoich(2)=xt/(1+xt)                                               
      ztm=z(2)*stoich(1)+z(3)*stoich(2)                                 
      atm=am(2)*stoich(1)+am(3)*stoich(2)                               
      atmrho=rhod*.6022/atm                                             
      atdist=atmrho**(-1./3.)                                           
      m1m2=am(1)/atm                                                    
!     screeening length of zbl potenzial                                
      ffpa=.5292*.8853/(z(1)**.23+ztm**.23)                             
      ffpf=ffpa*atm/(z(1)*ztm*14.4*(am(1)+atm))                         
      epsdg=tmin*ffpf*(1+m1m2)**2/(4.*m1m2)                             
!     bohr straggling of de/dx. number "12" normalizes dist.            
      stbohr=117.*z(1)*sqrt(ztm*atmrho)                                 
!     atom-atom scattering parameter                                    
      do 10 j=1,3                                                       
      do 10 i=2,3                                                       
      m2m1(j,i)=am(j)/am(i)                                             
      ec(j,i)=4.*m2m1(j,i)/(1+m2m1(j,i))**2                             
      a2(j,i)=.5292*.8853/(z(j)**.23+z(i)**.23)                         
  10  f(j,i)=a2(j,i)/(z(j)*z(i)*14.41*(1+m2m1(j,i)))                    
      epeak=1.0e5*z(1)**.67*am(1)                                       
!     dedx in target                                                    
      do 100 i=1,npts1                                                  
 100  dedx(i)=rhod*0.01*(cdedx(i,iflag)*am(2)/(am(2)+am(3)*xt)+ 
     &tdedx(i,iflag)*am(3)*xt/(am(2)+am(3)*xt))                        
!     ****************
      do 501 j=1,npts1 
      ea=ma+ed(j)
      pa=sqrt(ea*ea-ma*ma)
!     angles
      thl=-5
      do 500 n=1,npts2
      thl=thl+5
      th(j,n)=thl*pie/180.
      thcm(n)=thl*pie/180.
      coscm(j,n)=cos(th(j,n))
      sincm(j,n)=sqrt(1.-coscm(j,n)*coscm(j,n))
!     Legendre polynomials
      PL(1)=1.
      PL(2)=coscm(j,n)
      DO 20 L=2,10
      PL(L+1)=( PL(L)*coscm(j,n)*(2*L-1) - PL(L-1)*(L-1) )/L
   20 continue      
!     centre of mass xs
      xscman(j,n)=0.5*PL(1)+(3./2.)*coeff(1,j,iflag)*PL(2)+ 
     &(5./2.)*coeff(2,j,iflag)*PL(3)+(7./2.)*coeff(3,j,iflag)*PL(4)+ 
     &(9./2.)*coeff(4,j,iflag)*PL(5)+(11./2.)*coeff(5,j,iflag)*PL(6)+ 
     &(13./2.)*coeff(6,j,iflag)*PL(7)+(15./2.)*coeff(7,j,iflag)*PL(8)+ 
     &(17./2.)*coeff(8,j,iflag)*PL(9)+(19./2.)*coeff(9,j,iflag)*PL(10)+ 
     &(21./2.)*coeff(10,j,iflag)*PL(11)    
!    lab xs (relativistic) 
      s=ma*ma+mb*mb+2.*mb*ea
      rlambda=(s-(ma+mb)*(ma+mb))*(s-(ma-mb)*(ma-mb))
      rlambda1=(s-m1*m1-m2*m2)*(s-m1*m1-m2*m2)-4.*m1*m1*m2*m2
      betacm=sqrt(rlambda)/(s-ma*ma+mb*mb)
      beta1=sqrt(rlambda1)/(s+m1*m1-m2*m2)
      rg1=betacm/beta1
      gammacm=(s-ma*ma+mb*mb)/(2*mb*sqrt(s))
      theta1=atan(sincm(j,n)/(gammacm*(coscm(j,n)+rg1)))
      if(theta1.lt.0.) theta1=theta1+pie
      coslab(j,n)=cos(theta1)
      sinlab(j,n)=sqrt(1.-(cos(theta1))**2.)
      p1t1=((ea+mb)**2.-(pa**2.)*coslab(j,n)**2.)**(-1.)
      p1t2=(pa*coslab(j,n))*(mb*ea+0.5*(ma**2.+mb**2.+m1**2.-m2**2.))
      p1t3=(ea+mb)*((mb*ea+0.5*(ma**2.+mb**2.-m1**2.-m2**2.))**2.- 
     &m1**2.*m2**2.-m1**2.*pa**2.*sinlab(j,n)**2.)**0.5
      p1(n)=p1t1*(p1t2+p1t3)
      e1t1= ((ea+mb)**2.-(pa**2.)*(coslab(j,n)**2.))**(-1.)
      e1t2=(ea+mb)*(mb*ea+0.5*(ma**2.+mb**2.+m1**2.-m2**2.))
      e1t3=pa*coslab(j,n)*((mb*ea+0.5* 
     &(ma**2.+mb**2.-m1**2.-m2**2.))**2.- 
     &m1**2.*m2**2.-m1**2.*pa**2.*sinlab(j,n)**2.)**0.5
      e1(n)=e1t1*(e1t2+e1t3)      
      relj(n)=p1(n)**2./(mb*pa*((ea+mb)*p1(n)- 
     &pa*e1(n)*coslab(j,n)))
      xslaban(j,n)=xscman(j,n)*relj(n)
 500  continue 
 501  continue
      calb=1000/(2*pie)
      pemax=0.  
      do 251 i=1,npts1 
      do 251 j=1,npts2                                                  
      pe(i,j)=calb*xscman(i,j)*ed2c(i,iflag)/dedx(i)                    
      pe1(i,j)=calb*xslaban(i,j)*ed2c(i,iflag)
  251 if(pe(i,j).gt.pemax) pemax=pe(i,j)
      fuzz=1.d-7                                                        
      ebb=eb*1.d6                                                       
      emev=0.                                                           
 1111 if(emev.gt.emin) goto 290                                         
!  monte carlo ion transport                                            
 1112 irtt=50                                                           
      e=ebb                                                             
      distalb=0.
      distx=0.
      disty=0.
      distz=0.
      uione1=0.                                                         
      vione1=1.                                                         
      wione1=0.                                                         
  290 r1=rang()                                                         
      r3=rang()                                                         
      r4=rang()                                                         
      r5=rang()
      eold=e                                                            
      uione=uione1                                                      
      vione=vione1                                                      
      wione=wione1                                                      
!     free flight path                                                  
        eps=e*ffpf                                                      
        eeg=sqrt(eps*epsdg)                                             
        pmax1=ffpa/(eeg+sqrt(eeg)+.125*eeg**.1)                         
        p=pmax1*sqrt(r1)                                                
        ffpath=1./(pie*pmax1*pmax1*atmrho)
      ein=e/1.0d6                                                       
      call interp(ed,dedx,npts1,ein,exout)                              
      dee=exout*ffpath                                                  
!     bohr straggling                    
      stdee=(r3-.5)*stbohr*sqrt(ffpath)                                 
      if(e.le.epeak)  stdee=stdee*e/epeak                               
      if((dee+stdee).gt.0)  e=e-dee-stdee                               
      if(e.gt.ebb)e=ebb-1.0d-5                                          
!     collision atom
      j=2                                                               
      r4=r4-stoich(j-1)                                                 
      if(r4.lt.0.) then                                                 
         eps=e*f(1,j)                                                   
         b=p/a2(1,j)                                                    
      else                                                              
         j=j+1                                                          
         eps=e*f(1,j)                                                   
         b=p/a2(1,j)                                                    
      end if                                                            
!     rutherford scattering
      if(eps.gt.10.) then                                               
          s2=1./(1.+(1.+b*(1.+b))*(2.*eps*b)**2)                    
          c2=1.-s2                                                  
      else                                                              
!     magic formula
         icycle=0                                                       
         r=b                                                            
         rr=-2.7*log(abs(eps*b)+fuzz)                                   
         if(rr.lt.b) goto 390                                           
         rr=-2.7*log(abs(eps*rr)+fuzz)                                  
         if(rr.lt.b) goto 390                                           
         r=rr
!     below is zbl universal int. potential               
 390     if(r.gt.200.)  r=200.                                          
         ex1=.18175*exp(-3.1998*r)                                      
         ex2=.50986*exp(-.94229*r)                                      
         ex3=.28022*exp(-.40290*r)                                      
         ex4=.02817*exp(-.20162*r)                                        
         v=(ex1+ex2+ex3+ex4)/r                                          
         v1=-(v+3.1998*ex1+.94229*ex2+.4029*ex3+.20162*ex4)/r           
         fr=b*b/r+v*r/eps-r                                             
         fr1=-b*b/(r*r)+(v+v1*r)/eps-1                                  
         qq=fr/fr1                                                      
         r=r-qq                                                         
         icycle=icycle+1                                                
         if(icycle.gt.10) goto 395                                      
         if(abs(qq/r).gt.0.0001) goto 390                               
 395     roc=-2*(eps-v)/v1                                              
         sqe=sqrt(eps)                                                  
         cc=(.011615+sqe)/(.0071222+sqe)                                
         aa=2*eps*(1.+(.99229/sqe))*b**c!                               
         ff=(sqrt(aa**2+1)-aa)*((9.3066+eps)/(14.813+eps))              
         delta=(r-b)*aa*ff/(ff+1.)                                      
         c=(b+delta+roc)/(r+roc)                                        
         c2=c*c
         s2=1-c2                                                        
      end if                                                            
      ct=2*c2-1.                                                        
      st=sqrt(1.0001-ct*ct)                                             
      cu=ct+m2m1(1,j)                                                   
      psi=atan(st/cu)                                                   
      if(psi.lt.0.) psi=psi+pie                                         
      deny=ec(1,j)*s2*e                                                 
      e=e-deny                                                          
      uzero=cos(psi)                                                    
      call rot(uzero,uione,vione,wione,uione1,vione1,wione1,irtt)       
      emev=e/1.d6                                                       
      if(emev.lt.emin) goto 1112                                        
      eoldme=eold/1.d6                                                  
      delta=eoldme-emev                                                 
      delte=rang()*delta                                                
      ep=emev+delte                                                     
!     thickness condition
      distalb=distalb+ffpath*vione
      distx=distx+ffpath*uione
      disty=disty+ffpath*vione
      distz=distz+ffpath*wione
      yyalb=yy*100000000.   
      if(distalb.gt.yyalb) then
      emev=0. 
      goto 1111        
      end if
      if(distalb.lt.0.) then
      emev=0.
      goto 1111        
      end if
!     reaction
      t2=2.*rang()-1.                           
      a1=acos(t2)                                                   
      call xinterp2D(ed,thcm,pe,npts1,npts2,ep,a1,pn)                      
!     rejection method
      pmax=rang()*pemax*1.00  
      if(pmax.gt.pn) go to 1111
      ctn=t2                                                            
      stn=sqrt(1.-ctn*ctn)                                              
!     relativistic kinematics
      ean=ma+ep
      pana=sqrt(ean*ean-ma*ma)
      s=ma*ma+mb*mb+2.*mb*ean
      rlambda=(s-(ma+mb)*(ma+mb))*(s-(ma-mb)*(ma-mb))
      rlambda1=(s-m1*m1-m2*m2)*(s-m1*m1-m2*m2)-4.*m1*m1*m2*m2
      betacm=sqrt(rlambda)/(s-ma*ma+mb*mb)
      beta1=sqrt(rlambda1)/(s+m1*m1-m2*m2)
      rg1=betacm/beta1
      gammacm=(s-ma*ma+mb*mb)/(2*mb*sqrt(s))
      ph=atan(stn/(gammacm*(ctn+rg1)))
      if(ph.lt.0.) ph=ph+pie
      uzero=cos(ph)
      if(ctn.eq.1.) uzero=1.
      if(ctn.eq.-1.) uzero=-1.            
      sinlab1=sqrt(1.-uzero*uzero)      
      t1n=((ean+mb)**2.-(pana**2.)*(uzero**2.))**(-1.)
      t2n=(ean+mb)*(mb*ean+0.5* 
     &(ma**2.+mb**2.+m1**2.-m2**2.))
      t3n=pana*uzero*((mb*ean+0.5* 
     &(ma**2.+mb**2.-m1**2.-m2**2.))**2.- 
     &m1**2.*m2**2.-m1**2.*pana**2.*sinlab1**2.)**0.5
      e1n=t1n*(t2n+t3n)
      eklab=e1n-m1
      en=eklab
      erg=en                                                            
      call rot(uzero,uione,vione,wione,uuu,vvv,www,irtt)                
      ry2=ry*ry                                                         
  123 x1=(ry-2*ry*rang())                               
      z1=(ry-2*ry*rang())                               
      rr=x1*x1+z1*z1                                    
      if(rr.gt.ry2) go to 123                           
      xxx=xx+x1                                         
      yyy=yy         
      zzz=zz+z1
!   SOURCE CELL                                                         
      icl=alicl
!   SOURCE CELL                                                         
      jsu=0                                                             
      ipt=1                                                             
      tme=0.0                                                           
      wgt=1.0                                                           
      return                                                            
      end 
      



