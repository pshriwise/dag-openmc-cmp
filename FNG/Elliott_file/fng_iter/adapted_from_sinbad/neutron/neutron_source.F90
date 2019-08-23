!+ $Id: source.F90,v 1.2 2006/10/03 02:10:16 mashnik Exp $
! Copyright LANS/LANL/DOE - see file COPYRIGHT_INFO
!
! fng source adapted by wilson,bohm
! notes:
! 1) The data and initialization section should be cleaned up and put into
!     modules following the f90 style
!

subroutine source
  ! dummy subroutine.  aborts job if source subroutine is missing.
  ! if nsr=0, subroutine source must be furnished by the user.
  ! at entrance, a random set of uuu,vvv,www has been defined.  the
  ! following variables must be defined within the subroutine:
  ! xxx,yyy,zzz,icl,jsu,erg,wgt,tme and possibly ipt,uuu,vvv,www.
  ! subroutine srcdx may also be needed.
  use mcnp_global
  use mcnp_debug
  
  implicit real(dknd) (a-h,o-z)
  integer :: srcCellNumber,srcCellIdx
  
  common /mario/ dedx(100),emin,eb,sml,uione,vione,wione,et,b,d,srcCellNumber,srcCellIdx
  ! **********************************************************************
  !              dati standard
  ! **********************************************************************
  real(dknd):: cdedx(100),sig(100),tdedx(100),pl(100),am(3),pe(100)    
  real(dknd):: ed(100),suml(3601),th(3601),stoich(2),z(3)              
  real(dknd):: a(3,3),ec(3,3),f(3,3)                                   
  real(dknd):: md,mt,mn,ma,m1m2,m2m1(3,3)
  data md/2.01410219/
  data mt/3.01602994/
  data mn/1.00866544/
  data ma/4.00260361/
  data aux/25.20734546/
  data q/17.589/
  !                                                                        
  !  i dati da dare in input direttamente in questo programma sono:        
  !     il carico di trizio nel titanio in atomi per atomo di titanio      
  !     l'energia dei deutoni-tritoni in mev,fino a 0.5 mev;
  !data to give direct input into this program are
  !load of tritium in titanium
  !Triton Energy of deuteron in MeV, up to 0.5 MeV               
  !  **********************************************************************
  !               dati presi dai giapponesi (data taken by the Japanese)
  !  **********************************************************************
  data npts/50/                                                     
  data  ed/0.010,0.020,0.030,0.040,                                       &
       0.050,0.060,0.070,0.080,0.090,0.100,0.110,0.120,0.130,0.140,      &
       0.150,0.160,0.170,0.180,0.190,0.200,0.210,0.220,0.230,0.240,0.250,&
       0.260,0.270,0.280,0.290,0.300,0.310,0.320,0.330,0.340,0.350,0.360,&
       0.370,0.380,0.390,0.400,0.410,0.420,0.430,0.440,0.450,0.460,0.470,&
       0.480,0.490,0.500,50*0.0/                                         
  !                                                                       
  !    sezione d'urto della reazione deuterio su trizio
  !    cross section of reaction on deuterium, tritium                   
  !                                                                       
  data  sig/1.0e-4,4.3e-3,0.0196,0.0529,0.106,0.175,0.250,0.315,               &
       0.367,0.394,0.399,0.387,0.367,0.339,0.317,0.286,0.262,0.236,0.215, &
       0.199,0.181,0.167,0.153,0.142,0.133,0.122,0.114,0.106,0.100,0.0952,&
       0.0911,0.0873,0.0838,0.0806,0.0775,0.0743,0.0713,0.0686,0.0659,    &
       0.0635,0.0611,0.0589,0.0568,0.0549,0.0530,0.0513,0.0498,0.0483,    &
       0.0469,0.0455,50*0.0/                                             
  !                                                                       
  !    perdita di energia del deuterio nel titanio e nel trizio
  !    energy loss of deuterium and tritium in titanium         
  !                                                                       
  data  cdedx/0.142,0.1929,0.2299,0.2592,0.2835,0.3038,0.3209,                &
       0.3354,0.3475,0.3577,0.3660,0.3728,0.3781,0.3821,0.3851,0.3870, &  
       0.3880,0.3883,0.3879,0.3869,0.3854,0.3834,0.3811,0.3785,0.3756, &  
       0.3725,0.3692,0.3658,0.3622,0.3586,0.3550,0.3513,0.3476,0.3439, &  
       0.3402,0.3365,0.3329,0.3293,0.3258,0.3223,0.3189,0.3155,0.3122, &  
       0.3089,0.3058,0.3027,0.2996,0.2966,0.2937,0.2909,50*0.0/          
  data  tdedx/0.5959,0.8014,0.9420,1.046,1.124,1.182,1.225,1.255,            &
       1.275,1.287,1.291,1.290,1.285,1.275,1.262,1.247,1.231,1.212,   &   
       1.193,1.173,1.152,1.132,1.111,1.090,1.069,1.048,1.028,1.008,   &   
       0.9886,0.9695,0.9509,0.9327,0.9150,0.8978,0.8811,0.8649,0.8491,&   
       0.8338,0.8190,0.8046,0.7906,0.7771,0.7640,0.7513,0.7390,0.7270,&   
       0.7155,0.7042,0.6934,0.6828,50*0.0/                               
  !                                                                       
  ! **********************************************************************
!
! force initialization of starting ion phase space (emev,x,y,z,u,v,w) 
!       each time the source subroutine called 
  emev=0.
! force initialization of ion transport data (stopping powers, etc.) 
!       each time the source subroutine is called
!
     ! initialize data
     emin=0.010
     ! energia del fascio in mev,fino a 0.5 mev;
     ! beam energy in MeV up to .5 MeV
     eb=rdum(1)                                                        
     ! carico di trizio 
     ! load tritium                                                     
     xt=rdum(2)                                                        
     ! coordinate sorgente la sorgente a disco deve essere centrata  intorno
     !          a queste coordinate  con la normale in direzione yyy
     ! coordinates the source to the source disc must be centered around
     ! these coordinates with the normal direction yyy 
     xx=rdum(3)
     yy=rdum(4)
     zz=rdum(5)
     ry=rdum(6)
     ! **********************************************************************
     fuzz=1.e-7
     !  mumero di chiamate alla sub rot
     !  number of calls to the sub rot
     irtt=50
     !              dati medi del bersaglio triziato
     ! average data of the target tritiated
     z(1)=1.
     am(1)=md
     z(2)=22.
     z(3)=1.
     am(2)=47.9
     am(3)=mt
     rhome=4.80
     stoich(1)=1./(1+xt)
     stoich(2)=xt/(1+xt)
     !           energia minima trasferita (ev)
     !           minimum energy transferred
     tmin=5.
     ztm=z(2)*stoich(1)+z(3)*stoich(2)
     atm=am(2)*stoich(1)+am(3)*stoich(2)
     atmrho=rhome*.6022/atm
     atdist=atmrho**(-1./3.)
     !     pmax=atdist/sqrt(pie)
     m1m2=am(1)/atm
     !     calcolo del mean free flight path parametri del bersaglio
     ! calculating the mean free flight path of the target parameters
     !         screeening length of zbl potenzial (unita ev-anstrong)
     ffpa=.5292*.8853/(z(1)**.23+ztm**.23)
     ffpf=ffpa*atm/(z(1)*ztm*14.4*(am(1)+atm))
     epsdg=tmin*ffpf*(1+m1m2)**2/(4.*m1m2)
     !     write(32,23)tmin,ffpf,m1m2,epsdg
     !23   format(3x,'tmin,ffpf,m1m2,epsdg ',4e12.5/)
     !            bohr straggling of de/dx. number "12" normalizes dist.
     stbohr=117.*z(1)*sqrt(ztm*atmrho)
     !              atom-atom scattering parameter
     do j=1,3
        do i=2,3
           m2m1(j,i)=am(j)/am(i)
           ec(j,i)=4.*m2m1(j,i)/(1+m2m1(j,i))**2
           a(j,i)=.5292*.8853/(z(j)**.23+z(i)**.23)
           f(j,i)=a(j,i)/(z(j)*z(i)*14.41*(1+m2m1(j,i)))
        end do
     end do
     !         peak of electronic stopping (usato per calcolo di straggling)
     !         used for calculation of straggling
     epeak=1.0e5*z(1)**.67*am(1)
     !   potere frenante del mezzo (mev/mg/cm2)
     !   stopping power of the medium                              
     do i=1,npts                                                   
        dedx(i)=cdedx(i)*48./(48.+3.*xt)+tdedx(i)*3.*xt/(48.+3.*xt)       
     end do
! only need to calculate srcCell and sml the first time (i.e. for nps=1)
     if (nps .le. 1) then
        srcCellNumber=idum(1)
        srcCellIdx=namchg(1,srcCellNumber)
        !     calcolo del valore totale della funzione di distribuzione sml
        !     calculation of the total value of the distribution function sml
        !     questo valore serve  per il psc
        !     This value is for the CSP
        thl=-0.05
        do l=1,3601
           thl=thl+0.05
           th(l)=thl*pie/180.
           cth=cos(th(l))
           do i=1,npts
              et=ed(i)+q
              b=md*mn*ed(i)/et/aux
              d=mt*ma/aux*(1.+md*q/(mt*et))
              e=sqrt(d/b-1.+cth*cth)
              en=et*b*(cth+e)**2
              g=sqrt(b*d)*e/(en/et)
              pl(i)=sig(i)/g/dedx(i)
           end do
           call xint(ed,pl,npts,emin,eb,xr)
           suml(l)=xr*sin(th(l))
        end do
        call xint(th,suml,3601,th(1),th(3601),sml)
        !     questo valore serve  per dare i pesi ai neutroni
        ! This value is used to give weight to neutrons
     end if
     !     
     do i=1,npts
        pe(i)=sig(i)/dedx(i)
     end do

     iparti=0
     ebb=eb*1000000.
  
  do while (emev .lt. emin .or. pmax .gt. pn)
     !         energia fascio in ev (beam energy in ev)
     
     if((emev*0.8).le.emin) then
!       sample a new ion starting location using beam parameters passed by rdum
!       (for nps >1 can't rely on these being initialized by above code
!        unless charged particle transport data is forced to be initialized
        eb=rdum(1)                                                        
        xt=rdum(2)                                                        
        xx=rdum(3)
        yy=rdum(4)
        zz=rdum(5)
        ry=rdum(6)
!       convert eb from MeV to eV
        ebb=eb*1000000.0
!
        ry2=ry*ry
        x1=(ry-2*ry*rang())
        z1=(ry-2*ry*rang())
        rr=x1*x1+z1*z1
        do while( rr.gt.ry2)
           x1=(ry-2*ry*rang())
           z1=(ry-2*ry*rang())
           rr=x1*x1+z1*z1
        end do
        
        xxx=xx+x1
        yyy=yy
        zzz=zz+z1
        xxxold=xxx
        yyyold=yyy
        zzzold=zzz
        
        !      inizio del montecarlo per lo ione che entra  con vione=1.
        !      beginning of the MonteCarlo for ion entering with vion=1
        e=ebb
        uione=0.
        vione=1.
        wione=0.
        !   utilizzo un modello zero-dimensionale
     end if
    
    
!   set icl to corresponding program cell number
    icl=srcCellIdx
    jsu=0
    ipt=1
    tme=0.0
    xxx=xxxold
    yyy=yyyold
    zzz=zzzold
    wgt=1.0
    r1=rang()
    r3=rang()
    r4=rang()
    eold=e
    iparti=iparti+1
    ! ********* free flight path ******************
    if(e.eq.ebb) then
       ! ********+ prima collissione (first collision)
       ffpath=atdist
       p=sqrt(r1/(pie*ffpath*atmrho))
    else
       eps=e*ffpf
       eeg=sqrt(eps*epsdg)
       pmax1=ffpa/(eeg+sqrt(eeg)+.125*eeg**.1)
       p=pmax1*sqrt(r1)
       ffpath=1./(pie*pmax1*pmax1*atmrho)
    end if

    !     energia persa per rallentamento elettronico
    !     energy lost to electronic deceleration
    ein=e/1000000.
    call interp(ed,dedx,npts,ein,exout)
    !     trasformazione energia in ev/amstrong
    dee=exout*ffpath*45.189
    ! ********+ add bohr straggling perche ioni leggeri (because light ions)
    stdee=(r3-.5)*stbohr*sqrt(ffpath)
    if(e.le.epeak)  stdee=stdee*e/epeak
    if((dee+stdee).gt.0)  e=e-dee-stdee
    if(e.gt.ebb)e=ebb-1.0e-5
    !     write(32,89) ffpath,pmax1,epsdg,ffpf
    ! 89  format(3x,'ffpath,pmax1,epsdg,ffpf',4e12.5/)
    !     trova l'atomo con cui collidere j e i valori epsilon e b
    !     is the atom with which collide jei epsilon values b
    j=2
    r4=r4-stoich(j-1)
    if(r4.lt.0.) then
       eps=e*f(1,j)
       b=p/a(1,j)
    else
       j=j+1
       eps=e*f(1,j)
       b=p/a(1,j)
    end if
    !         inizia il calcolo di scattering
    !         start calculating scattering
    !  se epsilon>10, usa rutherford scattering altrimenti la magic formula
    !  if epsilon> 10, use Rutherford scattering otherwise the magic formula
    if(eps.gt.10.) then
       !       notazioni: s2=sin(theta/2)**2, ct=cos(theta) st=sin(theta)
       s2=1./(1.+(1.+b*(1.+b))*(2.*eps*b)**2)
       c2=1.-s2
    else
       icycle=0
       r=b
       rr=-2.7*log(abs(eps*b)+fuzz)
       if (rr.ge.b) then
          rr=-2.7*log(abs(eps*rr)+fuzz)
          if(rr.ge.b) r=rr
       end if
       !****************** below is zbl universal int. potential
       qq = r
       do while (icycle.le.10 .and. abs(qq/r).gt.0.0001)
          if(r.gt.200.)  r=200.
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
       end do

       roc=-2*(eps-v)/v1
       sqe=sqrt(eps)
       cc=(.011615+sqe)/(.0071222+sqe)
       aa=2*eps*(1.+(.99229/sqe))*b**cc
       ff=(sqrt(aa**2+1)-aa)*((9.3066+eps)/(14.813+eps))
       delta=(r-b)*aa*ff/(ff+1.)
       c=(b+delta+roc)/(r+roc)
       !**************** fine della formula magica****************
       c2=c*c
       s2=1-c2
    end if

    !************ energia dello ione e traiettoria dopo la collisione
    !             ion energy and trajectory after collision
    ct=2*c2-1.
    st=sqrt(1.0001-ct*ct)
    cu=ct+m2m1(1,j)
    psi=atan(st/cu)
    if(psi.lt.0.) psi=psi+pie
    !******************** psi e l'angolo di deflessione nel laboratorio
    !                     psi and the angle of deflection in the laboratory
    dden=ec(1,j)*s2*e
    e=e-dden
    !+++++++++++++++++++calcolo nuova direzione ione
    !                 calculating new direction ion
    !----------  psi = nuclear scattering angle
    !                      coseno di ulab
    uzero=cos(psi)
    ! ****** formula manuale mcnp pag.65 (first call to rot)
    call rot(uzero,uione,vione,wione,uione1,vione1,wione1,irtt)
    uione=uione1
    vione=vione1
    wione=wione1
    !
    !
    ! ++******+++++ e corrisponde all' energia attuale dello ione
    !                    and corresponds to 'actual energy ion
    ! ******++++++ inizia il calcolo nucleare
    !              starts calculating nuclear
    emev=e/1000000.

    if(emev.ge.emin) then 
       eoldme=eold/1000000.
       call xint(ed,pe,npts,emev,eoldme,pn)
       ! ++++++++ probabilita  di fare un neutrone mediante reiezione
       !          chance of rejection by a neutron
       !         0.04 e il valore massimo aspettato che puo avere pn
       !         0.04 and the maximum value that can be expected pn
       pmax=rang()*0.04
    end if
 end do
 
 
 ! ****** angolo di uscita del neutrone rispetto allo ione nel c.m.
 !        departure angle of the neutron compared to the ion in c.m.
 ctn=uione1*uuu+vione1*vvv+wione1*www
 stn=sqrt(1.-ctn*ctn)
 delta=eoldme-emev
 delte=rang()*delta
 ep=emev+delte
 et=ep+q
 b=md*mn*ep/et/aux
 d=mt*ma/aux*(1.+md*q/(mt*et))
 !   trasforma la direzione nelle nuove coordinate
 !   changes direction in the new coordinates
 !         ph e l'angolo nel sistema del laboratorio  0 - pie
 ph=atan(stn/(ctn+sqrt(b/d)))
 if(ph.lt.0.) ph=ph+pie
 !               coseno di ulab
 uzero=cos(ph)
 ex=sqrt(d/b-1.+uzero*uzero)
 en=et*b*(uzero+ex)**2
! Write ion phase space if nps<51 (rather than iparti<51) 
 if(nps.lt.51) then
    write(32,98) nps,psi,emev
98  format('nps=',i6,1x,'angolo di scattering ',e12.5,' energia ione ',2e12.5)
    write(32,99) ph,uzero,ctn
99  format(1x,'ang. lab ',e12.5,' cos lb ',e12.5,' cos cm',e12.5)
    write(32,93) uione1,vione1,wione1
93  format(1x,'coseni dir. ione ',3(1x,e12.5))
 else
    continue
 end if
 !***********************************************************************
 erg=en
 ! ****** formula manuale mcnp pag.65 (second call to rot)
 call rot(uzero,uione1,vione1,wione1,uuu,vvv,www,irtt)
 return
end subroutine source
 !***********************************************************************
 !***********************************************************************
 !***********************************************************************
 !***********************************************************************
subroutine rot(c,uold,vold,wold,uuu,vvv,www,irt)
  !        sample a direction uuu,vvv,www at an angle arccos(c) from
  !        uold,vold,wold and at an azimuthal angle sampled uniformly.
  use mcnp_random, only : rang
  use mcnp_params, only : dknd
  implicit real(dknd) (a-h,o-z)

  if (abs(c).lt.1.) then
     t1=2.*rang()-1.
     t2=2.*rang()-1.
     r=t1**2+t2**2
     do while (r>1.)
        t1=2.*rang()-1.
        t2=2.*rang()-1.
        r=t1**2+t2**2
     end do
     
     r=sqrt((1.-c**2)/r)
     t1=t1*r
     t2=t2*r
     
     if (abs(wold).le..9) then 
        
        s=sqrt(uold**2+vold**2)
        t=1./s
        uuu=uold*c+(t1*uold*wold-t2*vold)*t
        vvv=vold*c+(t1*vold*wold+t2*uold)*t
        www=wold*c-t1*s
        !
        !        renormalize every 50 calls to prevent error buildup.
        irt=irt-1
        if(irt.ne.0)return
        
        irt=50
        s=1./sqrt(uuu**2+vvv**2+www**2)
        uuu=uuu*s
        vvv=vvv*s
        www=www*s
        return
        
        !
        !        special handling for the case of exceptionally large wold.
     else
        s=sqrt(uold**2+wold**2)
        t=1./s
        uuu=uold*c+(t1*uold*vold+t2*wold)*t
        vvv=vold*c-t1*s
        www=wold*c+(t1*wold*vold-t2*uold)*t
        return
     end if
  else
  !special handling for abs(c)=1. or more.
     if (abs(c).le.1.0001) then
        uuu=c*uold
        vvv=c*vold
        www=c*wold
        return
     else
     ! c is greater than 1.0001   
        write(32,50) c
50      format(5x,'c=',e12.5,1x,'scattering cosine >1'/)
     end if
  end if
  stop
end subroutine rot
 
! interpolation function
subroutine interp(x,y,npts,xin,yout)
  use mcnp_params, only : dknd
  implicit real(dknd) (a-h,o-z)
  ! ----------------------------------------------------------------------
  !
  real(dknd):: x(npts),y(npts)

  ! only calculate if point is within range
  if (x(1).le.xin .and. xin.le.x(npts)) then
     ! find first point that is greater than or equal to sample
     i=1
     do while ( x(i).lt.xin )
        i = i+1
     end do

     if (xin .eq. x(i)) then
        ! if equal, return y value
        yout = y(i)
     else
        ! else, interpolate on segment [i-1,i]
        yout=y(i-1)+(y(i)-y(i-1))/(x(i)-x(i-1))*abs(x(i)-xin)
     end if
     return
  else
     ! sample is not in function
     write(32,100) xin
100  format(20x,'interpolation error',1x,'xin=',e12.5)
     stop
  end if
end subroutine interp

! subroutine for trapezoid quadrature of f(e)
subroutine xint(e,f,npts,e1,e2,sum)
  use mcnp_params, only : dknd
  implicit real(dknd) (a-h,o-z)
  !
  ! ----------------------------------------------------------------------
  !
  real(dknd):: e(npts),f(npts)
  sum=0.
  l=1
  
  ! if integral domain = 0 return sum = 0
  if (e2 .eq. e1) return
  
  ! if integral domain is reversed, switch and scale by -1
  if (e2 .lt. e1) then
     a=e2
     e2=e1
     e1=a
     l=-1
  end if
  
  ! only calculate if integral domain is within function domain
  if (e(1).le.e1 .and. e2.le.e(npts)) then

     ! find first point greater than lower bound
     i = 1
     do while (e(i) .le. e1)
        i = i+1
     end do
     ! interpolate on function at lower bound
     call interp(e,f,npts,e1,fout)
     ! add contribution from first segment of overlap
     sum = sum + (e(i)-e1)*(f(i)+fout)*0.5

     ! store where to start adding more segments
     kmin = i

     ! continue, finding first point greater or equal to upper bound
     do while(e(i) .lt. e2)
        i = i+1
     end do
     ! interpolate on upper bound
     call interp(e,f,npts,e2,fout)
     i = i-1
     ! add contribution from last segment of overlap
     sum = sum + (e2-e(i))*(f(i)+fout)*0.5
     
     ! store where to stop adding more segments
     kmax = i-1
     
     ! don't bother with loop if size is 0
     if (kmax .ge. kmin) then
        ! add full width of each segment
        do k=kmin,kmax
           sum = sum + (e(k+1)-e(k))*(f(k+1)+f(k))*0.5
        end do
     end if

     ! scale total by sign
     sum=sum*(l*1.0)
     return
  else
     write(32,100)
100  format(20x,'integral errors'/)
     stop
  end if
end subroutine xint
   
