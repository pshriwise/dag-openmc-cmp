!#ident srcdx.F from deck sx                                                    !/*sx          1*/
      subroutine srcdx                                                          !/*sx          2*/
!        dummy subroutine.                                                      !/*sx          3*/
      use global_data                                                           !/*sxx         1*/
!                                                                               !/*sxx         2*/
      implicit double precision (a-h,o-z)                                            !/*sxx         3*/
      implicit integer(kindi) (i-n)                                             !/*sxx         4*/
!                                                                               !/*sxx         5*/
!                                                                               !/*sx          5*/
!      return                                                                    !/*sx          6*/
!      end                                                                       !/*sx          7*/
!
! ************************START************************************************
      common /mario/ coslab(38,37),sinlab(38,37),pe1(38,37),dedx(38),
     &emin,eb,uione,vione,wione,etb,dsub,ean,iflag   
      dimension thlab(38,37),pe2(38,37),pl(38),ed(38),pyi(37),
     &thlab1(37),thlab2(37)
!
      data nterm/3/
!
      double precision m1,m2,ma,mb,m2v(2),mbv(2),m1m2,m2m1(3,3),q(2)
      double precision sml
      data ma/1875.612793d0/                                            
      data mbv/2808.920906d0,1875.612793d0/                             
      data m1/939.565346d0/                                             
      data m2v/3727.379109d0,2808.391383d0/                             
      data q/17.5892930d0,3.2689141d0/ 
      data npts1/38/
      data npts2/37/
      data  ed/0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10, 
     &0.108,0.12,0.14,0.16,0.18,0.20,0.22,0.24,0.26,0.28,0.30,0.32, 
     &0.34,0.36,0.38,0.40,0.50,0.75,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0, 
     &9.0,10.0/
      save

      if(sml.gt.0.0) go to 1111
      do 1000 j=1,npts1
      do 1000 n=1,npts2
      seno=sinlab(j,n)
      thlab(j,n)=acos(coslab(j,n))
      pe2(j,n)=pe1(j,n)/dedx(j)*seno
 1000 continue
      do 1001 j=1,npts1
      do 1002 n=1,npts2
      thlab1(n)=thlab(j,n)
 1002 pl(n)=pe2(j,n)
      call xint1d(thlab1,pl,npts2,nterm,thlab1(1),thlab1(npts2),sml1)
 1001 pyi(j)=sml1
       call xint1d(ed,pyi,npts1,nterm,emin,eb,sml)
 1111 cth=uione*uuu+vione*vvv+wione*www
      a1=acos(cth)
      do 350 i=1,npts1
      do 333 j=1,npts2
      thlab2(j)=acos(coslab(i,j))
 333  continue
      call xinterp2D(ed,thlab2,pe1,npts1,npts2,ed(i),a1,smd1)
 350  pl(i)=smd1/dedx(i)
      call xint1d(ed,pl,npts1,nterm,emin,eb,smd)
      mb=mbv(iflag)
      m2=m2v(iflag)
      pana=sqrt(ean*ean-ma*ma)
      sth=sqrt(1.-cth*cth)      
      t1n=((ean+mb)**2.-(pana**2.)*(cth**2.))**(-1.)
      t2n=(ean+mb)*(mb*ean+0.5* 
     &(ma**2.+mb**2.+m1**2.-m2**2.))
      t3n=pana*cth*((mb*ean+0.5* 
     &(ma**2.+mb**2.-m1**2.-m2**2.))**2.- 
     &m1**2.*m2**2.-m1**2.*pana**2.*sth**2.)**0.5
      e1n=t1n*(t2n+t3n)
      eklab=e1n-m1
      erg=eklab
      psc=smd/sml
      return
      end 

! *********************************************************

!     subroutine per l'integrazione numerica 1 dimensionale
      subroutine xint1d(E,F,NPTS,NTERMS,E1,E2,SUM)
!
      use global_data 
      implicit double precision (a-h,o-z)
      save
      DIMENSION E(1), F(1), X(70), Y(70)
!
      IST1=1
      IST2=1
      DO 100 I = IST1,NPTS
  100 IF (E1.LT.E(I)) GOTO 200
  200 I = I - 1
      IF (I.eq.0) GOTO 250
      IF (E1.LT.E(I)) GOTO 200
  250 IL = I + 1
      IST1 = IL
      SUM = 0.0
      DO 300 I = IST2, NPTS
  300 IF (E2.LT.E(I)) GOTO 400
  400 I = I - 1
      IF (I.eq.0) GOTO 450
      IF (E2.LT.E(I)) GOTO 400
  450 IH = I
      IST2 = IH
      IF (IH.LT.IL) GOTO 700
      ILA = IL - NTERMS/2
      IF (ILA.LE.0)   ILA = 1
      IF (ILA + NTERMS - 1.GT.NPTS)   ILA = NPTS - NTERMS + 1
      IHA = IH - NTERMS/2
      IF (IHA.LT.1)   IHA = 1
      IF (IHA + NTERMS - 1.GT.NPTS)   IHA = NPTS - NTERMS + 1
      CALL ITEG (E,F,NTERMS,ILA,E1,E(IL),SUM)
      CALL ITEG (E,F,NTERMS,IHA,E(IH),E2,SUM)
      GO TO 800
  700 ILA = IL - NTERMS/2
      IF (ILA.LE.0)   ILA = 1
      IF (ILA + NTERMS - 1.GT.NPTS)   ILA = NPTS - NTERMS + 1
      CALL ITEG (E,F,NTERMS,ILA,E1,E2,SUM)
  800 CONTINUE
      K = 0
      DO 500 I = IL, IH
      K = K + 1
      X(K) = E(I)
  500 Y(K) = F(I)
      IF (K.LE.1)   GO TO 900
      NPTSA = K
      SUM = SUM + AREA(X,Y,NPTSA,NTERMS)
  900 CONTINUE
      RETURN
      END

! *********************************************************

! subroutine per l'interpolazione
      subroutine interp(x,y,npts,xin,yout)
      use global_data 
      implicit double precision (a-h,o-z)
      save
! ---------------------------------------------------------------------
!
      dimension x(npts),y(npts)
      EPS=0.00001      
      do 1 i=1,npts
      XX=ABS(x(i))
      XINXIN=ABS(xin)
      if(ABS(x(i)-xin).LT.EPS*MAX(XX,XINXIN))go to 10
!      if(x(i).eq.xin)go to 10  
      if(x(i).gt.xin)go to 11
   1  continue
      go to 12
  11  yout=y(i-1)+(y(i)-y(i-1))/(x(i)-x(i-1))*(xin-x(i-1))
      return
  10  yout=y(i)
      return
  12  write(32,100)
 100  format(20x,'errore interpolazione 1d'/)
      stop
      end  

! *********************************************************

      subroutine rot(c,uoldbojan,vold,wold,
     &uuubojan,vvvbojan,wwwbojan,irtt)
!        sample a direction uuu,vvv,www at an angle arccos(c) from
!        uold,vold,wold and at an azimuthal angle sampled uniformly.
!
      use global_data 
      implicit double precision (a-h,o-z)
      common /mario/ dedx(62),emin,eb,uione,vione,wione,etb,dsub
!      implicit double precision (a-h,o-z)
      save
!
      if(abs(c).ge.1.)go to 30
   10 t1=2.*rang()-1.
      t2=2.*rang()-1.
      r=t1**2+t2**2
      if(r.gt.1.)go to 10
      r=sqrt((1.-c**2)/r)
      t1=t1*r
      t2=t2*r
      if(abs(wold).gt..9)go to 20
      s=sqrt(uoldbojan**2+vold**2)
      t=1./s
      uuubojan=uoldbojan*c+(t1*uoldbojan*wold-t2*vold)*t
      vvvbojan=vold*c+(t1*vold*wold+t2*uoldbojan)*t
      wwwbojan=wold*c-t1*s
!
!        renormalize every 50 calls to prevent error buildup.
      irtt=irtt-1
      if(irtt.ne.0)return
      irtt=50
      s=1./sqrt(uuubojan**2+vvvbojan**2+wwwbojan**2)
      uuubojan=uuubojan*s
      vvvbojan=vvvbojan*s
      wwwbojan=wwwbojan*s
      return
!
!        special handling for the case of exceptionally large wold.
   20 s=sqrt(uoldbojan**2+wold**2)
      t=1./s
      uuubojan=uoldbojan*c+(t1*uoldbojan*vold+t2*wold)*t
      vvvbojan=vold*c-t1*s
      wwwbojan=wold*c+(t1*wold*vold-t2*uoldbojan)*t
      return
!
!        special handling for abs(c)=1. or more.
   30 if(abs(c).gt.1.0001)go to 40
      uuubojan=c*uoldbojan
      vvvbojan=c*vold
      wwwbojan=c*wold
      return
   40 write(32,50)
   50 format(5x,'coseno di scattering >1'/)
      stop
      end

! *********************************************************

! subroutine per l'interpolazione
! su una matrice di punti
      subroutine xinterp2D(x1,x2,y,npts1,npts2,xin1,xin2,yout)
      use global_data 
      implicit double precision (a-h,o-z)
      save
! ---------------------------------------------------------------------
!
      dimension x1(npts1),x2(npts2),y(npts1,npts2),yout1(2)            
      EPS=0.00001   
      do 1 i=1,npts1
      X1X1=ABS(x1(i))
      XIN1XIN1=ABS(xin1)
      if(ABS(x1(i)-xin1).LT.EPS*MAX(X1X1,XIN1XIN1))go to 10
!      if(x1(i).eq.xin1)go to 10
      if(x1(i).gt.xin1)go to 11
   1  continue
      write(*,*)'x1>',i,x1(i),xin1
      go to 12
  10  ii=i
      go to 33
  11  ii=i-1
  33  do 21 j=1,npts2
      X2X2=ABS(x2(j))
      XIN2XIN2=ABS(xin2)
      if(ABS(x2(j)-xin2).LT.EPS*MAX(X2X2,XIN2XIN2))go to 101
!      if(x2(j).eq.xin2)go to 101
      if(x2(j).gt.xin2)go to 111
  21  continue
      go to 12
 101  yout=y(ii,j)+(y(ii+1,j)-y(ii,j))/(x1(ii+1)-x1(ii))*
     &(xin1-x1(ii))
      return
 111  yout1(1)=y(ii,j-1)+(y(ii+1,j-1)-y(ii,j-1))/(x1(ii+1)-x1(ii))* 
     &(xin1-x1(ii))
      yout1(2)=y(ii,j)+(y(ii+1,j)-y(ii,j))/(x1(ii+1)-x1(ii))*
     &(xin1-x1(ii))
      yout=yout1(1)+(yout1(2)-yout1(1))/(x2(j)-x2(j-1))*
     &(xin2-x2(j-1))
      return
  12  write(32,100)
 100  format(20x,'errore interpolazione 2d'/)
      stop
      end

! *********************************************************     
      
      FUNCTION AREA (X,Y,NPTS,NTERMS)
      use global_data 
      implicit double precision (a-h,o-z)
      DIMENSION X(1), Y(1)
   11 SUM = 0.0D0
      IF (NPTS - NTERMS) 21, 21, 13
   13 NEVEN = 2*(NTERMS/2)
      IDELTA = NTERMS/2 - 1
      IF (NTERMS - NEVEN) 31, 31, 51
!.... FIT ALL POINTS WITH ONE CURVE
   21 X1 = X(1)
      X2 = X(NPTS)
   23 CALL ITEG(X,Y,NPTS,1,X1,X2,SUM)
      GO TO 71
   31 X1 = X(1)
      J = NTERMS - IDELTA
      X2 = X(J)
      CALL ITEG (X,Y,NTERMS,1,X1,X2,SUM)
      I1 = NPTS - NTERMS + 1
      J = I1 + IDELTA
      X1 = X(J)
      X2 = X(NPTS)
   39 CALL ITEG (X,Y,NTERMS,I1,X1,X2,SUM)
      IF (I1 - 2) 71, 71, 41
   41 IMAX = I1 - 1
      DO 46 I = 2, IMAX
      J = I + IDELTA
      X1 = X(J)
      X2 = X(J+1)
   46 CALL ITEG (X,Y,NTERMS,I,X1,X2,SUM)
      GO TO 71
   51 X1 = X(1)
      J = NTERMS - IDELTA
      X2 = (X(J) + X(J-1))/2.
      CALL ITEG (X,Y,NTERMS,1,X1,X2,SUM)
      I1 = NPTS - NTERMS + 1
      J = I1 + IDELTA
      X1 = (X(J) + X(J+1))/2.
      X2 = X(NPTS)
   59 CALL ITEG (X,Y,NTERMS,I1,X1,X2,SUM)
      IF (I1 - 2) 71, 71, 61
   61 IMAX = I1 - 1
      DO 66 I = 2, IMAX
      J = I + IDELTA
      X1 = (X(J+1) + X(J))/2.
      X2 = (X(J+2) + X(J+1))/2.
   66 CALL ITEG (X,Y,NTERMS,I,X1,X2,SUM)
   71 AREA = SUM
      RETURN
      END

! *********************************************************

      SUBROUTINE ITEG (X, Y, NTERMS, I1, X1, X2, SUM)
!.... CONSTRUCT SQUARE MATRIX AND INVERT
      use global_data 
      implicit double precision (a-h,o-z)
!      implicit double precision (a-h,o-z)
      save
      DIMENSION X(1), Y(1)
      DIMENSION ARRAY (15,15)
   11 DO 17 J = 1, NTERMS
      I = J + I1 - 1
      DELTAX = X(I) - X(I1)
      XJK = 1.0D0
      DO 17 K = 1, NTERMS
      ARRAY(J,K) = XJK
   17 XJK = XJK*DELTAX
   21 CALL DMATINV (ARRAY,NTERMS,DET)
      IF (DET) 31, 23, 31
   23 IMID = I1 + NTERMS/2
      SUM = SUM + Y(IMID)*(X2 - X1)
      GO TO 40
!.... EVALUATE COEFFICIENTS AND INTEGRATE
   31 DX1 = X1 - X(I1)
      DX2 = X2 - X(I1)
   33 DO 39 J = 1, NTERMS
      A = 0.0D0
      DO 37 K = 1, NTERMS
      I = K + I1 - 1
   37 A = A + Y(I)*ARRAY(J,K)
      DENOM = J
   39 SUM = SUM + (A/DENOM)*(DX2**J - DX1**J)
   40 RETURN
      END

! *********************************************************
      
      SUBROUTINE DMATINV (ARRAY, NORDER, DET)
!.... FIND LARGEST ELEMENT ARRAY(I,J) IN REST OF MATRIX
      use global_data 

      implicit double precision (a-h,o-z)
      implicit integer(kindi) (i-n)
      save
      DIMENSION ARRAY(15,15), IK(15), JK(15)
   10 DET = 1.
   11 DO 100 K = 1, NORDER
      AMAX = 0.
   21 DO 30 I = K, NORDER
      DO 30 J = K, NORDER
   23 IF (DABS(AMAX) - DABS(ARRAY(I,J))) 24, 24, 30
   24 AMAX = ARRAY (I,J)
      IK(K) = I
      JK(K) = J
   30 CONTINUE
!.... INTERCHANGE ROWS AND COLUMNS TO PUT AMAX IN ARRAY(K,K)
   31 IF (AMAX) 41, 32, 41
   32 DET = 0.0D0
      GO TO 140
   41 I = IK(K)
      IF (I - K) 21, 51, 43
   43 DO 50 J = 1, NORDER
      SAVE = ARRAY(K,J)
      ARRAY(K,J) = ARRAY(I,J)
   50 ARRAY(I,J) = -SAVE
   51 J = JK(K)
      IF (J - K) 21, 61, 53
   53 DO 60 I = 1, NORDER
      SAVE = ARRAY (I,K)
      ARRAY (I,K) = ARRAY(I,J)
   60 ARRAY (I,J) = -SAVE
!.... ACCUMULATE ELEMENTS OF INVERSE MATRIX
   61 DO 70 I = 1, NORDER
      IF (I - K) 63, 70, 63
   63 ARRAY(I,K) = -ARRAY(I,K) / AMAX
   70 CONTINUE
   71 DO 80 I = 1, NORDER
      DO 80 J = 1, NORDER
      IF (I - K) 74, 80, 74
   74 IF (J - K) 75, 80, 75
   75 ARRAY (I,J) = ARRAY(I,J) + ARRAY(I,K)*ARRAY(K,J)
   80 CONTINUE
   81 DO 90 J = 1, NORDER
      IF (J - K) 83, 90, 83
   83 ARRAY(K,J) = ARRAY(K,J) / AMAX
   90 CONTINUE
      ARRAY(K,K) = 1. / AMAX
  100 DET = DET * AMAX
!.... RESTORE ORDERING OF MATRIX
  101 DO 130 L = 1, NORDER
      K = NORDER - L + 1
      J = IK(K)
      IF (J - K) 111, 111, 105
  105 DO 110 I = 1, NORDER
      SAVE = ARRAY(I,K)
      ARRAY(I,K) = - ARRAY(I,J)
  110 ARRAY(I,J) = SAVE
  111 I = JK(K)
      IF (I - K) 130, 130, 113
  113 DO 120 J = 1, NORDER
      SAVE = ARRAY(K,J)
      ARRAY(K,J) = -ARRAY(I,J)
  120 ARRAY(I,J) = SAVE
  130 CONTINUE
  140 RETURN
      END