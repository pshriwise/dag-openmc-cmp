      subroutine source
c
c        this source is for new target interaction and includes ion
c                      s c a t t e r i n g
c        following variables must be defined within the subroutine:
c        xxx,yyy,zzz,icl,jsu,erg,wgt,tme and possibly ipt,uuu,vvv,www.
c        subroutine srcdx may also be needed.
c
c
      implicit double precision (a-h,o-z)
c
c        code name and version number.
      character kod*8,ver*5
      parameter (kod='mcnp',ver='4a')
c
c        processor-dependent named constants.
c        mdas is the initial size of dynamically allocated storage.
c        on systems where memory adjustment is not available, set mdas
c        large enough for your biggest problem.
      parameter (mdas=8000000)
      parameter (ndp2=2,huge=1d37)
c
c        array dimensions.  i/o unit numbers.  general constants.
      parameter (maxf=16,maxi=34,maxv=19,maxw=3,mcoh=55,mcpu=32,minc=21,
     1 mink=200,mipt=3,mjsf=9,mkft=9,mktc=22,mlgc=100,mpb=5,mpng=21,
     2 mseb=301,mspare=3,mtop=49,mwng=25,mxdt=20,mxdx=5,mxlv=10,
     3 nbmx=100,ncolor=10,ndef=14,novr=5,nptr=13,nsp=602,nsp12=nsp+12,
     4 ntp=201,nspt=nsp+ntp+7,iui=31,iuo=32,iur=33,iux=34,iud=35,iub=60,
     5 iup=37,ius=38,iu1=39,iu2=40,iusw=41,iusr=42,iusc=43,iuc=44,
     6 iut=45,iuz=46,iuk=47,iu3=48,iu4=49,iupw=50,iupc=51,
     7 aneut=1.008664967d0,avogad=6.022043446928244d23,
     8 avgdn=1.d-24*avogad/aneut,dfdmp=-15.,dftint=100.,
     9 fscon=137.0393d0,hsll=1.e-30,one=1.d0,pie=3.1415926535898d0,
     9 planck=4.135732d-13,slite=299.7925d0,third=one/3.d0,zero=0.)
c
c ----------------------------------------------------------------------
c
c
      common /tables/ ebl(16),gpt(mipt),qfiss(23),rkt(mtop),talb(8,2),
     1 vco(mcoh),vic(minc),wco(mcoh),
     2 jrwb(16,mipt),jsf(mjsf),mfiss(22),nvs(maxv),itty,jtty
c
c        character common -- character variables and arrays.
      character aid*80,aid1*80,aids*80,chcd*10,exms*80,hbln(maxv,2)*3,
     1 hblw(maxw)*3,hcs(2)*7,hdpath*80,hdpth*80,hft(mkft)*3,hfu(2)*11,
     2 hip*(mipt),hmes*69,hnp(mipt)*8,hovr*8,hsd(2)*10,hsub*6,ibin*8,
     3 idtm*19,idtms*19,ilbl(8)*8,iname*8,klin*80,kods*8,ksf(29)*3,
     4 loddat*8,lods*8,msub(ndef)*8,probid*19,probs*19,rfq(11)*57,
     5 ufil(3,6)*11,vers*5
      common /charcm/ aid,aid1,aids,chcd,exms,hbln,hblw,hcs,hdpath,
     1 hdpth,hft,hfu,hip,hmes,hnp,hovr,hsd,hsub,ibin,idtm,idtms,ilbl,
     2 iname,klin,kods,ksf,loddat,lods,msub,probid,probs,rfq,ufil,vers
c        isub:  names of files
      character*8 inp,outp,runtpe,mctal,wssa,ptrac,comout,srctp,plotm,
     1 rssa,xsdir,com,dumn1,dumn2,isub(ndef)
      common /charcm/ inp,outp,runtpe,mctal,wssa,ptrac,comout,srctp,
     1 plotm,rssa,xsdir,com,dumn1,dumn2
      equivalence (isub,inp)
c
c ----------------------------------------------------------------------
c
c
c        whole-block declarations.
      parameter (nfixcm=maxi+3*maxv+mtop+mipt*(24+mxdt+7*mxdx)+nsp+78,
     1 lfixcm=3*mxdt+mink+11*mipt+2*maxv+2*maxf+255)
      dimension gfixcm(nfixcm),jfixcm(lfixcm)
      equivalence (bbrem,gfixcm),(ibad,jfixcm)
      parameter (nvarcm=102*mipt+203,lvarcm=mipt*(1+8*mxdx)+mcpu+326)
      dimension gvarcm(nvarcm),jvarcm(lvarcm)
      equivalence (cpk,gvarcm),(idum,jvarcm)
      parameter (nephcm=29,lephcm=nptr+novr+ncolor+59)
      dimension gephcm(nephcm),jephcm(lephcm)
      equivalence (cp0,gephcm),(ichan,jephcm)
      logical lockl
c
********************  statically allocated common  *********************
*                                                                      *
c        fixed common -- constant after the problem is initiated.
      common /fixcom/ bbrem(mtop),bcw(2,3),bnum,calph(maxi),
     1 ddg(mipt,mxdt),ddx(mipt,2,mxdx),dxw(mipt,3),dxx(mipt,5,mxdx),
     1 ecf(mipt),efac,emcf(mipt),emx(mipt),enum,espl(mipt,10),fnw,rim,
     2 hsb(nsp),
     2 rssp,rnok,rnfb,rnfs,rngb,rngs,rnmult,snit,srv(3,maxv),
     2 tco(mipt),thgf(0:50),wc1(mipt),wc2(mipt),wwg(7),wwp(mipt,5),
     3 zfixcm,
     3 ibad,icw,idefv(maxv),ides,idrc(mxdt),ifft,igm,igww,ikz,img,imt,
     4 indt,ink(mink),iphot,iplt,ipty(mipt),isb,issw,istrg,
     4 ivdd(maxf),ivdis(maxv),ivord(maxf),jgm(mipt),jtlx,junf,kf8,kfl,
     5 knods,knrm,kpt(mipt),ktls,kufil(2,6),ldr,lfcdg,lfcdj,
     5 locdt(2,mxdt),lvcdg,lvcdj,lxs,mbnk,mcal,mct,mgegbt(mipt),
     6 mgm(mipt+1),mgww(mipt+1),mix,mjss,mlaj,mlja,mrkp,mrl,msd,msrk,
     7 mtasks,nlat,nsrc,
     6 mww(mipt+1),mxa,mxafs,mxe,mxe1,mxf,mxj,mxt,mxtr,mxxs,ndet(mipt),
     7 ndnd,ndtt,ndx(mipt),nee,ngww(mipt),nhb,nilr,nilw,nips,niss,njsr,
     8 njss,nkxs,nlev,nlja,nmat,nmxf,nnpos,nocoh,nord,np1,npikmt,npn,
     9 nrcd,nrss,nsph,nsr,nsrck,nstrid,ntal,nvec,nww(mipt),nxnx
c        offsets for virtual arrays in dynamically allocated storage.
      common /fixcom/ lara,lasp,lawc,lawn,lcmg,lden,ldrs,ldxp,leaa,lear,
     1 leba,lebd,lebt,lech,ledg,leee,leek,legg,lelp,lesa,lewg,lfim,lfme,
     1 lfmg,lfor,lfrc,lfst,lftt,lgmg,lgvl,lgwt,lpbr,lpbt,lpkn,lpmg,lpru,
     2 lpxr,lqav,lqax,lqcn,lrho,lscf,lsmg,lspf,lsqq,lsso,ltbt,ltds,ltmp,
     2 ltrf,ltth,lvcl,lvec,lvol,lwwe,lwwf,lwwk,lxnm,lipa,lipt,liss,litd,
     3 lixl,liza,ljar,ljmd,ljpt,ljsc,ljss,ljtf,ljun,ljvc,ljxs,lkcp,lksd,
     3 lkst,lksu,lktp,lkxs,llaf,llat,llca,llfc,llft,llja,llme,llmt,llct,
     4 llph,llst,llsc,lmat,lmbd,lmbi,lmfl,lncl,lnmt,lnpq,lnsb,lnsf,lnty,
     5 lnxs,lddm,lddn,ldec,ldxc,ldxd,lflx,lfso,lgww,lpac,lpan,lpcc,lptr,
     5 lpts,lpwb,lrkp,
     6 ltfc,lwns,lise,ljfq,llaj,llcj,llse,lndp,lndr,lnpw,lnsl,lntb,lscr,
     7 ldrc,lemi,lfdd,lgnr,lpik,lrng,lrtc,ltgp,lifl,lpc2,lixc,ljfl,ljft,
     8 ljmt,lkmt,lktc,lkxd,llbb,llgtsk,lljtsk,lnhs,lshs,lstt,ltal,lgbn,
     9 lbnk,lxss,lexs,mfixcm
c
c        variable common -- variable but required for a continue run.
c        arrays that are backed up when a track is lost.
      common /varcom/ cpk,cts,dbcn(20),dmp,eacc(4),febl(2,16),osum(3),
     1 coll(mipt),
     1 osum2(3,3),pax(6,16,mipt),prn,rani,ranj,rdum(50),rijk,rkk,
     2 rlt(2,4,2),rltp(2,4),rnr,rsum(2,4),rsum2(2,2,4),skef(3,3),
     2 smul(3),sumk(3),tmav(mipt,3),
     3 twac,twss,wcs1(mipt),wcs2(mipt),wgts(2),wt0,wssi(7),
     4 zvarcm,
     5 idum(50),inif,ist,ist0,ixak,ixak0,jrad,kcsf,kct,kcy,knod,
     6 ksdef,kcz,lost(2),nbal(mcpu),nbhwm,nbov,nbt(mipt),
     7 ndmp,nerr,netb(2),nfer,npc(20),npd,npnm,npp,nppm,nps,npsout,npsr,
     8 nqss,nqsw,nrnh(3),nrrs,nrsw,nsa,nsa0,nskk,nss,nss0,nssi(8),ntc,
     9 ntc1,ntss,nwer,nwsb,nwse,nwsg(2),nwst,nwws(2,99),
     1 nziy(8,mxdx,mipt),
     2 mvarcm
c
c        ephemeral common -- not needed after the current run.
      common /ephcom/ cp0,cp1,cp2,cpa,ctme,fpi,freq,
     1 ssb(10),tdc,tlc,trm,tsc,wnvp(4),xhom,xnum,yhom,
     2 zephcm,
     3 ichan,ics,idmp,ifile,iln,iln1,inform,iovr,inpd,iptr,iptra(nptr),
     4 irup,itask,iterm,itfxs,itotnu,iuou,jchar,jfcn,jgf,jgxa(2),
     5 jgxo(2),jovr(novr),jtfc,jvp,kbp,kcolor(ncolor+6),kdbnps,kmplot,
     6 komout,konrun,kprod,krflg,krtm,ksr,ktfile,ldq,lfatl,lfll,locki,
     7 lspeed,mcolor,mdc,mmkdb,mnk,mpc,mrm,nde,nkrp,nomore,nrc,nst,
     8 ntasks,
     9 mephcm,lockl,mynum
      equivalence (kddm,lddm),(kddn,lddn),(kdec,ldec),(kdxc,ldxc),
     1 (kdxd,ldxd),(kflx,lflx),(kgww,lgww),(kpac,lpac),(kpan,lpan),
     2 (kpcc,lpcc),(kpwb,lpwb),(kwns,lwns),(kise,lise),(klaj,llaj),
     3 (klcj,llcj),(klse,llse),(kndp,lndp),(kndr,lndr),(kdrc,ldrc),
     4 (kfdd,lfdd),(kgnr,lgnr),(kpik,lpik),(krtc,lrtc),(ktgp,ltgp),
     5 (kifl,lifl),(kpc2,lpc2),(kjfl,ljfl),(kjft,ljft),(kktc,lktc),
     6 (ktal,ltal),(kgbn,lgbn),(kbnk,lbnk),(knhs,lnhs),(kshs,lshs),
     7 (kstt,lstt)
c
      common /pblcom/ xxx,yyy,zzz,uuu,vvv,www,erg,wgt,tme,vel,dls,
     1 dxl,dtc,elc(mipt),fiml(mipt),fismg,wtfasv,rnk,spare(mspare),
     2 zpblcm,
     3 xxx9(mpb),yyy9(mpb),zzz9(mpb),uuu9(mpb),vvv9(mpb),www9(mpb),
     4 erg9(mpb),wgt9(mpb),tme9(mpb),vel9(mpb),dls9(mpb),dxl9(mpb),
     5 dtc9(mpb),elc9(mpb,mipt),fiml9(mpb,mipt),fismg9(mpb),
     6 wtfas9(mpb),rnk9(mpb),spare9(mpb,mspare),
     7 zpb9cm(mpb),
     1 npa,icl,jsu,ipt,iex,node,idx,ncp,jgp,lev,iii,jjj,kkk,iap,
     2 mpblcm,
     3 npa9(mpb),icl9(mpb),jsu9(mpb),ipt9(mpb),iex9(mpb),node9(mpb),
     4 idx9(mpb),ncp9(mpb),jgp9(mpb),lev9(mpb),iii9(mpb),jjj9(mpb),
     5 kkk9(mpb),iap9(mpb),
     6 mpb9cm(mpb)
      parameter (npblcm=mspare+2*mipt+16,lpblcm=14)
      dimension gpblcm(npblcm+1),jpblcm(lpblcm+1),
     1 gpb9cm(mpb,npblcm+1),jpb9cm(mpb,lpblcm+1)
      equivalence (xxx,gpblcm),(npa,jpblcm),(xxx9,gpb9cm),(npa9,jpb9cm)
c
      common /tskcom/ amfp,ang(3),cbwf,cmult,colout(2,11),cpv,ddet,
     1 colltc(mipt),
     1 deb,dti(mlgc),eacctc(2),eg0,ergace,febltc(2,16),paxtc(6,16,mipt),
     1 pfp,ple,pmf,psc,qpl,ranb,ranitc,ranjtc,rans,rijktc,rlttc(2,4,2),
     2 rnrtc,rnrtc0,sff(3,maxf),siga,smultc(3),ssr,stp,sumktc(3),
     2 tmavtc(mipt,3),totgp1,totm,tpd(7),tpp(20),ttn,udt(10,0:mxlv),
     3 udtr(10*mxlv+10),udts(10*mxlv+10),udtt(10*mxlv+10),uold,vold,
     3 vtr(3),wcs1tc(mipt),wcs2tc(mipt),wgtstc(2),wold,ycn,
     4 ztskcm,
     4 iax,ibc,ibe,ibs,ibt,ibu,iclp(4,0:mxlv),idet,iet,imd,ipsc,irt,
     5 isic(maxf),ital,iti(mlgc),ixcos,ixre,jap,jbd,jbnk,jev,jtls,kdb,
     5 jlock,kqss,ktask,levp,lgc(mlgc+1),lsb,mbb,mkc,mpan,nbhwtc,nbnk,
     6 nbttc(mipt),nch(mipt),ngp,nlaj,nlse,nlt,npb,npsrtc,
     6 npstc,nrnhtc(3),nter,ntii,ntx,ntyn,nziytc(8,mxdx,mipt),
     9 mtskcm
      parameter (ntskcm=102*mipt+40*mxlv+3*maxf+mlgc+184,
     1 ltskcm=mipt*(2+8*mxdx)+4*mxlv+2*mlgc+maxf+50)
      dimension gtskcm(ntskcm),jtskcm(ltskcm),udt1(10*mxlv+10)
      equivalence (amfp,gtskcm),(iax,jtskcm),(udt,udt1)
c
********************  dynamically allocated common  ********************
*                                                                      *
      common /dac/ das(mdas/ndp2)
c
c        fixed dynamically allocated common.
      dimension aaafd(2),ara(1),asp(1),awc(1),awn(1),cmg(1),den(1),
     1 drs(1),dxcp(0:mxdx,mipt,1),eaa(1),ear(1),
     1 eba(mtop,1),ebd(mtop,1),ebt(mtop,1),
     1 ech(mpng,mwng,1),edg(1),eee(1),eek(1),egg(maxi,1),elp(mipt,1),
     2 esa(1),ewwg(1),fim(mipt,1),fme(1),fmg(1),for(mipt,1),frc(1),
     2 fst(1),ftt(1),gmg(1),gvl(1),gwt(1),pbr(1),pbt(1),pkn(1),pmg(1),
     3 pru(1),pxr(1),qav(1),qax(mipt,1),qcn(1),rho(1),scf(1),smg(1),
     3 spf(4,2),sqq(12,1),sso(1),tbt(1),tds(1),tmp(1),trf(17,0:1),
     4 tth(1),vcl(3,7,1),vec(3,1),vol(1),wwe(1),wwf(1),wwk(1),xnm(1),
     4 iiifd(1),ipan(1),iptal(8,5,1),iss(1),itds(1),ixl(3,1),iza(1),
     5 jasr(1),jmd(1),jptal(8,1),jscn(1),jss(1),jtf(8,1),jun(1),jvc(1),
     6 jxs(32,1),kcp(1),ksd(21,1),kst(1),ktp(mipt,1),kxs(1),laf(3,3),
     6 ksu(1),
     7 lat(2,1),lca(1),lfcl(1),lft(mkft,1),lja(1),lme(mipt,1),lmt(1),
     8 locct(mipt,1),locph(1),locst(mipt,1),lsc(1),mat(1),mbd(1),mbi(1),
     9 mfl(3,1),ncl(1),nmt(1),npq(1),nsb(1),nsf(1),nty(1),nxs(16,1)
      equivalence (das,aaafd,ara,asp,awc,awn,cmg,den,drs,dxcp,eaa,ear,
     1 eba,ebd,ebt,ech,edg,eee,eek,egg,elp,esa,ewwg,fim,fme,fmg,for,frc,
     2 fst,ftt,gmg,gvl,gwt,pbr,pbt,pkn,pmg,pru,pxr,qav,qax,qcn,rho,scf,
     3 smg,spf,sqq,sso,tbt,tds,tmp,trf,tth,vcl,vec,vol,wwe,wwf,wwk,xnm,
     4 iiifd,ipan,iptal,iss,itds,ixl,iza,jasr,jmd,jptal,jscn,jss,jtf,
     5 jun,jvc,jxs,kcp,ksd,kst,ktp,kxs,laf,lat,lca,lfcl,lft,lja,lme,lmt,
     6 ksu,
     6 locct,locph,locst,lsc,mat,mbd,mbi,mfl,ncl,nmt,npq,nsb,nsf,nty,
     7 nxs)
c
c        variable dynamically allocated common.
      dimension aaavd(1),ddm(2,1),ddn(24,1),dec(3,1),dxc(3,1),
     1 dxd(mipt,24,mxdx),flx(1),fso(1),gww(2,9,1),pac(mipt,10,1),
     2 pan(2,6,1),pcc(3,1),pwb(mipt,19,1),rkpl(19,1),shsd(nspt,1),
     3 stt(ntp,1),tfc(6,20,1),wns(2,1),
     4 iiivd(1),isef(2,1),jfq(8,0:1),laj(1),lcaj(1),lse(1),ndpf(6,1),
     5 ndr(1),nhsd(nsp12,1),npsw(1),nsl(2+4*mipt,1),ntbb(4,1)
      equivalence (das,aaavd,ddm,ddn,dec,dxc,dxd,flx,fso,gww,pac,pan,
     1 pcc,pwb,rkpl,tfc,wns,shsd,stt,iiivd,isef,jfq,laj,lcaj,lse,ndpf,
     2 ndr,nhsd,npsw,nsl,ntbb)
c
c        ephemeral dynamically allocated common.
      dimension scr(1),drc(16,1),emi(1),fdd(2,1),genr(1),pik(1),ptr(1),
     1 pts(1),rng(1),rtc(10,1),tgp(1),ifl(1),ipac2(1),ixc(61,1),jfl(1),
     2 jft(1),jmt(1),kmt(3,1),ktc(2,1),kxd(1),lbb(1)
      equivalence (das,scr,drc,emi,fdd,genr,pik,ptr,pts,rng,rtc,tgp,ifl,
     1 ipac2,ixc,jfl,jft,jmt,kmt,ktc,kxd,lbb)
c
c        tallies, bank, and cross-sections in dac.
      dimension tal(1),gbnk(1),ibnk(1),exs(1)
      real xss(1)
      equivalence (das,tal,gbnk,ibnk,xss,exs)
c
c        dynamically allocated common for imcn.
      dimension jtr(1),awt(1),bbv(1),prb(1),rtp(1),sfb(1),
     1 ipnt(2,mktc,0:1),jasw(1),kaw(1),kdr(1),kdup(1),kmm(1),ktr(1),
     2 lxd(mipt,1),mfm(1),nlv(1),nslr(2+4*mipt,1),
     3 aras(2,1),atsa(2,1),rscrn(2,1),rsint(2,1),scfq(5,1),vols(2,1),
     4 iint(1),icrn(3,1),ljav(1),ljsv(1),lsat(1)
      equivalence (das,jtr,awt,bbv,prb,rtp,sfb,ipnt,jasw,kaw,kdr,kdup,
     1 kmm,ktr,lxd,mfm,nlv,nslr,
     2 aras,atsa,rscrn,rsint,scfq,vols,iint,icrn,ljav,ljsv,lsat)
c
      common /mario/ dedx(100),emin,eb,sml,uione,vione,wione,et,b,d
c **********************************************************************
c              dati standard
c **********************************************************************
      dimension cdedx(100),sig(100),tdedx(100),pl(100),am(3),pe(100)    00007100
      dimension ed(100),suml(3601),th(3601),stoich(2),z(3)              00007200
      dimension a(3,3),ec(3,3),f(3,3)                                   00007200
      double precision md,mt,mn,ma,m1m2,m2m1(3,3)
      data md/2.01410219/
      data mt/3.01602994/
      data mn/1.00866544/
      data ma/4.00260361/
      data aux/25.20734546/
      data q/17.589/
c                                                                       00005300
c i dati da dare in input direttamente in questo programma sono:        00005400
c    il carico di trizio nel titanio in atomi per atomo di titanio      00005700
c    l'energia dei deutoni-tritoni in mev,fino a 0.5 mev;               00005900
c **********************************************************************00006800
c              dati presi dai giapponesi                                00006900
c **********************************************************************00007000
      data npts/50/                                                     00007400
      data  ed/0.010,0.020,0.030,0.040,                                 00007500
     *0.050,0.060,0.070,0.080,0.090,0.100,0.110,0.120,0.130,0.140,      00007600
     *0.150,0.160,0.170,0.180,0.190,0.200,0.210,0.220,0.230,0.240,0.250,00007700
     *0.260,0.270,0.280,0.290,0.300,0.310,0.320,0.330,0.340,0.350,0.360,00007800
     *0.370,0.380,0.390,0.400,0.410,0.420,0.430,0.440,0.450,0.460,0.470,00007900
     *0.480,0.490,0.500,50*0.0/                                         00008000
c                                                                       00008800
c    sezione d'urto della reazione deuterio su trizio                   00008900
c                                                                       00009000
      data  sig/1.0e-4,4.3e-3,0.0196,0.0529,0.106,0.175,0.250,0.315,    00009100
     *0.367,0.394,0.399,0.387,0.367,0.339,0.317,0.286,0.262,0.236,0.215,00009200
     *0.199,0.181,0.167,0.153,0.142,0.133,0.122,0.114,0.106,0.100,0.095200009300
     *,0.0911,0.0873,0.0838,0.0806,0.0775,0.0743,0.0713,0.0686,0.0659,  00009400
     *0.0635,0.0611,0.0589,0.0568,0.0549,0.0530,0.0513,0.0498,0.0483,   00009500
     *0.0469,0.0455,50*0.0/                                             00009600
c                                                                       00009700
c    perdita di energia del deuterio nel titanio e nel trizio           00009800
c                                                                       00009900
      data  cdedx/0.142,0.1929,0.2299,0.2592,0.2835,0.3038,0.3209,      00010000
     *0.3354,0.3475,0.3577,0.3660,0.3728,0.3781,0.3821,0.3851,0.3870,   00010100
     *0.3880,0.3883,0.3879,0.3869,0.3854,0.3834,0.3811,0.3785,0.3756,   00010200
     *0.3725,0.3692,0.3658,0.3622,0.3586,0.3550,0.3513,0.3476,0.3439,   00010300
     *0.3402,0.3365,0.3329,0.3293,0.3258,0.3223,0.3189,0.3155,0.3122,   00010400
     *0.3089,0.3058,0.3027,0.2996,0.2966,0.2937,0.2909,50*0.0/          00010500
      data  tdedx/0.5959,0.8014,0.9420,1.046,1.124,1.182,1.225,1.255,   00010600
     *1.275,1.287,1.291,1.290,1.285,1.275,1.262,1.247,1.231,1.212,      00010700
     *1.193,1.173,1.152,1.132,1.111,1.090,1.069,1.048,1.028,1.008,      00010800
     *0.9886,0.9695,0.9509,0.9327,0.9150,0.8978,0.8811,0.8649,0.8491,   00010900
     *0.8338,0.8190,0.8046,0.7906,0.7771,0.7640,0.7513,0.7390,0.7270,   00011000
     *0.7155,0.7042,0.6934,0.6828,50*0.0/                               00011100
c                                                                       00011500
c **********************************************************************
      if(sml.gt.0) go to 1111
      emin=0.010
c energia del fascio in mev,fino a 0.5 mev;                             00011600
      eb=rdum(1)                                                        00011700
c carico di trizio                                                      00011800
      xt=rdum(2)                                                        00012000
c coordinate sorgente la sorgente a disco deve essere centrata  intorno
c          a queste coordinate  con la normale in direzione yyy
      xx=rdum(3)
      yy=rdum(4)
      zz=rdum(5)
      ry=rdum(6)
c **********************************************************************00013600
      fuzz=1.e-7
c  mumero di chiamate alla sub rot
      irtt=50
c              dati medi del bersaglio triziato
      z(1)=1.
      am(1)=md
      z(2)=22.
      z(3)=1.
      am(2)=47.9
      am(3)=mt
      rhome=4.80
      stoich(1)=1./(1+xt)
      stoich(2)=xt/(1+xt)
c           energia minima trasferita (ev)
       tmin=5.
      ztm=z(2)*stoich(1)+z(3)*stoich(2)
      atm=am(2)*stoich(1)+am(3)*stoich(2)
      atmrho=rhome*.6022/atm
      atdist=atmrho**(-1./3.)
c     pmax=atdist/sqrt(pie)
      m1m2=am(1)/atm
c     calcolo del mean free flight path parametri del bersaglio
c         screeening length of zbl potenzial (unita ev-anstrong)
      ffpa=.5292*.8853/(z(1)**.23+ztm**.23)
      ffpf=ffpa*atm/(z(1)*ztm*14.4*(am(1)+atm))
      epsdg=tmin*ffpf*(1+m1m2)**2/(4.*m1m2)
c     write(32,23)tmin,ffpf,m1m2,epsdg
c23   format(3x,'tmin,ffpf,m1m2,epsdg ',4e12.5/)
c            bohr straggling of de/dx. number "12" normalizes dist.
      stbohr=117.*z(1)*sqrt(ztm*atmrho)
c              atom-atom scattering parameter
      do 10 j=1,3
      do 10 i=2,3
      m2m1(j,i)=am(j)/am(i)
      ec(j,i)=4.*m2m1(j,i)/(1+m2m1(j,i))**2
      a(j,i)=.5292*.8853/(z(j)**.23+z(i)**.23)
  10  f(j,i)=a(j,i)/(z(j)*z(i)*14.41*(1+m2m1(j,i)))
c         peak of eletronic stopping (usato per calcolo di straggling)
      epeak=1.0e5*z(1)**.67*am(1)
c   potere frenante del mezzo (mev/mg/cm2)                              00016200
      do 100 i=1,npts                                                   00016100
 100  dedx(i)=cdedx(i)*48./(48.+3.*xt)+tdedx(i)*3.*xt/(48.+3.*xt)       00016303
c     calcolo del valore totale della funzione di distribuzione sml
c     questo valore serve  per il psc
      thl=-0.05
      do 1000 l=1,3601
      thl=thl+0.05
      th(l)=thl*pie/180.
      cth=cos(th(l))
      do 250 i=1,npts
      et=ed(i)+q
      b=md*mn*ed(i)/et/aux
      d=mt*ma/aux*(1.+md*q/(mt*et))
      e=sqrt(d/b-1.+cth*cth)
      en=et*b*(cth+e)**2
      g=sqrt(b*d)*e/(en/et)
 250  pl(i)=sig(i)/g/dedx(i)
      call xint(ed,pl,npts,emin,eb,xr)
      suml(l)=xr*sin(th(l))
 1000 continue
      call xint(th,suml,3601,th(1),th(3601),sml)
c     questo valore serve  per dare i pesi ai neutroni
      do 251 i=1,npts
 251  pe(i)=sig(i)/dedx(i)
      iparti=0
      ebb=eb*1000000.
 1111 if((emev*0.8).gt.emin) goto 290
c         energia fascio in ev
      ry2=ry*ry
  123 x1=(ry-2*ry*rang())
      z1=(ry-2*ry*rang())
      rr=x1*x1+z1*z1
      if(rr.gt.ry2) go to 123
      xxx=xx+x1
      yyy=yy
      zzz=zz+z1
      xxxold=xxx
      yyyold=yyy
      zzzold=zzz
c      inizio del montecarlo per lo ione che entra  con vione=1.
      e=ebb
      uione=0.
      vione=1.
      wione=0.
c   utilizzo un modello zero-dimensionale
  290 icl=1
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
c ********* free flight path ******************
      if(e.eq.ebb) then
c ********+ prima collissione
            ffpath=atdist
            p=sqrt(r1/(pie*ffpath*atmrho))
      else
        eps=e*ffpf
        eeg=sqrt(eps*epsdg)
        pmax1=ffpa/(eeg+sqrt(eeg)+.125*eeg**.1)
        p=pmax1*sqrt(r1)
        ffpath=1./(pie*pmax1*pmax1*atmrho)
      end if
c     energia persa per rallentamento elettronico
      ein=e/1000000.
      call interp(ed,dedx,npts,ein,exout)
c     trasformazione energia in ev/amstrong
      dee=exout*ffpath*45.189
c ********+ add bohr straggling perche ioni leggeri
      stdee=(r3-.5)*stbohr*sqrt(ffpath)
      if(e.le.epeak)  stdee=stdee*e/epeak
      if((dee+stdee).gt.0)  e=e-dee-stdee
      if(e.gt.ebb)e=ebb-1.0e-5
c     write(32,89) ffpath,pmax1,epsdg,ffpf
c 89  format(3x,'ffpath,pmax1,epsdg,ffpf',4e12.5/)
c     trova l'atomo con cui collidere j e i valori epsilon e b
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
c         inizia il calcolo di scattering
c  se epsilon>10, usa rutherford scattering altrimenti la magic formula
      if(eps.gt.10.) then
c       notazioni: s2=sin(theta/2)**2, ct=cos(theta) st=sin(theta)
              s2=1./(1.+(1.+b*(1.+b))*(2.*eps*b)**2)
              c2=1.-s2
      else
         icycle=0
         r=b
         rr=-2.7*log(abs(eps*b)+fuzz)
         if(rr.lt.b) goto 390
         rr=-2.7*log(abs(eps*rr)+fuzz)
         if(rr.lt.b) goto 390
         r=rr
c****************** below is zbl universal int. potential
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
         aa=2*eps*(1.+(.99229/sqe))*b**cc
         ff=(sqrt(aa**2+1)-aa)*((9.3066+eps)/(14.813+eps))
         delta=(r-b)*aa*ff/(ff+1.)
         c=(b+delta+roc)/(r+roc)
c**************** fine della formula magica****************
         c2=c*c
         s2=1-c2
      end if
c************ energia dello ione e traiettoria dopo la collisione
      ct=2*c2-1.
      st=sqrt(1.0001-ct*ct)
      cu=ct+m2m1(1,j)
      psi=atan(st/cu)
      if(psi.lt.0.) psi=psi+pie
c******************** psi e l'angolo di deflessione nel laboratorio
      dden=ec(1,j)*s2*e
      e=e-dden
c++++++++++++++++++++++++++++ calcolo nuova direzione ione
c----------  psi = nuclear scattering angle
c                      coseno di ulab
      uzero=cos(psi)
c ****** formula manuale mcnp pag.65
      call rot(uzero,uione,vione,wione,uione1,vione1,wione1,irtt)
      uione=uione1
      vione=vione1
      wione=wione1
c
c
c ++******+++++ e corrisponde all' energia attuale dello ione
c ******++++++ inizia il calcolo nucleare
      emev=e/1000000.
      if(emev.lt.emin) goto 1111
      eoldme=eold/1000000.
      call xint(ed,pe,npts,emev,eoldme,pn)
c ++++++++ probabilita  di fare un neutrone mediante reiezione
c         0.04 e il valore massimo aspettato che puo avere pn
      pmax=rang()*0.04
      if(pmax.gt.pn) go to 1111
c ****** angolo di uscita del neutrone rispetto allo ione nel c.m.
      ctn=uione1*uuu+vione1*vvv+wione1*www
      stn=sqrt(1.-ctn*ctn)
      delta=eoldme-emev
      delte=rang()*delta
      ep=emev+delte
      et=ep+q
      b=md*mn*ep/et/aux
      d=mt*ma/aux*(1.+md*q/(mt*et))
c   trasforma la direzione nelle nuove coordinate
c         ph e l'angolo nel sistema del laboratorio  0 - pie
      ph=atan(stn/(ctn+sqrt(b/d)))
      if(ph.lt.0.) ph=ph+pie
c               coseno di ulab
      uzero=cos(ph)
      ex=sqrt(d/b-1.+uzero*uzero)
      en=et*b*(uzero+ex)**2
      if(iparti.lt.51) then
         write(32,98) psi,emev
  98    format(1x,'angolo di scattering ',e12.5,' energia ione ',2e12.5)
         write(32,99) ph,uzero,ctn
  99     format(1x,'ang. lab ',e12.5,' cos lb ',e12.5,' cos cm',e12.5)
         write(32,93) uione1,vione1,wione1
  93     format(1x,'coseni dir. ione ',3(1x,e12.5))
      else
      continue
      end if
c***********************************************************************
      erg=en
c ****** formula manuale mcnp pag.65
      call rot(uzero,uione1,vione1,wione1,uuu,vvv,www,irtt)
      return
      end
      subroutine rot(c,uold,vold,wold,uuu,vvv,www,irt)
c        sample a direction uuu,vvv,www at an angle arccos(c) from
c        uold,vold,wold and at an azimuthal angle sampled uniformly.
c
      implicit double precision (a-h,o-z)
c
      if(abs(c).ge.1.)go to 30
   10 t1=2.*rang()-1.
      t2=2.*rang()-1.
      r=t1**2+t2**2
      if(r.gt.1.)go to 10
      r=sqrt((1.-c**2)/r)
      t1=t1*r
      t2=t2*r
      if(abs(wold).gt..9)go to 20
      s=sqrt(uold**2+vold**2)
      t=1./s
      uuu=uold*c+(t1*uold*wold-t2*vold)*t
      vvv=vold*c+(t1*vold*wold+t2*uold)*t
      www=wold*c-t1*s
c
c        renormalize every 50 calls to prevent error buildup.
      irt=irt-1
      if(irt.ne.0)return
      irt=50
      s=1./sqrt(uuu**2+vvv**2+www**2)
      uuu=uuu*s
      vvv=vvv*s
      www=www*s
      return
c
c        special handling for the case of exceptionally large wold.
   20 s=sqrt(uold**2+wold**2)
      t=1./s
      uuu=uold*c+(t1*uold*vold+t2*wold)*t
      vvv=vold*c-t1*s
      www=wold*c+(t1*wold*vold-t2*uold)*t
      return
c
c        special handling for abs(c)=1. or more.
   30 if(abs(c).gt.1.0001)go to 40
      uuu=c*uold
      vvv=c*vold
      www=c*wold
      return
   40 write(32,50)
   50 format(5x,'coseno di scattering >1'/)
      stop
      end
c subroutine per l'interpolazione
      subroutine interp(x,y,npts,xin,yout)
      implicit double precision (a-h,o-z)
c ----------------------------------------------------------------------
c
      dimension x(npts),y(npts)
      i=1
      if(x(i)-xin)2,10,12
   2  do 1 i=2,npts
      if(x(i)-xin)1,10,11
   1  continue
      go to 12
  11  yout=y(i-1)+(y(i)-y(i-1))/(x(i)-x(i-1))*abs(x(i)-xin)
      return
  10  yout=y(i)
      return
  12  write(32,100)
 100  format(20x,'errore interpolazione '/)
      stop
      end
c subroutine per l'integrazione numerica
      subroutine xint(e,f,npts,e1,e2,sum)
      implicit double precision (a-h,o-z)
c
c ----------------------------------------------------------------------
c
      dimension e(npts),f(npts)
      sum=0
      l=1
      if(e2-e1)50,55,60
  50  a=e2
      e2=e1
      e1=a
      l=-1
  60  i=1
      if(e(i)-e1)2,10,12
   2  do 1 i=2,npts
      if(e(i)-e1)1,10,11
   1  continue
      go to 12
  11  call interp(e,f,npts,e1,fout)
      sum=sum+(e(i)-e1)*(f(i)+fout)*0.5
  10  kmin=i
      j=1
      if(e(j)-e2)3,13,12
   3  do 4 j=2,npts
      if(e(j)-e2)4,13,15
   4  continue
      go to 12
  15  call interp(e,f,npts,e2,fout)
      sum=sum+(e2-e(j-1))*(f(j-1)+fout)*0.5
      j=j-1
  13  kmax=j-1
      if(kmin.gt.kmax)go to 6
      do 5 k=kmin,kmax
   5  sum=sum+(e(k+1)-e(k))*(f(k+1)+f(k))*0.5
   6  sum=sum*l
  55  return
  12  write(32,100)
 100  format(20x,'errore integrale'/)
      stop
      end
      subroutine srcdx
c
      implicit double precision (a-h,o-z)
c
c        code name and version number.
      character kod*8,ver*5
      parameter (kod='mcnp',ver='4a')
c
c        processor-dependent named constants.
c        mdas is the initial size of dynamically allocated storage.
c        on systems where memory adjustment is not available, set mdas
c        large enough for your biggest problem.
      parameter (mdas=8000000)
      parameter (ndp2=2,huge=1d37)
c
c        array dimensions.  i/o unit numbers.  general constants.
      parameter (maxf=16,maxi=34,maxv=19,maxw=3,mcoh=55,mcpu=32,minc=21,
     1 mink=200,mipt=3,mjsf=9,mkft=9,mktc=22,mlgc=100,mpb=5,mpng=21,
     2 mseb=301,mspare=3,mtop=49,mwng=25,mxdt=20,mxdx=5,mxlv=10,
     3 nbmx=100,ncolor=10,ndef=14,novr=5,nptr=13,nsp=602,nsp12=nsp+12,
     4 ntp=201,nspt=nsp+ntp+7,iui=31,iuo=32,iur=33,iux=34,iud=35,iub=60,
     5 iup=37,ius=38,iu1=39,iu2=40,iusw=41,iusr=42,iusc=43,iuc=44,
     6 iut=45,iuz=46,iuk=47,iu3=48,iu4=49,iupw=50,iupc=51,
     7 aneut=1.008664967d0,avogad=6.022043446928244d23,
     8 avgdn=1.d-24*avogad/aneut,dfdmp=-15.,dftint=100.,
     9 fscon=137.0393d0,hsll=1.e-30,one=1.d0,pie=3.1415926535898d0,
     9 planck=4.135732d-13,slite=299.7925d0,third=one/3.d0,zero=0.)
c
c ----------------------------------------------------------------------
c
c
      common /tables/ ebl(16),gpt(mipt),qfiss(23),rkt(mtop),talb(8,2),
     1 vco(mcoh),vic(minc),wco(mcoh),
     2 jrwb(16,mipt),jsf(mjsf),mfiss(22),nvs(maxv),itty,jtty
c
c        character common -- character variables and arrays.
      character aid*80,aid1*80,aids*80,chcd*10,exms*80,hbln(maxv,2)*3,
     1 hblw(maxw)*3,hcs(2)*7,hdpath*80,hdpth*80,hft(mkft)*3,hfu(2)*11,
     2 hip*(mipt),hmes*69,hnp(mipt)*8,hovr*8,hsd(2)*10,hsub*6,ibin*8,
     3 idtm*19,idtms*19,ilbl(8)*8,iname*8,klin*80,kods*8,ksf(29)*3,
     4 loddat*8,lods*8,msub(ndef)*8,probid*19,probs*19,rfq(11)*57,
     5 ufil(3,6)*11,vers*5
      common /charcm/ aid,aid1,aids,chcd,exms,hbln,hblw,hcs,hdpath,
     1 hdpth,hft,hfu,hip,hmes,hnp,hovr,hsd,hsub,ibin,idtm,idtms,ilbl,
     2 iname,klin,kods,ksf,loddat,lods,msub,probid,probs,rfq,ufil,vers
c        isub:  names of files
      character*8 inp,outp,runtpe,mctal,wssa,ptrac,comout,srctp,plotm,
     1 rssa,xsdir,com,dumn1,dumn2,isub(ndef)
      common /charcm/ inp,outp,runtpe,mctal,wssa,ptrac,comout,srctp,
     1 plotm,rssa,xsdir,com,dumn1,dumn2
      equivalence (isub,inp)
c
c ----------------------------------------------------------------------
c
c
c        whole-block declarations.
      parameter (nfixcm=maxi+3*maxv+mtop+mipt*(24+mxdt+7*mxdx)+nsp+78,
     1 lfixcm=3*mxdt+mink+11*mipt+2*maxv+2*maxf+255)
      dimension gfixcm(nfixcm),jfixcm(lfixcm)
      equivalence (bbrem,gfixcm),(ibad,jfixcm)
      parameter (nvarcm=102*mipt+203,lvarcm=mipt*(1+8*mxdx)+mcpu+326)
      dimension gvarcm(nvarcm),jvarcm(lvarcm)
      equivalence (cpk,gvarcm),(idum,jvarcm)
      parameter (nephcm=29,lephcm=nptr+novr+ncolor+59)
      dimension gephcm(nephcm),jephcm(lephcm)
      equivalence (cp0,gephcm),(ichan,jephcm)
      logical lockl
c
********************  statically allocated common  *********************
*                                                                      *
c        fixed common -- constant after the problem is initiated.
      common /fixcom/ bbrem(mtop),bcw(2,3),bnum,calph(maxi),
     1 ddg(mipt,mxdt),ddx(mipt,2,mxdx),dxw(mipt,3),dxx(mipt,5,mxdx),
     1 ecf(mipt),efac,emcf(mipt),emx(mipt),enum,espl(mipt,10),fnw,rim,
     2 hsb(nsp),
     2 rssp,rnok,rnfb,rnfs,rngb,rngs,rnmult,snit,srv(3,maxv),
     2 tco(mipt),thgf(0:50),wc1(mipt),wc2(mipt),wwg(7),wwp(mipt,5),
     3 zfixcm,
     3 ibad,icw,idefv(maxv),ides,idrc(mxdt),ifft,igm,igww,ikz,img,imt,
     4 indt,ink(mink),iphot,iplt,ipty(mipt),isb,issw,istrg,
     4 ivdd(maxf),ivdis(maxv),ivord(maxf),jgm(mipt),jtlx,junf,kf8,kfl,
     5 knods,knrm,kpt(mipt),ktls,kufil(2,6),ldr,lfcdg,lfcdj,
     5 locdt(2,mxdt),lvcdg,lvcdj,lxs,mbnk,mcal,mct,mgegbt(mipt),
     6 mgm(mipt+1),mgww(mipt+1),mix,mjss,mlaj,mlja,mrkp,mrl,msd,msrk,
     7 mtasks,nlat,nsrc,
     6 mww(mipt+1),mxa,mxafs,mxe,mxe1,mxf,mxj,mxt,mxtr,mxxs,ndet(mipt),
     7 ndnd,ndtt,ndx(mipt),nee,ngww(mipt),nhb,nilr,nilw,nips,niss,njsr,
     8 njss,nkxs,nlev,nlja,nmat,nmxf,nnpos,nocoh,nord,np1,npikmt,npn,
     9 nrcd,nrss,nsph,nsr,nsrck,nstrid,ntal,nvec,nww(mipt),nxnx
c        offsets for virtual arrays in dynamically allocated storage.
      common /fixcom/ lara,lasp,lawc,lawn,lcmg,lden,ldrs,ldxp,leaa,lear,
     1 leba,lebd,lebt,lech,ledg,leee,leek,legg,lelp,lesa,lewg,lfim,lfme,
     1 lfmg,lfor,lfrc,lfst,lftt,lgmg,lgvl,lgwt,lpbr,lpbt,lpkn,lpmg,lpru,
     2 lpxr,lqav,lqax,lqcn,lrho,lscf,lsmg,lspf,lsqq,lsso,ltbt,ltds,ltmp,
     2 ltrf,ltth,lvcl,lvec,lvol,lwwe,lwwf,lwwk,lxnm,lipa,lipt,liss,litd,
     3 lixl,liza,ljar,ljmd,ljpt,ljsc,ljss,ljtf,ljun,ljvc,ljxs,lkcp,lksd,
     3 lkst,lksu,lktp,lkxs,llaf,llat,llca,llfc,llft,llja,llme,llmt,llct,
     4 llph,llst,llsc,lmat,lmbd,lmbi,lmfl,lncl,lnmt,lnpq,lnsb,lnsf,lnty,
     5 lnxs,lddm,lddn,ldec,ldxc,ldxd,lflx,lfso,lgww,lpac,lpan,lpcc,lptr,
     5 lpts,lpwb,lrkp,
     6 ltfc,lwns,lise,ljfq,llaj,llcj,llse,lndp,lndr,lnpw,lnsl,lntb,lscr,
     7 ldrc,lemi,lfdd,lgnr,lpik,lrng,lrtc,ltgp,lifl,lpc2,lixc,ljfl,ljft,
     8 ljmt,lkmt,lktc,lkxd,llbb,llgtsk,lljtsk,lnhs,lshs,lstt,ltal,lgbn,
     9 lbnk,lxss,lexs,mfixcm
c
c        variable common -- variable but required for a continue run.
c        arrays that are backed up when a track is lost.
      common /varcom/ cpk,cts,dbcn(20),dmp,eacc(4),febl(2,16),osum(3),
     1 coll(mipt),
     1 osum2(3,3),pax(6,16,mipt),prn,rani,ranj,rdum(50),rijk,rkk,
     2 rlt(2,4,2),rltp(2,4),rnr,rsum(2,4),rsum2(2,2,4),skef(3,3),
     2 smul(3),sumk(3),tmav(mipt,3),
     3 twac,twss,wcs1(mipt),wcs2(mipt),wgts(2),wt0,wssi(7),
     4 zvarcm,
     5 idum(50),inif,ist,ist0,ixak,ixak0,jrad,kcsf,kct,kcy,knod,
     6 ksdef,kcz,lost(2),nbal(mcpu),nbhwm,nbov,nbt(mipt),
     7 ndmp,nerr,netb(2),nfer,npc(20),npd,npnm,npp,nppm,nps,npsout,npsr,
     8 nqss,nqsw,nrnh(3),nrrs,nrsw,nsa,nsa0,nskk,nss,nss0,nssi(8),ntc,
     9 ntc1,ntss,nwer,nwsb,nwse,nwsg(2),nwst,nwws(2,99),
     1 nziy(8,mxdx,mipt),
     2 mvarcm
c
c        ephemeral common -- not needed after the current run.
      common /ephcom/ cp0,cp1,cp2,cpa,ctme,fpi,freq,
     1 ssb(10),tdc,tlc,trm,tsc,wnvp(4),xhom,xnum,yhom,
     2 zephcm,
     3 ichan,ics,idmp,ifile,iln,iln1,inform,iovr,inpd,iptr,iptra(nptr),
     4 irup,itask,iterm,itfxs,itotnu,iuou,jchar,jfcn,jgf,jgxa(2),
     5 jgxo(2),jovr(novr),jtfc,jvp,kbp,kcolor(ncolor+6),kdbnps,kmplot,
     6 komout,konrun,kprod,krflg,krtm,ksr,ktfile,ldq,lfatl,lfll,locki,
     7 lspeed,mcolor,mdc,mmkdb,mnk,mpc,mrm,nde,nkrp,nomore,nrc,nst,
     8 ntasks,
     9 mephcm,lockl,mynum
      equivalence (kddm,lddm),(kddn,lddn),(kdec,ldec),(kdxc,ldxc),
     1 (kdxd,ldxd),(kflx,lflx),(kgww,lgww),(kpac,lpac),(kpan,lpan),
     2 (kpcc,lpcc),(kpwb,lpwb),(kwns,lwns),(kise,lise),(klaj,llaj),
     3 (klcj,llcj),(klse,llse),(kndp,lndp),(kndr,lndr),(kdrc,ldrc),
     4 (kfdd,lfdd),(kgnr,lgnr),(kpik,lpik),(krtc,lrtc),(ktgp,ltgp),
     5 (kifl,lifl),(kpc2,lpc2),(kjfl,ljfl),(kjft,ljft),(kktc,lktc),
     6 (ktal,ltal),(kgbn,lgbn),(kbnk,lbnk),(knhs,lnhs),(kshs,lshs),
     7 (kstt,lstt)
c
      common /pblcom/ xxx,yyy,zzz,uuu,vvv,www,erg,wgt,tme,vel,dls,
     1 dxl,dtc,elc(mipt),fiml(mipt),fismg,wtfasv,rnk,spare(mspare),
     2 zpblcm,
     3 xxx9(mpb),yyy9(mpb),zzz9(mpb),uuu9(mpb),vvv9(mpb),www9(mpb),
     4 erg9(mpb),wgt9(mpb),tme9(mpb),vel9(mpb),dls9(mpb),dxl9(mpb),
     5 dtc9(mpb),elc9(mpb,mipt),fiml9(mpb,mipt),fismg9(mpb),
     6 wtfas9(mpb),rnk9(mpb),spare9(mpb,mspare),
     7 zpb9cm(mpb),
     1 npa,icl,jsu,ipt,iex,node,idx,ncp,jgp,lev,iii,jjj,kkk,iap,
     2 mpblcm,
     3 npa9(mpb),icl9(mpb),jsu9(mpb),ipt9(mpb),iex9(mpb),node9(mpb),
     4 idx9(mpb),ncp9(mpb),jgp9(mpb),lev9(mpb),iii9(mpb),jjj9(mpb),
     5 kkk9(mpb),iap9(mpb),
     6 mpb9cm(mpb)
      parameter (npblcm=mspare+2*mipt+16,lpblcm=14)
      dimension gpblcm(npblcm+1),jpblcm(lpblcm+1),
     1 gpb9cm(mpb,npblcm+1),jpb9cm(mpb,lpblcm+1)
      equivalence (xxx,gpblcm),(npa,jpblcm),(xxx9,gpb9cm),(npa9,jpb9cm)
c
      common /tskcom/ amfp,ang(3),cbwf,cmult,colout(2,11),cpv,ddet,
     1 colltc(mipt),
     1 deb,dti(mlgc),eacctc(2),eg0,ergace,febltc(2,16),paxtc(6,16,mipt),
     1 pfp,ple,pmf,psc,qpl,ranb,ranitc,ranjtc,rans,rijktc,rlttc(2,4,2),
     2 rnrtc,rnrtc0,sff(3,maxf),siga,smultc(3),ssr,stp,sumktc(3),
     2 tmavtc(mipt,3),totgp1,totm,tpd(7),tpp(20),ttn,udt(10,0:mxlv),
     3 udtr(10*mxlv+10),udts(10*mxlv+10),udtt(10*mxlv+10),uold,vold,
     3 vtr(3),wcs1tc(mipt),wcs2tc(mipt),wgtstc(2),wold,ycn,
     4 ztskcm,
     4 iax,ibc,ibe,ibs,ibt,ibu,iclp(4,0:mxlv),idet,iet,imd,ipsc,irt,
     5 isic(maxf),ital,iti(mlgc),ixcos,ixre,jap,jbd,jbnk,jev,jtls,kdb,
     5 jlock,kqss,ktask,levp,lgc(mlgc+1),lsb,mbb,mkc,mpan,nbhwtc,nbnk,
     6 nbttc(mipt),nch(mipt),ngp,nlaj,nlse,nlt,npb,npsrtc,
     6 npstc,nrnhtc(3),nter,ntii,ntx,ntyn,nziytc(8,mxdx,mipt),
     9 mtskcm
      parameter (ntskcm=102*mipt+40*mxlv+3*maxf+mlgc+184,
     1 ltskcm=mipt*(2+8*mxdx)+4*mxlv+2*mlgc+maxf+50)
      dimension gtskcm(ntskcm),jtskcm(ltskcm),udt1(10*mxlv+10)
      equivalence (amfp,gtskcm),(iax,jtskcm),(udt,udt1)
c
********************  dynamically allocated common  ********************
*                                                                      *
      common /dac/ das(mdas/ndp2)
c
c        fixed dynamically allocated common.
      dimension aaafd(2),ara(1),asp(1),awc(1),awn(1),cmg(1),den(1),
     1 drs(1),dxcp(0:mxdx,mipt,1),eaa(1),ear(1),
     1 eba(mtop,1),ebd(mtop,1),ebt(mtop,1),
     1 ech(mpng,mwng,1),edg(1),eee(1),eek(1),egg(maxi,1),elp(mipt,1),
     2 esa(1),ewwg(1),fim(mipt,1),fme(1),fmg(1),for(mipt,1),frc(1),
     2 fst(1),ftt(1),gmg(1),gvl(1),gwt(1),pbr(1),pbt(1),pkn(1),pmg(1),
     3 pru(1),pxr(1),qav(1),qax(mipt,1),qcn(1),rho(1),scf(1),smg(1),
     3 spf(4,2),sqq(12,1),sso(1),tbt(1),tds(1),tmp(1),trf(17,0:1),
     4 tth(1),vcl(3,7,1),vec(3,1),vol(1),wwe(1),wwf(1),wwk(1),xnm(1),
     4 iiifd(1),ipan(1),iptal(8,5,1),iss(1),itds(1),ixl(3,1),iza(1),
     5 jasr(1),jmd(1),jptal(8,1),jscn(1),jss(1),jtf(8,1),jun(1),jvc(1),
     6 jxs(32,1),kcp(1),ksd(21,1),kst(1),ktp(mipt,1),kxs(1),laf(3,3),
     6 ksu(1),
     7 lat(2,1),lca(1),lfcl(1),lft(mkft,1),lja(1),lme(mipt,1),lmt(1),
     8 locct(mipt,1),locph(1),locst(mipt,1),lsc(1),mat(1),mbd(1),mbi(1),
     9 mfl(3,1),ncl(1),nmt(1),npq(1),nsb(1),nsf(1),nty(1),nxs(16,1)
      equivalence (das,aaafd,ara,asp,awc,awn,cmg,den,drs,dxcp,eaa,ear,
     1 eba,ebd,ebt,ech,edg,eee,eek,egg,elp,esa,ewwg,fim,fme,fmg,for,frc,
     2 fst,ftt,gmg,gvl,gwt,pbr,pbt,pkn,pmg,pru,pxr,qav,qax,qcn,rho,scf,
     3 smg,spf,sqq,sso,tbt,tds,tmp,trf,tth,vcl,vec,vol,wwe,wwf,wwk,xnm,
     4 iiifd,ipan,iptal,iss,itds,ixl,iza,jasr,jmd,jptal,jscn,jss,jtf,
     5 jun,jvc,jxs,kcp,ksd,kst,ktp,kxs,laf,lat,lca,lfcl,lft,lja,lme,lmt,
     6 ksu,
     6 locct,locph,locst,lsc,mat,mbd,mbi,mfl,ncl,nmt,npq,nsb,nsf,nty,
     7 nxs)
c
c        variable dynamically allocated common.
      dimension aaavd(1),ddm(2,1),ddn(24,1),dec(3,1),dxc(3,1),
     1 dxd(mipt,24,mxdx),flx(1),fso(1),gww(2,9,1),pac(mipt,10,1),
     2 pan(2,6,1),pcc(3,1),pwb(mipt,19,1),rkpl(19,1),shsd(nspt,1),
     3 stt(ntp,1),tfc(6,20,1),wns(2,1),
     4 iiivd(1),isef(2,1),jfq(8,0:1),laj(1),lcaj(1),lse(1),ndpf(6,1),
     5 ndr(1),nhsd(nsp12,1),npsw(1),nsl(2+4*mipt,1),ntbb(4,1)
      equivalence (das,aaavd,ddm,ddn,dec,dxc,dxd,flx,fso,gww,pac,pan,
     1 pcc,pwb,rkpl,tfc,wns,shsd,stt,iiivd,isef,jfq,laj,lcaj,lse,ndpf,
     2 ndr,nhsd,npsw,nsl,ntbb)
c
c        ephemeral dynamically allocated common.
      dimension scr(1),drc(16,1),emi(1),fdd(2,1),genr(1),pik(1),ptr(1),
     1 pts(1),rng(1),rtc(10,1),tgp(1),ifl(1),ipac2(1),ixc(61,1),jfl(1),
     2 jft(1),jmt(1),kmt(3,1),ktc(2,1),kxd(1),lbb(1)
      equivalence (das,scr,drc,emi,fdd,genr,pik,ptr,pts,rng,rtc,tgp,ifl,
     1 ipac2,ixc,jfl,jft,jmt,kmt,ktc,kxd,lbb)
c
c        tallies, bank, and cross-sections in dac.
      dimension tal(1),gbnk(1),ibnk(1),exs(1)
      real xss(1)
      equivalence (das,tal,gbnk,ibnk,xss,exs)
c
c        dynamically allocated common for imcn.
      dimension jtr(1),awt(1),bbv(1),prb(1),rtp(1),sfb(1),
     1 ipnt(2,mktc,0:1),jasw(1),kaw(1),kdr(1),kdup(1),kmm(1),ktr(1),
     2 lxd(mipt,1),mfm(1),nlv(1),nslr(2+4*mipt,1),
     3 aras(2,1),atsa(2,1),rscrn(2,1),rsint(2,1),scfq(5,1),vols(2,1),
     4 iint(1),icrn(3,1),ljav(1),ljsv(1),lsat(1)
      equivalence (das,jtr,awt,bbv,prb,rtp,sfb,ipnt,jasw,kaw,kdr,kdup,
     1 kmm,ktr,lxd,mfm,nlv,nslr,
     2 aras,atsa,rscrn,rsint,scfq,vols,iint,icrn,ljav,ljsv,lsat)
c
c        dynamically allocated common for plot.
      dimension amx(4,4,1),coe(6,2,1),crs(1),jst(2,1),kcl(102,1),kfm(1),
     1 lcl(1),lsg(1),ncs(1),plb(1),qmx(3,3,2,1),zst(1)
      equivalence (das,amx,coe,crs,jst,kcl,kfm,lcl,lsg,ncs,plb,qmx,zst)
c
c        dynamically allocated common for mcplot.
      dimension ab1(1),ab2(1),erb(1),mcc(1),ord(1),xcc(1),ycc(1)
      real xrr(1),yrr(1)
      equivalence (das,ab1,ab2,erb,mcc,ord,xcc,xrr,ycc,yrr)
c
c ----------------------------------------------------------------------
c
      common /mario/ dedx(100),emin,eb,sml,uione,vione,wione,et,b,d
c **********************************************************************
c              dati standard
c **********************************************************************
      dimension sig(100),pl(100),ed(100)                                00007100
      double precision md,mt,mn,ma
      data md/2.01410219/
      data mt/3.01602994/
      data mn/1.00866544/
      data ma/4.00260361/
      data aux/25.20734546/
      data q/17.589/
c                                                                       00005300
c i dati da dare in input direttamente in questo programma sono:        00005400
c    il carico di trizio nel titanio in atomi per atomo di titanio      00005700
c    l'energia dei deutoni-tritoni in mev,fino a 0.5 mev;               00005900
c **********************************************************************00006800
c              dati presi dai giapponesi                                00006900
c **********************************************************************00007000
      data npts/50/                                                     00007400
      data  ed/0.010,0.020,0.030,0.040,                                 00007500
     *0.050,0.060,0.070,0.080,0.090,0.100,0.110,0.120,0.130,0.140,      00007600
     *0.150,0.160,0.170,0.180,0.190,0.200,0.210,0.220,0.230,0.240,0.250,00007700
     *0.260,0.270,0.280,0.290,0.300,0.310,0.320,0.330,0.340,0.350,0.360,00007800
     *0.370,0.380,0.390,0.400,0.410,0.420,0.430,0.440,0.450,0.460,0.470,00007900
     *0.480,0.490,0.500,50*0.0/                                         00008000
c                                                                       00008800
c    sezione d'urto della reazione deuterio su trizio                   00008900
c                                                                       00009000
      data  sig/1.0e-4,4.3e-3,0.0196,0.0529,0.106,0.175,0.250,0.315,    00009100
     *0.367,0.394,0.399,0.387,0.367,0.339,0.317,0.286,0.262,0.236,0.215,00009200
     *0.199,0.181,0.167,0.153,0.142,0.133,0.122,0.114,0.106,0.100,0.095200009300
     *,0.0911,0.0873,0.0838,0.0806,0.0775,0.0743,0.0713,0.0686,0.0659,  00009400
     *0.0635,0.0611,0.0589,0.0568,0.0549,0.0530,0.0513,0.0498,0.0483,   00009500
     *0.0469,0.0455,50*0.0/                                             00009600
c **********************************************************************
c  questo vale per una sorgente con simmetria azimutale
c
      cth=uione*uuu+vione*vvv+wione*www
      do 250 i=1,npts
      te=ed(i)+q
      bb=md*mn*ed(i)/te/aux
      dd=mt*ma/aux*(1.+md*q/(mt*te))
      e=sqrt(dd/bb-1.+cth*cth)
      en=te*bb*(cth+e)**2
      g=sqrt(bb*dd)*e/(en/te)
 250  pl(i)=sig(i)/g/dedx(i)
      call xint(ed,pl,npts,emin,eb,xr)
      smd=xr
c  calcola energia del neutrone con i dati che vengono da source
      ex=sqrt(d/b-1.+cth*cth)
      en=et*b*(cth+ex)**2
c***********************************************************************
      erg=en
      psc=smd/sml
c   enea
c     write(32,*) smd,psc
c   enea
      return
      end

