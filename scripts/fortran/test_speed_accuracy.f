C data file. onset early, y early, onset late, y late, coherence, stim
C type (face/car), RT, correct vs error.
C     include '/opt/intel/Compiler/11.1/064/mkl/include/mkl_vsl.fi'
      implicit double precision (a-h,o-z)
      include "mkl.fi"
      double precision X(39),S(39),aq(7),bq(7),naq(7),nbq(7)
      double precision rt(2000),yi(2000),cond(2000),gu(20000),y(39)
c mcond refers to difficulty condition
c minstr refers to speed/acc condition
      integer mch(2000),mcond(2000),minstr(2000),con(2000)
      character(95) d1,d2,ff,d3,d4
      character(8) aa
      character(9) cc
c     d1="/u/russ/diff4/pb1-fast-dm/subj00"
c     d4="/u/russ/diff4/pb1-fast-dm/subj0"
      d1="subj00"
      d4="subj0"
      d2=".speed.acc.test.csv"
      d1=adjustl(d1)
      d4=adjustl(d4)
      d2=adjustl(d2)
      d1=trim(d1)
      d4=trim(d4)
      d2=trim(d2)
c n is the number of trials
      n=1200
      nn=2000
      mrun=9000
      iseed=12333
      call ranunif(gu,mrun,iseed)
      mmc=128
      call OMP_SET_NUM_THREADS(mmc)
      ict=1
c k is subj #
      do 7 k=1,1
C     if(k.ne.22.and.k.ne.28.and.k.ne.43.and.k.ne.50)go to 7
      write(d3,"(i2)")k
      d3=adjustl(d3)
      ff=trim(d1)//trim(d3)//trim(d2)
      if(k.gt.9)ff=trim(d4)//trim(d3)//trim(d2)
      print*,trim(ff)
      open(1,file=trim(ff))
      do 1 i=1,n
      read(1,*)ich,rr,aa,cc
c      print*,ich,rr,aa,cc
      rt(i)=1000.*rr
      mch(i)=ich
C change this to specific condition names
      if(aa.eq."high")mcond(i)=1
      if(aa.eq."low")mcond(i)=2
      if(aa.eq."vlow")mcond(i)=3
      if(aa.eq."nonword")mcond(i)=4
C these are speed/accuracy conds
      if(cc.eq."accuracy")minstr(i)=1
      if(cc.eq."speed")minstr(i)=2
C     write(*,"(f8.3,2i5,f8.3,i5)")yi(i),mcond(i),con(i),rt(i),mch(i),minstr(i)
    1 continue
      OPEN(14,STATUS='SCRATCH')
      OPEN(15,STATUS='SCRATCH')
      OPEN(16,STATUS='SCRATCH')
      OPEN(17,STATUS='SCRATCH')
C a,ter,eta,sz,4 v's For jm use x1=.15
C NEED TO READ IN each RT and each y value and choice.
C Need to pass an array of choice, RT for each condition - internal
C files
      write(14,*)n,(mcond(i),i=1,nn)
C     write(*,*)n,(mcond(i),i=1,nn)
      write(15,*)n,(rt(i),i=1,nn)
C     write(*,*)n,(rt(i),i=1,nn)
      write(16,*)n,(mch(i),i=1,nn)
C     write(*,*)n,(mch(i),i=1,nn)
      write(17,*)n,(minstr(i),i=1,nn)
      rewind(14)
      rewind(15)
      rewind(16)
      rewind(17)
C initial points for estimation
C boundary acc and speed
      x(1)=.11
      x(2)=.11
C ter
      x(3)=.24
c eta
      x(4)=.13
c sa (was sz)
      x(5)=.03
C modify z to be a proportion of a
      x(6)=0.5
c st
      x(7)= .20
c po (contam)
      x(8)= .05
C set of drift rates
      x(9)= .32
      x(10)= .20
      x(11)= .1
      x(12)=-.35
      NV=12
      el1=990000.
      el2=0.
      ict=1
      xinc=.04
      do 27 ij=1,3000
      if(ij.gt.1500)xinc=.01
      if(ict.gt.8500)then
      iseed=iseed+1
      call ranunif(gu,mrun,iseed)
      ict=1
      endif
      do 2 i=1,nv
      y(i)=x(i)+(.5-gu(ict))*x(i)*xinc
      ict=ict+1
    2 continue
      y(8)=x(8)+(.5-gu(ict))*xinc
      ict=ict+1
      el2=fofs(nv,y)
      ra=exp(-el2+el1)
      rnd=gu(ict)
      ict=ict+1
      if(mod(ij,100).eq.1)write(*,"(i5,5f12.3)")ij,el2,el1,ra,rnd
      if(mod(ij,100).eq.1)write(*,"(9f8.3)")(x(i),i=1,nv)
      if(ij.eq.3000)write(122,"(11f8.3,f15.5)")(x(i),i=1,nv),el2
      if(ra.gt.rnd)then
      do 11 i=1,nv
   11 x(i)=y(i)
      write(132,"(2i5,9f7.3,f15.5)")k,ij,(x(i),i=1,nv),el2
      el1=el2
      endif
   27 continue

      ITMAX=400
C     do 4 j=1,2
      CRIT=1.0E-10
C     if(j.eq.2)ITMAX=800
      ITRACE=20
      IOPT=ITMAX
      DO 5 I=1,NV
    5 S(I)=X(I)/20.0
C     s(3)=.050
C     s(2)=.005
C     s(4)=.001
C     CALL SIMPLX (X,S,CRIT,ITMAX,ITRACE,IOPT,NV)
    4 continue
      WRITE(*,47)(X(I),I=1,NV)
C     WRITE(4,47)(X(I),I=1,NV)
   47 FORMAT(f7.4,9F8.4)
    7 CONTINUE
      STOP
      END 
      DOUBLE PRECISION FUNCTION FOFS(NV,X)
      implicit double precision (a-h,o-z)
      double precision X(NV),r(11),y(11),rs(11)
      double precision rt(2000),yi(2000),xml(2000)
      integer mch(2000),mcond(2000),con(2000)
      integer minstr(2000)
      nn=2000
      read(14,*)n,(mcond(i),i=1,nn)
      read(15,*)n,(rt(i),i=1,nn)
      read(16,*)n,(mch(i),i=1,nn)
      read(17,*)n,(minstr(i),i=1,nn)
      rewind(14)
      rewind(15)
      rewind(16)
      rewind(17)
      pxa=0.
      do 3 i=1,n
    3 if(rt(i).gt.pxa)pxa=rt(i)
      pxa=pxa/1000.
      s=.1
      if(x(8).lt.0.0D0)x(8)=0.0D0
      pz=x(8)
      m=5
      mm=1
      rmax=2.D0
      if(x(3).gt.0.640)x(3)=0.640
      terr=x(3)
      if(x(4).lt.0.01)x(4)=0.01
      if(x(4).gt.0.3)x(4)=0.3
      sc=x(4)
      if(x(7).gt..45)x(7)=.45
      if(x(7).le.0.03)x(7)=0.06
      st=x(7)
      fofs=0.
      if(x(5).lt.0.002)x(5)=0.002
C     if(.9*x(5).gt.z.or..9*x(5).gt.a-z)x(5)=min(1.8*z,1.8*(a-z))
      sg=x(5)
      xmlh=0.
!$omp parallel
!$omp do private(j,zz,v,vv,rr,chi,jj)
      do j=1,n
      kk=minstr(j)
      if(x(0+kk).lt.0.065)x(0+kk)=0.065
      if(x(0+kk).gt.0.240)x(0+kk)=0.240
      a=x(0+kk)
      z=x(6)*x(0+kk)
      jj=mcond(j)
      if(x(8+jj).gt.0.7)x(8+jj)=0.7
      if(x(8+jj).lt.-0.7)x(8+jj)=-0.7
      vv=X(8+jj)
      if(mch(j).eq.1)then
C     zz=z
      zz=x(6)
      v=-vv
      endif
      if(mch(j).eq.0)then
C     zz=a-z
      zz=1.-x(6)
      v=vv
      endif
      rr=rt(j)/1000.
C     write(*,"(4i5,2f9.3)")j,jj,con(j),mch(j),v,zz
      call cor(a,zz,v,s,terr,a1,rr,sc,m,sg,chi,st
     *,x1,x2,pz,pxa)
      xml(j)=chi
      enddo
!$omp end parallel
      xmlh=0.0D0
      do 1 i=1,n
      xmlh=xmlh+xml(i)
    1 continue
      fofs=xmlh
      RETURN
      END
      DOUBLE PRECISION FUNCTION FFC(T,PI,U,S,A,Z,XB,SC,TT,NN,KTORP)
      implicit double precision (a-h,o-z)
C     COMMON PI,U,S,A,Z,XB,SC,TT,N,KTORP     
      TT=T
      NN=11
      AA=XB-4.5*SC
      BB=XB+4.5*SC
      ZZZ=0.
      FFC=GQ(AA,ZZZ,NN,PI,U,S,A,Z,XB,SC,TT,N,KTORP)
      FFC=FFC+GQ(ZZZ,BB,NN,PI,U,S,A,Z,XB,SC,TT,N,KTORP)
      RETURN
      END
      DOUBLE PRECISION FUNCTION FC(U,PI,UU,S,A,Z,XB,SC,T,NN,KTORP)
      implicit double precision (a-h,o-z)
C     COMMON PI,UU,S,A,Z,XB,SC,T,NN,KTORP    
      xlim=.000001D0
      test=1.e-19
      B=U/S**2
      C=(PI*S/A)**2/2.D0
      D=PI*Z/A
      E=C*2.0D0/PI
      G=B*U/2.0D0
      H=Z*B
      SF=0.0D0
      IF(KTORP.EQ.1) GO TO 1
      M=8000
      DO 3 N=1,M
      R=G+C*N*N
      RR=R*T
      GG=SIN(N*D)
      FF=N*EXP(-RR)*GG/R
      SF=SF+FF
      if(abs(ff).lt.xlim*sf.and.abs(test).lt.xlim*sf)go to 1
      test=ff
    3 CONTINUE
    1 continue
      Q=E*EXP(-H)
      G=EXP(-0.5D0*((U-XB)/SC)**2)/(SQRT(2.0D0*PI)*SC)
      X=EXP(-2.0D0*H)
      EX=-2.0D0*A*B
      Y=EXP((EX/2.0D0))**2
      XX=(Y-X)/(Y-1.0D0)
      IF(Y.EQ.1) Y=Y+1.0D-10
      FB=XX-SF*Q
      FC=FB*G
      IF(KTORP.EQ.1)FC=XX*G
      RETURN
      END

      subroutine cor(aaa,zz,xxb,sss,terr,acc,r,scc,m,sg,chi,st
     *,x1,x2,pz,pxa)
      implicit double precision (a-h,o-z)
      dimension X(11),YF(11),W(11)    
      dimension rr(11),pr(11)
C     COMMON PI,U,S,A,Z,XB,SC,TI,NN,KK
      PI=4.0*ATAN(1.0)      
      dt=.0001
      m=5
      nn=1
      PI=4.0*ATAN(1.0)
      TER=terr
      sc=scc
      S=sss
      nsz=15
      nnsz=1+nsz/2
      gw=1./(1.0*nsz)
      gww=gw*gw
      pzz=pz/(pxa)
C     A=aaa
      XB=xxb
      chi=0.0
      ss=0.
      ts=r-TER
      kkk=0
      t3=2.
      accc=0.0D0
C     Z=zz-nnsz*sg*gw
      a=aaa-nnsz*sg*gw
      do 5 i6=1,nsz
C     z=z+sg*gw
      a=a+sg*gw
      z=a*zz
      accc=accc+gw*ffc(t3,PI,xb,S,A,z,XB,SC,TI,NN,KKk)
    5 continue
C     t1=.2
C     t2=.5
C     write(*,"('ffc',10f9.3)")ffc(t1,PI,xb,S,A,a/2.D0,XB,SC,TI,NN,KK),
C    *t1,ffc(t3,PI,xb,S,A,a/2.D0,XB,SC,TI,NN,KK),
C    *t2,ffc(t3,PI,xb,S,A,a/2.D0,XB,SC,TI,NN,KK),t3
C     Z=zz-nnsz*sg*gw
      a=aaa-nnsz*sg*gw
      do 1 i6=1,nsz
C     z=z+sg*gw
      a=a+sg*gw
      t=ts-nnsz*st*gw
      do 3 it=1,nsz
      t=t+st*gw
C     print*,t,it,i6
C     write(*,'(2i6,9f8.3)')i6,it,t,ts,z,st,gw,r,ter
C     if(t.lt.0.001)write(*,'(3i6,9f8.3)')i6,it,nss,t,ts,z,st,gw,r,ter
      if(t.lt.0.0001)xx=pzz*accc*gww
      if(t.lt.0.0001)go to 4
C modify z to be a proportion of a
      z=a*zz
      xx=(FFC(T+dt,PI,U,S,A,Z,XB,SC,TI,NN,KK)-
     *FFC(T,PI,U,S,A,Z,XB,SC,TI,NN,KK))/dt
C     write(*,"(i4,8f9.5)")i6,xx,pz,pzz
    4 continue
      y=y+xx*gww*(1.D0-pz)+pzz*accc*gww
    3 continue
    1 continue
C     if(nss.eq.0)print*,nss,ts,r
      if(y.gt.0.D0)chi=-log(y)
C     write(*,'(13f6.3)')pxa,(y(i),i=1,5),acc,(x(i),i=1,6)
      return
      END 
      subroutine ranunif(gu,nit,seed)
      USE MKL_VSL_TYPE
      USE MKL_VSL
      double precision gu(21000),q1,q2
      integer brng,method,seed
      TYPE (VSL_STREAM_STATE) :: stream
      brng=VSL_BRNG_MCG31
      method=0
      m=1
      q1=0.D0
      q2=1.D0
      ierr=vslnewstream( stream, brng,  seed )
        ierr=vdrnguniform( method, stream, nit, gu, q1,q2)
C     print*,idum,kg(1),kg(2),kg(3),kg(4),"rand"
      errcode=vsldeletestream( stream )
      return
      end

      DOUBLE PRECISION FUNCTION GQ(A,B,N,PI,U,S,AG,Z,XB,SC,TT,NN,KTORP)     
      implicit double precision (a-h,o-z)
      DIMENSION PT(100),WT(100),WZ(9),P(20),W(20)   
      DATA PT( 1),PT( 2),PT( 3),PT( 4),PT( 5),PT( 6),PT( 7),PT( 8),      
     *   PT( 9),PT( 10)/-5.773502691896259E-01,-7.745966692414834E-01,   
     *   -8.611363115940526E-01,-3.399810435848563E-01,    
     *   -9.061798459386640E-01,-5.384693101056832E-01,    
     *   -9.324695142031520E-01,-6.612093864662646E-01,    
     *   -2.386191860831969E-01,-9.491079123427586E-01/    
      DATA PT(11),PT(12),PT(13),PT(14),PT(15),PT(16),PT(17),PT(18),      
     *   PT(19),PT( 20)/-7.415311855993942E-01,-4.058451513773972E-01,   
     *   -9.602898564975362E-01,-7.966664774136267E-01,    
     *   -5.255324099163290E-01,-1.834346424956498E-01,    
     *   -9.681602395076261E-01,-8.360311073266637E-01,    
     *   -6.133714327005905E-01,-3.242534234038089E-01/    
      DATA PT(21),PT(22),PT(23),PT(24),PT(25),PT(26),PT(27),PT(28),      
     *   PT(29),PT( 30)/-9.739065285171717E-01,-8.650633666889845E-01,   
     *   -6.794095682990245E-01,-4.333953941292472E-01,    
     *   -1.488743389816312E-01,-9.782286581460570E-01,    
     *   -8.870625997680954E-01,-7.301520055740493E-01,    
     *   -5.190961292068119E-01,-2.695431559523450E-01/    
      DATA PT(31),PT(32),PT(33),PT(34),PT(35),PT(36),PT(37),PT(38),      
     *   PT(39),PT( 40)/-9.815606342467190E-01,-9.041172563704749E-01,   
     *   -7.699026741943046E-01,-5.873179542866175E-01,    
     *   -3.678314989981802E-01,-1.252334085114689E-01,    
     *   -9.841830547185882E-01,-9.175983992229781E-01,    
     *   -8.015780907333099E-01,-6.423493394403403E-01/    
      DATA PT(41),PT(42),PT(43),PT(44),PT(45),PT(46),PT(47),PT(48),      
     *   PT(49),PT( 50)/-4.484927510364468E-01,-2.304583159551348E-01,   
     *   -9.862838086968123E-01,-9.284348836635734E-01,    
     *   -8.272013150697650E-01,-6.872929048116856E-01,    
     *   -5.152486363581541E-01,-3.191123689278898E-01,    
     *   -1.080549487073437E-01,-9.879925180204854E-01/    
      DATA PT(51),PT(52),PT(53),PT(54),PT(55),PT(56),PT(57),PT(58),      
     *   PT(59),PT( 60)/-9.372733924007058E-01,-8.482065834104270E-01,   
     *   -7.244177313601699E-01,-5.709721726085388E-01,    
     *   -3.941513470775634E-01,-2.011940939974345E-01,    
     *   -9.894009349916499E-01,-9.445750230732326E-01,    
     *   -8.656312023878317E-01,-7.554044083550030E-01/    
      DATA PT(61),PT(62),PT(63),PT(64),PT(65),PT(66),PT(67),PT(68),      
     *   PT(69),PT( 70)/-6.178762444026438E-01,-4.580167776572274E-01,   
     *   -2.816035507792589E-01,-9.501250983763744E-02,
     *   -9.905754753144173E-01,-9.506755217687678E-01,    
     *   -8.802391537269859E-01,-7.815140038968014E-01,    
     *   -6.576711592166909E-01,-5.126905370864771E-01/    
      DATA PT(71),PT(72),PT(73),PT(74),PT(75),PT(76),PT(77),PT(78),      
     *   PT(79),PT( 80)/-3.512317634538763E-01,-1.784841814958478E-01,   
     *   -9.915651684209309E-01,-9.558239495713978E-01,    
     *   -8.926024664975557E-01,-8.037049589725230E-01,    
     *   -6.916870430603533E-01,-5.597708310739476E-01,    
     *   -4.117511614628426E-01,-2.518862256915055E-01/    
      DATA PT(81),PT(82),PT(83),PT(84),PT(85),PT(86),PT(87),PT(88),      
     *   PT(89),PT( 90)/-8.477501304173527E-02,-9.924068438435845E-01,   
     *   -9.602081521348301E-01,-9.031559036148179E-01,    
     *   -8.227146565371427E-01,-7.209661773352294E-01,    
     *   -6.005453046616811E-01,-4.645707413759609E-01,    
     *   -3.165640999636298E-01,-1.603586456402254E-01/    
      DATA PT(91),PT(92),PT(93),PT(94),PT(95),PT(96),PT(97),PT(98),      
     *   PT(99),PT(100)/-9.931285991850949E-01,-9.639719272779138E-01,   
     *   -9.122344282513259E-01,-8.391169718222189E-01,    
     *   -7.463319064601507E-01,-6.360536807265151E-01,    
     *   -5.108670019508271E-01,-3.737060887154196E-01,    
     *   -2.277858511416451E-01,-7.652652113349732E-02/    
      DATA WT( 1),WT( 2),WT( 3),WT( 4),WT( 5),WT( 6),WT( 7),WT( 8),      
     *   WT( 9),WT( 10)/ 1.000000000000000E 00, 5.555555555555557E-01,
     *    3.478548451374538E-01, 6.521451548625462E-01,    
     *    2.369268850561891E-01, 4.786286704993665E-01,    
     *    1.713244923791703E-01, 3.607615730481386E-01,    
     *    4.679139345726910E-01, 1.294849661688697E-01/    
      DATA WT(11),WT(12),WT(13),WT(14),WT(15),WT(16),WT(17),WT(18),      
     *   WT(19),WT( 20)/ 2.797053914892767E-01, 3.818300505051189E-01,   
     *    1.012285362903762E-01, 2.223810344533745E-01,    
     *    3.137066458778873E-01, 3.626837833783620E-01,    
     *    8.127438836157441E-02, 1.806481606948574E-01,    
     *    2.606106964029355E-01, 3.123470770400028E-01/    
      DATA WT(21),WT(22),WT(23),WT(24),WT(25),WT(26),WT(27),WT(28),      
     *   WT(29),WT( 30)/ 6.667134430868812E-02, 1.494513491505806E-01,   
     *    2.190863625159820E-01, 2.692667193099964E-01,    
     *    2.955242247147529E-01, 5.566856711617367E-02,    
     *    1.255803694649046E-01, 1.862902109277342E-01,    
     *    2.331937645919905E-01, 2.628045445102467E-01/    
      DATA WT(31),WT(32),WT(33),WT(34),WT(35),WT(36),WT(37),WT(38),      
     *   WT(39),WT( 40)/ 4.717533638651183E-02, 1.069393259953184E-01,   
     *    1.600783285433462E-01, 2.031674267230659E-01,    
     *    2.334925365383548E-01, 2.491470458134028E-01,    
     *    4.048400476531588E-02, 9.212149983772845E-02,    
     *    1.388735102197872E-01, 1.781459807619457E-01/    
      DATA WT(41),WT(42),WT(43),WT(44),WT(45),WT(46),WT(47),WT(48),      
     *   WT(49),WT( 50)/ 2.078160475368885E-01, 2.262831802628972E-01,   
     *    3.511946033175186E-02, 8.015808715976020E-02,    
     *    1.215185706879032E-01, 1.572031671581935E-01,    
     *    1.855383974779378E-01, 2.051984637212956E-01,    
     *    2.152638534631578E-01, 3.075324199611727E-02/    
      DATA WT(51),WT(52),WT(53),WT(54),WT(55),WT(56),WT(57),WT(58),      
     *   WT(59),WT( 60)/ 7.036604748810809E-02, 1.071592204671719E-01,   
     *    1.395706779261543E-01, 1.662692058169939E-01,    
     *    1.861610000155622E-01, 1.984314853271116E-01,    
     *    2.715245941175409E-02, 6.225352393864789E-02,    
     *    9.515851168249277E-02, 1.246289712555339E-01/    
      DATA WT(61),WT(62),WT(63),WT(64),WT(65),WT(66),WT(67),WT(68),      
     *   WT(69),WT( 70)/ 1.495959888165767E-01, 1.691565193950025E-01,   
     *    1.826034150449236E-01, 1.894506104550685E-01,    
     *    2.414830286854793E-02, 5.545952937398720E-02,    
     *    8.503614831717917E-02, 1.118838471934040E-01,    
     *    1.351363684685255E-01, 1.540457610768103E-01/    
      DATA WT(71),WT(72),WT(73),WT(74),WT(75),WT(76),WT(77),WT(78),      
     *   WT(79),WT( 80)/ 1.680041021564500E-01, 1.765627053669926E-01,   
     *    2.161601352648331E-02, 4.971454889496981E-02,    
     *    7.642573025488905E-02, 1.009420441062872E-01,    
     *    1.225552067114785E-01, 1.406429146706506E-01,    
     *    1.546846751262652E-01, 1.642764837458327E-01/    
      DATA WT(81),WT(82),WT(83),WT(84),WT(85),WT(86),WT(87),WT(88),      
     *   WT(89),WT( 90)/ 1.691423829631436E-01, 1.946178822972648E-02,   
     *    4.481422676569960E-02, 6.904454273764122E-02,    
     *    9.149002162244999E-02, 1.115666455473340E-01,    
     *    1.287539625393362E-01, 1.426067021736066E-01,    
     *    1.527660420658597E-01, 1.589688433939543E-01/    
      DATA WT(91),WT(92),WT(93),WT(94),WT(95),WT(96),WT(97),WT(98),      
     *   WT(99),WT(100)/ 1.761400713915212E-02, 4.060142980038694E-02,   
     *    6.267204833410905E-02, 8.327674157670474E-02,    
     *    1.019301198172404E-01, 1.181945319615184E-01,    
     *    1.316886384491766E-01, 1.420961093183820E-01,    
     *    1.491729864726037E-01, 1.527533871307258E-01/    
      DATA WZ/ 8.888888888888890E-01, 5.688888888888889E-01,      
     *    4.179591836734694E-01, 3.302393550012598E-01,    
     *    2.729250867779006E-01, 2.325515532308739E-01,    
     *    2.025782419255613E-01, 1.794464703562065E-01,    
     *    1.610544498487837E-01/      
      NSTART = ((N/2)*((N-1)/2))      
      ND2 = N/2  
      BA = (B-A) 
      DO 15 J=1,ND2     
      NJ = N-J+1 
      NSTJ = NSTART + J 
      P(J) = PT(NSTJ)   
      P(NJ) = -P(J)     
      W(J) = WT(NSTJ)   
      W(NJ) = W(J)      
   15 CONTINUE   
      IF ((ND2)*2.EQ.N) GO TO 25      
      P(ND2 + 1) = 0.0 
      W(ND2+ 1) = WZ(ND2)      
   25 CONTINUE   
      SUM = 0.0 
      DO 1 LOCATE=1,N   
      baa=(BA*P(LOCATE)+(B+A))/2.0D0
      SUM = SUM +.5*W(LOCATE)*BA*FC(baa
     *,PI,UU,S,AG,Z,XB,SC,TT,NN,KTORP)
    1 CONTINUE   
      GQ=SUM     
      RETURN     
      END 
      SUBROUTINE SIMPLX(X,SCALE,CRIT,ITMAX,ITRACE,IOPT,NV)
      implicit double precision (a-h,o-z)
C     X IS PARAMETER VALUES, SCALE DETERMINES SPREAD OF SIMPLX POINTS
C     AROUND STARTING VALUE, NV IS NO. OF PARAMETERS
C     CRIT IS CONVERGENCE CRITERION, ITMAX IS MAX. NO. OF ITERATIONS,
C     AND ITRACE GIVES PRINT OUT EVERY ITRACE ITERATIONS,
C     EVERY IOPT TRIALS TEST IF BEST PARAMETERS WITHIN .00001
C     PROGRAM IS DIMENSIONED FOR A MAXIMUM OF 39 PARAMETERS
      double precision X(39),SCALE(39),PL(40)
      DIMENSION P(40,39),Y(40),PSTAR(39),PBAR(39)
      INTEGER MATRIX(5,5)
C     THIS MATRIX COUNTS THE NUMBER OF EACH TYPE OF MOVE
C     FOLLOWING EACH TYPE OF PREVIOUS MOVE SUMMING MATRIX(I,J)
C     OVER I=1,5 GIVES THE NUMBER OF TIMES MOVE J WAS USED
      LOGICAL TRACE
      ALPH=1.0
      BET=0.5
      TRACE=.TRUE.
      GAMMA=2.0
      IF(CRIT.LE.0.0)CRIT=1.E-8
      IF(ITMAX.LE.0)ITMAX=1
      DO 5 I=1,5
      DO 5 J=1,5
    5 MATRIX(I,J)=0
      LASTM=1
      ITER=1
      NVP1=NV+1
      FNV=NV
      FNVP1=NVP1
      ASSIGN 35 TO NEXT
C     DO 15 J=1,NV
C     IF(SCALE(J).GT.0.0) GO TO 15
C     SCALE(J)=X(J)*.2
C  15 CONTINUE
C     GENERATE A REGULAR SIMPLX IN NV DIMENSIONS WITH VERTICES AT UNIT
C     DISTANCE FROM THE CENTROID AND CENTROID AT THE ORIGIN
      T1=(1.0-SQRT(FNVP1))/SQRT(FNV**3)
      T2=SQRT(FNVP1/FNV)+T1
      DO 23 I=1,NV
      DO 22 J=1,NV
      IF (I-J)20,21,20
   20 P(I,J)=T1
      P(J,I)=T1
      GO TO 22
   21 P(I,I)=T2
   22 CONTINUE
   23 CONTINUE
      T1=-1.0/SQRT(FNV)
      DO 24 J=1,NV
   24 P(NVP1,J)=T1
C     NOTE THAT THE NV POINTS P(I,J), J=1,NV DEFINE THE ITH
C     POINT IN THE NV DIMENSIONED SIMPLEX
C     MOVE CENTROID TO STARTING VECTOR AND SCALE COLUMNS
      DO 26 J=1,NV
      DO 25 I=1,NVP1
   25 P(I,J)=P(I,J)*SCALE(J)+X(J)
   26 CONTINUE
C     COMPUTE FUNCTION VALUES FOR SIMPLEX POINTS AND FIND MIN VALUE
C     OF FUNCTION
      DO 29 I=1,NVP1
      WRITE(*,669) I,(P(I,J),J=1,NV)
      DO 28 J=1,NV
   28 PSTAR(J)=P(I,J)
      T1=FOFS(NV,PSTAR)
   29 Y(I)=T1
   30 T1=Y(1)
      IL=1
      DO 32 I=2,NVP1
      IF(T1-Y(I))32,32,31
   31 T1=Y(I)
      IL=I
   32 CONTINUE
      GO TO NEXT,(35,80)
   35 IF(TRACE) WRITE(*,600)
  600 FORMAT(' ',24X,'NO',4X,'IL',3X,'FUNCTION',4X,'CRITERION',3X,'PARAM
     *ETERS')
      ASSIGN 80 TO NEXT
C     STARTING POINT OF ITERATIVE CYCLE, FIND Y(IH)
 1000 T1=Y(1)
      IH=1
      TRACE=.FALSE.
      IT=ITER-1
      IF(MOD(IT,ITRACE).EQ.0) TRACE=.TRUE.
      IF (MOD(IT,IOPT).NE.0) GO TO 44
      IF (IT.EQ.0) GO TO 43
      DO 41 I=1,NV
      IF (ABS(P(IL,I)-PL(I)).GT.0.00000001) GO TO 43
   41 CONTINUE
      WRITE(*,700) IOPT
  700 FORMAT(/,'NO IMPROVEMENT IN',I3,'TRIALS')
      GO TO 90
   43 DO 42 I=1,NV
   42 PL(I)=P(IL,I)
   44 DO 46 I=2,NVP1
      IF(T1-Y(I))45,46,46
   45 T1=Y(I)
      IH=I
   46 CONTINUE
C     COMPUTE CENTROID EXCLUDING POINT WITH MAX VALUE
      DO 50 J=1,NV
      T1=0.0
      DO 49 I=1,NVP1
      IF(I-IH)48,49,48
   48 T1=P(I,J)+T1
   49 CONTINUE
   50 PBAR(J)=T1/FNV
C     TRY A REFLECTION
      DO 51 J=1,NV
   51 PSTAR(J)=(1.0+ALPH)*PBAR(J)-ALPH*P(IH,J)
      T1=FOFS(NV,PSTAR)
      IF(T1.GT.Y(IL)) GO TO 54
C     REFLECTION SUCCEEDED, TRY AN EXPANSION
      DO 52 J=1,NV
      P(IH,J)=PSTAR(J)
   52 PSTAR(J)=(1.0+GAMMA)*PSTAR(J)-GAMMA*PBAR(J)
      T2=T1
      T1=FOFS(NV,PSTAR)
      IL=IH
      IF(T1.GT.T2) GO TO 76
C     EXPANSION SUCCEEDED
      IF(TRACE) WRITE(*,601)
  601 FORMAT(20H EXPANSION SUCCEEDED)
      MATRIX(LASTM,1)=MATRIX(LASTM,1)+1
      LASTM=1
      GO TO 72
   54 T2=Y(IL)
      I2=IL
C     REFLECTION FAILED, FIND NEXT TO BIGGEST Y AND TEST
      DO 57 I=1,NVP1
      IF(I-IH)55,57,55
   55 IF(Y(I)-T2)57,57,56
   56 T2=Y(I)
      I2=I
   57 CONTINUE
      IF(T1.LT.T2) GO TO 71
C     NEW POINT CLOSE TO MAXIMUM, EXCHANGE IF NECESSARY AND TRY A CONTRA
C     CTION
      IF(T1.GE.Y(IH)) GO TO 61
C     EXCHANGE PSTAR AND Y
      DO 60 J=1,NV
      T2=PSTAR(J)
      PSTAR(J)=P(IH,J)
   60 P(IH,J)=T2
C     CALCULATE NEW POINT FOR CONTRACTION
   61 DO 62 J=1,NV
   62 PSTAR(J)=(1.0-BET)*PBAR(J)+BET*P(IH,J)
      T1=FOFS(NV,PSTAR)
      IF(T1.LT.Y(IH)) GO TO 70
C     CONTRACTION FAILED
      IF(TRACE) WRITE(*,602)
  602 FORMAT(19H CONTRACTION FAILED)
      MATRIX(LASTM,5)=MATRIX(LASTM,5)+1
      LASTM=5
      DO 66 I=1,NVP1
      IF(I-IL)64,66,64
   64 DO 65 J=1,NV
      P(I,J)=(P(IL,J)+P(I,J))/2.0
   65 PSTAR(J)=P(I,J)
      Y(I)=FOFS(NV,PSTAR)
   66 CONTINUE
      GO TO 30
   70 IF(TRACE) WRITE(*,603)
  603 FORMAT(22H CONTRACTION SUCCEEDED)
      MATRIX(LASTM,4)=MATRIX(LASTM,4)+1
      LASTM=4
      IF(T1.LT.Y(IL))IL=IH
      GO TO 72
   71 IF(TRACE) WRITE(*,664)
  664 FORMAT(12H NORMAL MOVE)
      MATRIX(LASTM,3)=MATRIX(LASTM,3)+1
      LASTM=3
   72 Y(IH)=T1
   73 DO 74 J=1,NV
   74 P(IH,J)=PSTAR(J)
      GO TO 80
   76 IF(TRACE) WRITE(*,604)
  604 FORMAT(21H REFLECTION SUCCEEDED)
      MATRIX(LASTM,2)=MATRIX(LASTM,2)+1
      LASTM=2
      Y(IH)=T2
   80 T1=0.0
C     CHECK FOR MINIMUM
      DO 81 I=1,NVP1
   81 T1=Y(I)+T1
      T1=T1/FNVP1
      T2=0.0
      DO 82 I=1,NVP1
   82 T2=(Y(I)-T1)**2+T2
      T2=SQRT(T2/FNV)
      IF(T2.LT.CRIT) GO TO 90
      IF(TRACE) WRITE(*,665)ITER,IL,Y(IL),T2,(P(IL,J),J=1,NV)
  665 FORMAT(1H+,20X,2I6,2E12.4,39F7.3)
      ITER=ITER+1
      IF(ITER.LT.ITMAX) GO TO 1000
      WRITE(*,666)
  666 FORMAT(29H MAXIMUM NUMBER OF ITERATIONS)
   90 DO 91 J=1,NV
   91 X(J)=P(IL,J)
      WRITE(*,876)((MATRIX(I,J),J=1,5),I=1,5)
  876 FORMAT(/,29X,'SUBSEQUENT MOVE'/' ',24X,'EXP REF NOR CON FLD'
     */' ',17X,'EXP',5I5/' ',17X,'REF',5I5/' ','PREVIOUS',9X,'NOR',5I5/'
     * ','MOVE',11X,'CON',5I5/' ',17X,'FLD',5I5)
      DO 92 I=1,NVP1
      WRITE(*,669) I,(P(I,J),J=1,NV)
  669 FORMAT(' ','VERTEX',I3,2X,39F7.3)
   92 CONTINUE
      WRITE(*,667) ITER,IL,Y(IL),T2
  667 FORMAT(/,'ITER=',I4,1X,'IL=',I2,1X,'Y(IL)=',E11.4,3X,'CRITE
     *',E11.4)
      WRITE(*,668) (P(IL,J),J=1,NV),Y(IL)
      if(iter.ne.100)
     *print*,"a ter eta sz 4drifts (then 2 dummy drifts) z po st"
      if(iter.ne.60)WRITE(*,"(20f10.4)") (P(IL,J),J=1,NV),Y(IL)
      if(iter.ne.60)WRITE(12,"(9f10.4,f14.4,10f10.4)") 
     *(P(IL,J),J=1,NV),Y(IL)
  668 FORMAT(' ','BEST PARAMETER ESTIMATES',39F10.3)
      RETURN
      END 
