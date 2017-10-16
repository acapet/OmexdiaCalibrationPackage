!==========================================================================
! THE OMEXDIA model, implemented in FORTRAN
! Karline Soetaert, nioz-yerseke
!==========================================================================

!==========================================================================
! initialise the common block with parameter values, 
! followed by thicknesses, porosities, bioturbation values
!==========================================================================

      SUBROUTINE initomexdia (steadyparms)
      IMPLICIT NONE
      EXTERNAL steadyparms

      INTEGER,PARAMETER :: N=100
      INTEGER,PARAMETER :: nc = 46 + 3*N + 4*(N+1) 

      DOUBLE PRECISION parms(nc)
      COMMON /myparms/parms

       CALL steadyparms(nc, parms)
       
      RETURN
      END SUBROUTINE

!==========================================================================
! Initialise the forcing function common block (Carbon flux)
!==========================================================================

      SUBROUTINE initforc (steadyforcs)
      IMPLICIT NONE
      EXTERNAL steadyforcs

      INTEGER,PARAMETER :: N=1 

      DOUBLE PRECISION forcs(N)
      COMMON /myforcs/forcs

       CALL steadyforcs(N, forcs)
       
      RETURN
      END SUBROUTINE

!==========================================================================
!==========================================================================
! subroutine calculating the rate of change of
! the omexdia model - here the quantities in the common blocks are named
!==========================================================================
!==========================================================================

      SUBROUTINE oomexdia_mod (neq, t, Conc, dConc, yout, ip)
      IMPLICIT NONE

!......................... declaration section.............................
      INTEGER           :: neq, ip(*), i
      INTEGER,PARAMETER :: N=100         

      DOUBLE PRECISION  :: t, Conc(12*N), dConc(12*N), yout(*)
      DOUBLE PRECISION  :: por(N),intpor(N+1),Db(N+1),dx(N),dxInt(N+1),         &
     & AlphIrr(N),IrrEnh(N+1)
     
      DOUBLE PRECISION  :: tort(N+1)
!      DOUBLE PRECISION  :: PSI(N)
      
      DOUBLE PRECISION  :: Fdet(N),Sdet(N),O2(N),NO3(N),NH3(N),ODU(N),          &
     &                     DIC(N),Sidet(N),SiO(N),PO4(N),FeP(N),CaP(N)
      DOUBLE PRECISION  :: dFdet(N),dSdet(N),dO2(N),dNO3(N),                    &
     &                     dNH3(N),dODU(N),dDIC(N),dSidet(N),dSiO(N),           &
     &                     dPO4(N),dFeP(N),dCaP(N)

      DOUBLE PRECISION  :: FCmin(N), SCmin(N), SiDiss(N)
      DOUBLE PRECISION  :: cflux,siflux
      DOUBLE PRECISION  :: Cprod(N),Nprod(N),Pprod(N),Rescale(N),TOC(N)
      DOUBLE PRECISION  :: Oxicminlim(N),Denitrilim(N),Anoxiclim(N) 
      DOUBLE PRECISION  :: O2func(N),FePadsorp(N),FePdesorp(N),                 &
     & CaPprod(N),CaPdiss(N)                                               
      DOUBLE PRECISION  :: Oxicmin(N),Denitrific(N),anoxicmin(N)
      DOUBLE PRECISION  :: nitri(N),oduox(N),odudepo(N)
      DOUBLE PRECISION  :: Summ
      DOUBLE PRECISION  :: Flux(N+1), DS(N+1), dCIrr(N)
       
      DOUBLE PRECISION  :: Temp,w,MeanFlux,rFast,rSlow,pFast,pref,              &
     & NCrFdet,NCrSdet,                                                         &
     & rSi,SiCdet,SiEQ,                                                         &
     & PCrFdet,PCrSdet,rFePdesorp,rFePadsorp,rCaPprod,rCaPdiss,CPrCaP,          &
     & PO4ads,                                                                  &
     & Q,pdepo,NH3Ads,rnit,ksO2nitri,rODUox,ksO2oduox,                          &
     & ksO2oxic,ksNO3denit,kinO2denit,kinNO3anox,kinO2anox,bwO2,bwNH3,          &
     & bwNO3,bwODU,bwDIC,bwSiO,bwPO4,DispO2,DispNO3,DispNH3,DispODU,            &
     & DispDIC, DispSiO,DispPO4
      
      COMMON /myparms     /Temp,w,MeanFlux,rFast,rSlow,pFast,pref,              &
     & NCrFdet,NCrSdet,                                                         &
     & rSi,SiCdet,SiEQ,                                                         &
     & PCrFdet,PCrSdet,rFePdesorp,rFePadsorp,rCaPprod,rCaPdiss,CPrCaP,          &
     & PO4ads,                                                                  &
     & Q,pdepo,NH3Ads,rnit,ksO2nitri,rODUox,ksO2oduox,                          &
     & ksO2oxic,ksNO3denit,kinO2denit,kinNO3anox,kinO2anox,bwO2,bwNH3,          &
     & bwNO3,bwODU,bwDIC,bwSiO,bwPO4,                                           &
     & DispO2,DispNO3,DispNH3,DispODU,DispDIC, DispSiO,DispPO4,                 &
     & dx,dxint,por,intpor,Db,AlphIrr,IrrEnh
      
      DOUBLE PRECISION CarbonFlux
      COMMON /myforcs/ CarbonFlux

! output variables
      DOUBLE PRECISION  :: O2flux,  O2Irrflux(N),  O2deepflux
      DOUBLE PRECISION  :: NO3flux, NO3Irrflux(N), NO3deepflux
      DOUBLE PRECISION  :: NH3flux, NH3Irrflux(N), NH3deepflux
      DOUBLE PRECISION  :: ODUflux, ODUIrrflux(N), ODUdeepflux
      DOUBLE PRECISION  :: DICflux, DICIrrflux(N), DICdeepflux
      DOUBLE PRECISION  :: SIOflux, SIOIrrflux(N), SIOdeepflux
      DOUBLE PRECISION  :: PO4flux, PO4Irrflux(N), PO4deepflux
      DOUBLE PRECISION  :: FePdeepflux,CaPdeepflux
      DOUBLE PRECISION  :: SDETdeepflux, FDETdeepflux,Sidetdeepflux
      DOUBLE PRECISION  :: CorgDeepflux, NorgDeepflux,PorgDeepflux
      DOUBLE PRECISION  :: TotDenitrific, TotOxicMin, TotAnoxicMin,             &
     &  TotNitri, TotOduOx 
      DOUBLE PRECISION  :: NH3adsorption(N), PO4adsorption(N)
      COMMON /myout       /O2flux, O2Irrflux,O2deepflux,NO3flux,                &
     & NO3Irrflux,NO3deepflux,NH3flux,NH3Irrflux,NH3deepflux,ODUflux,           &
     & ODUIrrflux,ODUdeepflux,DICflux,DICIrrflux,DICdeepflux,                   &
     & SIOflux,SIOIrrflux,SIOdeepflux,                                          &  
     & PO4flux,PO4Irrflux,PO4deepflux,                                          &  
     & FePdeepflux, CaPdeepflux,                                                &  
     & Cflux,                                                                   &
     & CorgDeepflux,NorgDeepflux,Porgdeepflux,Sidetdeepflux,                    &
     & Denitrific,OxicMin,AnoxicMin,                                            &
     & Nitri,OduOx,Cprod,Nprod,Pprod,NH3adsorption,PO4adsorption,TOC,           &
     & FePadsorp,FePdesorp,CaPprod,CaPdiss

      CHARACTER(len=80) msg
!............................ statements ..................................

!     check memory allocated to output variables
      IF (ip(1) < 2221)  CALL rexit("nout should be at least 2221") 
      
c      write(*,*) "Hello there"
c      write(*,*) "Temp",Temp
c      write(*,*) " DispDIC", DispDIC    
c      write(*,*) " CarbonFlux",    CarbonFlux
c      write(*,*) " MeanFlux", MeanFlux  


      
! from Conc to fdet, sdet, o2,...
      DO I = 1, N
        Fdet(I) = Conc(I)
        Sdet(I) = Conc(N+I)
        O2(I)   = Conc(2*N+I)
        NO3(I)  = Conc(3*N+I)
        NH3(I)  = Conc(4*N+I)
        ODU(I)  = Conc(5*N+I)
        DIC(I)  = Conc(6*N+I)
        Sidet(I)= Conc(7*N+I)
        SiO(I)  = Conc(8*N+I)
        PO4(I)  = Conc(9*N+I)
        FeP(I)  = Conc(10*N+I)
        CaP(I)  = Conc(11*N+I)
      ENDDO
      
!      TOC = (FDET + SDET)*1200d0*1e-9/2.5 
      TOC =(FDET+SDET+MeanFlux*pRef/w/(1-intpor(N+1)))*1200.d0*1e-9/            &
     & 2.5d0 
c      write(*,*) " ### Start ###   "
c      write(*,*) " sum(dFdet)   ", sum(dFdet)
c      write(*,*) " sum(dSdet)   ", sum(dSdet)
c      write(*,*) " sum(dO2)     ", sum(dO2)
c      write(*,*) " sum(dNO3)    ", sum(dNO3)
c      write(*,*) " sum(dNH3)    ", sum(dNH3)
c      write(*,*) " sum(dODU)    ", sum(dODU)
c      write(*,*) " sum(dDIC)   ", sum(dDIC)

c      write(*,*) "               "

! --------------------------------------------------------------------------
! Rate of change due to transport 
! --------------------------------------------------------------------------
       
! Carbon deposition flux
      CFlux  =  CarbonFlux
      SiFlux =  CarbonFlux*SiCdet
       
! Solid 
      CALL diffadv1D(N,FDET,0.d0,0.d0,cFlux*pFast*(1.d0-pRef),                  &
     & 0.d0,0.d0,0.d0,1,3,Db,w,1.d0-intpor,1.d0-por, dx, dxint, Flux,           &
     & dFDET)
      FDETdeepflux  = Flux(N+1)

      CALL diffadv1D(N,SDET,0.d0, 0.d0, cFlux*(1.d0-pFast)*(1.d0-pRef),         &
     & 0.d0,0.d0,0.d0,1,3,Db,w,1.d0-intpor,1.d0-por, dx, dxint, Flux,           &
     & dSDET)
      SDETdeepflux  = Flux(N+1)

      CALL diffadv1D(N,SiDET,0.d0, 0.d0, SiFlux,                                &
     & 0.d0,0.d0,0.d0,1,3,Db,w,1.d0-intpor,1.d0-por, dx, dxint, Flux,           &
     & dSiDET)
      SiDETdeepflux  = Flux(N+1)
      
      CALL diffadv1D(N,FeP,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,                       &
     &             1, 3, Db, w, 1.d0-intpor,1.d0-por, dx, dxint, Flux,          &
     & dFeP)
      FePdeepflux  = Flux(N+1)
      
      CALL diffadv1D(N,CaP,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,                       &
     &             1, 3, Db, w, 1.d0-intpor,1.d0-por, dx, dxint, Flux,          &
     & dCaP)
      CaPdeepflux  = Flux(N+1)
      
! Liquid

      tort = (1.d0-log( intpor**2 ))! Tortuosity according to Boudreau 1997
      
      Ds = dispO2/tort*IrrEnh+ Db 
      CALL diffadv1D (N, O2 ,bwO2 ,0.d0, 0.d0, 0.d0, 0.d0, 0.d0,                &
     & 2,  3, Ds, w,intpor,por, dx,dxint, Flux, dO2)     
      O2flux     = Flux(1)
      O2deepflux = Flux(N+1)
      CALL BioIrrigation1D (N, O2, bwO2, AlphIrr*dispO2, dCIrr)
      O2Irrflux  = dCIrr
      dO2        = dO2+dCIrr
      
      Ds = dispNO3/tort*IrrEnh+ Db 
      CALL diffadv1D (N, NO3 ,bwNO3 ,0.d0, 0.d0, 0.d0, 0.d0, 0.d0,              &
     & 2,  3, Ds, w,intpor,por, dx,dxint, Flux, dNO3)     
      NO3flux     = Flux(1)
      NO3deepflux = Flux(N+1)
      CALL BioIrrigation1D (N, NO3, bwNO3, AlphIrr*dispNO3, dCIrr)
      NO3Irrflux  = dCIrr
      dNO3        = dNO3+dCIrr
      
      ! In the case of adsoprtion, diffusion coef for liquids only , irrigation coef and reaction rates
      ! are affected (divided by 1+K). In case of adsorption AND advection AND depth-varying porosity
      ! vertical velocity is also affected and has to be a vector.
      Ds = dispNH3/tort/(1.d0+NH3ads)*IrrEnh+ Db 
!      PSI = (1.d0-por(N))/(por(N))*por/(1.d0-por) 
!      PSI = ( 1.d0 + NH3ads * PSI) / (1.d0 + NH3ads)
!      CALL diffadv1Dads (N, NH3 ,bwNH3 ,0.d0, 0.d0, 0.d0, 0.d0, 0.d0,           &
!     & 2,  3, Ds, w*PSI,intpor,por, dx,dxint, Flux, dNH3)     
      CALL diffadv1D (N, NH3 ,bwNH3 ,0.d0, 0.d0, 0.d0, 0.d0, 0.d0,           &
     & 2,  3, Ds, w,intpor,por, dx,dxint, Flux, dNH3)     
      NH3flux     = Flux(1)*(1.d0+NH3Ads)
      NH3deepflux = Flux(N+1)*(1.d0+NH3Ads)
      CALL BioIrrigation1D (N, NH3, bwNH3, AlphIrr*dispNO3/(1.d0+NH3ads)&
     & , dCIrr)
      dNH3        = dNH3+dCIrr
      NH3Irrflux  = dCIrr*(1.d0+NH3Ads)
      
      Ds = dispODU/tort*IrrEnh+ Db 
      CALL diffadv1D (N, ODU ,bwODU ,0.d0, 0.d0, 0.d0, 0.d0, 0.d0,              &
     & 2,  3, Ds, w,intpor,por, dx,dxint, Flux, dODU)     
      ODUflux     = Flux(1)
      ODUdeepflux = Flux(N+1)
      CALL BioIrrigation1D (N, ODU, bwODU, AlphIrr*dispODU, dCIrr)
      dODU        = dODU+dCIrr
      ODUIrrflux  = dCIrr
      
      Ds = dispDIC/tort*IrrEnh+ Db 
      CALL diffadv1D (N, DIC ,bwDIC ,0.d0, 0.d0, 0.d0, 0.d0, 0.d0,              &
     & 2,  3, Ds, w,intpor,por, dx,dxint, Flux, dDIC)     
      DICflux     = Flux(1)
      DICdeepflux = Flux(N+1)
      CALL BioIrrigation1D (N, DIC, bwDIC, AlphIrr*dispDIC, dCIrr)
      dDIC        = dDIC+dCIrr
      DICIrrflux  = dCIrr
      
      Ds = dispSiO/tort*IrrEnh+ Db 
      CALL diffadv1D (N, SiO ,bwSiO ,0.d0, 0.d0, 0.d0, 0.d0, 0.d0,              &
     & 2,  3, Ds, w,intpor,por, dx,dxint, Flux, dSiO)     
      SiOflux     = Flux(1)
      SiOdeepflux = Flux(N+1)
      CALL BioIrrigation1D (N, SiO, bwSiO, AlphIrr*dispSiO, dCIrr)
      dSiO        = dSiO+dCIrr
      SiOIrrflux  = dCIrr
      
      ! In the case of adsoprtion, diffusion coef for liquids only , irrigation coef and reaction rates
      ! are affected (divided by 1+K). In case of adsorption AND advection AND depth-varying porosity
      ! vertical velocity is also affected and has to be a vector.
      Ds = dispPO4/tort/(1.d0+PO4ads)*IrrEnh+ Db 
!      PSI = (1.d0-por(N))/(por(N))*por/(1.d0-por) 
!      PSI = ( 1.d0 + PO4ads * PSI) / (1.d0 + PO4ads)
!      CALL diffadv1Dads (N, PO4 ,bwPO4 ,0.d0, 0.d0, 0.d0, 0.d0, 0.d0,           &
!     & 2,  3, Ds, w*PSI,intpor,por, dx,dxint, Flux, dPO4)     
      CALL diffadv1D (N, PO4 ,bwPO4 ,0.d0, 0.d0, 0.d0, 0.d0, 0.d0,           &
     & 2,  3, Ds, w,intpor,por, dx,dxint, Flux, dPO4)   
      PO4flux     = Flux(1)*(1.d0+PO4ads)
      PO4deepflux = Flux(N+1)*(1.d0+PO4ads)
      CALL BioIrrigation1D (N, PO4, bwPO4, AlphIrr*dispPO4/(1.d0+PO4ads)&
     & , dCIrr)
      dPO4        = dPO4+dCIrr
      PO4Irrflux  = dCIrr*(1.d0+PO4ads)
      
! --------------------------------------------------------------------------
! Rate of change due to biogeochemistry 
! --------------------------------------------------------------------------

! Production of DIC and DIN, expressed per cm3 LIQUID/day

      FCmin = rFast*Q**((Temp-20.d0)/10.d0)*FDET
      SCmin = rSlow*Q**((Temp-20.d0)/10.d0)*SDET
!      SiDiss= ( rSi*Q**((Temp-20)/10)*SiDET*                                   &
!     & (1.d0- SiO/(EquilSiO*Q**((Temp-20)/10)) ))* (1.d0-por)/por
      SiDiss= rSi*Q**((Temp-20.d0)/10.d0)*SiDET*                                &
     & max((SiEQ-SiO)/SiEQ,0.0d0)    
!     & (SiEQ-SiO)/SiEQ
      Cprod= ( FCmin         + SCmin         )*(1.d0-por)/por
      Nprod= ( FCmin*NCrFdet + SCmin*NCrSdet )*(1.d0-por)/por
      Pprod= ( FCmin*PCrFdet + SCmin*PCrSdet )*(1.d0-por)/por
! Oxygen limiting factor
      O2func = O2 / (O2 + 0.1)
! P adsorption to Fe-oxides mainly if oxygen in sufficient amounts
! P desorption other way around  
      FePadsorp  = rFePadsorp * O2func * PO4         ! nmol liquid/cm3/d
      FePdesorp  = rFePdesorp * (1 - O2func) * FeP   ! nmol  solid/cm3/d
    
! P binding to Ca and P-release  
      CaPprod    = rCaPprod * PO4                   ! nmol liquid/cm3/d
      CaPdiss    = rCaPdiss * CaP                   ! nmol  solid/cm3/d
      
! Oxic mineralisation, denitrification, anoxic mineralisation

! first the limitation terms
      Oxicminlim = O2/(O2+ksO2oxic)                ! limitation terms
      Denitrilim = (1.d0-O2/(O2+kinO2denit)) * NO3/(NO3+ksNO3denit)
      Anoxiclim  = (1.d0-O2/(O2+kinO2anox))*(1.d0-NO3/(NO3+kinNO3anox))
      Rescale    = 1.d0/(Oxicminlim+Denitrilim+Anoxiclim)

! then the mineralisation rates
      OxicMin    = Cprod*Oxicminlim*Rescale        ! oxic mineralisation
      Denitrific = Cprod*Denitrilim*Rescale        ! Denitrification
      AnoxicMin  = Cprod*Anoxiclim *Rescale        ! anoxic mineralisation
      
! reoxidation and ODU deposition
      Nitri      = rnit  *NH3*O2/(O2+ksO2nitri)
      OduOx      = rODUox*ODU*O2/(O2+ksO2oduox)
      
! pDepo now given as input parameter
!       pDepo      = MIN(1.d0,0.233*(w*365)**0.336 )
      OduDepo    = AnoxicMin*pDepo 

! --------------------------------------------------------------------------
! Update the rate of change with rates due to biogeochemical processes
! --------------------------------------------------------------------------
      dFDET = dFDET - FCmin
      dSDET = dSDET - SCmin
      dO2   = dO2   - OxicMin          -2.d0* Nitri -      OduOx
      dNH3  = dNH3  + (Nprod                - Nitri) / (1.d0+NH3Ads)
      dNO3  = dNO3  - 0.8d0*Denitrific      + Nitri 
      dODU  = dODU  + AnoxicMin                  - OduOx - OduDepo
      dDIC  = dDIC  + Cprod + CPrCaP*(CaPdiss *(1.d0-por)/por-CaPprod)
      dSiDET = dSiDET  - SiDiss
      dSiO   = dSiO    + SiDiss*(1.d0-por)/por 
      dPO4  = dPO4  + (Pprod -FePadsorp-CaPprod                                 &
     &              +(FePdesorp + CaPdiss)*(1.d0-por)/por)/(1.d0+PO4Ads)
      dFeP  = dFeP  + FePadsorp*por/(1.d0-por) - FePdesorp
      dCaP  = dCaP  + CaPprod  *por/(1.d0-por) - CaPdiss
      
! from dfdet, dsdet, do2,... to dconc, and calculate integrated rates
      TotDenitrific= 0.D0
      TotOxicMin   = 0.D0
      TotAnoxicMin = 0.D0
      TotNitri     = 0.D0
      TotOduOx     = 0.D0
            
      DO I = 1, N
         TotDenitrific =  TotDenitrific + Denitrific(I) * por(I)*dx(I)
         TotAnoxicMin  =  TotAnoxicMin  + AnoxicMin(I)  * por(I)*dx(I)
         TotOxicMin    =  TotOxicMin    + OxicMin(I)    * por(I)*dx(I)
         TotOduOx      =  TotOduOx      + OduOx(I)      * por(I)*dx(I)
         TotNitri      =  TotNitri      + Nitri(I)      * por(I)*dx(I)        
         dConc(I)      =  dFdet(I)
         dConc(N+I)    =  dSdet(I) 
         dConc(2*N+I)  =  dO2(I)  
         dConc(3*N+I)  =  dNO3(I) 
         dConc(4*N+I)  =  dNH3(I) 
         dConc(5*N+I)  =  dODU(I) 
         dConc(6*N+I)  =  dDIC(I)
         dConc(7*N+I)  =  dSidet(I)
         dConc(8*N+I)  =  dSIO(I)
         dConc(9*N+I)  =  dPO4(I)
         dConc(10*N+I) =  dFeP(I)
         dConc(11*N+I) =  dCaP(I)
      ENDDO
      
c      write(*,*) " sum(dFdet)   ", sum(dFdet)
c      write(*,*) " sum(dSdet)   ", sum(dSdet)
c      write(*,*) " sum(dDIC)    ", sum(dDIC)
c      write(*,*) "  *** *** End *** ***    "

! Other output diagnostics
!   the last term assumes steady state ? 
      CorgDeepflux  = FDETDeepflux + SDETDeepflux + CarbonFlux*pRef 
      NorgDeepflux  = FDETDeepflux*NCrFDET + SDETDeepflux*NCrSDET
      PorgDeepflux  = FDETDeepflux*PCrFDET + SDETDeepflux*NCrSDET
      NH3adsorption = (Nprod - Nitri) * (1.d0-1.d0/(1.d0+NH3Ads))
      PO4adsorption = (Pprod -FePadsorp-CaPprod                                &
     &+(FePdesorp + CaPdiss)*(1.d0-por)/por)*(1.d0-1.d0/(1.d0+PO4Ads))
     
           
      CALL getout(yout)
      
      RETURN
      END SUBROUTINE

!==========================================================================
! put output variables in one vector
!==========================================================================

      SUBROUTINE getout(yout)
      DOUBLE PRECISION :: yout(*), out(2221)
      INTEGER :: i

      COMMON /myout    /out
      DO i = 1, 2221
       yout(i) = out (i)
      ENDDO       
 
      END SUBROUTINE getout
       
 
!==============================================================================
! Diffusion in a 1-dimensional finite difference grid 
! all inputs are vectors
! subroutine from ReacTran in isnt\doc\fortran directory
! Added advection A.Capet May2015
!==============================================================================

      SUBROUTINE diffadv1d (N, C, Cup, Cdown, fluxup, fluxdown, aup,            &    
     &                   adown, BcUp, BcDown, D, v, VF,VFmid, dx, dxaux,        &
     &                   Flux, dC) 
      IMPLICIT NONE
      INTEGER N                  ! length of C
C input
      DOUBLE PRECISION C(N)

C Boundary concentrations (used if Bc..=2,4), fluxes (used if Bc= 1) 
C and convection coeff (used if Bc=4)
      DOUBLE PRECISION Cup, Cdown, fluxup, fluxdown, aup, adown

C v is the sedimentation velocity at depth \omega_\infty, 
      DOUBLE PRECISION v

C Diffusion, volume fraction
      DOUBLE PRECISION D(N+1), VF(N+1),VFmid(N)

C grid size, distance from mid to mid
      DOUBLE PRECISION dx(N), dxaux(N+1)

C boundary concitions (1= flux, 2=conc, 3 = 0-grad, 4=convect)
      INTEGER BcUp, BcDown   
      
C      DOUBLE PRECISION, parameter :: aa = 0.5
      double precision  aa(N+1), sig(n+1), Pe(N+1)
      
C output: fluxes and rate of change
      DOUBLE PRECISION Flux(N+1), dC(N)

C locals 
      INTEGER I
      DOUBLE PRECISION AVF

C -------------------------------------------------------------------------------
C     ART - Computing first the
      DO I = 2,N
         pe(i)  = v * dxaux(i)/2/D(i)
         sig(i) = 1/tanh(pe(i))-1/pe(i)
         aa(i)  = (1+sig(i))/2
      enddo
C Flux - first, internal cells
      DO I = 2,N
        Flux(I) = -VF(I)*D(I)*(C(I)-C(I-1))/dxaux(I)
C Advection ------- 
!        Flux(I) = Flux(I) + C(I-1)*VF(N+1)*v
        Flux(I) = Flux(I) + (aa(i)*C(I-1)+(1-aa(i))*C(I))*VF(N+1)*v
      ENDDO

C Then,  the outer cells 
C upstream boundary
      IF (BcUp .EQ. 1) THEN
        Flux(1) = fluxup

      ELSE IF (BcUp .EQ. 2) THEN
        Flux(1) = -VF(1)*D(1) * (C(1)-Cup) /dxaux(1)
C Advection 
        Flux(1) = Flux(1) + Cup*VF(N+1)*v

      ELSE IF (BcUp .EQ. 3) THEN
        Flux(1) = 0.D0

      ELSE 
        Flux(1) = aup * (Cup - C(1))
      ENDIF

C downstream boundary
      IF (BcDown .EQ. 1) THEN
        Flux(N+1) = fluxdown

      ELSE IF (BcDown .EQ. 2) THEN
        Flux(N+1) = -VF(N+1)*D(N+1) * (Cdown-C(N)) /dxaux(N+1)

      ELSE IF (BcDown .EQ. 3) THEN
        Flux(N+1) =0.D0
C Advection 
        Flux(N+1) = Flux(N+1) + C(N)*VF(N+1)*v
      ELSE
        Flux(N+1) = -adown * (Cdown-C(N))

      ENDIF

C Rate of change = negative flux gradient
      DO I = 1,N
        !AVF   = 0.5 * (VF(I)+VF(I+1))
        dC(I) = -(Flux(I+1) - Flux(I)) / VFmid(i) / dx(I)
      ENDDO
    
      RETURN
      END SUBROUTINE diffadv1D
      
      !==============================================================================
! Diffusion in a 1-dimensional finite difference grid 
! all inputs are vectors
! subroutine from ReacTran in isnt\doc\fortran directory
! Added advection A.Capet May2015
!==============================================================================

c      SUBROUTINE diffadv1dads (N, C, Cup, Cdown, fluxup, fluxdown, aup,         &    
c     &                   adown, BcUp, BcDown, D, v, VF,VFmid, dx, dxaux,        &
c     &                   Flux, dC) 
c      IMPLICIT NONE
c      INTEGER N                  ! length of C
cC input
c      DOUBLE PRECISION C(N),v(N)

cC Boundary concentrations (used if Bc..=2,4), fluxes (used if Bc= 1) 
cC and convection coeff (used if Bc=4)
c      DOUBLE PRECISION Cup, Cdown, fluxup, fluxdown, aup, adown

cC Diffusion, volume fraction
c      DOUBLE PRECISION D(N+1), VF(N+1),VFmid(N)

cC grid size, distance from mid to mid
c      DOUBLE PRECISION dx(N), dxaux(N+1)

cC boundary concitions (1= flux, 2=conc, 3 = 0-grad, 4=convect)
c      INTEGER BcUp, BcDown   
      
c      DOUBLE PRECISION, parameter :: aa = 0.5

cC output: fluxes and rate of change
c      DOUBLE PRECISION Flux(N+1), dC(N), difC(N)

cC locals 
c      INTEGER I

cC -------------------------------------------------------------------------------

cC Flux - first, internal cells
c      DO I = 2,N
c        Flux(I) = -VF(I)*D(I)*(C(I)-C(I-1))/dxaux(I)
c      ENDDO

cC Then,  the outer cells 
cC upstream boundary
c      IF (BcUp .EQ. 1) THEN
c        Flux(1) = fluxup

c      ELSE IF (BcUp .EQ. 2) THEN
c        Flux(1) = -VF(1)*D(1) * (C(1)-Cup) /dxaux(1)

c      ELSE IF (BcUp .EQ. 3) THEN
c        Flux(1) = 0.D0

c      ELSE 
c        Flux(1) = aup * (Cup - C(1))
c      ENDIF

cC downstream boundary
c      IF (BcDown .EQ. 1) THEN
c        Flux(N+1) = fluxdown

c      ELSE IF (BcDown .EQ. 2) THEN
c        Flux(N+1) = -VF(N+1)*D(N+1) * (Cdown-C(N)) /dxaux(N+1)

c      ELSE IF (BcDown .EQ. 3) THEN
c        Flux(N+1) =0.D0

c      ELSE
c        Flux(N+1) = -adown * (Cdown-C(N))

c      ENDIF

cC Rate of change = negative flux gradient
c      DO I = 2,N-1
c        difC(i)=(2.d0*aa-1.d0)*C(i) + (1.d0-aa)*C(i+1) - aa*C(i-1)
c      ENDDO
      
c      ! assumes imposed concetration at upper boudary and zero gradient at depth
c        difC(1)=(2.d0*aa-1.d0)*C(1) + (1.d0-aa)*C(2) - aa*Cup
c        difC(N)=0
      
c      DO I = 1,N
c        dC(I) = -(Flux(I+1)-Flux(I))/VFmid(i)/dx(I)-v(i)*VF(N+1)*difC(i)
c      ENDDO
    
c      RETURN
c      END SUBROUTINE diffadv1Dads

!==============================================================================
! Bioirrigation 
! A.Capet Jul 2015
!==============================================================================
      SUBROUTINE BioIrrigation1D (N, C, Cup, AlphIrr, dC) 
      IMPLICIT NONE
      INTEGER N                  ! length of C
C input
      DOUBLE PRECISION C(N)

C Bottom Water Concentration
      DOUBLE PRECISION Cup
      
C Bioirrgation coefficient
      DOUBLE PRECISION AlphIrr(N)
      
C output: fluxes and rate of change
      DOUBLE PRECISION dC(N)

C locals 
      INTEGER I
      
C -------------------------------------------------------------------------------
C Flux - first, internal cells
      DO I = 1,N
        dC(I)      = -(C(I)-Cup) * AlphIrr(I)
      ENDDO
   
      RETURN
      END SUBROUTINE BioIrrigation1D

