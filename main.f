!
!     Multilayer Back Propagation Neural Network Training
!
!     мN: Neural Network
!
!********************************************************************************************* 
      PROGRAM TrainBackPropagationNN
      USE IFPORT
      IMPLICIT REAL(a-h,o-z)
      INTEGER imode,irandom,irate,noe1,imoment,idecay2,noe2,NoOfSets,
     &iteration,ifunction,NoOfHiddenLayers,NOFN,NI,nfault,ICONT,NOFNMAX,
     $npa
!.....Dynamic memory allocation
      ALLOCATABLE :: NOFN(:),X(:,:),D(:,:),XMIN(:),XMAX(:),
     &OMIN(:),OMAX(:),W(:,:,:),Y(:,:)
      
!.....Calculate run time     
      elapsed_time = TIMEF( ) 
	   
!.....Subroutines of NN parameters, NN structure and NN training set are called     
      CALL Configuration (imode,irandom,rate,irate,noe1,per1,npa,
     &imoment,amoment,idecay2,noe2,per2,rms,iteration,ifunction,
     &alpha1,alpha2,beta,bias,ICONT)
      ALLOCATE(NOFN(NoOfHiddenLayers+2))
      CALL NetworkStructure (NoOfSets,NoOfHiddenLayers,NOFN,NOFNMAX)
      ALLOCATE(X(NoOfSets,NOFN(1)),D(NoOfSets,NOFN(NoOfHiddenLayers+2)))
      CALL TrainingInputsOutputs (X,D,NOFN,NoOfSets,NoOfHiddenLayers)
      
!.....Check input parameters     
      in=0
      CALL CheckInputParameters(imode,irandom,rate,irate,
     &imoment,amoment,idecay2,ifunction,alpha1,alpha2,beta,nfault,ICONT)
      
      IF (nfault==1) THEN
      PAUSE
      GOTO 10
      ENDIF
      
      WRITE (*,*)
      WRITE (*,*) '|----------------------------------------------|'
      WRITE (*,*) '| Please wait. Neural Network is being trained |'
      WRITE (*,*) '|----------------------------------------------|'

!.....Input vector normalization
      ALLOCATE(XMIN(NOFN(1)),XMAX(NOFN(1)))
      DO j=1, NOFN(1)
        XMIN(j)=X(1,j)
        XMAX(j)=X(1,j)
        DO i=2,NoOfSets
            IF (X(i,j)<XMIN(j)) XMIN(j)=X(i,j)
            IF (X(i,j)>XMAX(j)) XMAX(j)=X(i,j)           
        ENDDO

!.......If влим=-1 and влав=1 then no normalization takes place
!        XMIN(j)=-1.0
!        XMAX(j)=1.0
      ENDDO
      
!.....Output vector normalization
      ALLOCATE(OMIN(NOFN(NoOfHiddenLayers+2)),
     &OMAX(NOFN(NoOfHiddenLayers+2)))
      DO k=1, NOFN(NoOfHiddenLayers+2)
        OMIN(k)=D(1,k)
        OMAX(k)=D(1,k)
        DO i=2,NoOfSets
            IF (D(i,k)<OMIN(k)) OMIN(k)=D(i,k)
            IF (D(i,k)>OMAX(k)) OMAX(k)=D(i,k)            
        ENDDO
		

!.......This helps increasing the training rate!!!
        diafora=OMAX(k)-OMIN(k)
        OMIN(k)=OMIN(k)-0.1*diafora
        OMAX(k)=OMAX(k)+0.1*diafora
      ENDDO
      

      ALLOCATE(W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1),
     &Y(NoOfHiddenLayers+2,NOFNMAX+1))

!.....SequentialTrain or BatchTrain subroutine is called	 
      IF (imode==0) THEN      
        CALL SequentialTrain(NoOfSets,NoOfHiddenLayers,NOFN,iteration,
     &  rate,X,D,W,Y,XMIN,XMAX,OMIN,OMAX,ifunction,alpha1,alpha2,beta,
     &  irandom,irate,noe1,per1,npa,imoment,amoment,idecay2,noe2,per2,
     &  rms,NI,AverageError,NOFNMAX,bias,ICONT)
      ELSEIF (imode==1) THEN
        CALL BatchTrain(NoOfSets,NoOfHiddenLayers,NOFN,iteration,
     &  rate,X,D,W,Y,XMIN,XMAX,OMIN,OMAX,ifunction,alpha1,alpha2,beta,
     &  irandom,irate,noe1,per1,npa,imoment,amoment,idecay2,noe2,per2,
     &  rms,NI,AverageError,NOFNMAX,bias,ICONT)
      ENDIF
      elapsed_time = TIMEF( )
	  
!.....OutputWrite exports results to a file      
      CALL OutputWrite (NoOfHiddenLayers,NOFN,W,XMIN,XMAX,OMIN,
     &OMAX,ifunction,alpha1,alpha2,beta,NI,AverageError,NOFNMAX,bias,
     &elapsed_time)
   
   10 END PROGRAM TrainBackPropagationNN