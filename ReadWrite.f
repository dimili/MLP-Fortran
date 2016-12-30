!     
!     Subroutines Configuration, NetworkStructure, TrainingInputsOutputs, OutputWrite
!     and CheckInputParameters are implemented i this file.
!*********************************************************************************


      SUBROUTINE Configuration (i1,i2,a1,i3,i4,a2,i11,i5,a3,i6,
     &i7,a4,a5,i8,i9,a6,a7,a8,a9,i10)
      
      IMPLICIT REAL(a-h,o-z)
!.....Opens settings file. See "configuration.txt" 
      OPEN (1, file='configuration.txt', status='old')
!.....Read parameters  
      READ (1, *) i1
      READ (1, *) i2
      READ (1, *) a1
      READ (1, *) i3
      READ (1, *) i4
      READ (1, *) a2
      READ (1, *) i11      
      READ (1, *) i5
      READ (1, *) a3
      READ (1, *) i6
      READ (1, *) i7
      READ (1, *) a4
      READ (1, *) a5
      READ (1, *) i8
      READ (1, *) i9
      READ (1, *) a6
      READ (1, *) a7
      READ (1, *) a8
      READ (1, *) a9
      READ (1, *) i10
      CLOSE (1)
      RETURN
      END
      
      
      SUBROUTINE NetworkStructure (NoOfSets,NoOfHiddenLayers,
     &NOFN,NOFNMAX)
      
      INTEGER NoOfSets,NoOfHiddenLayers,NOFN,NOFNMAX
      DIMENSION NOFN(NoOfHiddenLayers+2)
!.....Open file that contains NN structure 
      OPEN (2, file='networkstructure.txt', status='old')
!.....Read parameters      
      READ (2, *) NoOfSets !Number of training samples
      READ (2, *) NoOfHiddenLayers !Number of hidden layers    
!.....NOFN contains the number of neurons of each layer   
      READ (2, *) (NOFN(i), i = 1, NoOfHiddenLayers+2)
      CLOSE (2) 
!.....Find maximum number of neurons. Used for dynamic memory allocation     
      NOFNMAX=NOFN(1)
      DO i = 2, NoOfHiddenLayers+2
      IF (NOFN(i)>NOFNMAX) NOFNMAX=NOFN(i)
      ENDDO
      RETURN
      END
      
      
      SUBROUTINE TrainingInputsOutputs (X,D,NOFN,NoOfSets,
     &NoOfHiddenLayers)
      
      IMPLICIT REAL(a-h,o-z)
      INTEGER NoOfSets,NoOfHiddenLayers,NOFN
      DIMENSION NOFN(NoOfHiddenLayers+2),
     &X(NoOfSets,NOFN(1)), D(NoOfSets,NOFN(NoOfHiddenLayers+2))
!.....Open file that contains training samples
      OPEN (3, file='traininput.txt', status='old')
!.....Create vectors InputVector X and DesiredOutput D
!.....NOFN(1):Inputs number
!.....NOFN(NoOfHiddenLayers+2):Outputs number 
      READ (3, *) ((X(i,j), j = 1, NOFN(1)),
     &(D(i,k), k = 1, NOFN(NoOfHiddenLayers+2)), i =1, NoOfSets)
      CLOSE (3)
      RETURN
      END
      
      
      
      SUBROUTINE OutputWrite (NoOfHiddenLayers,NOFN,W,XMIN,XMAX,OMIN,
     &OMAX,ifunction,alpha1,alpha2,beta,NI,AverageError,NOFNMAX,bias,
     &elapsed_time)
      
      IMPLICIT REAL(a-h,o-z)
      INTEGER NoOfHiddenLayers,NOFN,ifunction,NI,NOFNMAX
      DIMENSION NOFN(NoOfHiddenLayers+2),
     &W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1),XMIN(NOFN(1)),
     &XMAX(NOFN(1)),OMIN(NOFN(NoOfHiddenLayers+2)),
     &OMAX(NOFN(NoOfHiddenLayers+2))
!.....Export weight matrix W to file
      OPEN (4, file='output.txt')
      WRITE (4,100) 'Network trained after:', NI,'epochs'
      WRITE (4,101) 'AverageError=', AverageError
      WRITE (4,102) 'Training time=', elapsed_time, 'seconds'
      WRITE (4,*)
      WRITE (4,*) 'The number of hidden layers is:'
      WRITE (4,*) NoOfHiddenLayers
      WRITE (4,*)
      WRITE (4,*) 'The number of neurons in every layer are:'
      WRITE (4,*) (NOFN(i), i = 1, NoOfHiddenLayers+2)
      WRITE (4,*)
      WRITE (4,*) 'The weight values are:'
      WRITE (4,*) (((W(i,j,k),k=1,NOFN(i)+1), j = 1, NOFN(i+1)),
     &i =1, NoOfHiddenLayers+1)
      WRITE (4,*)
      WRITE (4,*) 'Some values needed so as UseNetwork works properly'
      WRITE (4,*) (XMIN(i),i=1, NOFN(1))
      WRITE (4,*) (XMAX(i),i=1, NOFN(1))  
      WRITE (4,*) (OMIN(i),i=1, NOFN(NoOfHiddenLayers+2))
      WRITE (4,*) (OMAX(i),i=1, NOFN(NoOfHiddenLayers+2)) 
      WRITE (4,*) ifunction
      WRITE (4,*) alpha1
      WRITE (4,*) alpha2
      WRITE (4,*) beta
      WRITE (4,*) bias
      CLOSE (4)
  100 FORMAT (A22,I7,A7)
  101 FORMAT (A13,F9.6)
  102 FORMAT (A14,F10.2,A8)      
      RETURN
      END
      
      
      
      SUBROUTINE CheckInputParameters(imode,irandom,rate,irate,
     &imoment,amoment,idecay2,ifunction,alpha1,alpha2,beta,nfault,ICONT)

!.....Check settings      
      IF ((imode/=0).AND.(imode/=1)) THEN
        WRITE(*,*) 'Problem with 1st parameter in configuration file'
        nfault=1
      ENDIF
      IF ((irandom/=0).AND.(irandom/=1)) THEN
        WRITE(*,*) 'Problem with 2nd parameter in configuration file'
        nfault=1
      ENDIF
      IF ((rate<=0).OR.(rate>=1)) THEN
        WRITE(*,*) 'Problem with 3rd parameter in configuration file'
        nfault=1
      ENDIF
      IF ((irate/=1).AND.(irate/=2).AND.(irate/=3)) THEN
        WRITE(*,*) 'Problem with 4th parameter in configuration file'
        nfault=1
      ENDIF
      IF ((imoment/=0).AND.(imoment/=1)) THEN
        WRITE(*,*) 'Problem with 8th parameter in configuration file'
        nfault=1
      ENDIF
      IF ((amoment<=0).OR.(amoment>=1)) THEN
        WRITE(*,*) 'Problem with 9th parameter in configuration file'
        nfault=1
      ENDIF
      IF ((idecay2/=0).AND.(idecay2/=1)) THEN
        WRITE(*,*) 'Problem with 10th parameter in configuration file'
        nfault=1
      ENDIF
      IF ((ifunction/=1).AND.(ifunction/=2).AND.(ifunction/=3)) THEN
        WRITE(*,*) 'Problem with 15th parameter in configuration file'
        nfault=1
      ENDIF
      IF (alpha1<=0) THEN
        WRITE(*,*) 'Problem with 16th parameter in configuration file'
        nfault=1
      ENDIF
      IF (alpha2<=0) THEN
        WRITE(*,*) 'Problem with 17th parameter in configuration file'
        nfault=1
      ENDIF
      IF (beta<=0) THEN
        WRITE(*,*) 'Problem with 18th parameter in configuration file'
        nfault=1
      ENDIF
      IF ((ICONT/=0).AND.(ICONT/=1)) THEN
        WRITE(*,*) 'Problem with 20th parameter in configuration file'
        nfault=1
      ENDIF                                        
      RETURN
      END