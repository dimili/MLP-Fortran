
!
!     UseBackPropagationNN calculates the output of a trained neural network
!
!*********************************************************************************

      PROGRAM UseBackPropagationNN
      USE IFPORT
      IMPLICIT REAL(a-h,o-z)
      INTEGER NoOfHiddenLayers,NOFN,ifunction,NOFNMAX,inp,NMBR
      CHARACTER*12 fname
      LOGICAL exists,result
      ALLOCATABLE :: NOFN(:),X(:,:),D(:,:),XMIN(:),XMAX(:),OMIN(:),
     &OMAX(:),W(:,:,:),Y(:,:),OUT(:)
      
!.....Weight values and other parameters are read from "output.txt"
!.....which is created during training     
      OPEN (4, file='output.txt', status='old')
      READ (4,*)
      READ (4,*)
      READ (4,*)
      READ (4,*)
      READ (4,*)
      READ (4,*) NoOfHiddenLayers
      READ (4,*)
      READ (4,*)
      ALLOCATE(NOFN(NoOfHiddenLayers+2))
      READ (4,*) (NOFN(i), i = 1, NoOfHiddenLayers+2)
      READ (4,*)
      READ (4,*)
      
      NOFNMAX=NOFN(1)
      DO i = 2, NoOfHiddenLayers+2
      IF (NOFN(i)>NOFNMAX) NOFNMAX=NOFN(i)
      ENDDO
      
      ALLOCATE(W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1))
      READ (4,*) (((W(i,j,k),k=1,NOFN(i)+1), j = 1, NOFN(i+1)),
     &i =1, NoOfHiddenLayers+1)
      READ (4,*)
      READ (4,*)
      ALLOCATE(XMIN(NOFN(1)),XMAX(NOFN(1)),
     &OMIN(NOFN(NoOfHiddenLayers+2)),OMAX(NOFN(NoOfHiddenLayers+2)))
      READ (4,*) (XMIN(i),i=1, NOFN(1))
      READ (4,*) (XMAX(i),i=1, NOFN(1))  
      READ (4,*) (OMIN(i),i=1, NOFN(NoOfHiddenLayers+2))
      READ (4,*) (OMAX(i),i=1, NOFN(NoOfHiddenLayers+2))
      READ (4,*) ifunction
      READ (4,*) alpha1
      READ (4,*) alpha2
      READ (4,*) beta
      READ (4,*) bias
      CLOSE(4)
      
!.....Choose input method for input vector:
!..... 1 keyboard
!..... 2 file
!..... 3 use the training set
   20 WRITE (*,*) 'Choose input method and press ENTER'
      WRITE (*,*) '1-Keyboard'
      WRITE (*,*) '2-File'
      WRITE (*,*) '3-Training Inputs File'
      READ (*,*) inp
      IF ((inp/=1).AND.(inp/=2).AND.(inp/=3)) GOTO 20
      IF (inp==1) THEN
        WRITE (*,*) 'How many input sets do you want to test?'
        READ (*,*) NMBR
        WRITE (*,*) 'Give the',NMBR, 'sets of inputs one by one'
        ALLOCATE(X(NMBR,NOFN(1)))
        DO nset=1,NMBR
            READ (*,*) (X(nset,j), j = 1, NOFN(1))
        ENDDO    
      ENDIF
      IF (inp==2) THEN
   10   WRITE (*, '(1X, A\)') 'Enter the file name: '      
        READ (*, '(A)') fname
        INQUIRE (FILE = fname, EXIST = exists)
        IF (.NOT. exists) THEN
            WRITE (*,'(2A/)') ' >> Cannot find file ', fname
            GOTO 10
        END IF
        result = SYSTEMQQ('type '//fname//' | find /v /c "hello">zx')
        OPEN (11,file='zx')
        READ (11,*) NMBR
        CLOSE(11)
        result = SYSTEMQQ('del zx') 
        OPEN (10, file=fname, status='old')
        ALLOCATE(X(NMBR,NOFN(1)))
        DO nset=1,NMBR
            READ (10, *) (X(nset,j), j = 1, NOFN(1))
        ENDDO
        CLOSE(10)
      ENDIF
      IF (inp==3) THEN
        result = SYSTEMQQ('type traininput.txt | find /v /c "hello">zx')
        OPEN (11,file='zx')
        READ (11,*) NMBR
        CLOSE(11)
        result = SYSTEMQQ('del zx')
        OPEN (16, file='traininput.txt', status='old')
        ALLOCATE(X(NMBR,NOFN(1)),D(NMBR,NOFN(NoOfHiddenLayers+2)))
        DO nset=1,NMBR
            READ (16, *) (X(nset,j), j = 1, NOFN(1)),
     &      (D(nset,k), k = 1, NOFN(NoOfHiddenLayers+2))
        ENDDO
        CLOSE(16)
      ENDIF  
      
      ALLOCATE(Y(NoOfHiddenLayers+2,NOFNMAX+1))
      ALLOCATE(OUT(NOFN(NoOfHiddenLayers+2)))

!.....If training set is used (input method 3) then results are saved in "results.txt"    
      OPEN (21, file='results.txt')
      WRITE (21,*) 'INPUTS / REAL OUTPUTS / PREDICTED OUTPUTS'
      WRITE (21,*)
      
      DO nset=1,NMBR
	  
!.....ForwardPass is called to calculate the output of the trained neural network      
      CALL ForwardPass(nset,NoOfHiddenLayers,NOFN,X,W,Y,XMIN,XMAX,
     &ifunction,alpha1,alpha2,beta,NMBR,NOFNMAX,bias)
      
!.......Inverse normalization of output vector      
        DO i=1,NOFN(NoOfHiddenLayers+2)
            IF ((ifunction==1)) THEN
            OUT(i)=EXO1A(Y(NoOfHiddenLayers+2,i),OMIN(i),OMAX(i))
            ELSEIF ((ifunction==2)) THEN
            OUT(i)=EXO2A(Y(NoOfHiddenLayers+2,i)/alpha2,OMIN(i),OMAX(i))
            ELSE
            OUT(i)=EXO2A(Y(NoOfHiddenLayers+2,i)/alpha2,OMIN(i),OMAX(i))
            ENDIF
        ENDDO
        
        IF ((inp==1).OR.(inp==2)) THEN
            WRITE(*,*)
            WRITE(*,*) 'INPUTS:'
            WRITE(*,*) (X(nset,j), j = 1, NOFN(1))
            WRITE(*,*) 'NEURAL NETWORK OUTPUTS:'
            WRITE(*,*) (OUT(i), i = 1, NOFN(NoOfHiddenLayers+2))
            WRITE(*,*)
            PAUSE
        ELSE
            WRITE (21,*) (X(nset,j), j = 1, NOFN(1)),' - ',
     &      (D(nset,k), k = 1, NOFN(NoOfHiddenLayers+2)),' - ',
     &      (OUT(i), i = 1, NOFN(NoOfHiddenLayers+2))
        ENDIF
      
      ENDDO
      CLOSE(21)
      END PROGRAM UseBackPropagationNN
      
