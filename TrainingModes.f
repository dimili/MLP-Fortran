!
!     Subroutines SequentialTrain and BatchTrain are implemented in this file
!*********************************************************************************


!.....��� ������ ����������� Sequential (���� �������: Online,Stochastic) � �������� 
!.....���� ����� ��� ���������� ����� ������� ���� ��� ���������� ���� ��� (�������-
!.....���������� ������) �����������. ������ �� ���� �����-����� ������ ����� ����������
!.....��� ��������� ���� ��� ��� �� ���. �� ������������� ��� ������� �����:
!.....������� �������� �����
!.....� ���������� ����������� ��� ������� �� ������ ����� ��� ����� �� ����� �����
!.....��� ��� ���������� ��������� ��� ����� ����������-->��������� � ���������� ��
!.....����������� � ���������� �� ������ ��������
!.....�������� ���������� �� ����������� ��������, �� ������� ��� ��� ����� ���� ����
!.....�� ��� ��� �����������

      SUBROUTINE SequentialTrain(NoOfSets,NoOfHiddenLayers,NOFN,
     &iteration,rate,X,D,W,Y,XMIN,XMAX,OMIN,OMAX,ifunction,alpha1,
     &alpha2,beta,irandom,irate,noe1,per1,npa,imoment,amoment,idecay2,
     &noe2,per2,rms,NI,AverageError,NOFNMAX,bias,ICONT)
      
      IMPLICIT REAL(a-h,o-z)
      INTEGER NoOfSets,NoOfHiddenLayers,NOFN,iteration,NS,NI,ifunction,
     &irandom,ithesi,irate,iter,noe1,imoment,idecay2,noe2,ICONT,npa
      DIMENSION NOFN(NoOfHiddenLayers+2),X(NoOfSets,NOFN(1)), 
     &D(NoOfSets,NOFN(NoOfHiddenLayers+2)),
     &W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1),
     &Y(NoOfHiddenLayers+2,NOFNMAX+1),E(NOFN(NoOfHiddenLayers+2)),
     &XMIN(NOFN(1)),XMAX(NOFN(1)),
     &OMIN(NOFN(NoOfHiddenLayers+2)),OMAX(NOFN(NoOfHiddenLayers+2)),
     &Delta(NoOfHiddenLayers+2,NOFNMAX+1),
     &PREV(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1),
     &DW(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1)
      

!.....�������� � WeightInitialization ��� ��� ������������ ��� ������ ��� ���������� �����
      CALL WeightInitialization(NoOfHiddenLayers,NOFN,W,NOFNMAX,ICONT)
         
!.....����������� ��� ������ PREV ��� ��������������� ��� �� ������� ��� �������� ��� ����� DW ��� ������������
!.....���������. ����� ���� ��������� ������ ����� ���� ��� ����������       
      DO ia=1,NoOfHiddenLayers+1
        DO ja=1,NOFN(ia+1)
            DO ka=1,NOFN(ia)+1
                PREV(ia,ja,ka)=0.0
            ENDDO
        ENDDO
      ENDDO
!.....���������� ��� ������� learning-curve ��� learning-curve 10
!.....�� ����� ���������� ��� ������� ��� Average Error �� ���� ����� - �����
!.....�� ������� ���������� ��� ������� ��� Average Error ��� 10 ������ ����
!.....�� ���������� �������� �������� �� ��������� ��� ������� �� ������� �� �������
      OPEN (15, file='learning-curve.txt')
      OPEN (16, file='learning-curve 10.txt')
      WRITE(15,*) 'Average error during iterations'
      WRITE(15,*)
      WRITE(16,*) 'Average error every 10 iterations'
      WRITE(16,*)
      
      NI=0      
      DO i=1,iteration
        NI=NI+1 !� NI ����� ��� ������ ��� ������ �����������
!.......���������� ��� ��������� ����������� �� ������ ����� ��� ����� �� �����
!.......���������� ������� ��� ��������� ������������ ��� ��������� ��� ������� X ��� D �� ������ �����,
!.......���� ���� ��� �������� ���� ������-�����       
        IF (irandom==1) THEN
            DO k=1,NoOfSets
            ithesi = k + MOD(IRAND(0),NoOfSets+1-k)
                DO l=1,NOFN(1) 
                    temp = X(k,l)
                    X(k,l) = X(ithesi,l)
                    X(ithesi,l) = temp
                ENDDO
                DO m=1,NOFN(NoOfHiddenLayers+2)
                    temp = D(k,m)
                    D(k,m) = D(ithesi,m)
                    D(ithesi,m) = temp
                ENDDO
            ENDDO
        ENDIF

!.......���� ��������� ��� ������ ��������� ����� ����������� ���������� �� �������� if
!.......�� irate=1 ������ ������� ����� ����������� ����� ��� ���������� �� if
!.......�� irate=2 ������ �������� ������ ��� ������ ����������� 
!.......���� iter ������������ �� �������� ��� ��������� ��� ������ ���� �����
!.......��������� �� ��������� �� ��� ������ ��� ������ ���� ��� ���������� ��� ������ �������
!.......�� ������� ������ ��� ������ �����������. �� iter=0, ��� �������� ��� ������������� ��
!.......������������ ������, ������� � ������. ��������� ������ � ������ ����������� �� ��
!.......������� ���� ��� ��� �������� ����
!.......�� irate=3 ������ ��� ��������� search-then-converge
!.......��� ��� ��������� � ������ ����������� �� �� ������� ���� ��� ��� �������� ����     
        IF (irate==2) THEN
            iter=MOD(NI,noe1)
            IF ((iter==0).AND.(rate>0.05)) THEN
            rate=rate-per1/100.0*rate
            ENDIF
        ELSEIF (irate==3) THEN
            IF (rate>0.05) THEN
            rate=rate/(1+(NI/npa))
            ENDIF
        ENDIF
        
      
        NS=0
        AverageError=0.0         
!.......��� ���� ������� ��� ��� �������� ����� � ForwardPass ��� �� ������� ��� ��������
!.......��� ��� ������ ��� �� ���� ����� ��� ��� �������� � BackwardPass ��� �� ������� ���
!.......��������� ��� ��� ����� ���� ������ ��� ��� ���������� ��� ������� �������� ������
!.......(local gradient).      
        DO j=1,NoOfSets
            NS=NS+1 !� NS ����� ��� ������ ��� ��� ����������� 
            CALL ForwardPass(NS,NoOfHiddenLayers,NOFN,X,W,Y,XMIN,XMAX,
     &      ifunction,alpha1,alpha2,beta,NoOfSets,NOFNMAX,bias)
            CALL BackwardPass(NS,NoOfHiddenLayers,NOFN,W,D,Y,E,OMIN,
     &      OMAX,ifunction,alpha1,alpha2,beta,Delta,NoOfSets,NOFNMAX)

!.......� ForwardPass ���������� ��� ������ ��� ������ Y ��� ��������. ����� ��������������� ���� ��
!.......������ ��� BackwardPass ��� ��� ��� ��� ���������� ���� ��������� ��� ���������� ����� ��������.           
!.......� BackwardPass ���������� ��� ������ ��� ������� �������� ������ Delta ��� ��������.

!...........�� ��������� if ������� �� ��������������� ���� ���� ��� ����������. �� ��������� if ������� ���
!...........��������� ��� ������ �������� ��� ���� ��� ����� �� �� ������� ��� ������. � ���������� ��� �����
!...........���� �� ��� �������� ���������� ��� ������ �����������. ������� �� �� �� ������ ���� � ���
!...........��������������� � ��������� ���������� ��������� ��� �����.                      
            IF (imoment==1) THEN
                IF (idecay2==1) THEN
                    iter2=MOD(NI,noe2)
                    IF ((iter2==0).AND.(amoment>0.05)) THEN
                        amoment=amoment-per2/100.0*amoment
                    ENDIF
                ENDIF
                DO ia=1,NoOfHiddenLayers+1
                    DO ja=1,NOFN(ia+1)
                        DO ka=1,NOFN(ia)+1
                            DW(ia,ja,ka)=amoment*PREV(ia,ja,ka)+
     &                      rate*Delta(ia+1,ja)*Y(ia,ka)
                            W(ia,ja,ka)=W(ia,ja,ka)+DW(ia,ja,ka)
                            PREV(ia,ja,ka)=DW(ia,ja,ka)
                        ENDDO
                    ENDDO
                ENDDO    
            ELSE
                DO ia=1,NoOfHiddenLayers+1
                    DO ja=1,NOFN(ia+1)
                        DO ka=1,NOFN(ia)+1
                            DW(ia,ja,ka)=rate*Delta(ia+1,ja)*Y(ia,ka)
                            W(ia,ja,ka)=W(ia,ja,ka)+DW(ia,ja,ka)
                        ENDDO
                    ENDDO
                ENDDO
            ENDIF   
!...........���� ����������� � ���������� ��������� ��� ����� ��� �� ���� ��� ���������� ��� ��������
!...........�������� � ErrorCalc ��� ��� ���������� ��� ��������� ������ ��� ��������             
            CALL ErrorCalc(NOFN,NoOfHiddenLayers,E,Error)
!...........������� ������� ��� ��� �� ������� ��� ���
            AverageError=AverageError+Error 
        ENDDO
!.......����������� ��� Average Squared Error        
        AverageError=SQRT(AverageError/NS)
!.......�������� ��� ����� ��� AverageError ��� ������ learning-curve ��� learning-curve 10        
        WRITE (15,20) i,AverageError
        IF ((i==1).OR.(mod(i,10)==0)) THEN
            WRITE (16,20) i,AverageError
        ENDIF
        IF (AverageError<rms) GOTO 11
      ENDDO
   11 CLOSE(15)
      CLOSE(16)
   20 FORMAT (I7,F9.5)
      RETURN
      END
      
      




!.....��� ������ ����������� Batch � �������� ���� ����� ��� ���������� ����� 
!.....������� ���� ��� ���������� ��������� ��� ��� (�������-���������� ������) �����������. 
!.....� ������� ���� ����������� ��� ����� �� ������������ ������������ (parallelization)
      
      SUBROUTINE BatchTrain(NoOfSets,NoOfHiddenLayers,NOFN,
     &iteration,rate,X,D,W,Y,XMIN,XMAX,OMIN,OMAX,ifunction,alpha1,
     &alpha2,beta,irandom,irate,noe1,per1,npa,imoment,amoment,idecay2,
     &noe2,per2,rms,NI,AverageError,NOFNMAX,bias,ICONT)
      
      IMPLICIT REAL(a-h,o-z)
      INTEGER NoOfSets,NoOfHiddenLayers,NOFN,iteration,NS,NI,ifunction,
     &irandom,ithesi,irate,iter,noe1,imoment,idecay2,noe2,ICONT,npa
      DIMENSION NOFN(NoOfHiddenLayers+2),X(NoOfSets,NOFN(1)), 
     &D(NoOfSets,NOFN(NoOfHiddenLayers+2)),
     &W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1),
     &Y(NoOfHiddenLayers+2,NOFNMAX+1),E(NOFN(NoOfHiddenLayers+2)),
     &XMIN(NOFN(1)),XMAX(NOFN(1)),
     &OMIN(NOFN(NoOfHiddenLayers+2)),OMAX(NOFN(NoOfHiddenLayers+2)),
     &Delta(NoOfHiddenLayers+2,NOFNMAX+1),
     &PREV(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1),
     &DW(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1)

!.....�������� � WeightInitialization ��� ��� ������������ ��� ������ ��� ���������� �����
      CALL WeightInitialization(NoOfHiddenLayers,NOFN,W,NOFNMAX,ICONT)
         
!.....����������� ���� ������� PREV ��� DW
!.....� DW ����� � ������� ��������� ��� �����
!.....� PREV ����� ��� ��������� ��� ����� ��� ������������ ������. ��������������� ���� ��������� ������ ����� 
      DO ia=1,NoOfHiddenLayers+1
        DO ja=1,NOFN(ia+1)
            DO ka=1,NOFN(ia)+1
                DW(ia,ja,ka)=0.0
                PREV(ia,ja,ka)=0.0
            ENDDO
        ENDDO
      ENDDO
!.....���������� ��� ������� learning-curve ��� learning-curve 10
!.....�� ����� ���������� ��� ������� ��� Average Error �� ���� ����� - �����
!.....�� ������� ���������� ��� ������� ��� Average Error ��� 10 ������ ����
!.....�� ���������� �������� �������� �� ��������� ��� ������� �� ������� �� �������
      OPEN (15, file='learning-curve.txt')
      OPEN (16, file='learning-curve 10.txt')
      WRITE(15,*) 'Average error during iterations'
      WRITE(15,*)
      WRITE(16,*) 'Average error every 10 iterations'
      WRITE(16,*)
      
      NI=0      
      DO i=1,iteration
      NI=NI+1 !� NI ����� ��� ������ ��� ������ �����������
!.......��������� ���������� ������ �����������,���� ��� SequentialTrain.          
        IF (irate==2) THEN
            iter=MOD(NI,noe1)
            IF ((iter==0).AND.(rate>0.05)) THEN
            rate=rate-per1/100.0*rate
            ENDIF
        ELSEIF (irate==3) THEN
            IF (rate>0.05) THEN
            rate=rate/(1+(NI/npa))
            ENDIF
        ENDIF
        
      
        NS=0
        AverageError=0.0         
!.......��� �� ������ ��� �������� ��� ��� �������� ����� � ForwardPass ��� �� ������� ����
!.......��� ��� ������ ��� �� ���� ����� ��� ��� �������� � BackwardPass ��� �� ������� ���
!.......��������� ��� ��� ����� ���� ������ ��� ��� ���������� ��� ������� �������� ������
!.......(local gradient).
        DO j=1,NoOfSets    
            NS=NS+1 !� NS ����� ��� ������ ��� ��� ����������� 
            CALL ForwardPass(NS,NoOfHiddenLayers,NOFN,X,W,Y,XMIN,XMAX,
     &      ifunction,alpha1,alpha2,beta,NoOfSets,NOFNMAX,bias)
            CALL BackwardPass(NS,NoOfHiddenLayers,NOFN,W,D,Y,E,OMIN,
     &      OMAX,ifunction,alpha1,alpha2,beta,Delta,NoOfSets,NOFNMAX)

            DO ia=1,NoOfHiddenLayers+1
                DO ja=1,NOFN(ia+1)
                    DO ka=1,NOFN(ia)+1                        
                       DW(ia,ja,ka)=DW(ia,ja,ka)+Delta(ia+1,ja)*Y(ia,ka)
                    ENDDO
                ENDDO
            ENDDO
!...........� BackwardPass ���������� ��� ������ ��������� � ��� �������� ������ � ������ ������� �� ������ 
!...........���� ErrorCalc ��� ��� ���������� ��� ��������� ������ ��� ��������             
            CALL ErrorCalc(NOFN,NoOfHiddenLayers,E,Error)
!...........������� ������� ��� ��� �� ������� ��� ���
            AverageError=AverageError+Error 
!.......��� ������ ����������� Batch ��������� � ������ ����������� ���� ��� ��� ����������� ���� ���������� ��� �������� ��� �����       
        ENDDO
              
!.......����������� ��� Average Squared Error        
        AverageError=SQRT(AverageError/NS)
!.......�������� ��� ����� ��� AverageError ��� ������ learning-curve ��� learning-curve 10        
        WRITE (15,30) i,AverageError
        IF ((i==1).OR.(mod(i,10)==0)) THEN
            WRITE (16,30) i,AverageError
        ENDIF
        IF (AverageError<rms) GOTO 12
        
!.......�� �� Average Squared Error ��� ���������� �� ���� ���� ��������� �� �������� ��� �����        
        IF (imoment==1) THEN
            IF (idecay2==1) THEN
                iter2=MOD(NI,noe2)
                IF ((iter2==0).AND.(amoment>0.05)) THEN
                    amoment=amoment-per2/100.0*amoment
                ENDIF
            ENDIF
            DO ia=1,NoOfHiddenLayers+1
                DO ja=1,NOFN(ia+1)
                    DO ka=1,NOFN(ia)+1
                        DW(ia,ja,ka)=amoment*PREV(ia,ja,ka)+
     &                  rate/NoOfSets*DW(ia,ja,ka)
                        W(ia,ja,ka)=W(ia,ja,ka)+DW(ia,ja,ka)
                        PREV(ia,ja,ka)=DW(ia,ja,ka)
                    ENDDO
                ENDDO
            ENDDO    
        ELSE
            DO ia=1,NoOfHiddenLayers+1
                DO ja=1,NOFN(ia+1)
                    DO ka=1,NOFN(ia)+1                        
                        DW(ia,ja,ka)=rate/NoOfSets*DW(ia,ja,ka)
                        W(ia,ja,ka)=W(ia,ja,ka)+DW(ia,ja,ka)
                    ENDDO
                ENDDO
            ENDDO
        ENDIF   

      ENDDO
   12 CLOSE(15)
      CLOSE(16)
   30 FORMAT (I7,F9.5) 
      RETURN
      END