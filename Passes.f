!
!     Subroutines WeightInitialization,ForwardPass, BackwardPass and ErrorCalc are implemented in this file.
!
!	  WeightInitialization: initializes the weight matrix
!     ForwardPass: propagates NN input towards NN output calculating the neuron outputs
!     BackwardPass: propagates NN output error towards the NN input 
!     ErrorCalc: calculates total error
!*********************************************************************************      
      
      
    
      
      SUBROUTINE WeightInitialization(NoOfHiddenLayers,NOFN,W,NOFNMAX,
     &ICONT)
      
      USE DFLIB   ! ��� ��� seed ��� random
      REAL W
      LOGICAL exists
      INTEGER NoOfHiddenLayers,NOFN,ks,ICONT
      DIMENSION NOFN(NoOfHiddenLayers+2),
     &W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1)
     
!.....������������ ��� ������ ��� ���������� �����
!.....�� ICONT=0 � ������������ ������� �� �������� ��������
!.....�� ICONT=1 �� ����� ��� ���������� ����� ���������� ��� �� ��������� ������ ������. ��������� ��������� �����������

      IF (ICONT==0) THEN
!.....���������� seed ��� ��� ��������� ������������ �������
      CALL SEED(RND$TIMESEED)

      
!.....� random ������� ������������� �������� �� ���������� �������� ��� �������� (0,1)
!.....�� ��� �������� �������������� ����������� �� �������� ��� �������� (-1,1) ���� �� 
!.....��������� ������ �� ������������ ����������      
!.....� ������ ����������� ��� ������ ����� ��� ���� �������� ���� �������� (������� � ������)
!.....��������� �� ���� �� ���� ������� ��� �������� �������� ��� ��� �������� ����� ��� �� ������� 
!.....�� �� ������� ��� �������
      DO i=1,NoOfHiddenLayers+1
        DO j=1,NOFN(i+1)
            DO k=1,NOFN(i)+1
!...............W(������� ��� ������,�������� ��� ���������,�������� ��� ����������)                        
                W(i,j,k)=2*(RANDOM(0)-0.5)
            ENDDO
        ENDDO
      ENDDO
      
      ELSEIF (ICONT==1) THEN

!.....��������� � ������ ��� ������� ��� �� ����� �� ���������� �� ����� ��� ���������� �����          
      INQUIRE (FILE = 'output.txt', EXIST = exists)
!.....���� ��������� ��� ��� ������� �� ������ ������� ������������ �� ������� �����      
      IF (.NOT. exists) THEN
        CALL SEED(RND$TIMESEED)
        DO i=1,NoOfHiddenLayers+1
            DO j=1,NOFN(i+1)
                DO k=1,NOFN(i)+1                     
                    W(i,j,k)=2*(RANDOM(0)-0.5)
                ENDDO
            ENDDO
        ENDDO
      ELSE
        OPEN (4, file='output.txt', status='old')
        READ (4,*)
        READ (4,*)
        READ (4,*)
        READ (4,*)
        READ (4,*)
        READ (4,*)
        READ (4,*)
        READ (4,*)
        READ (4,*)
        READ (4,*)
        READ (4,*)
        READ (4,*) (((W(i,j,k),k=1,NOFN(i)+1), j = 1, NOFN(i+1)),
     &  i =1, NoOfHiddenLayers+1)
        CLOSE(4)
      ENDIF
      
      ENDIF
         
!.....� WeightInitialization ���������� ��� ������ ��� ����� W(�������,��������-���������,��������-����������)      
      RETURN
      END
      
      
      
      
      
      
      
      SUBROUTINE ForwardPass(NS,NoOfHiddenLayers,NOFN,X,W,Y,XMIN,XMAX,
     &ifunction,alpha1,alpha2,beta,NoOfSets,NOFNMAX,bias)
      
      IMPLICIT REAL(a-h,o-z)
      INTEGER NS,NoOfHiddenLayers,NOFN,NoOfSets
      DIMENSION NOFN(NoOfHiddenLayers+2),XMIN(NOFN(1)),XMAX(NOFN(1)),
     &X(NoOfSets,NOFN(1)),W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1),
     &Y(NoOfHiddenLayers+2,NOFNMAX+1),V(NoOfHiddenLayers+2,NOFNMAX+1)
      
!.....������� �������������� ��� ������� ��� �������� (-1,1)
!.....�� ����������������� ����� ������������� ��� ��������� �(1,i),
!.....���������� ��� �� ������ ��� ������ �������� - ������� �������
      DO i=1, NOFN(1)
        Y(1,i)=EXO2(X(NS,i),XMIN(i),XMAX(i))
!        Y(1,i)=-1.0+2.0*(X(NS,i)-XMIN(i))/(XMAX(i)-XMIN(i)) ����������� ���������
      ENDDO
      
!.....����������� � ������ ��� �������� �� ����� ������� �� ���� 1
!.....���� ������� ��� �� ����� - ������� ������� ����� ��� �� ������������ - 
!.....��������� ����� �������, ���������� ��� ����� ��� �������� ������� �� 
!.....���� ������ 1, ��� ���������� ���� �������� ��� �������� ��������
      DO i=1, NoOfHiddenLayers+1
      Y(i,NOFN(i)+1)=bias  !������
      ENDDO

      
!.....������������ � ������ ���� ��� �������� ���� ��������, ��� �� ������� - 
!.....����� ����� ������� ����� �� ��������� - ������� ������. ���� ������� �� 
!.....��� ������. ����� ����������� ���� �� ������� ��� ���������� ��� �������
!.....������������������� ��� ��������. �� ������� ��� ������� �� ���� �������
!.....����� ���������� �� ������ ��� �������� ��� ������������ �������� �����-
!.....������������ �� �� ���������� �����. ��� �������� �� �������� ���� �������
!.....�� ������ ���� ��������� ��������� ������������� � ����� ��� ���������� ���
!.....����� ��� �������.    
      DO i=2, NoOfHiddenLayers+2
        DO j=1, NOFN(i)
            V(i,j)=0
            DO k=1,NOFN(i-1)+1
                V(i,j)=V(i,j)+W(i-1,j,k)*Y(i-1,k)    
            ENDDO
            IF (ifunction==1) THEN
            Y(i,j)=ACT1(V(i,j),alpha1)
            ELSEIF (ifunction==2) THEN
            Y(i,j)=ACT2(V(i,j),alpha2,beta)
            ELSE
            Y(i,j)=ACT3(V(i,j),alpha2,beta)
            ENDIF                 
        ENDDO
      ENDDO     
!.....� ForwardPass ���������� ��� ������ �(�������,��������) ��� ������ ��� ��������      
      RETURN
      END
      
    
      
     
      
      
      
      SUBROUTINE BackwardPass(NS,NoOfHiddenLayers,NOFN,W,D,Y,E,
     &OMIN,OMAX,ifunction,alpha1,alpha2,beta,Delta,NoOfSets,NOFNMAX)
      
      IMPLICIT REAL(a-h,o-z)
      INTEGER NS,NoOfHiddenLayers,NOFN,NoOfSets
      DIMENSION NOFN(NoOfHiddenLayers+2),
     &W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1),
     &D(NoOfSets,NOFN(NoOfHiddenLayers+2)),
     &Y(NoOfHiddenLayers+2,NOFNMAX+1),
     &Delta(NoOfHiddenLayers+2,NOFNMAX+1),
     &OMIN(NOFN(NoOfHiddenLayers+2)),
     &OMAX(NOFN(NoOfHiddenLayers+2)),E(NOFN(NoOfHiddenLayers+2))
      
!.....� �������� ���������� ����� ���� �������� ��� �������� ������. ��� ���� ������� ��� ������������ ����� �� ������:  
!.....E(i)=D(i)-Y(i) �
!.....[������]=[��������� ������ ���������������� ��� ���������� �������� ��� ���������� �������������]-[������������ ������ ��� �� ForwardPass]
!.....��� �������� �� ���� �� �(i)������������ � ������ ������� ������ ��� ������� (local gradient): Delta
      DO i=1,NOFN(NoOfHiddenLayers+2)
        IF (ifunction==1) THEN
        E(i)=EXO1(D(NS,i),OMIN(i),OMAX(i))-Y(NoOfHiddenLayers+2,i)
        Delta(NoOfHiddenLayers+2,i)=E(i)
     &  *ACT1DER(Y(NoOfHiddenLayers+2,i),alpha1)
!.......������������ ������ ���������� ����� �� ����� Function     
!        E(i)=(D(NS,i)-OMIN(i))/(OMAX(i)-OMIN(i))-Y(NoOfHiddenLayers+2,i)
!        Delta(NoOfHiddenLayers+2,i)=alpha*E(i)*Y(NoOfHiddenLayers+2,i)*
!     &  (1.0-Y(NoOfHiddenLayers+2,i))
        ELSEIF (ifunction==2) THEN
        E(i)=alpha2*EXO2(D(NS,i),OMIN(i),OMAX(i))
     &  -Y(NoOfHiddenLayers+2,i)
        Delta(NoOfHiddenLayers+2,i)=E(i)
     &  *ACT2DER(Y(NoOfHiddenLayers+2,i),alpha2,beta)
        ELSE
        E(i)=alpha2*EXO2(D(NS,i),OMIN(i),OMAX(i))
     &  -Y(NoOfHiddenLayers+2,i)
        Delta(NoOfHiddenLayers+2,i)=E(i)
     &  *ACT3DER(Y(NoOfHiddenLayers+2,i),alpha2,beta)
        ENDIF
      ENDDO
      
!.....� �������� ���������� ����� ���� �������� ���� ��� �������� ��� �� ������������ �������
!.....(��������� ����� �������) ��� �� ������� ������� (����� ����� �������). ���������� �����
!.....���� �������� ���� ��� ������ ��������. ��� ���� ����� ������� ��� ��� ���� ������� ���
!.....������������ � ������ ������� ������ �� ����: ������������ ������ �� �������� ���
!.....��������� ��� ������� �������� ������ ��� �������� ��� �������� �������� ��� �� ����
!.....��� �������� ���� �������� ������ �� ��� ������� �����������. ��� �������� �� ��������
!.....���� ���������������� �� ��� �������� ��� ���������� ���������� ������������� �� ������
!.....��� ����� ��� �������.
      DO i=NoOfHiddenLayers+1,2,-1
        DO j=1, NOFN(i)
            Delta(i,j)=0
            DO k=1,NOFN(i+1)
                Delta(i,j)=Delta(i,j)+Delta(i+1,k)*W(i,k,j)    
            ENDDO
            IF (ifunction==1) THEN
            Delta(i,j)=Delta(i,j)*ACT1DER(Y(i,j),alpha1)
!...........������������ ������ ���������� ����� ����� Function
!            Delta(i,j)=Delta(i,j)*alpha*Y(i,j)*(1.0-Y(i,j))            
            ELSEIF (ifunction==2) THEN
            Delta(i,j)=Delta(i,j)*ACT2DER(Y(i,j),alpha2,beta)
            ELSE
            Delta(i,j)=Delta(i,j)*ACT3DER(Y(i,j),alpha2,beta)
            ENDIF
        ENDDO
      ENDDO
!.....� BackwardPass ���������� ���� ������� Delta(�������,��������) ��� ������� �������� ������ ��� ��������
!.....��� �(��������) ��� ��������� ��� �������� ��� �������� ������         
      RETURN
      END
      
      
      
      SUBROUTINE ErrorCalc(NOFN,NoOfHiddenLayers,E,Error)
      
      IMPLICIT REAL(a-h,o-z)
      INTEGER NoOfHiddenLayers,NOFN
      DIMENSION NOFN(NoOfHiddenLayers+2),E(NOFN(NoOfHiddenLayers+2))
!.....����������� ��� ����������� ��� ���������� ��� ��������� ��� ���� ��� ������� ��� ��      
      Error=0.0
      DO i=1,NOFN(NoOfHiddenLayers+2)
        Error=Error+E(i)**2
      ENDDO
!.....�������� �� ��� ������ ��� ������ (������=������� �������� ���������� �������� ��� ��)      
      Error=Error/NOFN(NoOfHiddenLayers+2)
!.....� ErrorCalc ���������� �� �������� ������ Error ��� ��������      
      RETURN
      END