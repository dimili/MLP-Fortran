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
      
      USE DFLIB   ! Για τις seed και random
      REAL W
      LOGICAL exists
      INTEGER NoOfHiddenLayers,NOFN,ks,ICONT
      DIMENSION NOFN(NoOfHiddenLayers+2),
     &W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1)
     
!.....Αρχικοποίηση του πίνακα των συναπτικών βαρών
!.....Αν ICONT=0 η αρχικοποίηση γίνεται με τυχαίους αριθμούς
!.....Αν ICONT=1 οι τιμές των συναπτικών βαρών εισάγονται από το τελευταίο αρχείο εξόδου. Περίπτωση συνέχισης εκπαίδευσης

      IF (ICONT==0) THEN
!.....Δημιουργία seed για την γεννήτρια ψευδοτυχαίων αριθμών
      CALL SEED(RND$TIMESEED)

      
!.....Η random παράγει ψευδοτυχαίους αριθμούς με ομοιόμορφη κατανομή στο διάστημα (0,1)
!.....Με την παρακάτω κανονικοποίηση μετακινούμε τη κατανομή στο διάστημα (-1,1) ώστε το 
!.....νευρωνικό δίκτυο να εκπαιδεύεται ευκολότερα      
!.....Η λογική δημιουργίας του πίνακα είναι ότι κάθε νευρώνας ενός επιπέδου (εισόδου ή κρυφού)
!.....συνδέεται με βάρη με κάθε νευρώνα του επόμενου επιπέδου συν ένα επιπλέον βάρος για τη σύνδεση 
!.....με το νευρώνα της πόλωσης
      DO i=1,NoOfHiddenLayers+1
        DO j=1,NOFN(i+1)
            DO k=1,NOFN(i)+1
!...............W(επίπεδο που ανήκει,νευρώνας που καταλήγει,νευρώνας που προέρχεται)                        
                W(i,j,k)=2*(RANDOM(0)-0.5)
            ENDDO
        ENDDO
      ENDDO
      
      ELSEIF (ICONT==1) THEN

!.....Ελέγχεται η ύπαρξη του αρχείου από το οποίο θα διαβαστούν οι τιμές των συναπτικών βαρών          
      INQUIRE (FILE = 'output.txt', EXIST = exists)
!.....Στην περίπτωση που δεν υπάρχει το αρχείο γίνεται αρχικοποίηση με τυχαίες τιμές      
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
         
!.....Η WeightInitialization επιστρέφει τον πίνακα των βαρών W(Επίπεδο,Νευρώνας-καταλήγει,Νευρώνας-προέρχεται)      
      RETURN
      END
      
      
      
      
      
      
      
      SUBROUTINE ForwardPass(NS,NoOfHiddenLayers,NOFN,X,W,Y,XMIN,XMAX,
     &ifunction,alpha1,alpha2,beta,NoOfSets,NOFNMAX,bias)
      
      IMPLICIT REAL(a-h,o-z)
      INTEGER NS,NoOfHiddenLayers,NOFN,NoOfSets
      DIMENSION NOFN(NoOfHiddenLayers+2),XMIN(NOFN(1)),XMAX(NOFN(1)),
     &X(NoOfSets,NOFN(1)),W(NoOfHiddenLayers+1,NOFNMAX,NOFNMAX+1),
     &Y(NoOfHiddenLayers+2,NOFNMAX+1),V(NoOfHiddenLayers+2,NOFNMAX+1)
      
!.....Γίνεται κανονικοποίηση των εισόδων στο διάστημα (-1,1)
!.....Οι κανονικοποιημένες τιμές αποθηκεύονται στη μεταβλητή Υ(1,i),
!.....θεωρούνται δηλ ως έξοδος του πρώτου επιπέδου - επίπεδο εισόδου
      DO i=1, NOFN(1)
        Y(1,i)=EXO2(X(NS,i),XMIN(i),XMAX(i))
!        Y(1,i)=-1.0+2.0*(X(NS,i)-XMIN(i))/(XMAX(i)-XMIN(i)) Διαφορετική υλοποίηση
      ENDDO
      
!.....Προστίθεται η πόλωση των νευρώνων ως έξτρα είσοδος με τιμή 1
!.....Κάθε επίπεδο από το πρώτο - επίπεδο εισόδου μέχρι και το προτελευταίο - 
!.....τελευταίο κρυφό επίπεδο, θεωρούνται ότι έχουν ένα επιπλέον νευρώνα με 
!.....τιμή εξόδου 1, που τροφοδοτεί τους νευρώνες του επόμενου επιπέδου
      DO i=1, NoOfHiddenLayers+1
      Y(i,NOFN(i)+1)=bias  !Πόλωση
      ENDDO

      
!.....Υπολογίζεται η έξοδος όλων των νευρώνων κάθε επιπέδου, από το δεύτερο - 
!.....πρώτο κρυφό επίπεδο μέχρι το τελευταίο - επίπεδο εξόδου. Αυτό γίνεται σε 
!.....δύο στάδια. Πρώτα αθροίζονται όλες οι είσοδοι που καταλήγουν στο νευρώνα
!.....συμπεριλαμβανομένων των πολώσεων. Οι είσοδοι που φθάνουν σε κάθε νευρώνα
!.....είναι ουσιαστικά οι έξοδοι των νευρώνων του προηγούμενου επιπέδου πολλα-
!.....πλασιασμένες με το αντίστοιχο βάρος. Στη συνέχεια το άθροισμα αυτό δίνεται
!.....ως όρισμα στην κατάλληλη συνάρτηση ενεργοποίησης η οποία και επιστρέφει την
!.....έξοδο του νευρώνα.    
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
!.....Η ForwardPass επιστρέφει τον πίνακα Υ(Επίπεδο,Νευρώνας) των εξόδων των νευρώνων      
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
      
!.....Η παρακάτω ανακύκλωση αφορά τους νευρώνες του επιπέδου εξόδου. Για κάθε νευρώνα του υπολογίζεται πρώτα το σφάλμα:  
!.....E(i)=D(i)-Y(i) ή
!.....[Σφάλμα]=[Επιθυμητή έξοδος κανονικοποιημένη στο αντίστοιχο διάστημα της συνάρτησης ενεργοποίησης]-[Υπολογισμένη έξοδος από το ForwardPass]
!.....Στη συνέχεια με βάση το Ε(i)υπολογίζεται η τοπική βαθμίδα κλίσης του νευρώνα (local gradient): Delta
      DO i=1,NOFN(NoOfHiddenLayers+2)
        IF (ifunction==1) THEN
        E(i)=EXO1(D(NS,i),OMIN(i),OMAX(i))-Y(NoOfHiddenLayers+2,i)
        Delta(NoOfHiddenLayers+2,i)=E(i)
     &  *ACT1DER(Y(NoOfHiddenLayers+2,i),alpha1)
!.......Διαφορετικός τρόπος υλοποίησης χωρίς τη χρήση Function     
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
      
!.....Η παρακάτω ανακύκλωση αφορά τους νευρώνες όλων των επιπέδων από το προτελευταίο επίπεδο
!.....(τελευταίο κρυφό επίπεδο) έως το δεύτερο επίπεδο (πρώτο κρυφό επίπεδο). Ουσιαστικά αφορά
!.....τους νευρώνες όλων των κρυφών επιπέδων. Για κάθε κρυφό επίπεδο και για κάθε νευρώνα του
!.....υπολογίζεται η τοπική βαθμίδα κλίσης ως εξής: υπολογίζεται αρχικά το άθροισμα των
!.....γινομένων των τοπικών βαθμίδων κλίσης των νευρώνων του επόμενου επιπέδου επί τα βάρη
!.....που συνδέουν τους νευρώνες αυτούς με τον νευρώνα υπολογισμού. Στη συνέχεια το άθροισμα
!.....αυτό πολλαπλασιάζεται με την παράγωγο της κατάλληλης συνάρτησης ενεργοποίησης με όρισμα
!.....την έξοδο του νευρώνα.
      DO i=NoOfHiddenLayers+1,2,-1
        DO j=1, NOFN(i)
            Delta(i,j)=0
            DO k=1,NOFN(i+1)
                Delta(i,j)=Delta(i,j)+Delta(i+1,k)*W(i,k,j)    
            ENDDO
            IF (ifunction==1) THEN
            Delta(i,j)=Delta(i,j)*ACT1DER(Y(i,j),alpha1)
!...........Διαφορετικός τρόπος υλοποίησης χωρίς χρήση Function
!            Delta(i,j)=Delta(i,j)*alpha*Y(i,j)*(1.0-Y(i,j))            
            ELSEIF (ifunction==2) THEN
            Delta(i,j)=Delta(i,j)*ACT2DER(Y(i,j),alpha2,beta)
            ELSE
            Delta(i,j)=Delta(i,j)*ACT3DER(Y(i,j),alpha2,beta)
            ENDIF
        ENDDO
      ENDDO
!.....Η BackwardPass επιστρέφει τους πίνακες Delta(Επίπεδο,Νευρώνας) των τοπικών βαθμίδων κλίσης των νευρώνων
!.....και Ε(Νευρώνας) των σφαλμάτων των νευρώνων του επιπέδου εξόδου         
      RETURN
      END
      
      
      
      SUBROUTINE ErrorCalc(NOFN,NoOfHiddenLayers,E,Error)
      
      IMPLICIT REAL(a-h,o-z)
      INTEGER NoOfHiddenLayers,NOFN
      DIMENSION NOFN(NoOfHiddenLayers+2),E(NOFN(NoOfHiddenLayers+2))
!.....Υπολογισμός του αθροίσματος των τετραγώνων των σφαλμάτων για όλες τις εξόδους του ΝΔ      
      Error=0.0
      DO i=1,NOFN(NoOfHiddenLayers+2)
        Error=Error+E(i)**2
      ENDDO
!.....Διαίρεση με τον αριθμό των εξόδων (έξοδοι=αριμθός νευρώνων τελευταίου επιπέδου του ΝΔ)      
      Error=Error/NOFN(NoOfHiddenLayers+2)
!.....Η ErrorCalc επιστρέφει το συνολικό σφάλμα Error του προτύπου      
      RETURN
      END