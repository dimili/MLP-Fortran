!
!     Subroutines SequentialTrain and BatchTrain are implemented in this file
!*********************************************************************************


!.....Στη μέθοδο εκπαίδευσης Sequential (άλλα ονόματα: Online,Stochastic) η διόρθωση 
!.....στις τιμές των συναπτικών βαρών γίνεται μετά την παρουσίαση κάθε σετ (εισόδων-
!.....επιθυμητών εξόδων) εκπαίδευσης. Δηλαδή σε κάθε εποχή-κύκλο έχουμε τόσες διορθώσεις
!.....στα συναπτικά βάρη όσα και τα σετ. Τα πλεονεκτήματα της μεθόδου είναι:
!.....Απαιτεί λίγότερη μνήμη
!.....Η δυνατότητα παρουσίασης των εισόδων με τυχαίο τρόπο από εποχή σε εποχή κάνει
!.....την όλη διαδικασία διόρθωσης των βαρών στοχαστική-->Μειώνεται η πιθανότητα να
!.....εγκλωβιστεί ο αλγόριθμος σε τοπικό ελάχιστο
!.....Καλύτερη προσαρμογή σε πλεονάζοντα δεδομένα, σε πρότυπα δηλ που είναι ίδια μέσα
!.....σε ένα σετ εκπαίδευσης

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
      

!.....Καλείται η WeightInitialization για την αρχικοποίηση του πίνακα των συναπτικών βαρών
      CALL WeightInitialization(NoOfHiddenLayers,NOFN,W,NOFNMAX,ICONT)
         
!.....Μηδενίζουμε τον πίνακα PREV που χρησιμοποιείται για να κρατάει την μεταβολή των βαρών DW της προηγούμενης
!.....εκτέλεσης. Αφορά στην περίπτωση χρήσης ορμής κατά την εκπαίδευση       
      DO ia=1,NoOfHiddenLayers+1
        DO ja=1,NOFN(ia+1)
            DO ka=1,NOFN(ia)+1
                PREV(ia,ja,ka)=0.0
            ENDDO
        ENDDO
      ENDDO
!.....Δημιουργία των αρχείων learning-curve και learning-curve 10
!.....Το πρώτο καταγράφει την εξέλιξη του Average Error σε κάθε κύκλο - εποχή
!.....Το δεύτερο καταγράφει την εξέλιξη του Average Error ανά 10 εποχές ώστε
!.....να προκύπτουν λιγότερα δεδομένα σε περίπτωση που θέλουμε να κάνουμε τη γραφική
      OPEN (15, file='learning-curve.txt')
      OPEN (16, file='learning-curve 10.txt')
      WRITE(15,*) 'Average error during iterations'
      WRITE(15,*)
      WRITE(16,*) 'Average error every 10 iterations'
      WRITE(16,*)
      
      NI=0      
      DO i=1,iteration
        NI=NI+1 !Η NI κρατά τον αριθμό του κύκλου εκπαίδευσης
!.......Παρουσίαση των δεδομένων εκπαίδευσης με τυχαίο τρόπο από εποχή σε εποχή
!.......Ουσιαστικά γίνεται μια εσωτερική αντιμετάθεση των στοιχείων των πινάκων X και D με τυχαίο τρόπο,
!.......κάθε φορά που ξεκινάει νέος κύκλος-εποχή       
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

!.......Στην περίπτωση που έχουμε μεταβλητό ρυθμό εκπαίδευσης εκτελείται το παρακάτω if
!.......Αν irate=1 έχουμε σταθερό ρυθμό εκπαίδευσης οπότε δεν εκτελείται το if
!.......Αν irate=2 έχουμε βηματική μείωση του ρυθμού εκπαίδευσης 
!.......Στην iter αποθηκεύεται το υπόλοιπο της διαίρεσης του κύκλου στον οποίο
!.......βρίσκεται το πρόγραμμα με τον αριθμό των κύκλων μετά την ολοκλήρωση των οποίων θέλουμε
!.......να γίνεται μείωση του ρυθμού εκπαίδευσης. Αν iter=0, που σημαίνει ότι ολοκληρώθηκαν οι
!.......απαιτούμενοι κύκλοι, γίνεται η μείωση. Ελέγχεται επίσης ο ρυθμός εκπαίδευσης να μη
!.......μειωθεί πέρα από ένα ελάχιστο όριο
!.......Αν irate=3 έχουμε την περίπτωση search-then-converge
!.......Και εδώ ελέγχεται ο ρυθμός εκπαίδευσης να μη μειωθεί πέρα από ένα ελάχιστο όριο     
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
!.......Για κάθε πρότυπο του σετ καλείται πρώτα η ForwardPass για τη διάδοση του προτύπου
!.......από την είσοδο του ΝΔ στην έξοδο και στη συνέχεια η BackwardPass για τη διάδοση του
!.......σφάλματος από την έξοδο στην είσοδο και τον υπολογισμό των τοπικών βαθμίδων κλίσης
!.......(local gradient).      
        DO j=1,NoOfSets
            NS=NS+1 !Η NS κρατά τον αριθμό του σετ εκπαίδευσης 
            CALL ForwardPass(NS,NoOfHiddenLayers,NOFN,X,W,Y,XMIN,XMAX,
     &      ifunction,alpha1,alpha2,beta,NoOfSets,NOFNMAX,bias)
            CALL BackwardPass(NS,NoOfHiddenLayers,NOFN,W,D,Y,E,OMIN,
     &      OMAX,ifunction,alpha1,alpha2,beta,Delta,NoOfSets,NOFNMAX)

!.......Η ForwardPass επιστρέφει τον πίνακα των εξόδων Y των νευρώνων. Αυτός χρησιμοποιείται τόσο ως
!.......όρισμα της BackwardPass όσο και για τον υπολογισμό στις μεταβολές των συναπτικών βαρών παρακάτω.           
!.......Η BackwardPass επιστρέφει τον πίνακα των τοπικών βαθμίδων κλίσης Delta των νευρώνων.

!...........Το εξωτερικό if ελέγχει αν χρησιμοποιείται ορμή κατά την εκπαίδευση. Τα εσωτερικά if αφορούν την
!...........περίπτωση που έχουμε ελλάτωση του όρου της ορμής με το πέρασμα των εποχών. Η λειτουργία του είναι
!...........ίδια με την παραπάνω διαδικασία του ρυθμού εκπαίδευσης. Ανάλογα με το αν έχουμε ορμή ή όχι
!...........χρησιμοποιείται η κατάλληλη διαδικασία διόρθωσης των βαρών.                      
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
!...........Αφού ολοκληρωθεί η διαδικασία διόρθωσης των βαρών του ΝΔ μετά την παρουσίαση του προτύπου
!...........καλείται η ErrorCalc για τον υπολογισμό του σφάλματος εξόδου του προτύπου             
            CALL ErrorCalc(NOFN,NoOfHiddenLayers,E,Error)
!...........Γίνεται άθροιση για όλα τα πρότυπα του σετ
            AverageError=AverageError+Error 
        ENDDO
!.......Υπολογισμός του Average Squared Error        
        AverageError=SQRT(AverageError/NS)
!.......Εισαγωγή της τιμής του AverageError στα αρχεία learning-curve και learning-curve 10        
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
      
      




!.....Στη μέθοδο εκπαίδευσης Batch η διόρθωση στις τιμές των συναπτικών βαρών 
!.....γίνεται μετά την παρουσίαση ολόκληρου του σετ (εισόδων-επιθυμητών εξόδων) εκπαίδευσης. 
!.....Η μέθοδος αυτή ενδείκνυται για χρήση με πολυπύρηνους επεξεργαστές (parallelization)
      
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

!.....Καλείται η WeightInitialization για την αρχικοποίηση του πίνακα των συναπτικών βαρών
      CALL WeightInitialization(NoOfHiddenLayers,NOFN,W,NOFNMAX,ICONT)
         
!.....Μηδενίζουμε τους πίνακες PREV και DW
!.....Ο DW είναι ο πίνακας μεταβολής των βαρών
!.....Ο PREV κρατά τις μεταβολές των βαρών του προηγούμενου κύκλου. Χρησιμοποιείται στην περίπτωση χρήσης ορμής 
      DO ia=1,NoOfHiddenLayers+1
        DO ja=1,NOFN(ia+1)
            DO ka=1,NOFN(ia)+1
                DW(ia,ja,ka)=0.0
                PREV(ia,ja,ka)=0.0
            ENDDO
        ENDDO
      ENDDO
!.....Δημιουργία των αρχείων learning-curve και learning-curve 10
!.....Το πρώτο καταγράφει την εξέλιξη του Average Error σε κάθε κύκλο - εποχή
!.....Το δεύτερο καταγράφει την εξέλιξη του Average Error ανά 10 εποχές ώστε
!.....να προκύπτουν λιγότερα δεδομένα σε περίπτωση που θέλουμε να κάνουμε τη γραφική
      OPEN (15, file='learning-curve.txt')
      OPEN (16, file='learning-curve 10.txt')
      WRITE(15,*) 'Average error during iterations'
      WRITE(15,*)
      WRITE(16,*) 'Average error every 10 iterations'
      WRITE(16,*)
      
      NI=0      
      DO i=1,iteration
      NI=NI+1 !Η NI κρατά τον αριθμό του κύκλου εκπαίδευσης
!.......Περίπτωση μεταβλητού ρυθμού εκπαίδευσης,όπως στη SequentialTrain.          
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
!.......Για το σύνολο των προτύπων του σετ καλείται πρώτα η ForwardPass για τη διάδοση τους
!.......από την είσοδο του ΝΔ στην έξοδο και στη συνέχεια η BackwardPass για τη διάδοση του
!.......σφάλματος από την έξοδο στην είσοδο και τον υπολογισμό των τοπικών βαθμίδων κλίσης
!.......(local gradient).
        DO j=1,NoOfSets    
            NS=NS+1 !Η NS κρατά τον αριθμό του σετ εκπαίδευσης 
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
!...........Η BackwardPass επιστρέφει τον πίνακα σφαλμάτων Ε των νευρώνων εξόδου ο οποίος δίνεται ως όρισμα 
!...........στην ErrorCalc για τον υπολογισμό του σφάλματος εξόδου του προτύπου             
            CALL ErrorCalc(NOFN,NoOfHiddenLayers,E,Error)
!...........Γίνεται άθροιση για όλα τα πρότυπα του σετ
            AverageError=AverageError+Error 
!.......Στη μέθοδο εκπαίδευσης Batch τελειώνει ο κύκλος παρουσίασης όλων των σετ εκπαίδευσης πριν προχωρήσει στη διόρθωση των βαρών       
        ENDDO
              
!.......Υπολογισμός του Average Squared Error        
        AverageError=SQRT(AverageError/NS)
!.......Εισαγωγή της τιμής του AverageError στα αρχεία learning-curve και learning-curve 10        
        WRITE (15,30) i,AverageError
        IF ((i==1).OR.(mod(i,10)==0)) THEN
            WRITE (16,30) i,AverageError
        ENDIF
        IF (AverageError<rms) GOTO 12
        
!.......Αν το Average Squared Error δεν ικανοποιεί το όριο τότε προχωράμε σε διόρθωση των βαρών        
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