!
!     This file contains the activation functions of the neurons.
!     These functions are called by "ForwardPass" while their derivatives by "BackwardPass". 
!     It also contains the normalization functions and their inverse functions
!*******************************************************************************************


!.....SigmoidLogistic Function
!.....Output belongs at (0,1)
      FUNCTION ACT1(V,alpha)
      ACT1=1.0/(1.0+exp(-alpha*V))
      RETURN
      END
!.....SigmoidLogistic Function derivative     
      FUNCTION ACT1DER(V,alpha)
      ACT1DER=alpha*V*(1.0-V)
      RETURN
      END
      
      
!.....HyperbolicTangent Function
!.....Output belongs at (-alpha,alpha)            
      FUNCTION ACT2(V,alpha,beta)
      ACT2=alpha*tanh(beta*V)
      RETURN
      END
!.....HyperbolicTangent Function derivative     
      FUNCTION ACT2DER(V,alpha,beta)
      ACT2DER=(beta/alpha)*(alpha-V)*(alpha+V)
      RETURN
      END
      
      
!.....Linear Function
!.....Output belongs at (-alpha,alpha)       
      FUNCTION ACT3(V,alpha,beta)
      IF (V>=1.0/beta) THEN
      ACT3=alpha
      ELSEIF (V<=-1.0/beta) THEN
      ACT3=-alpha
      ELSE
      ACT3=beta*V
      ENDIF
      RETURN
      END
!.....Linear Function derivative    
      FUNCTION ACT3DER(V,alpha,beta)
      IF ((V>=1.0/beta).OR.(V<=-1.0/beta)) THEN
      ACT3DER=0.0
      ELSE
      ACT3DER=beta
      ENDIF
      RETURN
      END
      
      
!.....Normalization function (0,1)      
      FUNCTION EXO1(D,OMIN,OMAX)
      EXO1=(D-OMIN)/(OMAX-OMIN)
      RETURN
      END
      
!.....Normalization function (-1,1)     
      FUNCTION EXO2(D,OMIN,OMAX)
      EXO2=2.0*(D-OMIN)/(OMAX-OMIN)-1.0
      RETURN
      END
      
      
!.....ево1 inverse function     
      FUNCTION EXO1A(D,OMIN,OMAX)
      EXO1A=OMIN+D*(OMAX-OMIN)
      RETURN
      END
      
!.....ево2 inverse function    
      FUNCTION EXO2A(D,OMIN,OMAX)
      EXO2A=OMIN+(D+1.0)*(OMAX-OMIN)/2.0
      RETURN
      END
      