
 SUBROUTINE CATEGORIZACAO(NB,FONTE,K1RESET,K2RESET,K3RESET)
 
 IMPLICIT NONE 

 TYPE::FONTE_INIC
	INTEGER        ::BL3  ; &
	REAL(KIND=10)  ::K1IN,K1STEP,K1END,K2IN,K2STEP,K2END,K3IN,K3STEP,K3END ; &
	CHARACTER(14)  ::MOD_F
				  !EXPONENCIAL: F = K1*S, 
				  !LOGISTICO: 	F = K1*S*(1.0-S/K2), 
				  !GOLPERTZ: 	F = K1*S*ln(K2/S), 
				  !ALLEE: 	F = K1*S*(1.0-S/K2)*(S/K3-1.0)
  END TYPE 

 INTEGER,INTENT(IN)				::NB
 TYPE(FONTE_INIC),DIMENSION(NB),INTENT(IN)	::FONTE
 REAL(KIND=10),INTENT(OUT)			::K1RESET,K2RESET,K3RESET

  !DECLARA��O DAS VARI�VEIS LOCAIS
 !----------

 
 
 
 
 SELECT CASE (FONTE(1)%MOD_F)
 
    CASE('EXPONENCIAL')

	  IF ( (FONTE(1)%K1IN == FONTE(1)%K1STEP) .AND. (FONTE(1)%K1STEP == FONTE(1)%K1END) .AND. &
	       (FONTE(1)%K2IN == FONTE(1)%K2STEP) .AND. (FONTE(1)%K2STEP == FONTE(1)%K2END) .AND. &
	       (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN 
	      
	      K1RESET = FONTE(1)%K1END ; K2RESET = FONTE(1)%K2END ; K3RESET = FONTE(1)%K3END
	  
	  ELSE IF ( (FONTE(1)%K1IN > 0.0) .AND. (FONTE(1)%K1IN < FONTE(1)%K1END) .AND. (FONTE(1)%K1STEP > 0.0) .AND. &
	            (FONTE(1)%K2IN == FONTE(1)%K2STEP) .AND. (FONTE(1)%K2STEP == FONTE(1)%K2END) .AND. &
	            (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN
	  
	      K1RESET = FONTE(1)%K1IN ; K2RESET = FONTE(1)%K2END ; K3RESET = FONTE(1)%K3END  
	  
	  ELSE
	  
	      WRITE(*,*) 'ERRO  :  REVER AS ATRIBUICOES (SIM E/OU EQU) PARA K1, K2 E K3'
	      STOP
	      
	  END IF
     
    
    

    CASE('LOGISTICO')

    
	  IF ( (FONTE(1)%K1IN == FONTE(1)%K1STEP) .AND. (FONTE(1)%K1STEP == FONTE(1)%K1END) .AND. &
	       (FONTE(1)%K2IN == FONTE(1)%K2STEP) .AND. (FONTE(1)%K2STEP == FONTE(1)%K2END) .AND. &
	       (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN 
	      
	      K1RESET = FONTE(1)%K1END ; K2RESET = FONTE(1)%K2END ; K3RESET = FONTE(1)%K3END

	  ELSE IF ( (FONTE(1)%K1IN == FONTE(1)%K1STEP) .AND. (FONTE(1)%K1STEP == FONTE(1)%K1END) .AND. &
	            (FONTE(1)%K2IN > 0.0) .AND. (FONTE(1)%K2IN < FONTE(1)%K2END) .AND. (FONTE(1)%K2STEP > 0.0) .AND. & 
		    (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN
	
	      K1RESET = FONTE(1)%K1END ; K2RESET = FONTE(1)%K2IN ; K3RESET = FONTE(1)%K3END
	      
	  ELSE IF ( (FONTE(1)%K1IN > 0.0) .AND. (FONTE(1)%K1IN < FONTE(1)%K1END) .AND. (FONTE(1)%K1STEP > 0.0) .AND. &
	            (FONTE(1)%K2IN == FONTE(1)%K2STEP) .AND. (FONTE(1)%K2STEP == FONTE(1)%K2END) .AND. & 
		    (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN
		
	      K1RESET = FONTE(1)%K1IN ; K2RESET = FONTE(1)%K2END ; K3RESET = FONTE(1)%K3END
	      
	  ELSE IF ( (FONTE(1)%K1IN > 0.0) .AND. (FONTE(1)%K1IN < FONTE(1)%K1END) .AND. (FONTE(1)%K1STEP > 0.0) .AND. &
	            (FONTE(1)%K2IN > 0.0) .AND. (FONTE(1)%K2IN < FONTE(1)%K2END) .AND. (FONTE(1)%K2STEP > 0.0) .AND. & 
		    (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN
		
	      K1RESET = FONTE(1)%K1IN ; K2RESET = FONTE(1)%K2IN ; K3RESET = FONTE(1)%K3END
	      
	  ELSE
	  
	      WRITE(*,*) 'ERRO  :  REVER AS ATRIBUICOES (SIM E/OU EQU) PARA K1, K2 E K3'
	      STOP
	      
	  END IF
	
	
	
	
    CASE('GOLPERTZ')
    
	  WRITE(*,*) 'ERRO  :  O MODELO "" GOLPERTZ "" AINDA N�O FOI IMPLEMENTADO'
	  STOP


	  
	  
    CASE('ALLEE')
    
	  IF ( (FONTE(1)%K1IN == FONTE(1)%K1STEP) .AND. (FONTE(1)%K1STEP == FONTE(1)%K1END) .AND. &
	       (FONTE(1)%K2IN == FONTE(1)%K2STEP) .AND. (FONTE(1)%K2STEP == FONTE(1)%K2END) .AND. &
	       (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN 

	      K1RESET = FONTE(1)%K1END ; K2RESET = FONTE(1)%K2END ; K3RESET = FONTE(1)%K3END

	  ELSE IF ( (FONTE(1)%K1IN == FONTE(1)%K1STEP) .AND. (FONTE(1)%K1STEP == FONTE(1)%K1END) .AND. &
	            (FONTE(1)%K2IN == FONTE(1)%K2STEP) .AND. (FONTE(1)%K2STEP == FONTE(1)%K2END) .AND. &
	            (FONTE(1)%K3IN > 0.0) .AND. (FONTE(1)%K3IN < FONTE(1)%K3END) .AND. (FONTE(1)%K3STEP > 0.0) ) THEN 

	      K1RESET = FONTE(1)%K1END ; K2RESET = FONTE(1)%K2END ; K3RESET = FONTE(1)%K3IN
	     
	  ELSE IF ( (FONTE(1)%K1IN == FONTE(1)%K1STEP) .AND. (FONTE(1)%K1STEP == FONTE(1)%K1END) .AND. &
	            (FONTE(1)%K2IN > 0.0) .AND. (FONTE(1)%K2IN < FONTE(1)%K2END) .AND. (FONTE(1)%K2STEP > 0.0) .AND. &
	            (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN 

	      K1RESET = FONTE(1)%K1END ; K2RESET = FONTE(1)%K2IN ; K3RESET = FONTE(1)%K3END
	     
	  ELSE IF ( (FONTE(1)%K1IN == FONTE(1)%K1STEP) .AND. (FONTE(1)%K1STEP == FONTE(1)%K1END) .AND. &
	            (FONTE(1)%K2IN > 0.0) .AND. (FONTE(1)%K2IN < FONTE(1)%K2END) .AND. (FONTE(1)%K2STEP > 0.0) .AND. &
	            (FONTE(1)%K3IN > 0.0) .AND. (FONTE(1)%K3IN < FONTE(1)%K3END) .AND. (FONTE(1)%K3STEP > 0.0) ) THEN 

	      K1RESET = FONTE(1)%K1END ; K2RESET = FONTE(1)%K2IN ; K3RESET = FONTE(1)%K3IN
	     
	  ELSE IF ( (FONTE(1)%K1IN > 0.0) .AND. (FONTE(1)%K1IN < FONTE(1)%K1END) .AND. (FONTE(1)%K1STEP > 0.0) .AND. &
	       (FONTE(1)%K2IN == FONTE(1)%K2STEP) .AND. (FONTE(1)%K2STEP == FONTE(1)%K2END) .AND. &
	       (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN 

	      K1RESET = FONTE(1)%K1IN ; K2RESET = FONTE(1)%K2END ; K3RESET = FONTE(1)%K3END
	     
	  ELSE IF ( (FONTE(1)%K1IN > 0.0) .AND. (FONTE(1)%K1IN < FONTE(1)%K1END) .AND. (FONTE(1)%K1STEP > 0.0) .AND. &
	       (FONTE(1)%K2IN == FONTE(1)%K2STEP) .AND. (FONTE(1)%K2STEP == FONTE(1)%K2END) .AND. &
	       (FONTE(1)%K3IN > 0.0) .AND. (FONTE(1)%K3IN < FONTE(1)%K3END) .AND. (FONTE(1)%K3STEP > 0.0) ) THEN 

	      K1RESET = FONTE(1)%K1IN ; K2RESET = FONTE(1)%K2END ; K3RESET = FONTE(1)%K3IN
	     
	  ELSE IF ( (FONTE(1)%K1IN > 0.0) .AND. (FONTE(1)%K1IN < FONTE(1)%K1END) .AND. (FONTE(1)%K1STEP > 0.0) .AND. &
	            (FONTE(1)%K2IN > 0.0) .AND. (FONTE(1)%K2IN < FONTE(1)%K2END) .AND. (FONTE(1)%K2STEP > 0.0) .AND. &
	            (FONTE(1)%K3IN == FONTE(1)%K3STEP) .AND. (FONTE(1)%K3STEP == FONTE(1)%K3END) ) THEN 

	      K1RESET = FONTE(1)%K1IN ; K2RESET = FONTE(1)%K2IN ; K3RESET = FONTE(1)%K3END
	     
	  ELSE IF ( (FONTE(1)%K1IN > 0.0) .AND. (FONTE(1)%K1IN < FONTE(1)%K1END) .AND. (FONTE(1)%K1STEP > 0.0) .AND. &
	            (FONTE(1)%K2IN > 0.0) .AND. (FONTE(1)%K2IN < FONTE(1)%K2END) .AND. (FONTE(1)%K2STEP > 0.0) .AND. &
	            (FONTE(1)%K3IN > 0.0) .AND. (FONTE(1)%K3IN < FONTE(1)%K3END) .AND. (FONTE(1)%K3STEP > 0.0) ) THEN 

	      K1RESET = FONTE(1)%K1IN ; K2RESET = FONTE(1)%K2IN ; K3RESET = FONTE(1)%K3IN
	     	      
	  ELSE
	  
	      WRITE(*,*) 'ERRO  :  REVER AS ATRIBUICOES (SIM E/OU EQU) PARA K2 E K3'
	      STOP
	      
	  END IF


 END SELECT
 
 
 END SUBROUTINE CATEGORIZACAO
