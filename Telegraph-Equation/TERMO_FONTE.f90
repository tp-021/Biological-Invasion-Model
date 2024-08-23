
 SUBROUTINE TERMO_FONTE(NB,FONTE,SATUAL,K1VAR,K2VAR,K3VAR,F_S,D_F_S)
 
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
 REAL(KIND=10),INTENT(IN)			::SATUAL,K1VAR,K2VAR,K3VAR
 REAL(KIND=10),INTENT(OUT)			::F_S,D_F_S

  !DECLARAÇÃO DAS VARIÁVEIS LOCAIS
 !----------

 ! DEFINIÇÃO DA POPULAÇÃO INICIAL
 
 
 SELECT CASE (FONTE(1)%MOD_F)
 
    CASE('EXPONENCIAL')
    
! 	F_S = FONTE(1)%K1 * SATUAL
	F_S = K1VAR * SATUAL
	
! 	D_F_S = FONTE(1)%K1 
	D_F_S = K1VAR

    CASE('LOGISTICO')

!     	IF (FONTE(1)%K1 < 0.0) THEN
	IF (K1VAR < 0.0) THEN
	  WRITE(*,*) 'K1 > 0 :: ALTERE O VALOR'
	  STOP  
	END IF

! 	IF (FONTE(1)%K2 <= 0.0) THEN
	IF (K2VAR <= 0.0) THEN
	  WRITE(*,*) 'K2 > 0 :: ALTERE O VALOR'
	  STOP  
	END IF
	
! 	F_S = FONTE(1)%K1 * SATUAL * ( 1.0 - SATUAL / FONTE(1)%K2 )
! 	F_S = FONTE(1)%K1 * SATUAL * ( 1.0 - SATUAL / K2VAR )
	F_S = K1VAR * SATUAL * ( 1.0 - SATUAL / K2VAR )
	
! 	D_F_S = FONTE(1)%K1 - 2.0 * (FONTE(1)%K1/FONTE(1)%K2) * SATUAL
! 	D_F_S = FONTE(1)%K1 - 2.0 * (FONTE(1)%K1/K2VAR) * SATUAL
	D_F_S = K1VAR - 2.0 * (K1VAR/K2VAR) * SATUAL
	
    CASE('GOLPERTZ')
    
! 	IF (FONTE(1)%K1 < 0.0) THEN
	IF (K1VAR < 0.0) THEN
	  WRITE(*,*) 'K1 > 0 :: ALTERE O VALOR'
	  STOP  
	END IF

! 	IF (FONTE(1)%K2 <= 0.0) THEN
	IF (K2VAR <= 0.0) THEN
	  WRITE(*,*) 'K2 > 0 :: ALTERE O VALOR'
	  STOP  
	END IF

	IF (SATUAL == 0.0) THEN
	  WRITE(*,*) 'PROBLEMA: DIVISÃO POR ZERO!!!!'
	  STOP  
	END IF
	IF (SATUAL == 0.0) THEN
	  WRITE(*,*) 'O VALOR DE K2 NÃO PODE SER NULO, ALTERE O VALOR'
	  STOP  
	END IF
    
! 	F_S = FONTE(1)%K1 * SATUAL * LOG( FONTE(1)%K2 / SATUAL )
!         F_S = FONTE(1)%K1 * SATUAL * LOG( K2VAR / SATUAL )
	F_S = K1VAR * SATUAL * LOG( K2VAR / SATUAL )
	
! 	D_F_S = FONTE(1)%K1 * ( -1.0 + LOG( FONTE(1)%K2 / SATUAL ) ) 
!         D_F_S = FONTE(1)%K1 * ( -1.0 + LOG( K2VAR / SATUAL ) )
	D_F_S = K1VAR * ( -1.0 + LOG( K2VAR / SATUAL ) )

    CASE('ALLEE')
    
! 	IF (FONTE(1)%K1 < 0.0) THEN
	IF (K1VAR < 0.0) THEN
	  WRITE(*,*) 'K1 > 0 :: ALTERE O VALOR'
	  STOP  
	END IF

! 	IF (FONTE(1)%K2 <= 0.0) THEN
        IF (K2VAR <= 0.0) THEN
	  WRITE(*,*) 'K2 > 0 :: ALTERE O VALOR'
	  STOP  
	END IF

! 	IF (FONTE(1)%K3 <= 0.0) THEN
        IF (K3VAR <= 0.0) THEN
	  WRITE(*,*) 'K3 > 0 :: ALTERE O VALOR'
	  STOP  
	END IF                                                  
	
! 	F_S = FONTE(1)%K1 * SATUAL * ( 1.0 - SATUAL / FONTE(1)%K2 ) * ( SATUAL / FONTE(1)%K3 - 1.0 )
! 	F_S = FONTE(1)%K1 * SATUAL * ( 1.0 - SATUAL / K2VAR ) * ( SATUAL / K3VAR - 1.0 )
	F_S = K1VAR * SATUAL * ( 1.0 - SATUAL / K2VAR ) * ( SATUAL / K3VAR - 1.0 )

! 	D_F_S = - FONTE(1)%K1 &
! 	
! 	          + 2.0 * ( FONTE(1)%K1/FONTE(1)%K2 + FONTE(1)%K1/FONTE(1)%K3 ) * SATUAL &
! 	                        
! 	          - 3.0 * ( FONTE(1)%K1 / (FONTE(1)%K2 * FONTE(1)%K3) ) * (SATUAL**2.0) 
! 	D_F_S = - FONTE(1)%K1 &
! 	        
! 	          + 2.0 * ( FONTE(1)%K1/K2VAR + FONTE(1)%K1/K3VAR ) * SATUAL &
! 	                        
! 	          - 3.0 * ( FONTE(1)%K1 / (K2VAR * K3VAR) ) * (SATUAL**2.0) 
	D_F_S = - K1VAR &
	        
	          + 2.0 * ( K1VAR/K2VAR + K1VAR/K3VAR ) * SATUAL &
	                        
	          - 3.0 * ( K1VAR / (K2VAR * K3VAR) ) * (SATUAL**2.0) 

 END SELECT
 
 
 END SUBROUTINE TERMO_FONTE
