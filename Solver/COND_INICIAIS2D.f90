
 SUBROUTINE COND_INICIAIS2D(COMPDOMX,COMPDOMY,NI,MJ,NB,COMP_DOMI,COMP_DOMJ,PARCOND_I,X,Y,XS,YS,S2D)
 
 IMPLICIT NONE 

  TYPE::DOMINIO
	REAL(KIND=10)	::DOM1 ; REAL(KIND=10) ::DOM2 ; &
	CHARACTER(3) 	::DOM3
 END TYPE

 TYPE::CONDICOES_INIC
	INTEGER 	::BL1 ; &
	REAL(KIND=10)	::S_I_A ; REAL(KIND=10) ::S_I_B ; REAL(KIND=10) ::S_I_C ; &
    REAL(KIND=10)   ::S_I_D ; REAL(KIND=10) ::S_I_E ; &
	CHARACTER(13) 	::S_I_FUN
			!ATEQUADRATICO: 	S = A + B*X + C*X**2, 
			!DEGRAU: 		se X<A entao S=0; se X>=A and X<=C entao S = B; se X>C entao S=0
			!EXPONENCIAL: 		S = A*EXP(B*X)+ C, 
			!NORMAL: 		S = (A/(B*SQRT(2*PI)))*EXP(-0.5*((X-C)/B)**2.0)
 END TYPE
 
 
 TYPE(DOMINIO),INTENT(IN)			                   ::COMPDOMX,COMPDOMY
 INTEGER,INTENT(IN)				                       ::NI,MJ,NB
 REAL(KIND=10),INTENT(IN)		                       ::COMP_DOMI,COMP_DOMJ
 TYPE(CONDICOES_INIC),DIMENSION(NB),INTENT(IN)	       ::PARCOND_I
 REAL(KIND=10),DIMENSION(NI+2),INTENT(IN)		       ::X
 REAL(KIND=10),DIMENSION(MJ+2),INTENT(IN)		       ::Y
 REAL(KIND=10),DIMENSION(NI+1),INTENT(IN)		       ::XS
 REAL(KIND=10),DIMENSION(MJ+1),INTENT(IN)		       ::YS
 REAL(KIND=10),DIMENSION(MJ+1,NI+1),INTENT(OUT)	       ::S2D


 !DECLARAÇÃO DAS VARIÁVEIS LOCAIS
 INTEGER	         ::IS,JS
 !----------

 ! DEFINIÇÃO DA DENSIDADE POPULACIONAL INICIAL

 
 SELECT CASE (PARCOND_I(1)%S_I_FUN)
 
    CASE('SPLINE')
    
    CALL COND_INICIAIS2D_SPLINE(COMPDOMX,COMPDOMY,NI,MJ,NB,COMP_DOMI,COMP_DOMJ,PARCOND_I,X,Y,XS,YS,S2D)
    
    
    
    CASE('DEGRAU')
    
    DO IS=1,NI+1
        DO JS=1,MJ+1
            IF ((XS(IS)>=PARCOND_I(1)%S_I_A) .AND. (XS(IS)<=PARCOND_I(1)%S_I_B) .AND.  &
                (YS(JS)>=PARCOND_I(1)%S_I_C) .AND. (YS(JS)<=PARCOND_I(1)%S_I_D)) THEN
                
                S2D(JS,IS)=PARCOND_I(1)%S_I_E
                
            END IF
        END DO
    END DO

    
    
    CASE('GAUSS')
    DO IS=1,NI+1
        DO JS=1,MJ+1
            
            S2D(JS,IS)= PARCOND_I(1)%S_I_E * EXP(-( (((XS(IS) - PARCOND_I(1)%S_I_A)**2)/(2* PARCOND_I(1)%S_I_B**2))&
                                                   +(((YS(JS) - PARCOND_I(1)%S_I_C)**2)/(2* PARCOND_I(1)%S_I_D**2)) ))
                                                   
        END DO
    END DO
    
    
    
    CASE('CHEIO')
    DO IS=2,NI
        DO JS=2,MJ
        
            S2D(JS,IS) = PARCOND_I(1)%S_I_E
            
        END DO
    END DO


 END SELECT
 
 
 END SUBROUTINE COND_INICIAIS2D
