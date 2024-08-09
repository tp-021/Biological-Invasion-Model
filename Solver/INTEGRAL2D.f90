!CALCULA A INTEGRAL DUPLA (PEDRO GODOI 2020)
 SUBROUTINE INTEGRAL2D(NI,MJ,DELTA_X,DELTA_Y,S2D,POP)
 
 IMPLICIT NONE 


 INTEGER,INTENT(IN)				                    ::NI,MJ
 REAL(KIND=10),INTENT(IN)			                ::DELTA_X,DELTA_Y
 REAL(KIND=10),DIMENSION(MJ+1,NI+1),INTENT(IN)	    ::S2D
 REAL(KIND=10),INTENT(OUT)                          ::POP


 !DECLARAÇÃO DAS VARIÁVEIS LOCAIS
 INTEGER	        ::JS,IS
 
 !INICIA VALORES
 POP=0.0
 
 
 !!! CALCULA INTEGRAL !!!
 
 IF ((MOD(NI,2) == 0) .AND. (MOD(MJ,2) == 0)) THEN
 
 !METODO DE 1/3 DE SIMPSON
 
 DO JS=1,MJ-1,2
    DO IS=1,NI-1,2
    
        POP= POP + ((DELTA_X*DELTA_Y)/9.0)*(S2D(JS,IS)+4.0*S2D(JS+1,IS)+S2D(JS+2,IS))&
                     + (4.0*(DELTA_X*DELTA_Y)/9.0)*(S2D(JS,IS+1)+4.0*S2D(JS+1,IS+1)+S2D(JS+2,IS+1))&
                     + ((DELTA_X*DELTA_Y)/9.0)*(S2D(JS,IS+2)+4.0*S2D(JS+1,IS+2)+S2D(JS+2,IS+2))
    
    END DO
 END DO
 
 ELSE
 
 !METODO DOS TRAPEZIOS
 
 DO JS=1,MJ
    DO IS=1,NI
        
        POP= POP + ((DELTA_X*DELTA_Y)/4)*(S2D(JS,IS)+S2D(JS+1,IS)+S2D(JS,IS+1)+S2D(JS+1,IS+1))
        
    END DO
 END DO
 
 END IF
 
 
 
 
!  DEBUG
! PRINT*, DELTA_X
! PRINT*, DELTA_Y
! PRINT*, POP
  
 END SUBROUTINE INTEGRAL2D
