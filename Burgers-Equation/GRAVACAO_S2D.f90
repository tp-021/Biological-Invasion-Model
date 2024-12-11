

 SUBROUTINE GRAVACAO_S2D(NI,MJ,XS,YS,S2D,NOME,T)

 IMPLICIT NONE


CHARACTER(13),INTENT(IN)                               ::NOME
INTEGER,INTENT(IN)                                     ::NI,MJ
REAL(KIND=10),INTENT(IN)                               ::T
REAL(KIND=10),DIMENSION(NI+1),INTENT(IN)		       ::XS
REAL(KIND=10),DIMENSION(MJ+1),INTENT(IN)		       ::YS
REAL(KIND=10),DIMENSION(MJ+1,NI+1),INTENT(IN)	       ::S2D

!----------
!DECLARA��O DAS VARI�VEIS LOCAIS
INTEGER :: MANIPULA,I,J
!----------
  


!ESCREVER S2D EM ARQUIVO
MANIPULA = SYSTEM('if test -e '//NOME//'; then rm '//NOME//'; fi;')

OPEN(774,FILE=NOME,STATUS='NEW')
WRITE(774,*) '#T=',T

DO I=1,NI+1
    DO J=1,MJ+2
        IF (J<=MJ+1) THEN
            WRITE(774,110) XS(I) , YS(J) , S2D(I,J)
        ELSE
            WRITE(774,'(2X)')
        END IF
    END DO
END DO

CLOSE(774)

110 FORMAT(F12.8,4X,F12.8,4X,F12.8)
 
 END SUBROUTINE GRAVACAO_S2D
 
 
