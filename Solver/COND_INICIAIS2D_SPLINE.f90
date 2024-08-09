
 SUBROUTINE COND_INICIAIS2D_SPLINE(COMPDOMX,COMPDOMY,NI,MJ,NB,COMP_DOMI,COMP_DOMJ,PARCOND_I,X,Y,XS,YS,S2D)
 
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
 INTEGER	::CONTROLE,LIN,ITMAX,DIV,SPL_LIN,LADO,I,J,N,MANIPULA
 REAL       ::GS_TOL,TOL_X,MIN_X,MAX_X,X_CELL,Y_CELL,DIST_ATUAL
 REAL, DIMENSION(:,:), ALLOCATABLE   ::SPL
 REAL(KIND=10),DIMENSION(MJ+1,NI+1)	 ::DIST
 
 

!LE ARQUIVO DE PARAMETROS
OPEN(776,FILE='SPLINE/PARAMETROS_SPLINE.dat',STATUS='OLD')

READ(776,*) !-------------------------------------
READ(776,*) !-PARAMETROS PARA O CALCULO DA SPLINE-
READ(776,*) !-------------------------------------
READ(776,*) GS_TOL
READ(776,*) ITMAX
READ(776,*) DIV
READ(776,*) TOL_X

CLOSE(776)


LADO=1 !1=ESQUERDA, 2=DIREITA
DIST=100.0

DO LADO=1,2

IF (LADO==1) THEN
!VERIFICA NUMERO DE PONTOS NO ARQUIVO DE ENTRADA
OPEN(777,FILE='SPLINE/entrada_spline_E.dat',STATUS='OLD')
ELSE
OPEN(777,FILE='SPLINE/entrada_spline_D.dat',STATUS='OLD')
END IF
LIN=0

DO
READ(777,*,IOSTAT = CONTROLE)
    IF(CONTROLE < 0) EXIT
    LIN = LIN + 1
END DO
CLOSE(777)

!ALOCA E CALCULA A SPLINE
SPL_LIN=(DIV+1)*(LIN-1)+1 !NUMERO DE ELEMENTOS NA SPLINE
ALLOCATE(SPL(2,SPL_LIN))


CALL SPLINEPAR_PRINCIPAL(LADO,ITMAX,DIV,GS_TOL,SPL_LIN,SPL)


!VERIFICA DISTANCIA DAS CELULAS AOS PONTOS DA SPLINE
MIN_X=MINVAL(SPL(1,:))-TOL_X !INTERVALO PROXIMO A SPLINE
MAX_X=MAXVAL(SPL(1,:))+TOL_X


DO I=1,MJ+1
    Y_CELL=YS(I) !Calcula o valor do Y no centro da celula
    DO J=1,NI+1
        X_CELL=XS(J) !Calcula o valor do X no centro da celula
        IF ((X_CELL>=MIN_X) .AND. (X_CELL<=MAX_X)) THEN                         !Se X estiver no intervalo tolerado
            DO N=1,SPL_LIN                                                      !Calcula a distancia aos pontos da spline
                DIST_ATUAL=SQRT( ( X_CELL-SPL(1,N))**2.0 + (Y_CELL-SPL(2,N))**2.0 )
                IF (DIST_ATUAL<=DIST(I,J)) THEN                                 !Salva o resultado apenas se a distancia for a menor
                DIST(I,J)=DIST_ATUAL
                END IF
            END DO
        END IF
    END DO
END DO
        


        
DEALLOCATE(SPL)

END DO

!ESCREVER DISTANCIAS EM ARQUIVO
MANIPULA = SYSTEM('if test -e dist.dat; then rm dist.dat; fi;')

OPEN(773,FILE='dist.dat',STATUS='NEW')
DO I=1,NI+1
    DO J=1,MJ+2
        IF (J<=MJ+1) THEN
            WRITE(773,100) (X(I)+X(I+1))/2 , (Y(J)+Y(J+1))/2, DIST(J,I)
        ELSE
            WRITE(773,'(2X)')
        END IF
    END DO
END DO

CLOSE(773)

100 FORMAT(F12.4,4X,F12.4,4X,F12.4)


!CRIAR CONDICAO INICIAL

S2D=0.0


!VARRER PELA ESQUERDA
DO J=2,MJ
    DO I=1,NI
        IF (DIST(J,I)<DIST(J,I+1)) THEN
        S2D(J,2:I+1)=1.0
        GOTO 200
        END IF
    END DO
200 CONTINUE
END DO

!VARRER PELA DIREITA
DO J=2,MJ
    DO I=NI+1,2,-1
        IF (DIST(J,I)<DIST(J,I-1)) THEN
        S2D(J,I+1:NI)=1.0
        GOTO 210
        END IF
    END DO
210 CONTINUE
END DO


! !ESCREVER S2D EM ARQUIVO
! MANIPULA = SYSTEM('if test -e s2d.dat; then rm s2d.dat; fi;')
! 
! OPEN(774,FILE='s2d.dat',STATUS='NEW')
! DO I=1,NI+1
!     DO J=1,MJ+2
!         IF (J<=MJ+1) THEN
!             WRITE(774,110) XS(I) , YS(J) , S2D(J,I)
!         ELSE
!             WRITE(774,'(2X)')
!         END IF
!     END DO
! END DO
! 
! CLOSE(774)
! 
! 110 FORMAT(F12.4,4X,F12.4,4X,F12.4)



 
 END SUBROUTINE COND_INICIAIS2D_SPLINE
