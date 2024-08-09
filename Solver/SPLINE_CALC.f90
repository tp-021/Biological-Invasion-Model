!CALCULA SPLINE CUBICA CONFORME LIVRO DO BURDEN (PEDRO GODOI 2020)
SUBROUTINE SPLINE_CALC(P,LIN,DIV,ITMAX,GS_TOL,SPL_LIN,SPL)

IMPLICIT NONE

!IN
REAL, DIMENSION(2,LIN), INTENT(IN)              ::P !Matriz dos Pontos
INTEGER, INTENT(IN)                             ::LIN,ITMAX,DIV,SPL_LIN !Numero de pontos a interpolar entre cada valor
REAL, INTENT(IN)                                ::GS_TOL

!OUT
REAL, DIMENSION(2,SPL_LIN), INTENT(OUT)         ::SPL

!LOCAL
INTEGER                             ::J,J2,J_CONT,N,IT
REAL                                ::GS_ERRO
REAL, DIMENSION(:), ALLOCATABLE     ::H,C,C_ANT,B,D


!DIV=3

!CARREGA VALORES PARA P
!P(1,1)=0; P(1,2)=1; P(1,3)=2; P(1,4)=3
!P(2,1)=1; P(2,2)=2.7183; P(2,3)=7.3891; P(2,4)=20.086

!VERIFICA NUMERO DE ELEMENTOS NA MATRIZ P
N=SIZE(P,DIM=2)

!DEBUG 1
!PRINT*, P(1,:)
!PRINT*, P(2,:)
!PRINT*, N

!MONTA O ARRAY AUXILIAR H
ALLOCATE(H(N-1))
DO J=1,N-1
    H(J)=P(1,J+1)-P(1,J)
END DO

!DEBUG 2
!PRINT*, H

!CALCULA C VIA GAUSS-SEIDEL
ALLOCATE(C(N),C_ANT(N))
C=0.0; C_ANT=0.0

IT=1
!ITMAX=200
!GS_TOL=0.0001

GAUSS_SEIDEL: DO WHILE (IT < ITMAX)

DO J=1,N
    
    IF ( (J>=2) .AND. (J<=N-1)) THEN
        C(J)=(1/(2*(H(J)+H(J-1)))) * ( (3/H(J))*(P(2,J+1)-P(2,J)) - (3/H(J-1))*(P(2,J)-P(2,J-1)) - H(J-1)*C(J-1) - H(J)*C_ANT(J+1) )
    
    ELSE IF (J==1) THEN
        C(1)=0.0
    
    ELSE
        C(N)=0.0
    END IF
END DO

GS_ERRO=NORM2(C-C_ANT)/NORM2(C)
    IF (GS_ERRO < GS_TOL) THEN
        GOTO 100
    END IF

C_ANT=C
IT=IT+1
END DO GAUSS_SEIDEL

100 CONTINUE

!DEBUG 3
!PRINT*, C
!PRINT*, GS_ERRO, IT

!CALCULA B
ALLOCATE(B(N-1))
DO J=1,N-1
    B(J)=(1/H(J))*(P(2,J+1)-P(2,J)) - (H(J)/3)*(2*C(J)+C(J+1))
END DO

!DEBUG 4
!PRINT*, B

!CALCULA D
ALLOCATE(D(N-1))
DO J=1,N-1
    D(J)=(1/(3*H(J)))*(C(J+1)-C(J))
END DO

!DEBUG 5
!PRINT*, D

!CALCULA A SPLINE
SPL(1,1)=P(1,1)
SPL(2,1)=P(2,1)

J2=1
J_CONT=1
DO J=2,SPL_LIN

    SPL(1,J)=((P(1,J2+1)-P(1,J2))/(DIV+1))*J_CONT + P(1,J2) !CALCULA VALOR DE X
    
    SPL(2,J)=P(2,J2) + B(J2)*(SPL(1,J)-P(1,J2)) + C(J2)*((SPL(1,J)-P(1,J2))**2) + D(J2)*((SPL(1,J)-P(1,J2))**3) !SPLINE
    
    IF (J_CONT==(DIV+1)) THEN !AJUSTA QUAL POLINOMIO ESTAMOS USANDO
        J_CONT=1
        J2=J2+1
    ELSE
        J_CONT=J_CONT+1
    END IF
END DO

END SUBROUTINE SPLINE_CALC
