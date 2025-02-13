! RESOLVE A EQUA�AO TELEGRAFICA-REATIVA-DIFUSIVA-2D PELO ESQUEMA TOTALMENTE IMPLICITO 
 SUBROUTINE EQUACAO_S2D(NB,NI,MJ,DELTA_X,DELTA_Y,XS,YS,DELTA_T,FONTE,K1VAR,K2VAR,K3VAR,IT_SOL,W_SOL,TOL_SOL,&
            S2D_PAS,S2D_PAS_PAS,S2D,DADOS_LAMBDA,OCUP,TOL_OCUP,POP,POP_PAS,DADOS_GAMMA_X,DADOS_GAMMA_Y,&
            S2D_LAMBDA, S2D_VEL_X, S2D_VEL_Y,CONVERGE)
 
 IMPLICIT NONE 
 
 TYPE::FONTE_INIC
	INTEGER        ::BL3  ; &
	REAL(KIND=10)  ::K1IN,K1STEP,K1END,K2IN,K2STEP,K2END,K3IN,K3STEP,K3END ; &
	CHARACTER(14)  ::MOD_F
 END TYPE
 
 TYPE::F_LAMBDA
	INTEGER        ::BL4  ; &
	REAL(KIND=10)  ::L_ZERO ; &
	CHARACTER(13)  ::MOD_LAMBDA
				  !CONSTANTE:       LAMBDA = L_ZERO 
				  !TIAGO:           LAMBDA = L_ZERO + L_ZERO*(DELTA_OCUP + DELTA_POP)
 END TYPE

 TYPE::F_GAMMA
	INTEGER        ::BL5  ; &
	REAL(KIND=10)  ::GAMMA_ZERO ; &
	CHARACTER(13)  ::MOD_GAMMA

 END TYPE
 
 INTEGER,INTENT(IN)				                    ::NB,NI,MJ,IT_SOL
 REAL(KIND=10),INTENT(IN)			                ::DELTA_X,DELTA_Y,DELTA_T   !,VEL_X,VEL_Y
 TYPE(FONTE_INIC),DIMENSION(NB),INTENT(IN)	        ::FONTE
 TYPE(F_LAMBDA),DIMENSION(NB),INTENT(IN)	        ::DADOS_LAMBDA
 TYPE(F_GAMMA),DIMENSION(NB),INTENT(IN)             ::DADOS_GAMMA_X,DADOS_GAMMA_Y
 REAL(KIND=10),INTENT(IN)		                 	::W_SOL,TOL_SOL,K1VAR,K2VAR,K3VAR,POP_PAS,TOL_OCUP
 REAL(KIND=10),DIMENSION(NI+1),INTENT(IN)           ::XS
 REAL(KIND=10),DIMENSION(MJ+1),INTENT(IN)           ::YS
 REAL(KIND=10),DIMENSION(MJ+1,NI+1),INTENT(IN)	    ::S2D_PAS,S2D_PAS_PAS
 REAL(KIND=10),DIMENSION(MJ+1,NI+1),INTENT(INOUT)	::S2D
 REAL(KIND=10),DIMENSION(MJ+1,NI+1),INTENT(OUT)     ::S2D_LAMBDA, S2D_VEL_X, S2D_VEL_Y
 REAL(KIND=10),INTENT(OUT)                          ::OCUP,POP  !,LAMBDA
 LOGICAL,INTENT(OUT)                                ::CONVERGE
 


 !DECLARA��O DAS VARI�VEIS LOCAIS
 INTEGER	                             ::J,I,IT
 REAL(KIND=10)                           ::CN,CS,CW,CE,CP,LAMBDA,VEL_X,VEL_Y              
 REAL(KIND=10)                           ::CP_TIL,B_TIL,B_BAR,F_S,D_F_S,TAU
 REAL(KIND=10)                           ::X,Y
 REAL(KIND=10)                           ::SATUAL,ERRO
 REAL(KIND=10),DIMENSION(MJ+1,NI+1)	     ::S2DNOVO
 
 
 !!! RESETA TESTE DE CONVERGENCIA !!!
 CONVERGE=.FALSE.
 
 !!! INICIALIZA S2DNOVO !!!
 S2DNOVO=S2D

 
 !!! RESOLVE O SISTEMA !!!
 
 DO IT=1,IT_SOL
 
 
 !CALCULA COEFICIENTES NAO LINEARES GERAIS
 IF (DADOS_LAMBDA(1)%MOD_LAMBDA == 'TIAGO') THEN
 CALL OCUPACAO2D(NI,MJ,TOL_OCUP,S2D,OCUP) !OCUPACAO PARA CALCULAR LAMBDA
 
 CALL INTEGRAL2D(NI,MJ,DELTA_X,DELTA_Y,S2D,POP) !POPULACAO PARA CALCULAR LAMBDA
 
 END IF
 
 
 DO J=2,MJ
 
 Y=YS(J) !Y DA CELULA ATUAL
 
 DO I=2,NI
 
 X=XS(I) !X DA CELULA ATUAL
 
 SATUAL=S2D(J,I)
 
 !CALCULA PARAMETROS PONTUAIS
 CALL FUNCAO_LAMBDA(NB,DADOS_LAMBDA,OCUP,POP,POP_PAS,LAMBDA) !LAMBDA
 S2D_LAMBDA(J,I)=LAMBDA
 
 CALL FUNCAO_GAMMA(NB,DADOS_GAMMA_X,X,Y,VEL_X) !VEL_X
 S2D_VEL_X(J,I)=VEL_X
 
 CALL FUNCAO_GAMMA(NB,DADOS_GAMMA_Y,X,Y,VEL_Y) !VEL_Y
 S2D_VEL_Y(J,I)=VEL_Y
 
 TAU=1.0/(2.0*LAMBDA)
 
 CW = ((VEL_X**2)/(2*LAMBDA))*(1/(DELTA_X**2))
 CE = CW
 
 CN = ((VEL_Y**2)/(2*LAMBDA))*(1/(DELTA_Y**2))
 CS = CN
 
 CP = (1/DELTA_T) + 2*CN + 2*CE
 
 !CALCULA COEFICIENTES NAO LINEARES PONTUAIS
 CALL TERMO_FONTE(NB,FONTE,SATUAL,K1VAR,K2VAR,K3VAR,F_S,D_F_S)
 
 CP_TIL = (1/(DELTA_T**2)) - ((1/DELTA_T)*D_F_S)
 
 B_TIL = ((2/(DELTA_T**2)) - ((1/DELTA_T)*D_F_S))*S2D_PAS(J,I) - (1/(DELTA_T**2))*S2D_PAS_PAS(J,I)
 
 B_BAR = (1/DELTA_T)*S2D_PAS(J,I) + F_S
 
 
 !CALCULA EQUACAO
 
 S2DNOVO(J,I)=(1/(CP + TAU*CP_TIL)) * (CS*S2DNOVO(J-1,I) + CW*S2DNOVO(J,I-1) + CN*S2D(J+1,I) + CE*S2D(J,I+1) + B_BAR + TAU*B_TIL)
 
 
 END DO !I
 
 END DO !J
 
 !ERRO
 ERRO=NORM2(S2DNOVO-S2D)/NORM2(S2DNOVO)
 
 IF (ERRO <= TOL_SOL) THEN
 
    S2D = S2DNOVO
    CONVERGE=.TRUE.

    GOTO 101
    
 END IF 
 
 !PREPARA PROXIMA ITERACAO
 S2D=S2DNOVO
 
 END DO !GAUSS-SEIDEL
 
 
 101 CONTINUE
 PRINT*, 'ITERACOES:',IT
 PRINT*, 'ERRO:', ERRO
 
 !CALCULA COEFICIENTES NAO LINEARES GERAIS
 CALL OCUPACAO2D(NI,MJ,TOL_OCUP,S2D,OCUP) !OCUPACAO
 
 CALL INTEGRAL2D(NI,MJ,DELTA_X,DELTA_Y,S2D,POP) !POPULACAO
 
  
 END SUBROUTINE EQUACAO_S2D
