! PROGRAMA PRINCIPAL, ONDE SE GERENCIA O FLUXO DE DESENVOLVIMENTO DO CÓDIGO INVASÃO BIOLÓGICA
PROGRAM PRINCIPAL

 !DECLARAÇÃO DOS PARAMETROS GLOBAIS

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
				  !DEGRAU: 		se X<A entao S=0; se X>=A and X<=C entao S = B; se X>C entao S=0,
				  !EXPONENCIAL: 	S = A*EXP(B*X)+ C, 
				  !NORMAL: 		S = (A/(B*SQRT(2*PI)))*EXP(-0.5*((X-C)/B)**2.0)
 END TYPE

 TYPE::FONTE_INIC
	INTEGER        ::BL3  ; &
	REAL(KIND=10)  ::K1IN,K1STEP,K1END,K2IN,K2STEP,K2END,K3IN,K3STEP,K3END ; &
	CHARACTER(14)  ::MOD_F
				  !EXPONENCIAL:     F = K1*S, 
				  !LOGISTICO:       F = K1*S*(1.0-S/K2), 
				  !GOLPERTZ:        F = K1*S*ln(K2/S), 
				  !ALLEE:           F = K1*S*(1.0-S/K2)*(S/K3-1.0)
 END TYPE

 TYPE::CONDICOES_CONT
	INTEGER 	::PRI 
	REAL(KIND=10)	::CC1W ; REAL(KIND=10) ::CC2W
	REAL(KIND=10)	::CC1E ; REAL(KIND=10) ::CC2E
	REAL(KIND=10)	::CC1S ; REAL(KIND=10) ::CC2S
	REAL(KIND=10)	::CC1N ; REAL(KIND=10) ::CC2N
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

 CHARACTER(7)	::METODO_CONVECTIVO
 CHARACTER(3)   ::EXT_SN
 LOGICAL        ::ESCREVE_CABECALHO
 INTEGER	::NIINIC,MJINIC,IT_SOL,AM_FRAM,NB,BLOCO,MANIPULA,BL
 REAL(KIND=10)	::VELOCIDADE,T_FIN,DELTA_T,POP_C
 REAL(KIND=10)	::W_SOL,K1RESET,K2RESET,K3RESET,TOL_OCUP,AUTO_G_MAX,AUTO_G_STEP
 REAL(KIND=10)	::TOL_SOL,TOL_PER
 TYPE(CONDICOES_INIC)      ,DIMENSION(:),ALLOCATABLE	::PARCOND_I
 TYPE(FONTE_INIC)          ,DIMENSION(:),ALLOCATABLE	::FONTE
 TYPE(CONDICOES_CONT)      ,DIMENSION(:),ALLOCATABLE	::CONDCONTORNO
 TYPE(DOMINIO)						                    ::COMPDOMX,COMPDOMY
 TYPE(F_LAMBDA)            ,DIMENSION(:),ALLOCATABLE	::DADOS_LAMBDA
 TYPE(F_GAMMA)             ,DIMENSION(:),ALLOCATABLE	::DADOS_GAMMA_X,DADOS_GAMMA_Y

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! !!!!!!!!!!!!!!!! AQUISIÇÃO DE PARÂMETROS !!!!!!!!!!!!!!!!!!!!!!!!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


 ! VAMOS CONSIDERAR UM ÚNICO BLOCO
 NB = 1
 
 
 OPEN(2, FILE='PARAMETROS_PP.dat',STATUS='OLD')

 READ (2,*) !DADOS DE ENTRADA PARA RESOLUÇÃO DO PROBLEMA INVASÃO BIOLÓGICA
 READ (2,*) !----------------------------------------------------------------------
 
 READ (2,*) !----------------------------------------------------------
 READ (2,*) !--------- PARÂMETROS PARA A MONTAGEM DE ESQUEMAS NUMÉRICOS
 READ (2,*) !----------------------------------------------------------

 READ (2,*) METODO_CONVECTIVO
 READ (2,*) VELOCIDADE
 
 READ (2,*) !---------------------------------------------
 READ (2,*) !--------- PARÂMETROS GLOBAIS PARA A SIMULAÇÃO
 READ (2,*) !---------------------------------------------

 READ (2,*) T_FIN
 READ (2,*) DELTA_T
 READ (2,*) NIINIC 
 READ (2,*) MJINIC
 READ (2,*) W_SOL
 READ (2,*) IT_SOL
 READ (2,*) TOL_SOL
 READ (2,*) TOL_PER
 READ (2,*) AM_FRAM
 READ (2,*) TOL_OCUP
 READ (2,*) EXT_SN
 READ (2,*) POP_C
 READ (2,*) !----------------------------------------------------------------------------------------
 READ (2,*) !--------- DOMÍNIO - COMPRIMENTOS: INICIAL, FINAL E FIX/VAR (FIX = FIXO E VAR = VARIAVEL)
 READ (2,*) !----------------------------------------------------------------------------------------
 
 READ (2,*) COMPDOMX
 READ (2,*) COMPDOMY
 
 READ (2,*) !-------------------------------------------------
 READ (2,*) !--------- PARÂMETROS PARA LAMBDA
 READ (2,*) !------------------------------------------------- 
 READ (2,*) !BLOCO		L_ZERO		TIPO
 
 ALLOCATE(DADOS_LAMBDA(NB))
 READ (2,*) (DADOS_LAMBDA(BLOCO),BLOCO=1,NB)
 
 READ (2,*) !-------------------------------------------------
 READ (2,*) !--------- PARÂMETROS PARA AS PROPRIEDADES FÍSICAS
 READ (2,*) !-------------------------------------------------
 READ (2,*) !BLOCO		GAMMA_ZERO	TIPO		(CONSTANTE: GAMMA CONSTANTE)

 ALLOCATE(DADOS_GAMMA_X(NB),DADOS_GAMMA_Y(NB))
 
 READ (2,*) (DADOS_GAMMA_X(BLOCO),BLOCO=1,NB)
 READ (2,*) (DADOS_GAMMA_Y(BLOCO),BLOCO=1,NB)
 READ (2,*) AUTO_G_STEP
 READ (2,*) AUTO_G_MAX
 
 
 READ (2,*) !---------------------------------------------------
 READ (2,*) !--------- PARÂMETROS PARA A MONTAGEM DO TERMO FONTE
 READ (2,*) !---------------------------------------------------
 READ (2,*) !BLOCO     F

 ALLOCATE(FONTE(NB))
 READ (2,*) (FONTE(BLOCO),BLOCO=1,NB)
 
 READ (2,*) !--------------------------------------------------------
 READ (2,*) !--------- PARÂMETROS PARA A MONTAGEM DA CONDIÇÃO INICIAL
 READ (2,*) !--------------------------------------------------------
 READ (2,*) !BLOCO     S_I

 ALLOCATE(PARCOND_I(NB))
 READ (2,*) (PARCOND_I(BLOCO),BLOCO=1,NB)
  
 READ (2,*) !---------------------------------------------
 READ (2,*) !--------- APLICAÇÃO DAS CONDIÇÕES DE CONTORNO
 READ (2,*) !---------------------------------------------
 READ (2,*) ! BLOCO      W         E

 ALLOCATE(CONDCONTORNO(NB))
 READ (2,*) (CONDCONTORNO(BL),BL=1,NB)
 CLOSE(2)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!! EXECUÇÃO DO CÓDIGO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 !!! PASTAS E ARQUIVOS !!!

 ! SE A PASTA SAI_PD (SAIDAS DE DENSIDADE POPULACIONAL) EXISTE ENTÃO ELA É DELETADA
 
 MANIPULA = SYSTEM('if test -e SAI_PD; then rm -r SAI_PD; fi;')
 MANIPULA = SYSTEM('if test -e SAI_AUTO; then rm -r SAI_AUTO; fi;') 
      
 !CRIA A PASTA SAI_PD
 
 MANIPULA = SYSTEM('mkdir SAI_PD')
 
 !CRIA A PASTA GAMMAX
 
 MANIPULA = SYSTEM('mkdir SAI_PD/GAMMAX')
 
 !CRIA A PASTA GAMMAY
 
 MANIPULA = SYSTEM('mkdir SAI_PD/GAMMAY')
 
 !CRIA A PASTA LAMBDA
 
 MANIPULA = SYSTEM('mkdir SAI_PD/LAMBDA')
 
 !CRIA A PASTA NEGATIVOS
 
 MANIPULA = SYSTEM('mkdir SAI_PD/NEGATIVOS')
 
 !CRIA A PASTA VS
 
 MANIPULA = SYSTEM('mkdir SAI_PD/VS')
 
 !CRIA A PASTA ANIMA
 
 MANIPULA = SYSTEM('mkdir SAI_PD/ANIMA')

 ! SE O ARQUIVO EVOLUCAO.dat EXISTE ENTÃO É DELETADO
 
 MANIPULA = SYSTEM('if test -e EVOLUCAO.dat; then rm EVOLUCAO.dat; fi;') 

 ! SE O ARQUIVO DIAGONAL_DOMINANTE.dat EXISTE ENTÃO É DELETADO
 
 MANIPULA = SYSTEM('if test -e DIAGONAL_DOMINANTE.dat; then rm DIAGONAL_DOMINANTE.dat; fi;') 

 ! SE O ARQUIVO QUANT_POPULACIONAL.dat EXISTE ENTÃO É DELETADO 

 MANIPULA = SYSTEM('if test -e QUANT_POPULACIONAL.dat; then rm QUANT_POPULACIONAL.dat; fi;')
 
 ! SE O ARQUIVO EQUILIBRIO.dat EXISTE ENTÃO É DELETADO 

 MANIPULA = SYSTEM('if test -e EQUILIBRIO.dat; then rm EQUILIBRIO.dat; fi;')
 
 ! SE O ARQUIVO POP_TOTAL_NUMERICO.dat EXISTE ENTÃO É DELETADO 

 MANIPULA = SYSTEM('if test -e POP_TOTAL_NUMERICO.dat; then rm POP_TOTAL_NUMERICO.dat; fi;')
 
 ! SE O ARQUIVO POP_TOTAL_ANALITICO.dat EXISTE ENTÃO É DELETADO 

 MANIPULA = SYSTEM('if test -e POP_TOTAL_ANALITICO.dat; then rm POP_TOTAL_ANALITICO.dat; fi;')

 ! SE O ARQUIVO SN_******.dat EXISTE ENTÃO É DELETADO 

 MANIPULA = SYSTEM('if test -e SN_*; then rm SN_*; fi;')
 
 ! SE O ARQUIVO EVO_NEGATIVA.dat EXISTE ENTÃO É DELETADO 

 MANIPULA = SYSTEM('if test -e EVO_NEGATIVA.dat; then rm EVO_NEGATIVA.dat; fi;')
 

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!! CATEGORIZACAO DA SIMULACAO PELO TERMO FONTE (EX: EXPONENCIAL, LOGISTICO ETC) !!!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 !CALL CATEGORIZACAO(NB,FONTE,K1RESET,K2RESET,K3RESET)
 K1RESET = FONTE(1)%K1IN ; K2RESET = FONTE(1)%K2IN ; K3RESET = FONTE(1)%K3IN
 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!! PRINCIPAL SIMULACAO !!!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 IF ( COMPDOMX%DOM3 == 'AUT' ) THEN
 
 MANIPULA = SYSTEM('mkdir SAI_AUTO')
 
 CALL PRINCIPAL_SIM_AUTO(ESCREVE_CABECALHO,K1RESET,K2RESET,K3RESET,FONTE,COMPDOMX,NIINIC,COMPDOMY,MJINIC,NB,&
                    PARCOND_I,CONDCONTORNO,VELOCIDADE,DELTA_T,T_FIN,W_SOL,TOL_SOL,&
                    IT_SOL,TOL_PER,AM_FRAM,TOL_OCUP,DADOS_LAMBDA,DADOS_GAMMA_X,DADOS_GAMMA_Y,POP_C,AUTO_G_STEP,&
                    AUTO_G_MAX,EXT_SN)
                    
 ELSE
 
 CALL PRINCIPAL_SIM(ESCREVE_CABECALHO,K1RESET,K2RESET,K3RESET,FONTE,COMPDOMX,NIINIC,COMPDOMY,MJINIC,NB,&
                    PARCOND_I,CONDCONTORNO,VELOCIDADE,DELTA_T,T_FIN,W_SOL,TOL_SOL,&
                    IT_SOL,TOL_PER,AM_FRAM,TOL_OCUP,DADOS_LAMBDA,DADOS_GAMMA_X,DADOS_GAMMA_Y,POP_C,EXT_SN)
 
 
 END IF
 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!! MOVIMENTACAO DE ARQUIVOS !!!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 !ARQUIVOS DA SPLINE
 MANIPULA = SYSTEM('if test -e dist.dat; then mv dist.dat SAI_PD; fi;')
 MANIPULA = SYSTEM('if test -e splineparE.dat; then mv splineparE.dat SAI_PD; fi;')
 MANIPULA = SYSTEM('if test -e splineparD.dat; then mv splineparD.dat SAI_PD; fi;')
 
 !OUTROS ARQUIVOS
 MANIPULA = SYSTEM('mv '//'QUANT_POPULACIONAL.dat'//' SAI_PD')
 MANIPULA = SYSTEM('mv '//'EVO_NEGATIVA.dat'//' SAI_PD')
 MANIPULA = SYSTEM('cp '//'PARAMETROS_PP.dat'//' SAI_PD')
 
 
 
 
!  ! CATEGORIZACAO DA SIMULACAO EM: EXPONENCIAL, LOGISTICO, GOLPERTZ OU ALLEE
!  
!  CALL CATEGORIZACAO(NB,FONTE,K1RESET,K2RESET,K3RESET)
!  
!  ! NECESSARIO PARA MONTAR O ARQUIVO QUE AVALIA DIAGONAL DOMINANTE
!  
!  ESCREVE_CABECALHO = .TRUE.
!  
!  
!  IF (COMPDOMX%DOM3 == 'EQU') THEN
!  
!  
!     CALL PRINCIPAL_EQU(ESCREVE_CABECALHO,NB,NIINIC,IT_SOL,AM_FRAM,K1RESET,K2RESET,K3RESET,&
!                     TAU,VELOCIDADE,D,W_SOL,TOL_SOL,TOL_PER,FONTE,&
!                        COMPDOMX,PARCOND_I,CONDCONTORNO)
!     
!     
!  ELSE IF ((COMPDOMX%DOM3 == 'SIM').AND.&
! 	      (FONTE(1)%K1IN == FONTE(1)%K1STEP).AND.(FONTE(1)%K1STEP == FONTE(1)%K1END).AND.&
! 	      (FONTE(1)%K2IN == FONTE(1)%K2STEP).AND.(FONTE(1)%K2STEP == FONTE(1)%K2END).AND.&
! 	      (FONTE(1)%K3IN == FONTE(1)%K3STEP).AND.(FONTE(1)%K3STEP == FONTE(1)%K3END)) THEN 
!  
!  
!     CALL PRINCIPAL_SIM(ESCREVE_CABECALHO,K1RESET,K2RESET,K3RESET,FONTE,COMPDOMX,NIINIC,COMPDOMY,MJINIC,NB,&
!                     PARCOND_I,CONDCONTORNO,VELOCIDADE,D,TAU,DT,T_FIN,EXI,W_SOL,TOL_SOL,&
!                     IT_SOL,TOL_PER,AM_FRAM)
!     
!     
!  ELSE 
!  
!  
!     WRITE(*,*)'ERRO DE DEFINICAO.'
!     WRITE(*,*)'VERIFIQUE SE FOI INSERIDO "SIM" PARA SIMULACAO OU "EQU" PARA CALCULAR EQUILIBRIO'
!     WRITE(*,*)'VERIFIQUE OS VALORES INSERIDOS PARA K1, K2 E K3 NO ARQUIVO PARAMETROS_PP.dat'
!     
!     STOP
!     
!     
!  END IF
!   
!  
!  
!  ! CÓPIA DO ARQUIVO EVOLUCAO.dat
!    
!    MANIPULA = SYSTEM('mv '//'EVOLUCAO.dat'//' SAI_PD')
! 
!  ! CÓPIA DO ARQUIVO DIAGONAL_DOMINANTE.dat
!    
!    MANIPULA = SYSTEM('mv '//'DIAGONAL_DOMINANTE.dat'//' SAI_PD')
!    
!  ! CÓPIA DO ARQUIVO PARAMETROS_PP.dat
!    
!    MANIPULA = SYSTEM('cp '//'PARAMETROS_PP.dat'//' SAI_PD')
!             
!  ! CALCULO APENAS PARA SIMULACAO
!  
!  IF ( COMPDOMX%DOM3 == 'SIM' ) THEN
!  
!     ! PARA GERAR FIGURA 3D NO GNUPLOT COMO MALHA
!       
!       MANIPULA = SYSTEM('mv '//'POP_TOTAL_NUMERICO.dat'//' SAI_PD')
! 
!     ! FINALIZACAO DO ARQUIVO QUANT_POPULACIONAL.dat E MOVIMENTACAO PARA A PASTA SAI_PD  
!       
!       MANIPULA = SYSTEM('mv '//'QUANT_POPULACIONAL.dat'//' SAI_PD')
!     
!  END IF
!  
!  IF ( COMPDOMX%DOM3 == 'EQU' ) THEN
!  
!     ! FINALIZACAO DO ARQUIVO EQUILIBRIO.dat E MOVIMENTACAO PARA A PASTA SAI_PD
!     
!       MANIPULA = SYSTEM('mv '//'EQUILIBRIO.dat'//' SAI_PD')
!     
!  END IF
 
  

 END PROGRAM PRINCIPAL
