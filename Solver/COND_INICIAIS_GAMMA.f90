SUBROUTINE COND_INICIAIS_GAMMA(NI,MJ,NB,MOD_GAMMA,XS,YS,GAMMAX, GAMMAY)
 
IMPLICIT NONE 

integer,intent(in) :: NI,MJ,NB
character(10), intent(in) :: MOD_GAMMA
real(kind=10), dimension(NI+1), intent(in) :: XS
real(kind=10), dimension(MJ+1), intent(in) :: YS
real(kind=10), intent(out) :: GAMMAX, GAMMAY

integer :: i,j

! TYPE::F_GAMMA
! INTEGER        ::BL5  ; &
! REAL(KIND=10)  ::GAMMA_ZERO ; &
! CHARACTER(13)  ::MOD_GAMMA


! TYPE(F_GAMMA),DIMENSION(:),ALLOCATABLE	::DADOS_GAMMA_X,DADOS_GAMMA_Y

TYPE ::F_GAMMA
    integer :: BL; &
    real(kind=10) :: GAMMA_ZERO
    character(13) :: MOD_GAMMA
END TYPE


!  TYPE(DOMINIO),INTENT(IN)			                   ::COMPDOMX,COMPDOMY
!  INTEGER,INTENT(IN)				                       ::NI,MJ,NB
!  REAL(KIND=10),INTENT(IN)		                       ::COMP_DOMI,COMP_DOMJ
!  TYPE(CONDICOES_INIC),DIMENSION(NB),INTENT(IN)	       ::PARCOND_I
!  REAL(KIND=10),DIMENSION(NI+2),INTENT(IN)		       ::X
!  REAL(KIND=10),DIMENSION(MJ+2),INTENT(IN)		       ::Y
!  REAL(KIND=10),DIMENSION(NI+1),INTENT(IN)		       ::XS
!  REAL(KIND=10),DIMENSION(MJ+1),INTENT(IN)		       ::YS
!  REAL(KIND=10),DIMENSION(MJ+1,NI+1),INTENT(OUT)	       ::S2D


DO i=1,NI+1
    DO j=1,MJ+1
    
        GAMMAX(i,j) = 0.0001 
        GAMMAY(i,j) = 0.0001

    END DO
END DO
 

END SUBROUTINE COND_INICIAIS_GAMMA
