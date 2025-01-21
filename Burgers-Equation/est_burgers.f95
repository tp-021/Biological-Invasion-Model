subroutine est_burgers(x0,xf,y0,yf,dx,dy,ni,mj,nu,tol_erro,it_max,exemplo,frame,pasta,parametros)
    implicit none

    ! Informações sobre o domínio

    real(kind=10), intent(in) :: x0
    real(kind=10), intent(in) :: xf

    real(kind=10),intent(in) :: y0 
    real(kind=10),intent(in) :: yf 

    real,intent(in) :: dx, dy
    integer, intent(in) :: ni, mj

    ! Outros parâmetros

    real(kind=10),intent(in) :: nu 
    
    real(kind=10),intent(in) :: tol_erro 

    real(kind=10), parameter :: tol_est = 0.000001

    integer,intent(in) :: frame, it_max  !A cada frame, o resultado é gravado

    ! Exemplo que será calculado (Alterar os parâmetros acima)

    character,intent(in) :: exemplo  

    ! Informações para os arquivos

    character(19), intent(in) :: pasta
    character(18), intent(in) :: parametros

    ! Outros
    real(kind=10), parameter :: pi = acos(-1.0)


    ! ............................................................................................................................ ! 
    ! Iniciação das variáveis globais

    real(kind=10), dimension(ni+1,mj+1) :: gamma_x, IT_gamma_x, exata_gamma_x
    real(kind=10), dimension(ni+1,mj+1) :: gamma_y, IT_gamma_y, exata_gamma_y
    real(kind=10), dimension(ni+1) :: x
    real(kind=10), dimension(mj+1) :: y

    real(kind=10) :: media_xe, media_xw, media_yn, media_ys, CP, CE, CW, CN, CS
    
    integer :: Se, Sw, Sn, Ss, i, j, it

    ! Variáveis para cálculo do erro

    real(kind=10) :: erro_x, erro_y
    ! real(kind=10) :: alpha
    ! character(30) :: A

    integer :: MANIPULA

    ! character(len=30) :: NOME
    ! character(6)      :: NOME_M
    ! integer :: MANIPULA

    open(unit=1,file='est_gamma.dat', status='replace')
    write(1,'(A12,4x,A12,4x,A12,4x,A12)') '#x', 'y', 'gamma_x', 'gamma_y'
    close(1)

    open(unit=2,file='est_erro.dat', status='replace')
    write(2,'(A8,4x,A12,4x,A12)') '#it', 'erro_x', 'erro_y'
    close(2)

    open(unit=3, file='est_erro_it.dat', status='replace')
    write(3,'(A10,2X,A15,2X,A15)') '#it', 'erro_x', 'erro_y' 
    close(3)

    gamma_x = 0.0; IT_gamma_x = 0.0; exata_gamma_x = 0.0
    gamma_y = 0.0; IT_gamma_y = 0.0; exata_gamma_y = 0.0
    x = 0.0; y = 0.0

    ! ======================================================================================================================== !
    ! CRIAÇÃO DA MALHA

    ! Valores iniciais e finais 

    x(1) = x0     !x1
    x(ni+1) = xf  !xni

    y(1) = y0
    y(mj+1) = yf

    ! Posição dos outros pontos (malha deslocada)
    do i = 2, ni
        x(i) =  x0 + dx * (i - (3.0/2.0))
    end do 

    do j = 2, mj
        y(j) =  y0 + dy * (j - (3.0/2.0))
    end do 


    ! ======================================================================================================================== !
    ! Condição de contorno

    select case (exemplo)

    case('1')
    !--------------------------------!
    ! Dissertação claudia página 61

    do i = 1, ni+1

        gamma_x(i,mj+1) = 0
        gamma_x(i,1)    = 0
        
        gamma_y(i,1)    = -nu*pi * exp(-5*(pi**2)*nu) * sin(2*pi*x(i))
        gamma_y(i,mj+1) =  nu*pi * exp(-5*(pi**2)*nu) * sin(2*pi*x(i))
        
    end do ! Condição de contorno j

    do j = 1, mj+1

        gamma_x(1,j)    = -2*nu*pi * exp(-5*(pi**2)*nu) * sin(pi*y(j))
        gamma_x(ni+1,j) = -2*nu*pi * exp(-5*(pi**2)*nu) * sin(pi*y(j))
    
        gamma_y(1,j)    = 0
        gamma_y(ni+1,j) = 0
        
    end do ! Condição de contorno j

    case('2')
    !--------------------------------!
    ! Dissertação claudia página 65

        do i = 1, ni+1

            gamma_x(i,1)    = 3.0/4.0 - 1.0/( 4.0* (1.0 + exp(    (-4.0*x(i) ) / (32.0*nu) ) ) )
            gamma_x(i,mj+1) = 3.0/4.0 - 1.0/( 4.0* (1.0 + exp( (4.0-4.0*x(i) ) / (32.0*nu) ) ) )
        
            gamma_y(i,1)    = 3.0/4.0 + 1.0/( 4.0* (1.0 + exp(    (-4.0*x(i) ) / (32.0*nu) ) ) )
            gamma_y(i,mj+1) = 3.0/4.0 + 1.0/( 4.0* (1.0 + exp( (4.0-4.0*x(i) ) / (32.0*nu) ) ) )
            
        end do ! Condição de contorno j

        do j = 1, mj+1

            gamma_x(1,j)    = 3.0/4.0 - 1.0/( 4.0* (1.0 + exp(     (4.0*y(j) ) / (32.0*nu) ) ) )
            gamma_x(ni+1,j) = 3.0/4.0 - 1.0/( 4.0* (1.0 + exp( (4.0*y(j)-4.0 ) / (32.0*nu) ) ) )

            gamma_y(1,j)    = 3.0/4.0 + 1.0/( 4.0* (1.0 + exp(     (4.0*y(j) ) / (32.0*nu) ) ) )
            gamma_y(ni+1,j) = 3.0/4.0 + 1.0/( 4.0* (1.0 + exp( (4.0*y(j)-4.0 ) / (32.0*nu) ) ) )
            
        end do ! Condição de contorno j

end select

    it = 1

    do while (it < it_max)

        do i = 2, ni
            do j = 2, mj

                media_xe = (IT_gamma_x(i,j) + IT_gamma_x(i+1,j))/2
                media_xw = (IT_gamma_x(i,j) + IT_gamma_x(i-1,j))/2
                media_yn = (IT_gamma_y(i,j) + IT_gamma_y(i,j+1))/2
                media_ys = (IT_gamma_y(i,j) + IT_gamma_y(i,j-1))/2

                if ( media_xe >= 0 ) then
                    Se = 1
                else 
                    Se = -1 
                end if

                if ( media_xw >= 0 ) then
                    Sw = 1
                else 
                    Sw = -1 
                end if

                if ( media_yn >= 0 ) then
                    Sn = 1
                else 
                    Sn = -1 
                end if

                if ( media_ys >= 0 ) then
                    Ss = 1
                else 
                    Ss = -1 
                end if

                CP = (1/dx) * (media_xe * (1+Se)/2 - media_xw * (1-Sw)/2) + (1/dy) * (media_yn * (1+Sn)/2 - &
                media_ys * (1-Ss)/2) + (2*nu)/(dx**2) + (2*nu)/(dy**2)  

                if ( CP ==0 ) then
                    write(*,'(A30)') 'CP == 0' 
                    CP = 0.000000001               
                end if

                CE = - 1/dx * media_xe * (1-Se)/2 + nu/(dx**2)
                CW =   1/dx * media_xw * (1+Sw)/2 + nu/(dx**2)
                CN = - 1/dy * media_yn * (1-Sn)/2 + nu/(dy**2)                                                                                      
                CS =   1/dy * media_ys * (1+Ss)/2 + nu/(dy**2)

                ! Cálcula a função gamma no ponto atual

                gamma_x(i,j) = ( CE * IT_gamma_x(i+1,j) + CW * gamma_x(i-1,j) + &
                CN * IT_gamma_x(i,j+1) + CS * gamma_x(i,j-1) )/CP

                gamma_y(i,j) = ( CE * IT_gamma_y(i+1,j) + CW * gamma_y(i-1,j) + &
                CN * IT_gamma_y(i,j+1) + CS * gamma_y(i,j-1) )/CP
                  
            end do
        end do

        erro_x = norm2(gamma_x - IT_gamma_x)/norm2(gamma_x)
        erro_y = norm2(gamma_y - IT_gamma_y)/norm2(gamma_y)   

        open(unit=3, file='est_erro_it.dat', status='old', position='append')
        write(3,'(I10,2X,F15.8,2X,F15.8)') it, erro_x, erro_y
        close(3)

        if ( erro_x < tol_erro .and. erro_y < tol_erro ) then
            exit
        end if
        
        if (mod(it,frame) == 0) then
            open(unit=2,file='est_erro.dat', status='old', position='append')
            write(2,'(I8.0,4x,F12.10,4x,F12.10)') it, erro_x, erro_y
            close(2)
        end if
        
        IT_gamma_x = gamma_x; IT_gamma_y = gamma_y
        it = it + 1

    end do

    open(unit=1,file='est_gamma.dat', status='old', position='append')

    do i = 1, ni+1
        do j = 1, mj+1
            
            write(1,'(F12.8,4x,F12.8,4x,F12.10,4x,F12.10)') x(i), y(j), gamma_x(i,j), gamma_y(i,j)
                 
        end do  
    end do
    
    close(1)

    MANIPULA = SYSTEM('if test -e est_gamma.dat; then mv est_gamma.dat SAI_AUTO/'//trim(pasta)//'; fi;')

    MANIPULA = SYSTEM('if test -e est_gamma.dat; then mv est_gamma.dat SAI_AUTO/'//trim(pasta)//'; fi;')

    MANIPULA = SYSTEM('if test -e est_erro.dat; then mv est_erro.dat SAI_AUTO/'//trim(pasta)//'; fi;')

    MANIPULA = SYSTEM('if test -e est_erro_it.dat; then mv est_erro_it.dat SAI_AUTO/'//trim(pasta)//'; fi;')
     
end subroutine est_burgers