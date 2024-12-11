subroutine est_burgers(x,y,dx,dy,ni,mj,nu,pi,tol_erro,gamma_x,gamma_y)
    implicit none

    real(kind=10), intent(in):: tol_erro
    real(kind=10), intent(in)  :: dx, dy, nu, pi
    integer, intent(in) :: ni, mj
    
    real(kind=10), dimension(ni+1,mj+1), intent(out) :: gamma_x
    real(kind=10), dimension(ni+1,mj+1), intent(out) :: gamma_y
    real(kind=10), dimension(ni+1), intent(in) :: x
    real(kind=10), dimension(mj+1), intent(in) :: y

    real(kind=10), dimension(ni+1,mj+1) :: IT_gamma_x, IT_gamma_y

    real(kind=10) :: CP, CE, CW, CN, CS, media_xe, media_xw, media_yn, media_ys, erro_x, erro_y
    integer :: Se, Sw, Sn, Ss, i, j, it

    character(len=30) :: NOME
    character(6)      :: NOME_M
    integer :: MANIPULA


    open(unit=1,file='est_gamma.dat', status='replace')
    write(1,'(A12,4x,A12,4x,A12,4x,A12)') 'x', 'y', 'gamma_x', 'gamma_y'
    close(1)

    open(unit=2,file='est_erro.dat', status='replace')
    write(2,'(A8,4x,A12,4x,A12)') 'it', 'erro_x', 'erro_y'
    close(2)


    gamma_x = 1.0; gamma_y = 1.0; IT_gamma_x = 1.0; IT_gamma_y = 1.0

    do i = 1, ni+1

        gamma_x(i,mj+1) = 0
        gamma_x(i,1)    = 0
        
        ! gamma_y(i,1)    = -nu*pi * exp(-5*(pi**2)*nu*t(k)) * sin(2*pi*x(i))
        ! gamma_y(i,mj+1) =  nu*pi * exp(-5*(pi**2)*nu*t(k)) * sin(2*pi*x(i))
        
        ! gamma_y(i,1)    = -nu*pi * exp(-5*(pi**2)*nu) * sin(2*pi*x(i))
        ! gamma_y(i,mj+1) =  nu*pi * exp(-5*(pi**2)*nu) * sin(2*pi*x(i))

        gamma_y(i,1)    = 0
        gamma_y(i,mj+1) = 0

        
    end do 

    do j = 1, mj+1

        ! gamma_x(1,j)    = -2*nu*pi * exp(-5*(pi**2)*nu*t(k)) * sin(pi*y(j))
        ! gamma_x(ni+1,j) = -2*nu*pi * exp(-5*(pi**2)*nu*t(k)) * sin(pi*y(j))
        
        ! gamma_x(1,j)    = -2*nu*pi * exp(-5*(pi**2)*nu) * sin(pi*y(j))
        ! gamma_x(ni+1,j) = -2*nu*pi * exp(-5*(pi**2)*nu) * sin(pi*y(j))
        
        gamma_x(1,j)    = 0
        gamma_x(ni+1,j) = 0

        gamma_y(1,j)    = 0
        gamma_y(ni+1,j) = 0

    end do 


    do it = 1, 10000

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

                gamma_x(i,j) = (CE* IT_gamma_x(i+1,j) + CW* gamma_x(i-1,j) + &
                CN* IT_gamma_x(i,j+1) + CS* gamma_x(i,j-1) )/CP

                gamma_y(i,j) = ( CE* IT_gamma_y(i+1,j) + CW* gamma_y(i-1,j) + &
                CN* IT_gamma_y(i,j+1) + CS* gamma_y(i,j-1) )/CP
                  
            end do
        end do

        erro_x = norm2(gamma_x - IT_gamma_x)/norm2(gamma_x)
        erro_y = norm2(gamma_y - IT_gamma_y)/norm2(gamma_y)   

        if ( erro_x < tol_erro .and. erro_y < tol_erro ) then
            exit

        else
            IT_gamma_x = gamma_x; IT_gamma_y = gamma_y

        end if
        
        open(unit=2,file='est_erro.dat', status='old', position='append')
        write(2,'(I8.0,4x,F12.10,4x,F12.10)') it, erro_x, erro_y
        close(2)


    end do

    open(unit=1,file='est_gamma.dat', status='old', position='append')

    do i = 1, ni+1
        do j = 1, mj+1
            
            write(1,'(F12.8,4x,F12.8,4x,F12.10,4x,F12.10)') x(i), y(j), gamma_x(i,j), gamma_y(i,j)
                 
        end do  
    end do
    
    close(1)
     
end subroutine est_burgers