program main
    implicit none
    
    real(kind=10), parameter :: x0 = 0.0
    real(kind=10), parameter :: xf = 10.0
    real(kind=10), parameter :: y0 = 0.0
    real(kind=10), parameter :: yf = 10.0
    real(kind=10), parameter :: t0 = 0.0
    real(kind=10), parameter :: tf = 20.0

    real(kind=10), parameter :: x1 = 4.0
    real(kind=10), parameter :: x2 = 6.0

    real(kind=10), parameter :: y1 = 4.0
    real(kind=10), parameter :: y2 = 6.0

    integer, parameter :: ni = 20
    integer, parameter :: mj = 20
    integer, parameter :: tt = 10

    real(kind=10), dimension(ni+1,mj+1) :: gamma_x, ANT_gamma_X, IT_gamma_x
    real(kind=10) :: media_e, media_w, media_n, media_s, nu, dx, dy, dt, CP, CE, CW, CN, CS, BP, t
    integer :: Se, Sw, Sn, Ss, i, j, it, k, MANIPULA
    real(kind=10), dimension(ni+1) :: x
    real(kind=10), dimension(mj+1) :: y
    character(len=30) :: NOME
    character(6)      :: NOME_N

    nu = 1
    
    dx = (x0-xf)/(ni-1)
    dy = (y0-yf)/(mj-1)
    dt = (t0-tf)/tt


    !CONDIÇÃO INICIAL E DE CONTORNO
    
    
    do i = 1, ni+1
        do j = 1, mj+1

            x(i) = dx*(i-1) + x0
            y(j) = dy*(j-1) + y0

            if ( (x(i)>=x1 .and. x(i)<=x2) .and. (y(j)>=y1 .and. y(j)<=y2)) then
                gamma_x(i,j) = 1
            else
                gamma_x(i,j) = 0
            end if

            gamma_x(i,1) = 0
            gamma_x(i,mj+1) = 0
    
            gamma_x(i,1) = 0
            gamma_x(i,mj+1) = 0
            
        end do
    end do

    k = 0.0

    WRITE(NOME_N,'(F0.6)') k
    NOME='S2D'//NOME_N//'.dat'
    call GRAVACAO_S2D(ni,mj,x,y,gamma_x,NOME,t)
    MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_PD; fi;')

    close(1)

    IT_gamma_x = gamma_x
    ANT_gamma_x = gamma_x


    do k = 1, tt


        do it = 1, 500
            

            do i = 2, ni
                do j = 2, mj
                    
                    media_e = (gamma_x(i,j) + gamma_x(i+1,j))/2
                    media_w = (gamma_x(i,j) + gamma_x(i-1,j))/2
                    media_n = (gamma_x(i,j) + gamma_x(i,j+1))/2
                    media_s = (gamma_x(i,j) + gamma_x(i,j-1))/2

                    if ( media_e >= 0 ) then
                        Se = 1
                    else 
                        Se = -1 
                    end if

                    if ( media_w >= 0 ) then
                        Sw = 1
                    else 
                        Sw = -1 
                    end if

                    if ( media_n >= 0 ) then
                        Sn = 1
                    else 
                        Sn = -1 
                    end if

                    if ( media_e >= 0 ) then
                        Ss = 1
                    else 
                        Ss = -1 
                    end if

                    CP = 1/dt + (1/dx) * (media_e * (1+Se)/2 - media_w * (1-Sw)/2) + (1/dy) * (media_n * (1+Sn)/2 - &
                     media_s * (1-Ss)/2) + (2*nu)/(dx**2) + (2*nu)/(dy**2)  
                    
                    CE = 1/dx * media_e * (1-Se)/2 - nu/(dx**2)
                    CW = 1/dx * media_w * (1-Sw)/2 - nu/(dx**2)
                    CN = 1/dy * media_n * (1-Sn)/2 - nu/(dy**2)
                    CS = 1/dy * media_s * (1-Ss)/2 - nu/(dy**2)
                    
                    gamma_x (i,j) = (1/CP)*((1/dt)*ANT_gamma_X(i,j) - CE* IT_gamma_x(i+1,j) + CW* gamma_x(i-1,j) - &
                     CN* IT_gamma_x(i,j+1) + CS* gamma_x(i,j-1))
                    
                    
                end do
            end do

        IT_gamma_x = gamma_x
        end do

        t = k*dt

        ANT_gamma_X = gamma_x

    
    end do

end program main
