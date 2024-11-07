program main
    implicit none
    
    real(kind=10), parameter :: x0 = 0.0
    real(kind=10), parameter :: xf = 20.0
    real(kind=10), parameter :: y0 = 0.0
    real(kind=10), parameter :: yf = 20.0
    real(kind=10), parameter :: t0 = 0.0
    real(kind=10), parameter :: tf = 5.0

    real(kind=10), parameter :: x1 = 5.0
    real(kind=10), parameter :: x2 = 15.0

    real(kind=10), parameter :: y1 = 5.0
    real(kind=10), parameter :: y2 = 15.0

    integer, parameter :: ni = 100
    integer, parameter :: mj = 100
    integer, parameter :: tt = 200

    real, parameter    :: tol_erro = 0.00001

    real(kind=10), dimension(ni+1,mj+1) :: gamma_x, ANT_gamma_X, IT_gamma_x
    real(kind=10) :: media_e, media_w, media_n, media_s, nu, dx, dy, dt, CP, CE, CW, CN, CS, t, erro
    integer :: Se, Sw, Sn, Ss, i, j, it, k, MANIPULA
    real(kind=10), dimension(ni+1) :: x
    real(kind=10), dimension(mj+1) :: y
    character(len=30) :: NOME
    character(6)      :: NOME_N

    nu = 0
     
    dx = (xf-x0)/(ni-1)
    dy = (yf-y0)/(mj-1)
    dt = (tf-t0)/(tt-1)


    !CONDIÇÃO INICIAL E DE CONTORNO
    
    
    do j = 1, mj+1
        do i = 1, ni+1

            x(i) = dx*(i-1) + x0
            y(j) = dy*(j-1) + y0

            if ( (x(i)>=x1 .and. x(i)<=x2) .and. (y(j)>=y1 .and. y(j)<=y2)) then
                gamma_x(i,j) = 10
            else
                gamma_x(i,j) = 1
            end if

            gamma_x(i,1) = 0
            gamma_x(i,mj+1) = 0
    
            gamma_x(1,j) = 0
            gamma_x(ni+1,j) = 0
            
        end do
    end do

    k = 0.0
    t = t0
    it = 1

    WRITE(NOME_N,'(I0.6)') k
    NOME='GX'//NOME_N//'.dat'
    call GRAVACAO_S2D(ni,mj,x,y,gamma_x,NOME,t)
    MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_PD; fi;')

    IT_gamma_x = gamma_x
    ANT_gamma_x = gamma_x


    do k = 1, tt
        
        do while(it <= 2000000)

            do i = 2, ni
                do j = 2, mj
                    
                    media_e = (ANT_gamma_X(i,j) + ANT_gamma_x(i+1,j))/2
                    media_w = (ANT_gamma_X(i,j) + ANT_gamma_X(i-1,j))/2
                    media_n = (ANT_gamma_X(i,j) + ANT_gamma_X(i,j+1))/2
                    media_s = (ANT_gamma_X(i,j) + ANT_gamma_X(i,j-1))/2

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
                    
                    gamma_x (i,j) = ((1/dt)*ANT_gamma_X(i,j) - CE* IT_gamma_x(i+1,j) + CW* gamma_x(i-1,j) - &
                     CN* IT_gamma_x(i,j+1) + CS* gamma_x(i,j-1))/CP
                    
                end do
            end do
            
            it = it+1

            erro = norm2(gamma_x - IT_gamma_x)/norm2(gamma_x)

            if ( erro < tol_erro ) then
                write(*,'(F10.6)') erro
                write(*,'(I10.0)') it
                exit
            end if

            IT_gamma_x = gamma_x
        
        end do

        ANT_gamma_X = gamma_x

        t = t + dt

        WRITE(NOME_N,'(I0.6)') k
        NOME='GX'//NOME_N//'.dat'
        call GRAVACAO_S2D(ni,mj,x,y,gamma_x,NOME,t)
        MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_PD; fi;')

    end do

end program main
