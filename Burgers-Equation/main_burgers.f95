program main
    implicit none
    
    real(kind=10), parameter :: x0 = 5.0
    real(kind=10), parameter :: xf = 25.0
    real(kind=10), parameter :: y0 = 5.0
    real(kind=10), parameter :: yf = 25.0
    real(kind=10), parameter :: t0 = 0.0
    real(kind=10), parameter :: tf = 5.0

    real(kind=10), parameter :: x1 = 10.0
    real(kind=10), parameter :: x2 = 20.0

    real(kind=10), parameter :: y1 = 10.0
    real(kind=10), parameter :: y2 = 20.0

    integer, parameter :: ni = 100
    integer, parameter :: mj = 100
    integer, parameter :: tt = 200

    real, parameter    :: pi = acos(-1.0)

    real, parameter    :: tol_erro = 0.0001

    real(kind=10), dimension(ni+1,mj+1) :: gamma_x, ANT_gamma_x, IT_gamma_x, gamma_y, ANT_gamma_y, IT_gamma_y
    real(kind=10) :: media_xe, media_xw, media_yn, media_ys, nu, dx, dy, dt, CP, CE, CW, CN, CS, t, erro_x, erro_y
    integer :: Se, Sw, Sn, Ss, i, j, it, k, MANIPULA
    real(kind=10), dimension(ni+1) :: x
    real(kind=10), dimension(mj+1) :: y
    character(len=30) :: NOME
    character(6)      :: NOME_N, NOME_M

    nu = 5
     
    dx = (xf-x0)/(ni-1)
    dy = (yf-y0)/(mj-1)
    dt = (tf-t0)/(tt-1)

    
    do j = 1, mj+1
        do i = 1, ni+1
            
            !MALHA

            x(i) = dx*(i-1) + x0
            y(j) = dy*(j-1) + y0

            !CONDIÇÃO INICIAL E DE CONTORNO GAMMA X E GAMMA Y

            if ( (x(i)>=x1 .and. x(i)<=x2) .and. (y(j)>=y1 .and. y(j)<=y2)) then
                ! gamma_x(i,j) = 5
                ! gamma_y(i,j) = 20

                gamma_x(i,j) = 12*sin( (x(i)-10)*(pi/10) )
                gamma_y(i,j) = 12*sin( (y(j)-10)*(pi/10) )
                

            else
                gamma_x(i,j) = 0
                gamma_y(i,j) = 0

            end if

            gamma_x(i,1) = 0
            gamma_x(i,mj+1) = 0
    
            gamma_x(1,j) = 0
            gamma_x(ni+1,j) = 0


            gamma_y(i,1) = 0
            gamma_y(i,mj+1) = 0
    
            gamma_y(1,j) = 0
            gamma_y(ni+1,j) = 0
            
        end do
    end do

    k = 0.0
    t = t0


    WRITE(NOME_N,'(I0.6)') k
    NOME='GX'//NOME_N//'.dat'
    call GRAVACAO_S2D(ni,mj,x,y,gamma_x,NOME,t)
    MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_GX; fi;')

    WRITE(NOME_M,'(I0.6)') k
    NOME='GY'//NOME_M//'.dat'
    call GRAVACAO_S2D(ni,mj,x,y,gamma_y,NOME,t)
    MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_GY; fi;')

    IT_gamma_x  = gamma_x
    ANT_gamma_x = gamma_x

    IT_gamma_y  = gamma_y
    ANT_gamma_y = gamma_y


    do k = 1, tt

        it = 0

        do while(it <= 5000)

            do i = 2, ni
                do j = 2, mj
                    
                    media_xe = (ANT_gamma_x(i,j) + ANT_gamma_x(i+1,j))/2
                    media_xw = (ANT_gamma_x(i,j) + ANT_gamma_x(i-1,j))/2
                    media_yn = (ANT_gamma_y(i,j) + ANT_gamma_y(i,j+1))/2
                    media_ys = (ANT_gamma_y(i,j) + ANT_gamma_y(i,j-1))/2

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

                    CP = 1/dt + (1/dx) * (media_xe * (1+Se)/2 - media_xw * (1-Sw)/2) + (1/dy) * (media_yn * (1+Sn)/2 - &
                    & media_ys * (1-Ss)/2) + (2*nu)/(dx**2) + (2*nu)/(dy**2)  
                    
                    CE = 1/dx * media_xe * (1-Se)/2 - nu/(dx**2)
                    CW = 1/dx * media_xw * (1-Sw)/2 + nu/(dx**2)
                    CN = 1/dy * media_yn * (1-Sn)/2 - nu/(dy**2)
                    CS = 1/dy * media_ys * (1-Ss)/2 + nu/(dy**2)

                    gamma_x(i,j) = ((1/dt)*ANT_gamma_x(i,j) - CE* IT_gamma_x(i+1,j) + CW* gamma_x(i-1,j) - &
                    CN* IT_gamma_x(i,j+1) + CS* gamma_x(i,j-1))/CP
                    
                    gamma_y(i,j) = ((1/dt)*ANT_gamma_y(i,j) - CE* IT_gamma_y(i+1,j) + CW* gamma_y(i-1,j) - &
                    CN* IT_gamma_y(i,j+1) + CS* gamma_y(i,j-1))/CP
                    
                end do
            end do

            it = it+1

            erro_x = norm2(gamma_x - IT_gamma_x)/norm2(gamma_x)
            erro_y = norm2(gamma_y - IT_gamma_y)/norm2(gamma_y)

            write(*,'(A,I10.0)') 'atual:', it
            write(*,'(A,F10.8)') 'Erro gamma_x:', erro_x 
            write(*,'(A,F10.8)') 'Erro gamma_y:', erro_y 
            write(*,'(A)') '-------------------------------------------------'

            if ( erro_x < tol_erro .and. erro_y < tol_erro ) then
                write(*,'(A)') '======================================================'
                write(*,'(A,I10.0)') 'tempo:', k
                write(*,'(A,F10.8)') 'Erro gamma_x:', erro_x 
                write(*,'(A,F10.8)') 'Erro gamma_y:', erro_y 
                write(*,'(A,I10.0)') 'Iteração maxima:', it
                write(*,'(A)') '======================================================'
                exit
            end if

            IT_gamma_x = gamma_x
            IT_gamma_y = gamma_y
        
        end do

        ANT_gamma_x = gamma_x
        ANT_gamma_y = gamma_y

        t = t + dt

        WRITE(NOME_N,'(I0.6)') k
        NOME='GX'//NOME_N//'.dat'
        call GRAVACAO_S2D(ni,mj,x,y,gamma_x,NOME,t)
        MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_GX; fi;')

        WRITE(NOME_M,'(I0.6)') k
        NOME='GY'//NOME_M//'.dat'
        call GRAVACAO_S2D(ni,mj,x,y,gamma_y,NOME,t)
        MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_GY; fi;')

    end do

end program main
