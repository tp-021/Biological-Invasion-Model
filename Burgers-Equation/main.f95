program main
    implicit none
    
    real, parameter :: x0 = 0.0
    real, parameter :: xf = 10.0
    real, parameter :: y0 = 0.0
    real, parameter :: yf = 10.0
    real, parameter :: t0 = 0.0
    real, parameter :: tf = 20.0

    integer, parameter :: ni = 20
    integer, parameter :: mj = 20
    integer, parameter :: k = 10

    real, dimension(:,:), :: gamma_x, ANT_gamma_X
    real :: media_e, media_w, media_n, media_s, nu, dx, dy, dt
    integer :: Se, Sw, Sn, Ss, i, j

    nu = 1
    
    dx = (x0-xf) /ni
    dy = (y0-yf) /mj
    dt = (t0-tf) /k

    do i = 1, ni
        do j = 1, mj
            
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

            CP = 1/dt + 1/dx{media_e * {1+Se}/2 - media_w * {1-Sw}/2} + 1/dy{media_n * {1+Sn}/2 - media_s * {1-Ss}/2} + & 
            & {2*nu}/{dx**2} + {2*nu}/{dy**2}  
            
            CE = 1/dx * media_e * {1-Se}/2 - nu/{dx**2}
            CW = 1/dx * media_w * {1-Sw}/2 - nu/{dx**2}
            CN = 1/dy * media_n * {1-Sn}/2 - nu/{dy**2}
            CS = 1/dy * media_s * {1-Ss}/2 - nu/{dy**2}

            gamma_x (i,j) = {1/Cp}*{Bp - CE* ANT_gamma_x(i+1,j) + CW* gamma_x(i-1,j) - CN* ANT_gamma_x(i,j+1) + CS* gamma_x(i,j-1)

        end do
    end do
 
end program main
