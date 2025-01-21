subroutine burgers(x0,xf,y0,yf,t0,tf,dx,dy,dt,ni,mj,tk,nu,tol_erro,it_max,exemplo,frame,pasta,parametros)
   
    implicit none

    ! Informações sobre o domínio

    real(kind=10), intent(in) :: x0
    real(kind=10), intent(in) :: xf

    real(kind=10),intent(in) :: y0 
    real(kind=10),intent(in) :: yf 

    real(kind=10),intent(in) :: t0 
    real(kind=10),intent(in) :: tf 

    real,intent(in) :: dx, dy, dt
    integer, intent(in) :: ni, mj, tk

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

    real(kind=10), dimension(ni+1,mj+1) :: gamma_x, ANT_gamma_x, IT_gamma_x, exata_gamma_x
    real(kind=10), dimension(ni+1,mj+1) :: gamma_y, ANT_gamma_y, IT_gamma_y, exata_gamma_y
    real(kind=10), dimension(ni+1) :: x
    real(kind=10), dimension(mj+1) :: y
    real(kind=10), dimension(tk+1) :: t

    real(kind=10) :: media_xe, media_xw, media_yn, media_ys, CP, CE, CW, CN, CS, tempo
    
    integer :: Se, Sw, Sn, Ss, i, j, it, k

    ! Variáveis para cálculo do erro

    real(kind=10) :: erro_exato_gx, erro_exato_gy, dif_ant_gx, dif_ant_gy, erro_x, erro_y, alpha
    character(30) :: A

    ! Variáveis auxiliáres para gravação 

    character(len=30) :: NOME
    character(6)      :: NOME_M
    integer :: MANIPULA

    real :: start_time, end_time, elapsed_time

    ! ............................................................................................................................ ! 
    ! Variáveis para calculo da solu estacionária

    ! real(kind=10), dimension(max_ni+1,max_mj+1) :: est_gamma_x, est_gamma_y 
    
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
    call cpu_time(start_time) 
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
    ! PREPARAÇÃO DA SAIDA
    ! Criando as pastas para receberem os resultados

    open(unit=2,file='erro_exato.dat', status='replace')
    write(2,'(A15,2X,A15,2X,A15,2X,A15,2X,A15,2X,A15,2X,A15)') '#k', 'Tempo', 'Erro GX', 'Erro GY','Erro ANT_GX', &
    'Erro ANT_GY', 'Alpha'
    close(2)

    open(unit=3, file='erro_it.dat', status='replace')
    write(3,'(A10,2X,A15,2X,A15)') '#it', 'erro_x', 'erro_y' 
    close(3)

    ! ======================================================================================================================== !
    ! Iniciação das variáveis 

    gamma_x = 0.0; ANT_gamma_x = 0.0; IT_gamma_x = 0.0; exata_gamma_x = 0.0
    gamma_y = 0.0; ANT_gamma_y = 0.0; IT_gamma_y = 0.0; exata_gamma_y = 0.0
    x = 0.0; y = 0.0; t = 0.0

    ! ======================================================================================================================== !
    ! CRIAÇÃO DA MALHA

    ! Valores iniciais e finais 

    x(1) = x0     !x1
    x(ni+1) = xf  !xni

    y(1) = y0
    y(mj+1) = yf

    t(1) = t0
    t(tk+1) = tf

    ! Posição dos outros pontos (malha deslocada)
    do i = 2, ni
        x(i) =  x0 + dx * (i - (3.0/2.0))
    end do 

    do j = 2, mj
        y(j) =  y0 + dy * (j - (3.0/2.0))
    end do 

    do k = 2, tk
        t(k) =  t0 + dt * (k - 1)
    end do 
    
    ! ======================================================================================================================== !
    ! CALCULO DE GAMMA NO ESTADO ESTACIONÁRIO

    ! est_gamma_x = 0.0; est_gamma_y = 0.0

    ! call est_burgers(x,y,dx,dy,ni,mj,nu,pi,tol_erro,pasta,est_gamma_x,est_gamma_y)

    ! MANIPULA = SYSTEM('if test -e est_gamma.dat; then mv est_gamma.dat SAI_AUTO/'//trim(pasta)//'; fi;')

    ! ======================================================================================================================== !
    ! CONDIÇÃO INICIAL

    k = 1
    tempo = t(1)

    do i = 1, ni+1
        do j = 1, mj+1

            select case (exemplo)

                case('1')
                !--------------------------------!
                ! Dissertação claudia página 61
            
                    gamma_x(i,j) = ( -4.0*nu*pi * cos(2.0*pi*x(i)) * sin(pi*y(j)) ) / ( 2.0 + sin(2.0*pi*x(i)) * sin(pi*y(j)) )
                    gamma_y(i,j) = ( -2.0*nu*pi * sin(2.0*pi*x(i)) * cos(pi*y(j)) ) / ( 2.0 + sin(2.0*pi*x(i)) * sin(pi*y(j)) )

                case('2')
                !--------------------------------!
                ! Dissertação claudia página 65

                    gamma_x(i,j) = 3.0/4.0 - 1.0/( 4* (1 + exp( (4.0*y(j)-4.0*x(i)) / (32.0*nu) ) ) )
                    gamma_y(i,j) = 3.0/4.0 + 1.0/( 4* (1 + exp( (4.0*y(j)-4.0*x(i)) / (32.0*nu) ) ) )

            end select

        end do
    end do 

    exata_gamma_x = gamma_x
    exata_gamma_y = gamma_y

    ANT_gamma_x = gamma_x
    ANT_gamma_y = gamma_y


    ! Gravando a condição inicial

    WRITE(NOME_M,'(I0.6)') k


    NOME='EGX'//NOME_M//'.dat'
    call GRAVACAO_S2D(ni,mj,x,y,exata_gamma_x,NOME,tempo)
    ! MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_EXATAS_GX; fi;')
    MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_AUTO/'//trim(pasta)//'/SAIDA_EXATA_GX; fi;')

    NOME='EGY'//NOME_M//'.dat'
    call GRAVACAO_S2D(ni,mj,x,y,exata_gamma_y,NOME,tempo)
    ! MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_EXATAS_GY; fi;')
    MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_AUTO/'//trim(pasta)//'/SAIDA_EXATA_GY; fi;')

    NOME='GX'//NOME_M//'.dat'
    call GRAVACAO_S2D(ni,mj,x,y,gamma_x,NOME,tempo)
    ! MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_GX; fi;')
    MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_AUTO/'//trim(pasta)//'/SAIDA_GX; fi;')

    NOME='GY'//NOME_M//'.dat'
    call GRAVACAO_S2D(ni,mj,x,y,gamma_y,NOME,tempo)
    ! MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_GY; fi;')
    MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_AUTO/'//trim(pasta)//'/SAIDA_GY; fi;')

    ! ======================================================================================================================== !
    ! LAÇO DO TEMPO PARA CÁLCULO DAS SOLUÇÕES
    ! ======================================================================================================================== !


    do k = 2, tk+1
        write(*,'(I6.0)') k

        tempo = t(k)

        it = 1
        
        CE = 0; CW = 0; CN = 0; CS = 0; CP = 0
        gamma_x = 0.0; gamma_y = 0.0; IT_gamma_x = 0.0; IT_gamma_y = 0.0

        ! .................................................................................................................... !
        ! Atualização do tempo no arquivo de erro_it

        open(unit=3, file='erro_it.dat', status='old', position='append')
        write(3,'(A10,F15.8)') '#Tempo = ', tempo
        close(3)

        ! .................................................................................................................... !
        ! Condição de contorno

        select case (exemplo)

            case('1')
            !--------------------------------!
            ! Dissertação claudia página 61

            do i = 1, ni+1

                gamma_x(i,mj+1) = 0
                gamma_x(i,1)    = 0
                
                gamma_y(i,1)    = -nu*pi * exp(-5*(pi**2)*nu*t(k)) * sin(2*pi*x(i))
                gamma_y(i,mj+1) =  nu*pi * exp(-5*(pi**2)*nu*t(k)) * sin(2*pi*x(i))
                
            end do ! Condição de contorno j

            do j = 1, mj+1

                gamma_x(1,j)    = -2*nu*pi * exp(-5*(pi**2)*nu*t(k)) * sin(pi*y(j))
                gamma_x(ni+1,j) = -2*nu*pi * exp(-5*(pi**2)*nu*t(k)) * sin(pi*y(j))
            
                gamma_y(1,j)    = 0
                gamma_y(ni+1,j) = 0
                
            end do ! Condição de contorno j

            case('2')
            !--------------------------------!
            ! Dissertação claudia página 65

                do i = 1, ni+1

                    gamma_x(i,1)    = 3.0/4.0 - 1.0/( 4.0* (1.0 + exp(    (-4.0*x(i)-t(k)) / (32.0*nu) ) ) )
                    gamma_x(i,mj+1) = 3.0/4.0 - 1.0/( 4.0* (1.0 + exp( (4.0-4.0*x(i)-t(k)) / (32.0*nu) ) ) )
                
                    gamma_y(i,1)    = 3.0/4.0 + 1.0/( 4.0* (1.0 + exp(    (-4.0*x(i)-t(k)) / (32.0*nu) ) ) )
                    gamma_y(i,mj+1) = 3.0/4.0 + 1.0/( 4.0* (1.0 + exp( (4.0-4.0*x(i)-t(k)) / (32.0*nu) ) ) )
                    
                end do ! Condição de contorno j

                do j = 1, mj+1

                    gamma_x(1,j)    = 3.0/4.0 - 1.0/( 4.0* (1.0 + exp(     (4.0*y(j)-t(k)) / (32.0*nu) ) ) )
                    gamma_x(ni+1,j) = 3.0/4.0 - 1.0/( 4.0* (1.0 + exp( (4.0*y(j)-4.0-t(k)) / (32.0*nu) ) ) )

                    gamma_y(1,j)    = 3.0/4.0 + 1.0/( 4.0* (1.0 + exp(     (4.0*y(j)-t(k)) / (32.0*nu) ) ) )
                    gamma_y(ni+1,j) = 3.0/4.0 + 1.0/( 4.0* (1.0 + exp( (4.0*y(j)-4.0-t(k)) / (32.0*nu) ) ) )
                    
                end do ! Condição de contorno j

        end select

        exata_gamma_x = gamma_x
        exata_gamma_y = gamma_y

        ! -------------------------------------------------------------------------------------------------------------------- !
        ! SOLUÇÃO ANALÍTICA
        ! -------------------------------------------------------------------------------------------------------------------- !
        
        do i = 2, ni
            do j = 2, mj

                select case (exemplo)

                case('1')
                !--------------------------------!
                ! Dissertação claudia página 61
                
                exata_gamma_x (i,j) = -2.0*nu * ( ( 2.0*pi * exp(-5.0*(pi**2.0)*nu*t(k)) * cos(2.0*pi*x(i)) * sin(pi*y(j)) ) / &
                                                ( 2.0    + exp(-5.0*(pi**2.0)*nu*t(k)) * sin(2.0*pi*x(i)) * sin(pi*y(j)) ) )

                exata_gamma_y (i,j) = -2.0*nu * ( ( pi  * exp(-5.0*(pi**2.0)*nu*t(k)) * sin(2.0*pi*x(i)) * cos(pi*y(j)) ) / &
                                                ( 2.0 + exp(-5.0*(pi**2.0)*nu*t(k)) * sin(2.0*pi*x(i)) * sin(pi*y(j)) ) )
            
                case('2')
                !--------------------------------!
                ! Dissertação claudia página 65

                exata_gamma_x (i,j) = 3.0/4.0 - 1.0/( 4.0 * (1.0 + exp( (4.0*y(j)-4.0*x(i)-t(k)) / (32.0*nu) ) ) )

                exata_gamma_y (i,j) = 3.0/4.0 + 1.0/( 4.0 * (1.0 + exp( (4.0*y(j)-4.0*x(i)-t(k)) / (32.0*nu) ) ) )

                end select

            end do ! Sol analítica
        end do ! Sol analítica

        ! .................................................................................................................... !
        ! Gravação da solução analítica

        if (mod(k,frame) == 0 .or. k == tk+1)  then

            WRITE(NOME_M,'(I0.6)') k

            NOME='EGX'//NOME_M//'.dat'
            call GRAVACAO_S2D(ni,mj,x,y,exata_gamma_x,NOME,tempo)
            ! MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_EXATAS_GX; fi;')
            MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_AUTO/'//trim(pasta)//'/SAIDA_EXATA_GX; fi;')

            NOME='EGY'//NOME_M//'.dat'
            call GRAVACAO_S2D(ni,mj,x,y,exata_gamma_y,NOME,tempo)
            ! MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_EXATAS_GY; fi;')
            MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_AUTO/'//trim(pasta)//'/SAIDA_EXATA_GY; fi;')

        end if  

        ! -------------------------------------------------------------------------------------------------------------------- !
        ! CÁLCULO DA SOLUÇÃO NUMÉRICA
        ! -------------------------------------------------------------------------------------------------------------------- !

        do while(it <= it_max)

            !Cálculo dos coeficientes

            do i = 2, ni
                do j = 2, mj 

                    ! media_xe = (ANT_gamma_x(i,j) + ANT_gamma_x(i+1,j))/2
                    ! media_xw = (ANT_gamma_x(i,j) + ANT_gamma_x(i-1,j))/2
                    ! media_yn = (ANT_gamma_y(i,j) + ANT_gamma_y(i,j+1))/2
                    ! media_ys = (ANT_gamma_y(i,j) + ANT_gamma_y(i,j-1))/2

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

                    CP = 1/dt + (1/dx) * (media_xe * (1+Se)/2 - media_xw * (1-Sw)/2) + (1/dy) * (media_yn * (1+Sn)/2 - &
                    media_ys * (1-Ss)/2) + (2*nu)/(dx**2) + (2*nu)/(dy**2)  

                    CE =   1/dx * media_xe * (1-Se)/2 - nu/(dx**2)
                    CW = - 1/dx * media_xw * (1+Sw)/2 - nu/(dx**2)
                    CN =   1/dy * media_yn * (1-Sn)/2 - nu/(dy**2)                                                                                      
                    CS = - 1/dy * media_ys * (1+Ss)/2 - nu/(dy**2)

                    ! Cálcula a função gamma no ponto atual

                    gamma_y(i,j) = ((1/dt)*ANT_gamma_y(i,j) - CE* IT_gamma_y(i+1,j) - CW* gamma_y(i-1,j) - &
                    CN* IT_gamma_y(i,j+1) - CS* gamma_y(i,j-1))/CP

                    gamma_x(i,j) = ((1/dt)*ANT_gamma_x(i,j) - CE* IT_gamma_x(i+1,j) - CW* gamma_x(i-1,j) - &
                    CN* IT_gamma_x(i,j+1) - CS* gamma_x(i,j-1))/CP
                

                end do ! Sol numérica
            end do ! Sol numérica

            ! ......................................... !
            ! Erro relativo para sair do laço iterativo

            erro_x = norm2(gamma_x - IT_gamma_x)/norm2(gamma_x)
            erro_y = norm2(gamma_y - IT_gamma_y)/norm2(gamma_y)
            
            open(unit=3, file='erro_it.dat', status='old', position='append')
            write(3,'(I10,2X,F15.8,2X,F15.8)') it, erro_x, erro_y
            close(3)

            if ( erro_x < tol_erro .and. erro_y < tol_erro ) then
                exit
            end if

            ! Verificação do critério de convergência 

            alpha =  abs(CP) - abs(CE) - abs(CW) - abs(CN) - abs(CS)  

            if ( alpha <= 0 ) then
                A = 'Alpha negativo encontrado' 
            else
                A = 'ok'
            end if

                ! ......................................... !
            ! Atualiza valores para a próxima iteração

            IT_gamma_x = gamma_x
            IT_gamma_y = gamma_y

            it = it + 1
            
        end do ! Laço iterativo while
    
        ! Calcula do erro (comparação com a ANALÍTICA)

        erro_exato_gx = norm2(exata_gamma_x - gamma_x)/norm2(exata_gamma_x)
        erro_exato_gy = norm2(exata_gamma_y - gamma_y)/norm2(exata_gamma_y)

        ! Calcula do erro (comparação com o TEMPO ANTERIOR)

        dif_ant_gx = norm2(ANT_gamma_x - gamma_x)/norm2(ANT_gamma_x)
        dif_ant_gy = norm2(ANT_gamma_y - gamma_y)/norm2(ANT_gamma_y)
        
        ! Gravação do erro absoluto e do critério de convergência

        open(unit=2, file='erro_exato.dat', status='old', position='append')
        write(2,'(I15.0,2X,F15.6,2X,F15.8,2X,F15.8,2X,F15.8,2X,F15.8,2X,F15.8)') k, tempo, erro_exato_gx, erro_exato_gy, & 
        dif_ant_gx, dif_ant_gy, alpha
        close(2)

        

        ! ......................................... !
        ! Saída do laço temporal

        if ( dif_ant_gx < tol_est .and. dif_ant_gy < tol_est ) then
            exit
        end if


        ! ......................................... !
        ! Gravação da solução numérica

        if (mod(k,frame) == 0 .or. k == tk+1)  then

            WRITE(NOME_M,'(I0.6)') k
            NOME='GX'//NOME_M//'.dat'
            call GRAVACAO_S2D(ni,mj,x,y,gamma_x,NOME,tempo)
            ! MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_GX; fi;')
            MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_AUTO/'//trim(pasta)//'/SAIDA_GX; fi;')


            WRITE(NOME_M,'(I0.6)') k
            NOME='GY'//NOME_M//'.dat'
            call GRAVACAO_S2D(ni,mj,x,y,gamma_y,NOME,tempo)
            ! MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAIDA_GY; fi;')
            MANIPULA = SYSTEM('if test -e '//NOME//'; then mv '//NOME//' SAI_AUTO/'//trim(pasta)//'/SAIDA_GY; fi;')

        end if 

        ! Atualização do gamma anterior

        ANT_gamma_x = gamma_x
        ANT_gamma_y = gamma_y

        
    end do ! Laço temporal
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
    ! Gravação de informações extras e mudança da localização dos arquivos restantes
    call cpu_time(end_time)
    elapsed_time = end_time - start_time

    open(unit=1,file=parametros, status='old', position='append')
    write(1,'(A30)') A
    write(1,*) 'Tempo de execução: ', elapsed_time
    close(1)

    open(unit=1,file='Testes.dat', status='old', position='append')
    write(1,*) 'Tempo de execução (burgers.f95): ', elapsed_time
    close(1)

    MANIPULA = SYSTEM('if test -e erro_exato.dat; then mv erro_exato.dat SAI_AUTO/'//trim(pasta)//'; fi;')

    MANIPULA = SYSTEM('if test -e erro_it.dat; then mv erro_it.dat SAI_AUTO/'//trim(pasta)//'; fi;')

    ! MANIPULA = SYSTEM('if test -e Parametros.dat; then mv Parametros.dat SAI_AUTO/'//trim(pasta)//'; fi;')


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
    
end subroutine burgers