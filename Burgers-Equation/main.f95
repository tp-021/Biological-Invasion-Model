program main
    implicit none

    ! Recebe os valores de todos os parâmetros e inicia as variávies. É possível decidir quais processos queremos realizar: solução
    ! numérica, exata e estacionária

    integer :: n_max!Número de testes para o espaço(cada teste divide por 2 delta a seguir:)
    integer  :: m_max !Número de testes para o tempo(cada teste divide por 2 delta a seguir:)
    !OBS: NÃO VARIAR OS DOIS AO MESMO TEMPO
    
    real :: dx, dy, dt, dx_inicial, dy_inicial, dt_inicial

    integer :: ni,mj,tk

    ! Informações sobre o domínio

    real(kind=10) :: x0
    real(kind=10) :: xf 

    real(kind=10) :: y0 
    real(kind=10) :: yf 

    real(kind=10) :: t0 
    real(kind=10) :: tf 

    ! Outros parâmetros

    real(kind=10) :: nu 
    
    real(kind=10) :: tol_erro 
    ! real(kind=10) :: tol_est 
    integer :: it_max

    integer :: frame  !A cada frame, o resultado é gravado

    ! Exemplo que será calculado

    character(3) :: exemplo, modo  ! '1': pagina 61; '2': pagina 65 (dissertação da Cláudia)
    
    
    ! Variáveis locais

    character(18) :: parametros
    character(2) :: nome

    integer :: teste, n, m, MANIPULA

    character(19) :: pasta
    character(4) :: nome_p
    character(4) :: nome_q

    real :: start_time, end_time, tempo_total, start, end, time_teste

    call cpu_time(start_time)

    MANIPULA = SYSTEM('if test -d SAI_AUTO; then rm -rf SAI_AUTO; fi;')

    do teste = 5,5

        write(*,'(I2)') teste

        MANIPULA = SYSTEM('mkdir -p SAI_AUTO')

        write(nome,'(I0.2)') teste
        parametros = 'Parametros_'//trim(nome)//'.dat'

        write(*,'(A17)') parametros

        open(unit=1,file=parametros,status='old')

            read(1,*) !Domínio

            read(1,*) !x_inicial x_final
            read(1,*) x0, xf

            read(1,*) !x_inicial x_final
            read(1,*) y0, yf

            read(1,*) !x_inicial x_final
            read(1,*) t0, tf

            ! ............... !
            read(1,*) !Malha

            read(1,*) !dx dy dt
            read(1,*) dx_inicial, dy_inicial, dt_inicial

            read(1,*) !n_max
            read(1,*) n_max

            read(1,*) !m_max
            read(1,*) m_max

            ! ............... !
            read(1,*) !Coeficiente nu
            read(1,*) nu

            ! ............... !
            read(1,*) !Simulação
            read(1,*) !tol_erro
            read(1,*) tol_erro
            read(1,*) !it_max
            read(1,*) it_max
            read(1,*) !it_max
            read(1,*) exemplo
            read(1,*) !Frames
            read(1,*) frame
            read(1,*) !Modo
            read(1,*) modo

        close(1)

        do n = 1, n_max
            do m = 1, m_max
                
                call cpu_time(start)

                ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
                ! Determina as partições atuais dos domínios

                dx = dx_inicial / ( 2 ** (n-1) )
                dy = dy_inicial / ( 2 ** (n-1) )
                dt = dt_inicial / ( 2 ** (m-1) )

                ni = int(nint( (xf - x0)/dx +1 )) 
                mj = int(nint( (yf - y0)/dy +1 )) 
                tk = int(nint( (tf - t0)/dt +1 )) 

                ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
                ! PREPARAÇÃO DA SAIDA
                ! Criando as pastas para receberem os resultados

                write(nome_p,'(I0.4)') ni
                write(nome_q,'(I0.4)') tk
                pasta = 'SAIDA_TK' // trim(adjustl(nome_q)) //'_NI'// trim(adjustl(nome_p))

                MANIPULA = SYSTEM('mkdir -p SAI_AUTO/' //trim(pasta))
                MANIPULA = SYSTEM('mkdir -p SAI_AUTO/' //trim(pasta)// '/SAIDA_GX')
                MANIPULA = SYSTEM('mkdir -p SAI_AUTO/' //trim(pasta)// '/SAIDA_GY')
                MANIPULA = SYSTEM('mkdir -p SAI_AUTO/' //trim(pasta)// '/SAIDA_EXATA_GX')
                MANIPULA = SYSTEM('mkdir -p SAI_AUTO/' //trim(pasta)// '/SAIDA_EXATA_GY')

                open(unit=5,file='Testes.dat', status='old', position='append')
                write(5,*) '............................................'
                write(5,*) 'Teste: ', teste , ni , mj , tk
                write(5,*) '............................................'
                close(5)

                ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
                write(*,*) modo
                if (modo == 'exa') then
                    call burgers(x0,xf,y0,yf,t0,tf,dx,dy,dt,ni,mj,tk,nu,tol_erro,it_max,exemplo,frame,pasta,parametros)         
                elseif (modo == 'est') then
                    call est_burgers(x0,xf,y0,yf,dx,dy,ni,mj,nu,tol_erro,it_max,exemplo,frame,pasta)
                else 
                    write(*,*) 'Modo inválido'
                endif
                    
                call cpu_time(end)
                time_teste = end - start

                open(unit=5,file='Testes.dat', status='old', position='append')
                write(5,*) 'Tempo de execução (laço n/m): ', time_teste 
                write(5,*)
                close(5)

            end do     
        end do

        MANIPULA = SYSTEM('if test -e'//trim(parametros)//'; then mv '//trim(parametros)//' SAI_AUTO; fi;')
        MANIPULA = SYSTEM('if test -d SAI_AUTO'//trim(nome)//'; then rm -rf SAI_AUTO'//trim(nome)//'; fi;')
        MANIPULA = SYSTEM('if test -d SAI_AUTO; then mv SAI_AUTO SAI_AUTO_'//trim(nome)//'; fi;')

    end do

    call cpu_time(end_time)
    tempo_total = end_time - start_time

    open(unit=5,file='Testes.dat', status='old', position='append')
    write(5,*) 'Tempo total de execução: ', tempo_total 
    write(5,*) '===================================='
    close(5)

end program main