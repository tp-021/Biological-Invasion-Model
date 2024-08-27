program main
    implicit none
    
    ! Declaração de variáveis
    ! Gerais
    integer :: end
    
    ! Locais
    integer :: i,j

    ! Loop temporal
    do t = start, endt
        
        u_ant = u
        v_ant = v

        ! Loop x,y
        do i = start, end
            do j = start, end
                
                ! Termo convectivo C_u
                ! médias
                ! m_u (i-1,j) = [u(i-2,j)+u(i)]/2
                ! m_u (i+1,j) = [u(i,j)+u(i+2)]/2

                
                m_ur = [u(i-2,j)+u(i)]/2
                m_ul = [u(i,j)+u(i+2)]/2

                if ( m_ur >= 0 ) then
                    S = 1
                else
                    S = -1  
                end if

                C_u =

                ! Termo convectivo C_v
                C_v =

                ! Termo difusivo
                V_u = [u(i-2,j) - 2*u(i,j) + u(i+2,j)]/(dx^2) + [u(i,j-2) - 2*u(i,j) + u(i,j+2)]/(dy^2)
                V_v = [v(i-2,j) - 2*v(i,j) + v(i+2,j)]/(dx^2) + [v(i,j-2) - 2*u(i,j) + v(i,j+2)]/(dy^2)

                ! Solução numérica
                u(i,j) = [V_u - C_u + u_ant(i,j)]
                v(i,j) = [V_v - C_v + v_ant(i,j)]
            end do
            
        end do
    end do
end program main