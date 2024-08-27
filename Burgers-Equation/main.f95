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

                
                m_ue = [u(i-2,j)+u(i,j)]/2

                if ( m_ue >= 0) then
                    Se = 1
                else
                    Se = -1  
                end if

                m_uw = [u(i,j)+u(i+2),j]/2

                if ( m_uw >= 0) then
                    Sw = 1
                else
                    Sw = -1  
                end if

                m_vne = [v(i,j+2)+v(i,j)]/2

                if ( m_vne >= 0) then
                    Sn = 1
                else
                    Sn = -1  
                end if

                m_use = [v(i,j)+v(i,j-2)]/2

                if ( m_vse >= 0) then
                    Ss = 1
                else
                    Ss = -1  
                end if 


                C_u = {m_ue * [(1+Re)/2*u(i,j) + (1-Re)/2*e(i+2,j)] - m_up * [(1+Rp)/2*u(i-2,j) + (1-Rp)/2*e(i,j)]}/dx + &
                      {m_vne * [(1+Sn)/2*u(i,j) + (1-Sn)/2*u(i,j+2)] - m_vse * [(1+Ss)/2*u(i,j-2) + (1-Ss)/2*u(i,j)]}/dy

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