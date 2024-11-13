import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib.ticker as ticker
import matplotlib.colors as colors
from colormaps_telegrafica import get_densidade

cmap_densidade = get_densidade()

# Configurações iniciais do plot (Cria um figura, cria um subplot, define os limites de cada axis)
fig = plt.figure(figsize=(10, 8)) 
ax = fig.add_subplot(111)
# ax.set_xlim([-10, 1290])
# ax.set_ylim([-10, 970])
# ax.set_zlim([0, 1.2])

cbar = None

# Função que atualiza o plot (Será aplicado em um for)
def update_plot(i):
    
    global cbar
    
    #Limpa o plot anterior
    ax.clear()

    # Carrega dados para a iteração atual
    file_path = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_PD/S2D{i:06d}.dat'
    
    # file_path = f'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\SAI_PD\\S2D{i:06d}.dat'
    
    # file_path = f'C:\\Users\\Talita\\Desktop\\resultados 2407\\SAI_PD gx01 gy9\\S2D{i:06d}.dat'
    # file_path = f'C:\\Users\\Talita\\Desktop\\resultados 2407\\SAI_PD gx9 gy01\\S2D{i:06d}.dat'
    # file_path = f'C:\\Users\\Talita\\Desktop\\resultados 2407\\SAI_PD gxgy 01\\S2D{i:06d}.dat'
    # file_path = f'C:\\Users\\Talita\\Desktop\\resultados 2407\\SAI_AUTO\\G1-1.00K1-1.00\\SAI_PD\\S2D{i:06d}.dat'
    # file_path = f'C:\\Users\\Talita\\Desktop\\resultados 2407\\SAI_AUTO\\G1-3.00K1-1.00\\SAI_PD\\S2D{i:06d}.dat'
    # file_path = f'C:\\Users\\Talita\\Desktop\\resultados 2407\\SAI_AUTO\\G1-9.00K1-1.00\\SAI_PD\\S2D{i:06d}.dat'
       
    

    x, y, s = np.loadtxt(file_path, unpack=True)

    X = np.transpose(np.reshape(x,(201,201)))
    Y = np.transpose(np.reshape(y,(201,201)))
    S = np.transpose(np.reshape(s,(201,201)))
    
    # Gera o grafico (subplot)
    contour = ax.contourf(X, Y, S, cmap=cmap_densidade, levels=np.linspace(-2, 10, 100), norm = colors.SymLogNorm(linthresh=0.04,linscale=1,vmin=-10, vmax=10) )

    if cbar is None:
        cbar = fig.colorbar(contour, ax=ax, orientation='vertical')
        cbar.set_ticks([-2,0,2,4,6,8,10])
        
    # Ajusta as legendas e as fontes para este subplot
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_title(f'gamma_x{i:06d} ')
    ax.tick_params(axis='both', which='major', labelsize=8)

    # Pausa para visualização
    plt.pause(0.001)

# Loop que altera o valor de i, atualizando o gráfico
for i in range(0,192):  
    update_plot(i)
    

# Mantém a janela do gráfico aberta
plt.show()
