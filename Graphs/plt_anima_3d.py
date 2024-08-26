import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

# Configurações iniciais do plot (Cria um figura, cria um subplot, define os limites de cada axis)
fig = plt.figure(figsize=(10, 8)) 
ax = fig.add_subplot(111, projection='3d')
# ax.set_xlim([-10, 1290])
# ax.set_ylim([-10, 970])
# ax.set_zlim([0, 1.2])

# Função que atualiza o plot (Será aplicado em um for)
def update_plot(i):
    
    #Limpa o plot anterior
    ax.clear()

    # Carrega dados para a iteração atual
    file_path = f'/home/pgmac/Desktop/Talita/Telegrafica/SAI_PD/S2D{i:06d}.dat'
    # file_path = f'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D{i:06d}.dat'

    x, y, s = np.loadtxt(file_path, unpack=True)

    # Gera o grafico (subplot)
    ax.plot_trisurf(x, y, s, cmap='viridis', edgecolor='none')

    # Ajusta o angulo do grafico (90,90 é a visão superior)
    ax.view_init(38, 80)

    # Ajusta as legendas e as fontes para este subplot
    ax.set_xlabel('X Label')
    ax.set_ylabel('Y Label')
    ax.set_zlabel('Z Label')
    ax.set_title(f'S2D{i:06d}')
    ax.tick_params(axis='both', which='major', labelsize=8)

    # Pausa para visualização
    plt.pause(0.001)

# Loop que altera o valor de i, atualizando o gráfico
for i in range(0,500):  
    update_plot(i)

# Mantém a janela do gráfico aberta
plt.show()
