import matplotlib.pyplot as plt
import numpy as np
import matplotlib.ticker as ticker
import matplotlib.colors as colors

from colormaps_telegrafica import get_tau
from colormaps_telegrafica import get_densidade

# Obter o colormap personalizado
cmap_tau = get_tau()
cmap_densidade = get_densidade()

m = 0 # contador dos subplots

# Inicia as duas figuras

fig1 = plt.figure()
fig2 = plt.figure()


for i in range(70,121,20):
    
    print(i)
    
    m = m+1

    # Input dos dados
    
    file1  = f'C:\\Users\\Talita\\Desktop\\Resultados\\SAI_PD\\S2D{i:06d}.dat'
    file2 = f'C:\\Users\\Talita\\Desktop\\Resultados\\SAI_PD\\LAMBDA\\LDA{i:06d}.dat'
   
    # file = f'/home/pgmac/Desktop/Talita/Telegrafica/SAI_PD//S2D{i:06d}.dat'
    # file1 = f'/home/pgmac/Desktop/Talita/Telegrafica/SAI_PD/LAMBDA/LDA{i:06d}.dat'       

   
    # ------- GRAFICO DA DENSIDADE -------
    
    x1,y1,s = np.loadtxt(file1, unpack=True)
    
    # Modificação dos dados para utilizar no contour
    
    X1 = np.transpose(np.reshape(x1,(245,245)))
    Y1 = np.transpose(np.reshape(y1,(245,245)))
    S = np.transpose(np.reshape(s,(245,245)))

    # Cria um subplot da figura 1  

    ax1 = fig1.add_subplot(1,3,m)
    contour1 = ax1.contourf(X1,Y1,S, cmap=cmap_densidade, levels=np.linspace(-2, 10, 10000), norm = colors.SymLogNorm(linthresh=0.02,linscale=1,vmin=-10, vmax=10))
    ax1.contour(X1, Y1, S, levels=[0.001], colors='green')
    ax1.contour(X1, Y1, S, levels=[0.1], colors='green')
    ax1.set_xlim(-0.0065, 3.1480) 
    ax1.set_ylim(-0.0065, 3.1480) 
    ax1.set_xticks([])
    ax1.set_yticks([])


    # ----------- GRAFICO DA TAU -----------
    
    x2,y2,l = np.loadtxt(file2, unpack=True) 

    # Modificação dos dados para utilizar no contour
    
    l_corrigido= [0.0001 if valor == 0.0000 else valor for valor in l]

    X2 = np.transpose(np.reshape(x2,(245,245)))
    Y2 = np.transpose(np.reshape(y2,(245,245)))
    L= np.transpose(np.reshape(l_corrigido,(245,245)))  

    # Cria um subplot da figura 2  
    
    ax2  = fig2.add_subplot(1,3,m)
    contour2 = ax2.contourf(X2, Y2, 1/(2*L), cmap=cmap_tau, levels=np.logspace(np.log10(0.0001), np.log10(5000), 500), norm=colors.LogNorm(vmin=0.0001, vmax=5000))
    ax2.set_xlim(-0.0065, 3.1480) 
    ax2.set_ylim(-0.0065, 3.1480) 
    ax2.set_xticks([])
    ax2.set_yticks([])

# Colorbar da figura 1 (densidade)
cbar1 = fig1.colorbar(contour1, ax=fig1.axes, orientation='vertical')
cbar1.set_ticks([-2, 0, 2, 2, 4, 6, 8, 10 ])
cbar1.set_label('Densidade de partículas')

# Colorbar da figura 2 (tau)
cbar2 = fig2.colorbar(contour2, ax=fig2.axes, orientation='vertical', fraction=0.046, pad=0.04)
cbar2.set_ticks([0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000])
cbar2.set_ticklabels(['0.0001', '0.001', '0.01', '0.1', '1', '10', '100', '1000'])
cbar2.set_label('Tempo de atraso')

# Ajustar manualmente o layout
# plt.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1, wspace=0.05, hspace=0.1)

plt.show()
    