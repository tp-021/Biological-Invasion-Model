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

# for i in range(0,501,100):
for i in [0,5,10,15,20,35]:
    
    print(i)
    
    m = m+1

    # Input dos dados
    
    # file1  = f'C:\\Users\\Talita\\Desktop\\Portifólio\\Burgers-Equation\\GX{i:06d}.dat'
   
    file1 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_PD//GX{i:06d}.dat'
    # file1 = f'/home/pgmac/Desktop/Talita/Telegrafica/SAI_PD/LAMBDA/LDA{i:06d}.dat'  
    # 

    print(file1)     

    # ------- GRAFICO DA DENSIDADE -------
    
    x1,y1,gx = np.loadtxt(file1, unpack=True)
    
    # Modificação dos dados para utilizar no contour
    
    X1 = np.transpose(np.reshape(x1,(101,101)))
    Y1 = np.transpose(np.reshape(y1,(101,101)))
    GX = np.transpose(np.reshape(gx,(101,101)))

    # Cria um subplot da figura 1  

    ax1 = fig1.add_subplot(2,3,m)
    contour1 = ax1.contourf(X1,Y1,GX, cmap=cmap_densidade, levels=np.linspace(-2, 10, 10000), norm = colors.SymLogNorm(linthresh=0.002,linscale=1,vmin=-10, vmax=10))
    # ax1.contour(X1, Y1, GX, levels=[0.001], colors='green')
    # ax1.contour(X1, Y1, GX, levels=[10], colors='green')
    ax1.set_xlim(0, 20.0) 
    ax1.set_ylim(0, 20.0) 
    ax1.set_xticks([])
    ax1.set_yticks([])

# Colorbar da figura 1 (densidade)
cbar1 = fig1.colorbar(contour1, ax=fig1.axes, orientation='vertical')
cbar1.set_ticks([-2, 0, 2, 2, 4, 6, 8, 10 ])
cbar1.set_label('Densidade de partículas')

# Ajustar manualmente o layout
# plt.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1, wspace=0.05, hspace=0.1)

plt.show()
    