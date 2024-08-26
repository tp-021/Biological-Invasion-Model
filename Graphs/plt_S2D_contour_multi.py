import matplotlib.pyplot as plt
import numpy as np
import matplotlib.ticker as ticker
import matplotlib.colors as colors
from colormaps_telegrafica import get_densidade

cmap_densidade = get_densidade()

# row_labels = ["t=0.0", "t=1.0", "t=2.0", "t=3.0", "t=4.0", "t=5.0"]
# column_labels = ["tau=1.00", "tau=0.25", "tau=0.01"]
# num_rows = 6  # Adjust this to match the actual number of rows
# num_columns = len(column_labels)

# FIGURA 1

m = 0
vetor = []
fig = plt.figure()

for i in range(0,201,40):

    for j in ('A','B','C'):

        # n = i*40
        m = m+1
        print(m) 

        file = f'C:\\Users\\Talita\\Desktop\\Resultados\\Conjunto 3\\{j}\\S2D{i:06d}.dat'
        # file = f'/home/pgmac/Desktop/Talita/Dados/Conjunto 3/{j}/S2D{i:06d}.dat'

        x,y,s = np.loadtxt(file, unpack=True)
        
        max_s = np.max(s)
        min_s = np.min(s)

            
        X = np.transpose(np.reshape(x,(245,245)))
        Y = np.transpose(np.reshape(y,(245,245)))
        S = np.transpose(np.reshape(s,(245,245)))

        ax  = fig.add_subplot(6,3,m)
        contour = ax.contourf(X,Y,S, cmap=cmap_densidade, levels=np.linspace(-2, 10, 10000), norm = colors.SymLogNorm(linthresh=0.02,linscale=1,vmin=-10, vmax=10)) 


        ax.set_xlim(-0.0065, 3.1480) 
        ax.set_ylim(-0.0065, 3.1480) 
        ax.set_xticks([])
        ax.set_yticks([])
        

# # Add labels to the beginning of each row
# for row in range(num_rows):
#     fig.text(0.09, 1 - (row + 0.5) / num_rows, row_labels[row], ha='right', va='center', fontsize=5, rotation='vertical')

# # Add labels to the top of each column
# for col in range(num_columns):
#     fig.text(0.2 + col * (0.8 / num_columns), 0.97, column_labels[col], ha='center', va='bottom', fontsize=5)
 

cbar = fig.colorbar(contour, ax=fig.axes, orientation='vertical', fraction=0.02, pad=0.04)
cbar.set_ticks([-2, 0, 2, 4, 6, 8, 10])
cbar.set_label('Densidade de partículas')

#Ajusta os números que aparecem na colorbar
# cbar.set_ticks([min_s, 0.00, max_s])
# cbar.set_ticklabels(['-1','0.00', '1'])

# Definir os ticks da colorbar para mostrar apenas inteiros
# cbar.set_ticks(np.arange(int(np.min(S)), int(np.max(S)) + 1))
# cbar.ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda x, pos: f'{int(x)}'))

# Ajustar manualmente o layout
plt.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1, wspace=0.05, hspace=0.1)


plt.show()
    

    