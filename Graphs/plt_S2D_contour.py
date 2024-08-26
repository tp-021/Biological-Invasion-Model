import matplotlib.pyplot as plt
import numpy as np
import matplotlib.ticker as ticker

# Obter os dados

# file_path = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
file_path = f'/home/pgmac/Desktop/Talita/Dados/Conjunto 1/A/S2D000160.dat'

x,y,s = np.loadtxt(file_path, unpack=True)

# Ajustar os dados para o formato correto

X = np.transpose(np.reshape(x,(245,245)))
Y = np.transpose(np.reshape(y,(245,245)))
S = np.transpose(np.reshape(s,(245,245)))
print(S)
# Plotar o gráfico

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

contour  = ax.contourf(X, Y, S, cmap='viridis', levels = 50)
contour1 = ax.contour(X, Y, S, colors='green', levels=2)

cbar = fig.colorbar(contour, ax=fig.axes, orientation='vertical', fraction=0.02, pad=0.04)
cbar.set_label('Densidade de partículas')

# Definir os ticks da colorbar para mostrar apenas inteiros
cbar.set_ticks(np.arange(int(np.min(S)), int(np.max(S)) + 1))
cbar.ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda x, pos: f'{int(x)}'))

# # Ajustar manualmente o layout
# plt.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1, wspace=0.05, hspace=0.1)


# Adicionar animação

plt.show()