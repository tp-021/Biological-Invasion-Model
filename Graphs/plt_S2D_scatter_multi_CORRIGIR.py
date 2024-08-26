import matplotlib.pyplot as plt
import numpy as np
import matplotlib.ticker as ticker

# FIGURA 1

m = 0

fig = plt.figure()

for i in range(0,201,40):

    for j in ('A','B','C'):

        # n = i*40
        m = m+1

        file = f'C:\\Users\\Talita\\Desktop\\Resultados\\Conjunto 3\\{j}\\S2D{i:06d}.dat'
        # file = f'/home/pgmac/Desktop/Talita/Dados/Conjunto 1/{j}/S2D{i:06d}.dat'

        print(file)        

        x,y,s = np.loadtxt(file, unpack=True)
            
        X = np.transpose(np.reshape(x,(245,245)))
        Y = np.transpose(np.reshape(y,(245,245)))
        S = np.transpose(np.reshape(s,(245,245)))

        ax  = fig.add_subplot(6,3,m)
        scatter = ax.scatter(X,Y, c=S, cmap='viridis')
        ax.set_xticks([])
        ax.set_yticks([])

# cbar = fig.colorbar(scatter, ax=fig.axes, orientation='vertical', fraction=0.02, pad=0.04)
# cbar.set_label('Densidade de partículas')

# # Definir os ticks da colorbar para mostrar apenas inteiros
# cbar.set_ticks(np.arange(int(np.min(S)), int(np.max(S)) + 1))
# cbar.ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda x, pos: f'{int(x)}'))

# Ajustar manualmente o layout
plt.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1, wspace=0.05, hspace=0.1)


# MOSTRAR OS GRÁFICOS
plt.show()
    

    

# file1 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file2 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file3 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file4 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file5 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file6 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file7 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file8 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file9 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file10 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file11 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file12 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file13 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file14 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file15 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file16 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file17 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'
# file18 = 'C:\\Users\\Talita\\Desktop\\Mesa de trabalho\\Dissertação - Talita\\Telegrafica\\SAI_PD\\S2D000025.dat'

# x1,y1,s1 = np.loadtxt(file1, unpack=True)
# x2,y2,s2 = np.loadtxt(file2, unpack=True)
# x3,y3,s3 = np.loadtxt(file3, unpack=True)
# x4,y4,s4 = np.loadtxt(file4, unpack=True)
# x5,y5,s5 = np.loadtxt(file5, unpack=True)
# x6,y6,s6 = np.loadtxt(file6, unpack=True)
# x7,y7,s7 = np.loadtxt(file7, unpack=True)
# x8,y8,s8 = np.loadtxt(file8, unpack=True)
# x9,y9,s9 = np.loadtxt(file9, unpack=True)
# x10,y10,s10 = np.loadtxt(file10, unpack=True)
# x11,y11,s11 = np.loadtxt(file11, unpack=True)
# x12,y12,s12 = np.loadtxt(file12, unpack=True)
# x13,y13,s13 = np.loadtxt(file13, unpack=True)
# x14,y14,s14 = np.loadtxt(file14, unpack=True)
# x15,y15,s15 = np.loadtxt(file15, unpack=True)
# x16,y16,s16 = np.loadtxt(file16, unpack=True)
# x17,y17,s17 = np.loadtxt(file17, unpack=True)
# x18,y18,s18 = np.loadtxt(file18, unpack=True)

# # Plotar o gráfico

# fig = plt.figure()

# # Ajustar os dados para o formato correto

# X1 = np.transpose(np.reshape(x1,(245,245)))
# Y1 = np.transpose(np.reshape(y1,(245,245)))
# S1 = np.transpose(np.reshape(s1,(245,245)))
# ax1 = fig.add_subplot(6,3,1)
# ax1.contourf(X1, Y1, S1, cmap='viridis')
# ax1.contour(X1, Y1, S1, colors='green', levels=1)

# X2 = np.transpose(np.reshape(x2,(245,245)))
# Y2 = np.transpose(np.reshape(y2,(245,245)))
# S2 = np.transpose(np.reshape(s2,(245,245)))
# ax2 = fig.add_subplot(6,3,2)
# ax2.contourf(X2, Y2, S2, cmap='viridis')
# ax2.contour(X2, Y2, S2, colors='green', levels=1)


# X3 = np.transpose(np.reshape(x3,(245,245)))
# Y3 = np.transpose(np.reshape(y3,(245,245)))
# S3 = np.transpose(np.reshape(s3,(245,245)))
# ax3 = fig.add_subplot(6,3,3)
# ax3.contourf(X3, Y3, S3, cmap='viridis')
# ax3.contour(X3, Y3, S3, colors='green', levels=1)

# X4 = np.transpose(np.reshape(x4,(245,245)))
# Y4 = np.transpose(np.reshape(y4,(245,245)))
# S4 = np.transpose(np.reshape(s4,(245,245)))
# ax4 = fig.add_subplot(6,3,4)
# ax4.contourf(X4, Y4, S4, cmap='viridis')
# ax4.contour(X4, Y4, S4, colors='green', levels=1)

# X5 = np.transpose(np.reshape(x5,(245,245)))
# Y5 = np.transpose(np.reshape(y5,(245,245)))
# S5 = np.transpose(np.reshape(s5,(245,245)))
# ax5 = fig.add_subplot(6,3,5)
# ax5.contourf(X5, Y5, S5, cmap='viridis')
# ax5.contour(X5, Y5, S5, colors='green', levels=1)

# X6 = np.transpose(np.reshape(x6,(245,245)))
# Y6 = np.transpose(np.reshape(y6,(245,245)))
# S6 = np.transpose(np.reshape(s6,(245,245)))
# ax6 = fig.add_subplot(6,3,6)
# ax6.contourf(X6, Y6, S6, cmap='viridis')
# ax6.contour(X6, Y6, S6, colors='green', levels=1)

# X7 = np.transpose(np.reshape(x7,(245,245)))
# Y7 = np.transpose(np.reshape(y7,(245,245)))
# S7 = np.transpose(np.reshape(s7,(245,245)))
# ax7 = fig.add_subplot(6,3,7)
# ax7.contourf(X7, Y7, S7, cmap='viridis')
# ax7.contour(X7, Y7, S7, colors='green', levels=1)

# X8 = np.transpose(np.reshape(x8,(245,245)))
# Y8 = np.transpose(np.reshape(y8,(245,245)))
# S8 = np.transpose(np.reshape(s8,(245,245)))
# ax8 = fig.add_subplot(6,3,8)
# ax8.contourf(X8, Y8, S8, cmap='viridis')
# ax8.contour(X8, Y8, S8, colors='green', levels=1)

# X9 = np.transpose(np.reshape(x9,(245,245)))
# Y9 = np.transpose(np.reshape(y9,(245,245)))
# S9 = np.transpose(np.reshape(s9,(245,245)))
# ax9 = fig.add_subplot(6,3,9)
# ax9.contourf(X9, Y9, S9, cmap='viridis')
# ax9.contour(X9, Y9, S9, colors='green', levels=1)

# X10 = np.transpose(np.reshape(x10,(245,245)))
# Y10 = np.transpose(np.reshape(y10,(245,245)))
# S10 = np.transpose(np.reshape(s10,(245,245)))
# ax10 = fig.add_subplot(6,3,10)
# ax10.contourf(X10, Y10, S10, cmap='viridis')
# ax10.contour(X10, Y10, S10, colors='green', levels=1)

# X11 = np.transpose(np.reshape(x10,(245,245)))
# Y11 = np.transpose(np.reshape(y10,(245,245)))
# S12 = np.transpose(np.reshape(s10,(245,245)))
# ax11 = fig.add_subplot(6,3,11)
# ax1.contourf(X11, Y11, S11, cmap='viridis')
# ax1.contour(X11, Y11, S11, colors='green', levels=1)

# X12 = np.transpose(np.reshape(x12,(245,245)))
# Y12 = np.transpose(np.reshape(y12,(245,245)))
# S12 = np.transpose(np.reshape(s12,(245,245)))
# ax12 = fig.add_subplot(6,3,12)
# ax12.contourf(X12, Y12, S12, cmap='viridis')
# ax12.contour(X12, Y12, S12, colors='green', levels=1)

# X13 = np.transpose(np.reshape(x13,(245,245)))
# Y13 = np.transpose(np.reshape(y13,(245,245)))
# S13 = np.transpose(np.reshape(s13,(245,245)))
# ax13 = fig.add_subplot(6,3,13)
# ax13.contourf(X13, Y13, S13, cmap='viridis')
# ax13.contour(X13, Y13, S13, colors='green', levels=1)

# X14 = np.transpose(np.reshape(x14,(245,245)))
# Y14 = np.transpose(np.reshape(y14,(245,245)))
# S14 = np.transpose(np.reshape(s14,(245,245)))
# ax14 = fig.add_subplot(6,3,14)
# ax14.contourf(X14, Y14, S14, cmap='viridis')
# ax14.contour(X14, Y14, S14, colors='green', levels=1)

# X15 = np.transpose(np.reshape(x15,(245,245)))
# Y15 = np.transpose(np.reshape(y15,(245,245)))
# S15 = np.transpose(np.reshape(s15,(245,245)))
# ax15 = fig.add_subplot(6,3,15)
# ax15.contourf(X15, Y15, S15, cmap='viridis')
# ax15.contour(X15, Y15, S15, colors='green', levels=1)

# X16 = np.transpose(np.reshape(x16,(245,245)))
# Y16 = np.transpose(np.reshape(y16,(245,245)))
# S16 = np.transpose(np.reshape(s16,(245,245)))
# ax16 = fig.add_subplot(6,3,16)
# ax16.contourf(X16, Y16, S16, cmap='viridis')
# ax16.contour(X16, Y16, S16, colors='green', levels=1)

# X17 = np.transpose(np.reshape(x17,(245,245)))
# Y17 = np.transpose(np.reshape(y17,(245,245)))
# S17 = np.transpose(np.reshape(s17,(245,245)))
# ax17 = fig.add_subplot(6,3,17)
# ax17.contourf(X17, Y17, S17, cmap='viridis')
# ax17.contour(X17, Y17, S17, colors='green', levels=1)

# X18 = np.transpose(np.reshape(x18,(245,245)))
# Y18 = np.transpose(np.reshape(y18,(245,245)))
# S18 = np.transpose(np.reshape(s18,(245,245)))
# ax18 = fig.add_subplot(6,3,18)
# ax18.contourf(X18, Y18, S18, cmap='viridis')
# ax18.contour(X18, Y18, S18, colors='green', levels=1)


# plt.show()