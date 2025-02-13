import matplotlib.pyplot as plt
import numpy as np
import matplotlib.ticker as ticker
import matplotlib.colors as colors

from colormaps_telegrafica import get_tau
from colormaps_telegrafica import get_densidade



# -----------------------------------------------------------------------------------------

# Obter o colormap personalizado
cmap_tau = get_tau()
cmap_densidade = get_densidade()

# -----------------------------------------------------------------------------------------


# m = 0 # contador dos subplots

# # Inicia as duas figuras

# fig1 = plt.figure()

# # for i in range(0,501,100):
# for i in [1,250,500]:
    
#     print(i)
    
#     m = m+1

#     # Input dos dados
    
#     # file1  = f'C:\\Users\\Talita\\Desktop\\Portifólio\\Burgers-Equation\\GX{i:06d}.dat'
   
#     # file1 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAIDA_GX//GX{i:06d}.dat'
#     # file1 = f'/home/pgmac/Desktop/Talita/Telegrafica/SAI_PD/LAMBDA/LDA{i:06d}.dat'  

#     file1 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO/SAIDA_NI_081/SAIDA_EXATA_GX/EGX{i:06d}.dat'

#     # 

#     print(file1)     

#     # ------- GRAFICO DA DENSIDADE -------
    
#     x1,y1,gx1 = np.loadtxt(file1, unpack=True)
    
#     # Modificação dos dados para utilizar no contour
    
#     X1  = np.transpose(np.reshape(x1,(82,82)))
#     Y1  = np.transpose(np.reshape(y1,(82,82)))
#     GX1 = np.transpose(np.reshape(gx1,(82,82)))

#     # Cria um subplot da figura 1  

#     # ax1 = fig1.add_subplot(2,3,m)
#     # contour1 = ax1.contourf(X1,Y1,GX1, cmap=cmap_densidade, levels=np.linspace(-2, 10, 10000), norm = colors.SymLogNorm(linthresh=0.002,linscale=1,vmin=-10, vmax=10))
#     ax1 = fig1.add_subplot(2,3,m, projection='3d')
#     contour1 = ax1.plot_surface(X1, Y1, GX1, cmap=cmap_densidade, norm = colors.SymLogNorm(linthresh=0.002,linscale=1,vmin=-5, vmax=5))
    
#     # ax1.contour(X1, Y1, GX, levels=[0.001], colors='green')
#     # ax1.contour(X1, Y1, GX, levels=[10], colors='green')
#     ax1.set_xlim(0, 1.0) 
#     ax1.set_ylim(0, 1.0) 
#     ax1.set_zlim(-5, 5) 

#     ax1.set_xticks([])
#     ax1.set_yticks([])

    # ax1.legend(f'Solução exata para k={i:03d}')
    

# for i in [5,250,500]:
    
#     print(i)
    
#     m = m+1

#     # Input dos dados
    
#     # file1  = f'C:\\Users\\Talita\\Desktop\\Portifólio\\Burgers-Equation\\GX{i:06d}.dat'
   
#     # file1 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAIDA_GX//GX{i:06d}.dat'
#     # file1 = f'/home/pgmac/Desktop/Talita/Telegrafica/SAI_PD/LAMBDA/LDA{i:06d}.dat'  

#     file1 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO/SAIDA_NI_081/SAIDA_EXATA_GX/GX{i:06d}.dat'

#     # 

#     print(file1)     

#     # ------- GRAFICO DA DENSIDADE -------
    
#     x1,y1,gx1 = np.loadtxt(file1, unpack=True)
    
#     # Modificação dos dados para utilizar no contour
    
#     X1  = np.transpose(np.reshape(x1,(82,82)))
#     Y1  = np.transpose(np.reshape(y1,(82,82)))
#     GX1 = np.transpose(np.reshape(gx1,(82,82)))

    # Cria um subplot da figura 1  

    # ax1 = fig1.add_subplot(2,3,m)
    # contour1 = ax1.contourf(X1,Y1,GX1, cmap=cmap_densidade, levels=np.linspace(-2, 10, 10000), norm = colors.SymLogNorm(linthresh=0.002,linscale=1,vmin=-10, vmax=10))
    # ax1 = fig1.add_subplot(2,3,m, projection='3d')
    # contour1 = ax1.plot_surface(X1, Y1, GX1, cmap=cmap_densidade, norm = colors.SymLogNorm(linthresh=0.002,linscale=1,vmin=-10, vmax=10))

    # ax1.contour(X1, Y1, GX, levels=[0.001], colors='green')
    # ax1.contour(X1, Y1, GX, levels=[10], colors='green')
    # ax1.set_xlim(0, 1.0) 
    # ax1.set_ylim(0, 1.0) 
    # ax1.set_zlim(-6.0, 6.0) 

    # ax1.set_xticks([])
    # ax1.set_yticks([])

    # ax1.legend(f'Solução numérica para k={i:03d}')

# # # Colorbar da figura 1 (densidade)
# cbar1 = fig1.colorbar(contour1, ax=fig1.axes, orientation='vertical')
# cbar1.set_ticks([-6, -4, -2, -0.1, -0.001, 0, 0.001, 0.1, 1, 2, 4, 6 ])
# cbar1.set_label('Densidade de partículas')

# # # Ajustar manualmente o layout
# # plt.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1, wspace=0.05, hspace=0.1)

# ===========================================================================================
# # Obter o colormap personalizado
# cmap_tau = get_tau()
# cmap_densidade = get_densidade()

# m = 0 # contador dos subplots

# # Inicia as duas figuras

# fig1 = plt.figure()

# # for i in range(0,501,100):
# for i in [0,5,35]:
    
#     print(i)
    
#     m = m+1

#     # Input dos dados
    
#     # file1  = f'C:\\Users\\Talita\\Desktop\\Portifólio\\Burgers-Equation\\GX{i:06d}.dat'
   
#     file1 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAIDA_GX//GX{i:06d}.dat'
   
#     # file1 = f'/home/pgmac/Desktop/Talita/Telegrafica/SAI_PD/LAMBDA/LDA{i:06d}.dat'  
#     # 

#     print(file1)     

#     # ------- GRAFICO DA DENSIDADE -------
    
#     x1,y1,gx1 = np.loadtxt(file1, unpack=True)

#     print(max(gx1))
#     print(min(gx1))
#     print(min('---------'))
    
    
#     # Modificação dos dados para utilizar no contour
    
#     X1  = np.transpose(np.reshape(x1,(22,22)))
#     Y1  = np.transpose(np.reshape(y1,(22,22)))
#     GX1 = np.transpose(np.reshape(gx1,(22,22)))
    
#     # Cria um subplot da figura 1  

#     ax1 = fig1.add_subplot(2,3,m)
#     contour1 = ax1.contourf(X1,Y1,GX1, cmap=cmap_densidade, levels=np.linspace(-12, 12, 10000), norm = colors.SymLogNorm(linthresh=0.002,linscale=1,vmin=-10, vmax=10))
#     # ax1.contour(X1, Y1, GX, levels=[0.001], colors='green')
#     # ax1.contour(X1, Y1, GX, levels=[10], colors='green')
#     ax1.set_xlim(0, 2.0) 
#     ax1.set_ylim(0, 2.0) 
#     ax1.set_xticks([])
#     ax1.set_yticks([])

# # Cria um subplot da figura 2 

# for i in [0,5,35]:
    
#     m = m+1

#     file2 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAIDA_GY//GY{i:06d}.dat'

#     print(file1) 

#     x2,y2,gx2 = np.loadtxt(file2, unpack=True)

#     print(max(gx2))
#     print(min(gx2))
#     print(min('---------'))

#     X2  = np.transpose(np.reshape(x2,(22,22)))
#     Y2  = np.transpose(np.reshape(y2,(22,22)))
#     GX2 = np.transpose(np.reshape(gx2,(22,22)))

#     ax2 = fig1.add_subplot(2,3,m)
#     contour2 = ax2.contourf(X2,Y2,GX2, cmap=cmap_densidade, levels=np.linspace(-12, 12, 10000), norm = colors.SymLogNorm(linthresh=0.002,linscale=1,vmin=-10, vmax=10))
#     # ax1.contour(X1, Y1, GX, levels=[0.001], colors='green')
#     # ax1.contour(X1, Y1, GX, levels=[10], colors='green')
#     ax2.set_xlim(0, 2.0) 
#     ax2.set_ylim(0, 2.0) 
#     ax2.set_xticks([])
#     ax2.set_yticks([])

   

# # Colorbar da figura 1 (densidade)
# cbar1 = fig1.colorbar(contour1, ax=fig1.axes, orientation='vertical')
# cbar1.set_ticks([-2, 0, 2, 2, 4, 6, 8, 10 ])
# cbar1.set_label('Densidade de partículas')

# # Ajustar manualmente o layout
# # plt.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1, wspace=0.05, hspace=0.1)

# ===========================================================================================

# tempo = 0

# Y  = 10.10101010 
# X  = 10.10101010 

# fig2 = plt.figure()

# # ---------------------------
# # file2 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAIDA_GX/GX{tempo:06d}.dat'
# file2 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAIDA_EXATA/SAIDA_NI_081/SAIDA_EXATA_GX/EX000001.dat'

# x2,y2,gx2 = np.loadtxt(file2, unpack=True)
# posicoes = [i for i, y in enumerate(y2) if y==Y]

# X2  = [x2[i] for i in posicoes] 
# GX2 = [gx2[i] for i in posicoes]

# ax1 = fig2.add_subplot(1,2,1)
# curve = ax1.plot(X2,GX2)  

# ---------------------------
# file2 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAIDA_GY//GY{tempo:06d}.dat'

# x2,y2,gx2 = np.loadtxt(file2, unpack=True)

# posicoes = [i for i, x in enumerate(x2) if x==X]

# Y2  = [y2[i] for i in posicoes] 
# GX2 = [gx2[i] for i in posicoes]

# ax2 = fig2.add_subplot(1,2,2)
# curve = ax2.plot(Y2,GX2)  


# ==============================================================================================

# fig2 = plt.figure()
# ax1 = fig2.add_subplot(1,1,1)
# # ax2 = fig2.add_subplot(1,2,2)

# def update(t):
    
#     # file2 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAIDA_GX//GX{t:06d}.dat'
#     # file2 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAIDA_EXATAS_GX//EX{t:06d}.dat'
#     file2 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO/SAIDA_NI_081/SAIDA_EXATA_GX/EGX{t:06d}.dat'


#     x2,y2,gx2 = np.loadtxt(file2, unpack=True)

#     Y  = 0.38125001

#     posicoes = [i for i, y in enumerate(y2) if y==Y]

#     X2  = [x2[i] for i in posicoes] 
#     GX2 = [gx2[i] for i in posicoes]

#     curve = ax1.plot(X2,GX2)    
#     # ..............................

#     # x2,y2,gx2 = np.loadtxt(file2, unpack=True)

#     # X  = 10.10101010 

#     # posicoes = [i for i, x in enumerate(x2) if x==X]

#     # Y2  = [y2[i] for i in posicoes] 
#     # GX2 = [gx2[i] for i in posicoes]
#     # curve = ax2.plot(Y2,GX2) 

#     plt.pause(0.1)


# for t in range(10,500,10):
#     update(t)

# -----------------------------------------------------------------------------------------

plt.show()


    