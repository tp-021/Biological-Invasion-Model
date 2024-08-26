import matplotlib.pyplot as plt
import numpy as np

from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator

fig  = plt.figure()


file1 = f'C:\\Users\\Talita\\Desktop\\Resultados\\Conjunto 4\\A\\QUANT_POPULACIONAL.dat' 
file2 = f'C:\\Users\\Talita\\Desktop\\Resultados\\Conjunto 4\\B\\QUANT_POPULACIONAL.dat' 
file3 = f'C:\\Users\\Talita\\Desktop\\Resultados\\Conjunto 4\\C\\QUANT_POPULACIONAL.dat' 
# file1 = '/home/pgmac/Desktop/Talita/Dados/Conjunto 4/A/QUANT_POPULACIONAL.dat' 
# file2 = '/home/pgmac/Desktop/Talita/Dados/Conjunto 4/B/QUANT_POPULACIONAL.dat' 
# file3 = '/home/pgmac/Desktop/Talita/Dados/Conjunto 4/C/QUANT_POPULACIONAL.dat'    

t1, pop1, rea1, tem1, dix1, diy1, res1, ocup1, erro1 = np.loadtxt(file1, comments='#', unpack=True, skiprows=1)
t2, pop2, rea2, tem2, dix2, diy2, res2, ocup2, erro2 = np.loadtxt(file2, comments='#', unpack=True, skiprows=1)
t3, pop3, rea3, tem3, dix3, diy3, res3, ocup3, erro3 = np.loadtxt(file3, comments='#', unpack=True, skiprows=1)
        
ax  = fig.add_subplot(1,2,1)
ax.plot(t1, pop1, color='r', marker='s', markersize=3, markerfacecolor='r', markeredgecolor='r',markevery=200, label='D=1.00')
ax.plot(t2, pop2, color='g', marker='o', markersize=3, markerfacecolor='g', markeredgecolor='g',markevery=200, label='D=0.50')
ax.plot(t3, pop3, color='b', marker='^', markersize=3, markerfacecolor='b', markeredgecolor='b',markevery=200, label='D=0.25')

ax.tick_params(axis='both', which='major', labelsize=8)
ax.set_title(f'Populacão')
ax.set_xlim(0, 5)   
ax.set_ylim(0, 25) 
ax.legend()



ax1  = fig.add_subplot(1,2,2)
ax1.plot(t1, ocup1, color='r', marker='s', markersize=3, markerfacecolor='r', markeredgecolor='r',markevery=200, label='D=1.00')
ax1.plot(t2, ocup2, color='g', marker='o', markersize=3, markerfacecolor='g', markeredgecolor='g',markevery=200, label='D=0.50')
ax1.plot(t3, ocup3, color='b', marker='^', markersize=3, markerfacecolor='b', markeredgecolor='b',markevery=200, label='D=0.25')

ax1.tick_params(axis='both', which='major', labelsize=8)
ax1.set_title(f'Ocupação')
ax1.set_xlim(0, 5)   
ax1.set_ylim(0, 100) 
ax1.legend()

fig1  = plt.figure()
ax  = fig1.add_subplot(1,1,1)
ax.plot(t3, tem3/(243*243), color='r', marker='s', markersize=3, markerfacecolor='r', markeredgecolor='r',markevery=200, label='Termo Temporal')
ax.plot(t3, dix3/(243*243), color='g', marker='o', markersize=3, markerfacecolor='g', markeredgecolor='g',markevery=200, label='Termo Difusivo')
ax.plot(t3, rea3/(243*243), color='y', marker='^', markersize=3, markerfacecolor='y', markeredgecolor='y',markevery=200, label='Termo Reativo')
ax.plot(t3, res3/(243*243), color='k', marker='D', markersize=3, markerfacecolor='k', markeredgecolor='k',markevery=200, label='Resíduo Numérico')

ax.set_title('Parcelas (D=0.25)')
ax.set_xlim(0, 5)  
ax.legend() 

fig2  = plt.figure()
ax  = fig2.add_subplot(1,1,1)
ax.plot(t1, tem1/(243*243), color='r', marker='s', markersize=3, markerfacecolor='r', markeredgecolor='r',markevery=200, label='Termo Temporal')
ax.plot(t1, dix1/(243*243), color='g', marker='o', markersize=3, markerfacecolor='g', markeredgecolor='g',markevery=200, label='Termo Difusivo')
ax.plot(t1, rea1/(243*243), color='y', marker='^', markersize=3, markerfacecolor='y', markeredgecolor='y',markevery=200, label='Termo Reativo')
ax.plot(t1, res1/(243*243), color='k', marker='D', markersize=3, markerfacecolor='k', markeredgecolor='k',markevery=200, label='Resíduo Numérico')

ax.set_title('Parcelas (D=1.00)')
ax.set_xlim(0, 5) 
ax.legend()  


plt.show()     

# Figura 1 - população

# fig1 = plt.figure()
# ax1 = fig1.add_subplot(111)

# ax1.plot(t, pop1,'r')
# ax1.plot(t, pop2,'b')
# ax1.plot(t, pop3,'g')
# ax1.set_xlabel('Tempo')
# ax1.set_ylabel('Populacao')
# ax1.set_title('Populacao por Tempo')

# # Figura 1 - ocupação

# fig1 = plt.figure()
# ax1 = fig1.add_subplot(111)

# ax1.plot(t, ocup1,'r')
# ax1.plot(t, ocup2,'b')
# ax1.plot(t, ocup3,'g')
# ax1.set_xlabel('Tempo')
# ax1.set_ylabel('Populacao')
# ax1.set_title('Populacao por Tempo')




