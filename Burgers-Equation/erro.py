import matplotlib.pyplot as plt
import numpy as np




# file1 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO/SAIDA_NI_021/SAIDA_EXATA_GX/EGX000500.dat'
# file2 = f'/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO/SAIDA_NI_021/SAIDA_GX/GX000500.dat'

# x1,y1,gx1 = np.loadtxt(file1, unpack=True)
# x2,y2,gx2 = np.loadtxt(file2, unpack=True)

# r  = gx1 - gx2
# # r1 = np.linalg.norm(r, 'fro')
# r2 = np.linalg.norm(r, 2)

# erro = abs(r)

# with open('erro.txt', 'w') as f:
#     np.savetxt(f, np.column_stack((x1, y1, x2, y2, gx1, gx2, erro)),
#                fmt=['%12.6f', '%12.6f', '%12.6f', '%12.6f', '%12.8f', '%12.8f', '%12.8f'])

# # print(r1)

# print(r2)

# ==============================================================================================

# file1 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_1/SAIDA_NI_0041/erro_exato.dat'
# file2 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_1/SAIDA_NI_0081/erro_exato.dat'
# file3 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_1/SAIDA_NI_0161/erro_exato.dat'
# file4 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_1/SAIDA_NI_0321/erro_exato.dat'
# file5 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_1/SAIDA_NI_0641/erro_exato.dat'


# k1,t1,ex1,ey1,aex1,aey1,a1 = np.loadtxt(file1, unpack=True)
# k2,t2,ex2,ey2,aex2,aey2,a2 = np.loadtxt(file2, unpack=True)
# k3,t3,ex3,ey3,aex3,aey3,a3 = np.loadtxt(file3, unpack=True)
# k4,t4,ex4,ey4,aex4,aey4,a4 = np.loadtxt(file4, unpack=True)
# k5,t5,ex5,ey5,aex5,aey5,a5 = np.loadtxt(file5, unpack=True)

# # r  = gx1 - gx2
# # # r1 = np.linalg.norm(r, 'fro')
# # r2 = np.linalg.norm(r, 2)

# # erro = abs(r)

# data = np.column_stack(( t1, ex1, ex2, ex3, ex4, ex5))

# with open('erro.txt', 'w') as f:
#     for row in data[::100]:
#         f.write(' & '.join(f"{value:12.6f}" for value in row) + ' \\\\\n')

# with open('erro.txt', 'w') as f:
#     np.savetxt(f, np.column_stack((t1, '&', t2, '&', ex1, '&', ex2, '&', ex3, '&', ex4, '&', ex5)),
#                fmt=['%12.6f', '%12.6f', '%12.6f', '%12.6f', '%12.8f', '%12.8f', '%12.8f'])

# print(r1)

# print(r2)

# ==============================================================================================
# Os arquivos para tempos diferentes não analisam os mesmos pontos, mudar isso no código

# file1 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_3/SAIDA_TT_0501/erro_exato.dat'
# file2 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_3/SAIDA_TT_1001/erro_exato.dat'
# file3 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_3/SAIDA_TT_2001/erro_exato.dat'
# file4 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_3/SAIDA_TT_4001/erro_exato.dat'
# file5 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_3/SAIDA_TT_8001/erro_exato.dat'


# k1,t1,ex1,ey1,aex1,aey1,a1 = np.loadtxt(file1, unpack=True)
# k2,t2,ex2,ey2,aex2,aey2,a2 = np.loadtxt(file2, unpack=True)
# k3,t3,ex3,ey3,aex3,aey3,a3 = np.loadtxt(file3, unpack=True)
# k4,t4,ex4,ey4,aex4,aey4,a4 = np.loadtxt(file4, unpack=True)
# k5,t5,ex5,ey5,aex5,aey5,a5 = np.loadtxt(file5, unpack=True)

# # r  = gx1 - gx2
# # # r1 = np.linalg.norm(r, 'fro')
# # r2 = np.linalg.norm(r, 2)

# # erro = abs(r)

# data = np.column_stack(( t1, ex1, ex2, ex3, ex4, ex5))

# with open('erro.txt', 'w') as f:
#     for row in data[::100]:
#         f.write(' & '.join(f"{value:12.6f}" for value in row) + ' \\\\\n')

# ==============================================================================================


file1 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_1/SAIDA_NI_0081/erro_exato.dat'
file2 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_4/SAIDA_NI_0081/erro_exato.dat'
file3 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_5/SAIDA_NI_0081/erro_exato.dat'
file4 = '/home/pgmac/Desktop/Portifólio/Burgers-Equation/SAI_AUTO_6/SAIDA_NI_0081/erro_exato.dat'


k1,t1,ex1,ey1,aex1,aey1,a1 = np.loadtxt(file1, unpack=True)
k2,t2,ex2,ey2,aex2,aey2,a2 = np.loadtxt(file2, unpack=True)
k3,t3,ex3,ey3,aex3,aey3,a3 = np.loadtxt(file3, unpack=True)
k4,t4,ex4,ey4,aex4,aey4,a4 = np.loadtxt(file4, unpack=True)

# r  = gx1 - gx2
# # r1 = np.linalg.norm(r, 'fro')
# r2 = np.linalg.norm(r, 2)

# erro = abs(r)

data = np.column_stack(( t1, ex1, ex2, ex3, ex4))

with open('erro.txt', 'w') as f:
    for row in data[::100]:
        f.write(' & '.join(f"{value:12.6f}" for value in row) + ' \\\\\n')
