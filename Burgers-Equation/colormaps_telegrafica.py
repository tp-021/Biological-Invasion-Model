import matplotlib.colors as mcolors

# -----------------------------------------------------------
# TAU 

def get_tau():
    
    # Convertendo as cores para valores RGB
    maroon = (0.5, 0.0, 0.0)
    orangered = (1.0, 0.27, 0.0)
    yellow = (0.1, 0.1, 0)
    lightyellow = (1.0, 1.0, 0.88)
    yellow = (1.0, 0.8, 0.0)
    black = (0, 0, 0)
    white = (1, 1, 1)

    cdict = {
        'red': [
            (0.0, black[0], black[0]), 
            (0.2, maroon[0], maroon[0]),
            (0.4, orangered[0], orangered[0]),
            (0.6, yellow[0], yellow[0]),
            (0.9, lightyellow[0], lightyellow[0]),
            (1.0, white[0], white[0])
        ],
        'green': [
            (0.0, black[1], black[1]),  
            (0.2, maroon[1], maroon[1]),
            (0.4, orangered[1], orangered[1]),
            (0.6, yellow[1], yellow[1]),
            (0.9, lightyellow[1], lightyellow[1]),
            (1.0, white[1], white[1])
        ],
        'blue': [
            (0.0, black[2], black[2]),
            (0.2, maroon[2], maroon[2]), 
            (0.4, orangered[2], orangered[2]),
            (0.6, yellow[2], yellow[2]),
            (0.9, lightyellow[2], lightyellow[2]),
            (1.0, white[2], white[2])
        ]
    }

    # Criando o colormap personalizado
    custom_cmap_1 = mcolors.LinearSegmentedColormap('ColormapTau', cdict)
    return custom_cmap_1

# ==============================================================================

# Densidade/S2D 

def get_densidade():
    red = (1.00,0.00,0.17)
    pink = (1.00,0.20,0.87)
    lightpink = (0.87,0.60,1.00)
    
    darkorange = (0.28,0.05,0.00)
    orange = (1.00,0.50,0.00)
    lightorange= (1.00,1.00,0.30)
    
    black = (0, 0, 0)

    darkgreen  = (0,0.39,0)
    green      = (0.13,0.54,0.13)
    lightgreen = (0.56,0.93,0.56)
    blue = (0.0, 0.0, 1)
    white = (1, 1, 1)

    cdict = {
        'red': [
            (0.0,  white[0], white[0]),
            (1/12,  blue[0],  blue[0]),

            (2.5/12, lightgreen[0], lightgreen[0]),
            (3.5/12,      green[0],      green[0]), 
            (4.5/12,  darkgreen[0], darkgreen[0] ), 
            
            (5/12, black[0], black[0]), 
            (6/12, black[0], black[0]),
            (7/12, black[0], black[0]), 
            
            (7.5/12, darkorange[0], darkorange[0]),
            (8.5/12, orange[0], orange[0]),
            # (7.09/12, darkyellow[0], darkyellow[0]),
            (9.5/12, lightorange[0], lightorange[0]),  
           
            (10/12,lightpink[0],lightpink[0]), 
            (11/12,pink[0],pink[0]), 
            (1.0,red[0],red[0] )  
    ],      
        'green': [
            (0.0,  white[1], white[1]),
            (1/12,  blue[1],  blue[1]),

            (2.5/12, lightgreen[1], lightgreen[1]),
            (3.5/12,      green[1],      green[1]), 
            (4.5/12,  darkgreen[1],  darkgreen[1]),
            
            (5/12, black[1], black[1]), 
            (6/12, black[1], black[1]),
            (7/12, black[1], black[1]), 

            (7.5/12,  darkorange[1],  darkorange[1]),
            (8.5/12,      orange[1],      orange[1]),
            (9.5/12, lightorange[1], lightorange[1]), 
            
            (10/12, lightpink[1], lightpink[1]), 
            (11/12,      pink[1],      pink[1]), 
            (1.0,         red[1],       red[1])  
    ],
        'blue': [
            (0.0,  white[2], white[2]),
            (1/12,  blue[2],  blue[2]),

            (2.5/12, lightgreen[2], lightgreen[2]),
            (3.5/12,      green[2],      green[2]), 
            (4.5/12,  darkgreen[2],  darkgreen[2]),
            
            (5/12, black[2], black[2]), 
            (6/12, black[2], black[2]),
            (7/12, black[2], black[2]), 
             
            (7.5/12,  darkorange[2],  darkorange[2]),
            (8.5/12,      orange[2],      orange[2]),
            (9.5/12, lightorange[2], lightorange[2]), 
             
            (10/12, lightpink[2], lightpink[2]), 
            (11/12,      pink[2],      pink[2]), 
            (1.0,         red[2],       red[2])  
    ]
    }


    custom_cmap_2 = mcolors.LinearSegmentedColormap('ColormapS2D_1', cdict)
    return custom_cmap_2

# ------------------------------------------------------------------------------

# Densidade/S2D (versão 2)

def get_densidade_2():
    
    red = (1.00,0.00,0.17)
    pink = (1.00,0.00,0.50)
    lightblue = (0.70, 0.80, 1.00)
    blue = (0.0, 0.0, 1)
    darkblue = (0.0, 0.0, 0.4)
    darkorange = (0.60,0.50,0.00)
    orange = (1.00,0.84,0.00)
    lightorange= (1.00,0.90,0.40)
    black = (0, 0, 0)
    white = (1, 1, 1)
   

    cdict = {
        'red': [
            (0.0, white[0], white[0] ),
            (1/12,lightblue[0], lightblue[0]), #blue
            (2.5/12,blue[0], blue[0] ), #blue
            (4/12-(0.002/20), darkblue[0], darkblue[0]), 
            
            (5/12, black[0], black[0]), #limite black
            (6/12, black[0], black[0]),
            (7/12, black[0], black[0]), #limite black
            
            (7.5/12, darkorange[0], darkorange[0]),
            (8.5/12, orange[0], orange[0]),
            (9.5/12, lightorange[0], lightorange[0]),  
           
            (11/12,pink[0],pink[0]), #pink
            (1.0,red[0],red[0] )  #red
    ],      
        'green': [
            (0.0, white[1], white[1] ),
            (1/12, lightblue[1], lightblue[1] ), #blue
            (2.5/12,blue[1], blue[1] ), #blue
            (4/12-(0.002/20), darkblue[1], darkblue[1]),
            
            (5/12, black[1], black[1]), #limite black
            (6/12, black[1], black[1]),
            (7/12, black[1], black[1]), #limite black
            
            (7.5/12, darkorange[1], darkorange[1]),
            (8.5/12, orange[1], orange[1]),
            (9.5/12, lightorange[1], lightorange[1]), 
            
            (11/12,pink[1],pink[1]), #pink
            (1.0,red[1],red[1] )  #red
    ],
        'blue': [
            (0.0, white[2], white[2] ),
            (1/12, lightblue[2], lightblue[2] ), #blue
            (2.5/12, blue[2], blue[2] ), #blue
            (4/12-(0.002/20), darkblue[2], darkblue[2]),
            
            (5/12, black[2], black[2]), #limite black
            (6/12, black[2], black[2]),
            (7/12, black[2], black[2]), #limite black
             
            (7.5/12, darkorange[2], darkorange[2]),
            (8.5/12, orange[2], orange[2]),
            (9.5/12, lightorange[2], lightorange[2]), 
             
            (11/12, pink[2], pink[2]), #pink
            (1.0, red[2], red[2] )  #red
    ]
    }

    custom_cmap_3 = mcolors.LinearSegmentedColormap('ColormapS2D_2', cdict)
    return custom_cmap_3
