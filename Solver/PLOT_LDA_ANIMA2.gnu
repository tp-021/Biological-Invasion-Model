reset

#set view map scale 1
set tics font ", 20"
set xrange [*:*]
set yrange [*:*]
set zrange [0:11]
set cbrange [0:11]
set pm3d
#set contour surface
#set view 90, 90, 1, 1

#set pal gray #negative

#set palette defined ( 0 "white", 1 "white", 2 "white", 3 "black" )
#set palette defined ( 0 "white", 1 "white") #, 2 "orange", 3 "red" )
#set palette maxcolors 2 ; set palette defined ( 0 'light-blue', 1 'light-red') # , 2 '#2bbe00', 3 '#ee0000')

unset colorbox

#set pm3d interpolate 1,1 flush begin noftriangles border linecolor rgb "black"  linewidth 0.5 dashtype solid corners2color mean

#splot "SPLINE/condinic2.png" binary filetype=png dx=1 dy=1 with rgbimage notitle

#splot "SAI_PD/dist.dat" using 1:2:3 with pm3d
#splot "SAI_PD/S2D000100.dat" using 1:2:3 with pm3d

#rep     "SAI_PD/splineparE.dat" using 1:2:(1) with lines lt rgb "blue" lw 5 notitle
#rep     "SAI_PD/splineparD.dat" using 1:2:(1) with lines lt rgb "red" lw 5 notitle

do for [i=1:200] { splot sprintf('SAI_PD/LAMBDA/LDA%0.6d.dat', i) using 1:2:3 with pm3d; pause 0.01 }
