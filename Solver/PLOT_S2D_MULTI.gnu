reset
#set view map scale 1
set pm3d
do for [i=1:200] {

clear
set terminal pngcairo size 1280,720
outfile = sprintf('SAI_PD/ANIMA/GRAF%0.6d.png',i);
set output outfile


set multiplot layout 2,2


set view 60, 30, 1, 1
set xrange [*:*]
set yrange [*:*]
set zrange [0:4]
set cbrange [0:4]
set title "S2D"
splot sprintf('SAI_PD/S2D%0.6d.dat', i) using 1:2:3 with pm3d


set view map scale 1
set xrange [*:*]
set yrange [*:*]
set zrange [0:*]
set cbrange [0:5]
set title "LAMBDA"
splot sprintf('SAI_PD/LAMBDA/LDA%0.6d.dat', i) using 1:2:3 with pm3d


set title "TAU"
set xrange [*:*]
set yrange [*:*]
set zrange [0:*]
set cbrange [0:*]
splot sprintf('SAI_PD/LAMBDA/LDA%0.6d.dat', i) using 1:2:(1/(2*$3)) with pm3d


set title "VS"
set xrange [*:*]
set yrange [*:*]
set zrange [0:70]
set cbrange [0:70]
splot sprintf('SAI_PD/VS/VS%0.6d.dat', i) using 1:2:3 with pm3d


#SALVA


#LIMPA
unset multiplot

}
