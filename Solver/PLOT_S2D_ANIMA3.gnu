reset


set tics font ", 20"
set xrange [-10:1290]
set yrange [-10:970]
set zrange [0:1.2]
set pm3d
set contour surface
set terminal pngcairo size 800,600
set view 38, 15, 1, 1

unset colorbox



do for [i=0:500] { outfile = sprintf('ANIMA/GRAF%0.6d.png',i);
set output outfile;
splot sprintf('SAI_PD/S2D%0.6d.dat', i) using 1:2:3 with pm3d;

}
