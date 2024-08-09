reset


set xrange [*:*]
set yrange [*:*]

set key outside bottom center maxrows 2 font ", 15"


plot "SAI_PD/QUANT_POPULACIONAL.dat" using 1:3 with lines lt rgb "red" lw 3 title "Reativa",\
     "SAI_PD/QUANT_POPULACIONAL.dat" using 1:4 with lines lt rgb "blue" lw 3 title "Temporal",\
     "SAI_PD/QUANT_POPULACIONAL.dat" using 1:5 with lines lt rgb "green" lw 3 title "Difusiva X",\
     "SAI_PD/QUANT_POPULACIONAL.dat" using 1:6 with lines lt rgb "yellow" lw 3 title "Difusiva Y"
