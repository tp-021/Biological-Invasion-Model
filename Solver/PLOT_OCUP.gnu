reset


set xrange [*:*]
set yrange [*:*]




plot "SAI_PD/QUANT_POPULACIONAL.dat" using ($1):(1/(2*$9)) with lines lt rgb "red" lw 3 title "TAU"
