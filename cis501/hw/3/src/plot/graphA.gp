set autoscale
set title "Graph A"
set xlabel "The number of predictor bits"
set ylabel "Mis-prediction rate"
set xrange [0:22]
set xtics 1
set yrange [0:40]
set ytics 5
set terminal png size 1000,500
set output "grapha.png"
plot "graphA.2.3.5" using 1:($2*100) title "bimodal" with linespoints, \
     "graphA.2.3.5" using 1:($3*100) title "gshare-8" with linespoints, \
     "graphA.2.3.5" using 1:($4*100) title "gshare-n" with linespoints
