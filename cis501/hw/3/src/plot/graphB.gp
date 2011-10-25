set autoscale
set title "Graph B"
set xlabel "History register length (bits)"
set ylabel "Mis-prediction rate (%)"
set xrange [0:22]
set xtics 1
set yrange [0:40]
set ytics 5
set terminal png size 1000,500
set output "graphb.png"
plot "graphB.4" using 1:($2*100) title "2^10" with linespoints, \
     "graphB.4" using 1:($3*100) title "2^16" with linespoints
