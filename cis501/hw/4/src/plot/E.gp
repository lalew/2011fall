set autoscale
set title "Graph E"
set xlabel "Way predictor size (bits)"
set ylabel "Mis-prediction rate (%)"
set xrange [0:16]
set xtics 1
set yrange [0:40]
set ytics 5
set terminal png size 1000,500
set output "graphe.png"
plot "graphE" using 1:($2*100) title "32KB two-way cache with 64B block and way predictor" with linespoints
