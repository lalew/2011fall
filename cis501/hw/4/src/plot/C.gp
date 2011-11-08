set autoscale
set title "Graph C"
set xlabel "Block sizes (bit)"
set ylabel "Miss rate (%)"
set xrange [2:10]
set xtics 1
set yrange [0:3]
set ytics 0.5
set terminal png size 1000,500
set output "graphc.png"
plot "graphCD" using 1:($2*100) title "Two-way 32KB cache" with linespoints

set title "Graph D"
set ylabel "Traffic (bytes/op)"
set yrange [0:18]
set ytics 1
set output "graphd.png"
plot "graphCD" using 1:3 title "Two-way 32KB cache" with linespoints
