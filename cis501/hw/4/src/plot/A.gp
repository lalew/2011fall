set autoscale
set title "Graph A"
set xlabel "Cache sizes (log)"
set ylabel "Cache miss rate (%)"
set xrange [7:23]
set xtics 1
set yrange [0:50]
set ytics 5
set terminal png size 1000,500
set output "grapha.png"
plot "graphA" using 1:($2*100) title "Direct-mapped" with linespoints,\
     "graphA" using 1:($3*100) title "Two-way set assoc" with linespoints
