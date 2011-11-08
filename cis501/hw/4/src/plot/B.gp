set autoscale
set title "Graph B"
set xlabel "Cache Sizes (bit)"
set ylabel "Total data traffic per memory operation (bytes/op)"
set xrange [7:23]
set xtics 1
set yrange [0:40]
set ytics 5
set terminal png size 1000,500
set output "graphb.png"
plot "graphB" using 1:2 title "Write-through cache" with linespoints,\
     "graphB" using 1:3 title "Write-back cache" with linespoints
