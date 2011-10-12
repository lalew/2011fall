set autoscale
set xtics 1
set ytics 0.2
set xrange [0:16]
set yrange [1:5]
set xlabel "Load latency"
set ylabel "CPI"
set terminal png size 1000,500
set output "q2_3.png"
plot "q2_3" using 1:2 title "Estimated CPI" with linespoints,\
     "q2_3" using 1:3 title "Simulated CPI" with linespoints
