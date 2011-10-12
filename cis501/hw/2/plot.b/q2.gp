set autoscale
set title "Question 2"
set xlabel "Die area of the chip"
set ylabel "Yield"
set xtics 25
set ytics 10
set xrange [0:500]
set yrange [0:100]
set format y "%g %%"
set terminal png size 1000, 500
set output "q2.png"
plot "q2" using 1:($2*100) title "yield" with linespoints
