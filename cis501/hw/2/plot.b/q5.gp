set autoscale
set title "Question 5"
set xlabel "Die area of the chip"
set ylabel "Per-die cost"
set xtics 25
set xrange [0:500]
set terminal png size 1000,500
set output "q5.png"
plot "q5" using 1:2 title "cost" with linespoints
