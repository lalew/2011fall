set autoscale
set title "uIPC vs ROB size"
set ylabel "uIPC"
set xlabel "ROB size (log)"
set yrange [1.5:8.5]
set ytics 0.5
set xrange [3:11]
set xtics 1
set terminal png size 1000,500
set output "output.png"
plot "exp1" using 1:2 title "Exp1" with linespoints, \
     "exp2" using 1:2 title "Exp2" with linespoints, \
     "exp3" using 1:2 title "Exp3" with linespoints

set output "output2.png"
plot "exp3" using 1:2 title "Exp3" with linespoints, \
     "exp4" using 1:2 title "Exp4" with linespoints, \
     "exp5" using 1:2 title "Exp5" with linespoints
     
