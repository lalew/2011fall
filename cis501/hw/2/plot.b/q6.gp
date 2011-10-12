set autoscale
set ylabel "Dollars per billion transistors"
set xlabel "Number of transistors(in billions)"
set terminal png size 1000,500
set output "q6.png"
plot "q6" u ($1/1000):2 title "cost for 5m transistors per 1mm^2" with linespoints, \
     "q6" u ($1/1000):3 title "with additional cost" with linespoints,\
     "q9" u ($1/1000):2 title "cost for 10m transistors per 1mm^2" with linespoints
     
