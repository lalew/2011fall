set autoscale
set ytics 10
set origin 0.0, 0.0
set boxwidth 1 
set style data histogram
set style histogram cluster gap 1
set style fill solid 1.0 border -1
set xlabel "Micro-ops per Macro-op"
set ylabel "Percentage"
set terminal png size 1000,500
set yrange [0:20]
set xlabel "Total Speedup"
set output "q5.png"
plot "q5" using 2:xticlabels(1) title "Speedup", \
     ''   using 0:2:2 with labels center offset 0,1 notitle
