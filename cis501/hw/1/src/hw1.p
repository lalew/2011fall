set autoscale
set ytics 10
set yrange [0 : 100]
set origin 0.0, 0.0
set boxwidth 1 
set style data histogram
set style histogram cluster gap 1
set style fill solid 1.0 border -1
set xlabel "Micro-ops per Macro-op"
set ylabel "Percentage"
set title "Question 1"
set terminal png size 1000,500
set output "q1.png"
plot "q1" using 2:xticlabels(1) notitle , \
     ''   using 0:2:2 with labels center offset 0,1 notitle

set title "Question 2"
set xlabel "Size of Instructions in Bytes"
set output "q2.png"
plot "q2" using 2:xticlabels(1) notitle , \
     ''   using 0:2:2 with labels center offset 0,1 notitle

set title "Question 3"
set yrange [0:120]
set xlabel "Branch Distances"
set output "q3.png"
plot "q3" using 2:xticlabels(1) notitle , \
     ''   using 0:2:2 with labels center offset 0,1 notitle

set title "Question 4"
set yrange [0:100]
set xlabel "Instruction Mix"
set output "q4.png"
plot "q4" using 2:xticlabels(1) notitle , \
     ''   using 0:2:2 with labels center offset 0,1 notitle

set title "hw1b"
set yrange [0:20]
set xlabel "Total Speedup"
set output "q5.png"
plot "q5" using 2:xticlabels(1) title "Speedup", \
     ''   using 0:2:2 with labels center offset 0,1 notitle
