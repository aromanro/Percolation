set xrange [0:1]
set yrange [0:1]
plot "data2.dat" index 0 u 1:3 w l lt 1 lw 2 lc rgb "green" title "Exact", "data2.dat" index 0 u 1:2:4 w e pt 7 ps 2 lc rgb "red" title "Stats"