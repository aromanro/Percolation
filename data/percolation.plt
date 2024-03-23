set xrange [0.55:0.65]
set yrange [0:1]
plot "percolation.dat" index 0 u 1:2:3 w e pt 7 ps 2 lc rgb "red" title "10", "percolation.dat" index 1 u 1:2:3 w e pt 7 ps 2 lc rgb "green" title "40", "percolation.dat" index 2 u 1:2:3 w e pt 7 ps 2 lc rgb "blue" title "160", "percolation.dat" index 3 u 1:2:3 w e pt 7 ps 2 lc rgb "magenta" title "640"