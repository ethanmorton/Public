set title "Jupiter Mass x1000"
set xlabel "x (A.U.)"
set ylabel "y (A.U.)"
set size square
set key outside right
set xzeroaxis
set yzeroaxis
set xrange [-7.5:7.5]
set yrange [-7.5:7.5]
plot "output_3Body.dat" u 2:3 t 'Earth orbit' w l, \
     "output_3Body.dat" u 4:5 t 'Jupiter orbit' w l , \
     "-" t 'Center (SUN)' w p lt 1 pt 7 ps 0.5
     0 0
     e
pause -1
 
