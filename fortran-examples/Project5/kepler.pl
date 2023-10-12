set size square
set st da dots
set nokey
set title "Mercury, Beta = 2.1"
set xlabel "x (A.U.)"
set ylabel "y (A.U.)"
set xr [-.75:.75]; set yr [-.75:.75]
# set xr [-75:75]; set yr [-75:75]
set xzeroaxis; set yzeroaxis
plot 'output_kep_mercury-b1.9.dat' u 2:3 # pt 7 ps 0.1 
pause -1
