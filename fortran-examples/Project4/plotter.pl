# This statement is short for  "set style data points"
set st da l
#
set xlabel 'time (second)'
set ylabel 'phi (radians)'
# Let's change the default position of the legend.
set key off
# The next statement plots the zero axes.
set yzeroaxis
# Set the bounds of the graph (not always necessary)
#set xrange auto
#set yrange auto
#Let's plot some graphs
set title "Phi vs Time"
#set yr [0:4]
#f(x) = 0.2*cos(x)
plot 'outputfile.dat' u 1:2 
#replot f(x) not
pause -1
# The "pause -1" is needed to keep your beautiful picture on the screen until you type somethihng else.
set st da p 
set xlabel 'phi (radians)'
set ylabel 'omega (radians/sec)'
#set xr [1.6:1.63]
#set yr [-2.3:-2.2]
set xr [-pi:pi]
set yr [-4:4]
set size square
set xzeroaxis
set title "Omega vs. Phi"
#set yr [0:8]
plot 'outputfile.dat' u 2:3 ps 0.1 pt 7
pause -1

