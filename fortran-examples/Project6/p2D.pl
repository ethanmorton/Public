set grid
set contours
set cntrparam levels incremental -1, 0.2, 1
set colorbox user origin 0.91,0.05
set xr [-10:10]
set yr [-10:10]
set zr [-1:1]
set xlabel "x"                                            
set ylabel "y"
set zlabel "Potential (V)"
set zlabel offset -3
set view 75,335
set title "Jacobi Method"
splot "output_j.dat" u 1:2:3 w l palette
pause -1
set view map
unset surface
unset colorbox
set size square
set xr [-10:10]
set yr [-10:10]
set key outside
set title "Equipotential Lines for a Parallel Plate Capacitor"
rep
pause -1