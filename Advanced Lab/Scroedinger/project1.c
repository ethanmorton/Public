#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

typedef struct complex {
    double real;
    double imaginary;
} complex;

double gaussian(double x) {
    return (1/sqrt(2*M_PI)) * exp((-.5) * pow(x - .5, 2));
}

void initial_conditions(int m, complex values[m], double dx) {
    for (size_t i = 0; i < m; i++)
    {
        // values[i].real = gaussian((double) i);
        // values[i].imaginary = 0;

        values[i].real = sin(dx * i);
        values[i].imaginary = cos(dx * i);
    }
}

complex msd2Func(complex x1t1,
				 complex x0t2,
                 complex x1t2,
                 complex x2t2,
                 complex x1,
                 double dt,
                 double dx,
                 double v,
                 double a,
                 double b) {
	complex out = {0, 0};

	out.real=x1t1.real - (((x2t2.real - x1t2.real) - (x1t2.real - x0t2.real)) / pow(dx,2)) / 2;
	out.imaginary=x1t1.imaginary - 2*dt - (((x2t2.imaginary - x1t2.imaginary) - (x1t2.imaginary - x0t2.imaginary)) / pow(dx,2)) / 2;

	if((x1.real <= b) && (x1.real >= a)) {
		out.real += v * x1t2.real;
		out.imaginary += v * x1t2.imaginary;
	}
	return out;
}

void timeStepToFile (int NumberOfGridpoints, complex wave[NumberOfGridpoints], double x0, double dx, double t,const char filename[]) {
	FILE * f;
	f = fopen(filename,"a");
	
	int i;
	for(i=0;i<NumberOfGridpoints;i++){
		fprintf(f,"%lf %lf %lf %lf %lf %s", t, x0+dx*i, wave[i].real, wave[i].imaginary,
		 pow(wave[i].real,2)+pow(wave[i].imaginary,2), "\n");
	}

	fclose(f);
}

void approximate(int m, complex values[m], complex values_old[m], complex values_oold[m], double dt, double dx, double v, double a, double b) {
    for (size_t i = 1; i < m; i++)
    {
        complex num = {0, 0};
        num = msd2Func(values_oold[i], values_old[i-1], values_old[i], values_old[i+1], values[i], dt, dx, v, a, b);
        // printf("approximating round %zu with val real->%lf imaginary->%lf\n", i, num.real, num.imaginary);
        values[i] = msd2Func(values_oold[i], values_old[i-1], values_old[i], values_old[i+1], values[i], dt, dx, v, a, b); // actual calculation function
    }
}

int main(int argc, char const *argv[])
{
    // time related variables
    double t_start = atof(argv[1]);
    double t_end = atof(argv[2]);
    double dt = atof(argv[3]);
    int timesteps = (t_end - t_start) / dt;

    // x related variables
    double x_start = atof(argv[4]);
    double x_end = atof(argv[5]);
    double dx = atof(argv[6]);
    int x_intervals = (x_end - x_start) / dx;

    //boundary conditions, either 'R' or 'F'
    double bc = atof(argv[7]);

    //get the bounds of the potential wall function, and the value in between
    double a = atof(argv[8]);
    double b = atof(argv[9]);
    double v = atof(argv[10]);
    
    // double hbar = 1.054571817 * pow(10, -34);
    // double m;
    complex values[x_intervals];
    complex values_old[x_intervals];
    complex values_oold[x_intervals];
    complex transit[x_intervals];

    //prep the pervious two timesteps for the new calculation
    initial_conditions(x_intervals, values_old, dx);
    initial_conditions(x_intervals, values_oold, dx);

    printf("Running simulation with values:\n");
    printf("x: %lf -> %lf, dx = %lf, gives %d intervals.\n", x_start, x_end, dx, x_intervals);
    printf("t: %lf -> %lf, dt = %lf, gives %d timesteps.\n", t_start, t_end, dt, timesteps);
    printf("Using %s boundary conditions.\n", bc == 1 ? "reflective" : "fixed" );
    printf("V(x): %lf -> %lf at a wall value of %lf\n", a, b, v);

    for (size_t i = 0; i < timesteps; i++)
    {
        approximate(x_intervals, values, values_old, values_oold, dt, dx, v, a, b);
        if (i % 99 == 0)
        {
            timeStepToFile(x_intervals, values, x_start, dx, t_start + i*dt, "data.txt"); // print to file
        }
        switch ((int) bc)
        {
        case 1:
            values[0] = values[1];
            values[x_intervals-1] = values[x_intervals-2];
            break;
        case 2:
            values[0] = (complex) {0, 0};
            values[x_intervals-1] = (complex) {0, 0};
            break;
        default:
            break;
        }
        memcpy(values_oold, values_old, x_intervals * sizeof(complex));
        memcpy(values_old, values, x_intervals * sizeof(complex));
    }
    printf("Calculation Completed.\n");
    
    return 0;
}