#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define a 1.0
#define v 1.0
#define dx 0.1
#define dt 0.1

typedef struct{
	double re;
	double im;
}complex;

complex *msd2Func(complex *x1t1,
				  complex *x1t2, 
				  complex *x0t2, 
				  complex *x2t2, 
				  double x1);
						
						
void timeStepToFile (complex *wave, int NumberOfGridpoints, double x0, double t, const char *filename);
						
complex *msd2Func(complex *x1t1,
				  complex *x1t2, 
				  complex *x0t2, 
				  complex *x2t2,
				  double x1)  {
	complex *out = malloc(sizeof(complex));
	out->re=x1t1->re - (((x2t2->re - x1t2->re) - (x1t2->re - x0t2->re)) / pow(dx,2))/2;
	out->im=x1t1->im - 2*dt - (((x2t2->im - x1t2->im) - (x1t2->im - x0t2->im)) / pow(dx,2))/2;
	if((x1<=a) && (x1>=0)){
		out->re+=v*x1t2->re;
		out->im+=v*x1t2->im;
	}
	return out;
}

void timeStepToFile (complex *wave,int NumberOfGridpoints, 
						double x0, double t,const char *filename){
	FILE * f;
	f = fopen(filename,"a");
	
	int i;
	for(i=0;i<NumberOfGridpoints;i++){
		fprintf(f,"%d %d %d %d %d %s", t, x0+dx*i, wave[i].re, wave[i].im,
		 pow(wave[i].re,2)+pow(wave[i].im,2), "\n");
	}
	
	fclose(f);
}

int main(int argc, char** argv) {
	printf("Hello world!\n");
	return 0;
}

