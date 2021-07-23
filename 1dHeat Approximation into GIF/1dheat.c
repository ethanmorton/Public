#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include "gifenc.h"
// all credit for the gif encoder goes to github users lecram, jacobgelling, and wuerfel21 :: https://github.com/lecram/gifenc


/**
 * @brief fills an array with step size dx at position i and applying sin(2 * pi * i * dx)
 * 
 * @param m length of array
 * @param arr array
 * @param dx step size (used as x = i * dx)

--initialize_x takes a pointer to an array of size m and a step size of dx, and returns a pointer to an array of x stepping by dx

--ie, takes a pointer to an array u_0[101], and returns a pointer to an array of x from [0,1] with step .01
*/
void initialize_x(int m, double arr[m], double dx)
{
    for (size_t i = 0; i < m; i++)
    {
        arr[i] = (double) i / (double) m;
    }
}

/**
 * @brief prints each value of an array
 * 
 * @param m length of array
 * @param arr array

--takes in a pointer to an array of size m, and prints out each element on a new line
*/
void print_arr(int m, double arr[m])
{
    for (size_t i = 0; i < m; i++)
    {
        printf("arr[%zu] :: %lf \n", i, arr[i]);
    }
}

/**
 * @brief writes a color to every pixel in the GIF frame array
 * 
 * @param gif pointer to the gif struct 
 * @param m index of color from the color palette in the gif 
*/
void initialize_frame(ge_GIF *gif, int m)
{
    for (size_t i = 0; i < ((gif->w) * gif->h); i++)
    {
        gif->frame[i] = m;
    }
    
}

/**
 * @brief draws a straight horizontal line across the frame
 * 
 * @param gif pointer to the gif struct
 * @param y y value where the line will be drawn
 * @param m color of the line being drawn, from gif palette
*/
void draw_line_hor(ge_GIF *gif, int y, int m)
{
    for (size_t i = 0; i < gif->w; i++)
    {
        gif->frame[y * gif->h + i] = m;
    }   
}

/**
 * @brief draws a straight line vertically along the frame
 * 
 * @param gif pointer tto gif struct
 * @param x x value where the line will be drawn
 * @param m color of the line being drawn, from gif palette
*/
void draw_line_vert(ge_GIF *gif, int x, int m)
{
    for (size_t i = 0; i < gif->h; i++)
    {
        gif->frame[i * gif->w + x] = m;
    }
}

void draw_line_p2p(ge_GIF *gif, int x1, int y1, int x2, int y2, int m)
{
    double slope = (y2 - y1) / (x2 - x1);
    for (size_t i = 0; i < x2 -x1; i++)
    {
        /*          |original point|     |added length for next point|     */
        gif->frame[(y1 * gif->w + x1) + (i + (int)(i * slope * gif->w))] = m;
    }
}

int main(int argc, char const *argv[])
{
    clock_t begin = clock();
    /*
    initialize constants -- eventually these will come from argc and *argv, for use in commandline

    x_domain   = [0,n]
    k          = coefficient of heat transfer, defaults to 1.0 to simplify
    xintervals = number of spaces between nodes
                 ie, 100 intervals -> 101 nodes -> u_0[101]
    dx         = the distance between each x node
    tend       = time to end approximation at
    dt         = time step
    timesteps  = number of times the approximation will be calculated
                 ie, tend = .1 and dt = .00001 -> 10000 timesteps -> for i in range(10000):
    coef       = mathematical coefficient, result of approximation
    */
    double x_end = 1.0;
    double x_start = 0.0;
    double k = 1.0;
    double xintervals = 100.0;
    size_t n = (int) (xintervals + 1);
    double dx = x_end - x_start / xintervals;
    double tend = .1;
    double dt = .00001;
    int timesteps = tend / dt;
    double coef = (k * dt) / pow(dx, 2.0);
    int ymax = 1;
    
    double u_0[n];
    double u_old[n];
    double u_new[n];


    /*Set up gif encoder*/
    uint8_t palette[] = {
            0x00, 0x00, 0x00, /* 0 -> black */
            0xFF, 0x00, 0x00, /* 1 -> red   */
            0xFF, 0xFF, 0xFF, /* 2 -> white */
            0x00, 0x00, 0xFF, /* 3 -> blue  */
    };

    int res_width = 960;
    int res_height = 960;
    int left_buffer = 100;
    /*bottom buffer needs to be subtracted because the gif writes top to bottom. So the bottom is res_height, and up is down, and 100 pixels up from the bottom is res_height - 100*/
    int bottom_buffer = res_height - 100;
    int pallette_depth = 2;
    int loop = 0;
    double xint_length = (res_width - left_buffer) / xintervals;
    double yint_length = bottom_buffer / ymax;

    ge_GIF *gif = ge_new_gif(
        "1dheat.gif",
        res_width, res_height,
        palette,
        pallette_depth,
        loop
    );

    initialize_x(xintervals + 1, u_0, dx);

    memcpy(u_old, u_0, n * sizeof(double));
    memcpy(u_new, u_0, n * sizeof(double));
    
    for (size_t i = 0; i < timesteps; i++)
    {
        for (size_t j = 1; j < (xintervals); j++)
        {
            u_new[j] = coef * (u_old[j+1] - 2*u_old[j] + u_old[j-1]) + u_old[j];
        }

        if (i % 999 == 0)
        {
            initialize_frame(gif, 2);
            draw_line_hor(gif, bottom_buffer, 0);
            draw_line_vert(gif, left_buffer, 0);
            
            /*write graph*/
            for (size_t i = 0; i < n; i++)
            {
                draw_line_p2p(gif, left_buffer + i * xint_length, bottom_buffer - u_new[i] * yint_length, left_buffer + (i + 1) * xint_length, bottom_buffer - u_new[(i + 1)] * yint_length, 3);
            }

            ge_add_frame(gif, 1);
        }

        memcpy(u_new, u_old, n * sizeof(double));
    }

    print_arr(xintervals + 1, u_old);
    print_arr(xintervals + 1, u_new);

    ge_close_gif(gif);
    
    clock_t end = clock();
    double time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("took %f seconds \n", time_spent);
    
    return 0;
}
