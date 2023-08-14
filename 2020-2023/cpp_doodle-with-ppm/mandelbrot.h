#pragma once
#include "ppm.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <unistd.h>

auto const configname = "../mandelbrot.txt";
auto const filename = "../img/mandelbrot.ppm";

namespace manderbolt {

#define TO_REAL(x, width, min, max) (x * ((max - min) / width) + min)
#define TO_IMG(y, height, min, max) (y * ((max - min) / height) + min)

int find(double creal, double cimg, int n) {
    int i = 0;
    double zr = 0.0, zi = 0.0;

    for (; i < n && zr * zr + zi * zi < 4.0; ++i) {
        double tmp = zr * zr - zi * zi + creal;
        zi = 2.0 * zr * zi + cimg;
        zr = tmp;
    }

    return i;
}

inline void draw() {
    int width, height, max_n;
    double min_r, max_r, min_i, max_i;

    {
        std::ifstream fin(configname);
        if (!fin) {
            std::cout << "Can't open the file" << std::endl;
            std::cin.ignore();
            return;
        }

        fin >> width >> height >> max_n;
        fin >> min_r >> max_r >> min_i >> max_i;
    }

    {
        std::ofstream fout(filename);
        fout << "P6" << std::endl;
        fout << width << " " << height << std::endl;
        fout << "255" << std::endl;

        for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                double creal = (x * ((max_r - min_r) / width) + min_r);
                double cimg = (y * ((max_r - min_r) / width) + min_r);

                int n = find(creal, cimg, max_n);
                char rgb[] = { (char)(n % 255), (char)(n % 255),
                               (char)(n % 255) };

                // P6 format store pixels in bytes.
                fout.write(rgb, sizeof(rgb) / sizeof(rgb[0]));
            }
        }
    }

    {
        std::stringstream ss;
        ss << "convert " << filename << " " << filename << ".png";
        system(ss.str().c_str());
    }
    std::cout << "finished\n";
}

} // namespace manderbolt
