. /opt/intel/oneapi/setvars.sh
ifort -O3 fit_sa_speed_accuracy.f -qopenmp -qmkl

ifort -O3 mlhsa.f -qopenmp -qmkl

ifort -O3 fit_sa_SIMPLEX_Odd.f -qopenmp -qmkl

ifort -O3 fit_sz_speed_accuracy.f -qopenmp -qmkl


./a.out