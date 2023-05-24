. /opt/intel/oneapi/setvars.sh
ulimit -s unlimited
ifort -O3 fit_sa_SIMPLEX_10.f -qopenmp -qmkl
./a.out '.e.fast-dm.csv'