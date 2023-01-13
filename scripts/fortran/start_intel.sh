. /opt/intel/oneapi/setvars.sh
ifort -O3 test_blair.f -qopenmp -qmkl
./a.out