source /opt/intel/oneapi/setvars.sh
rm -r build
if [ "$#" -eq 0 ]; then
  compiler="gfortran"
  echo Creating build files for $compiler compiler
  echo
  echo cmake -B build 
  echo
  cmake -B build 
else 
  compiler=$1
  echo Creating build files for $compiler compiler
  echo
  echo cmake -B build -D CMAKE_Fortran_COMPILER=$compiler
  echo
  cmake -B build -D CMAKE_Fortran_COMPILER=$compiler
fi
echo
echo "Building swatplus executable with $compiler compiler"
echo
cmake --build build -j 12
echo
echo "Finished building swatplus.exe with $compiler compiler"
echo

