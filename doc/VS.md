## Developing with Visual Studio

### Software Package Installation

Install the required packages in the order below. The Intel compiler requires Visual Studio to be installed first. Include the two workloads (C++ and Python) to add needed support for `cmake` and testing. The Intel package will install the `ifort` (soon to be deprecated) and `ifx` compiler. Although Visual Studio bundles Git, a separate install is needed to support version tagging during the `cmake` build.

- Install the latest **Visual Studio 2022** (17.2.3) Community edition (https://visualstudio.microsoft.com/vs/), include the **Desktop development with C++** and **Python** workload

- Install the **Intel Fortran Compiler Classic and Intel Fortran Compiler for Windows** package from Intel
  
  ([Intel® Fortran Compiler](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html))

- Install **Python** from the MS App store and **Git** from ([https://git-scm.com](https://git-scm.com) )

### Starting Visual Studio

- Open a Windows terminal by pressing `Windows+r` and run `cmd`

- In the terminal run the script `C:\"Program Files (x86)"\Intel\oneAPI\setvars.bat`

- In the same (!) terminal run the command `devenv`. This will start Visual Studio with the proper compiler settings.

### Cloning the SWAT+ repository from GitHub

Clone the SWAT+ repository from GitHub ...
