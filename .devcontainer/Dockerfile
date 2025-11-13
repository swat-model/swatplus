# Use the official Ubuntu Jammy-based Dev Container image as a starting point
FROM mcr.microsoft.com/devcontainers/base:jammy

# 1. Install dependencies needed to add Intel’s repo, install CMake, gfortran, and GDB
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        wget \
        gpg \
        lsb-release \
        apt-transport-https \
        ca-certificates \
        cmake \
        gdb \
        gfortran \
        && rm -rf /var/lib/apt/lists/*

# 2. Add Intel’s GPG key and repository for oneAPI
RUN wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
    | gpg --dearmor \
    | tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

RUN echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" \
    > /etc/apt/sources.list.d/oneAPI.list

# 3. Install Intel oneAPI Fortran Compiler
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        intel-oneapi-compiler-fortran \
        && rm -rf /var/lib/apt/lists/*

# 4. Source Intel’s setvars script automatically for all users
#    (In a dev container, /etc/bash.bashrc is typically read by non-login shells)
RUN echo "source /opt/intel/oneapi/setvars.sh" >> /etc/bash.bashrc

# 5. Prepend ifx path to the global PATH environment
# ENV PATH=/opt/intel/oneapi/compiler/2025.0/bin:$PATH
# ENV LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2025.0/lib:$LD_LIBRARY_PATH

# That’s it! The container now has ifx + CMake installed and configured.
