# Elastic Wave Modeling: Tsunami Simulation

This repository contains a Fortran-based code structure for simulating elastic wave propagation using nonlinear wave modeling. The code is designed with flexibility in mind, allowing for multiple sources, user-defined boundary conditions, and the ability to insert sources at different locations and times. This framework is particularly suited for applications such as tsunami modeling and other geophysical wave simulations.

## Features

- **Elastic Wave Propagation**: Simulates the propagation of elastic waves in 2D and 3D environments.
- **Nonlinear Wave Modeling**: Incorporates nonlinear effects to capture realistic wave behavior.
- **Multiple Sources**: Supports multiple wave sources with customizable positions and timing.
- **User-Defined Boundary Conditions**: Allows users to specify boundary conditions according to their requirements.
- **Modular Structure**: Organized into separate files for better readability and maintainability.

---

## Repository Structure

### Files

- **`BathtubModel.m`**: A MATLAB script used for post-processing or visualization of simulation results.
- **`Geo11.f`**: A Fortran module containing geophysical parameter definitions and wave propagation algorithms.
- **`Separate.f`**: A Fortran module dedicated to source separation and integration routines.
- **`README.md`**: This file, providing an overview of the repository and usage instructions.

---

## Requirements

- **Fortran Compiler**: The code has been tested with GNU Fortran (gfortran) but should work with any modern Fortran compiler.
- **MATLAB**: For visualization and post-processing using the provided `BathtubModel.m` script.
- **Operating System**: Linux, macOS, or Windows with a compatible Fortran compiler.

---

## Usage Instructions

### 1. Clone the Repository
```bash
git clone https://github.com/VTeknik/Elastic-Wave-modeling-Tsunami-.git
cd Elastic-Wave-modeling-Tsunami-
```

### 2. Compile the Fortran Code
Compile the Fortran source files using a Fortran compiler:
```bash
gfortran -o elastic_wave Geo11.f Separate.f
```

### 3. Run the Simulation
Execute the compiled program:
```bash
./elastic_wave
```
Follow the prompts to input simulation parameters or modify the source files directly for custom configurations.

### 4. Post-Process Results
Use the MATLAB script `BathtubModel.m` to visualize the results:
```matlab
run('BathtubModel.m')
```

---

## Customization

1. **Defining Sources**: Edit the `Separate.f` file to define multiple sources and specify their locations and timing.
2. **Boundary Conditions**: Modify `Geo11.f` to implement custom boundary conditions.
3. **Simulation Parameters**: Adjust the physical and numerical parameters in the `Geo11.f` file to suit your simulation needs.

---

## Example

A sample configuration is provided in the default setup. Run the simulation to observe elastic wave propagation from multiple sources with predefined boundary conditions.

---

## Contributing

Contributions are welcome! Please follow these steps:
1. Fork the repository.
2. Create a new branch for your feature or bugfix.
3. Submit a pull request with a detailed explanation of your changes.

---

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

---

## Contact

For questions or issues, please contact [Vahid Teknik](mailto:vahid.teknik@gmail.com).

