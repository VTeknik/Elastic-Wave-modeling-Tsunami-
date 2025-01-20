# Elastic Wave Propagation Simulation using Nonlinear Wave Modeling

## Overview
This repository provides a Fortran-based code structure for simulating elastic wave propagation using nonlinear wave modeling techniques. The code is designed with flexibility and scalability in mind, allowing users to model wave propagation scenarios with multiple sources, custom source insertion at different locations and times, and user-defined boundary conditions.

## Features
- **Nonlinear Wave Modeling**: Simulates complex elastic wave behaviors in nonlinear media.
- **Multiple Source Support**: Add multiple wave sources to the simulation.
- **Custom Source Insertion**: Define specific locations and times for each source.
- **User-Defined Boundary Conditions**: Tailor the simulation boundaries to match your experimental or theoretical setup.
- **Efficient and Modular Codebase**: The code is structured for easy customization and scalability.

## Prerequisites
- Fortran compiler (e.g., `gfortran`, `ifort`, etc.)
- A compatible build system, such as `make`
- Basic knowledge of Fortran and numerical modeling techniques

## Installation
1. Clone the repository:
    ```bash
    git clone https://github.com/username/elastic-wave-simulation.git
    cd elastic-wave-simulation
    ```
2. Compile the code using the provided `Makefile`:
    ```bash
    make
    ```
3. Ensure the executable file (`wave_simulation.out`) is created in the root directory.

## Usage
### Input Files
- **`input_parameters.dat`**: Defines simulation parameters such as grid size, time step, material properties, and boundary conditions.
- **`source_data.dat`**: Specifies the number, locations, and activation times of wave sources.

### Running the Simulation
Run the compiled executable:
```bash
./wave_simulation.out
```

### Output Files
The simulation generates output files for analysis:
- **`wavefield_data_*.dat`**: Snapshots of the wavefield at specified time intervals.
- **`boundary_condition_logs.dat`**: Logs of the applied boundary conditions.

## Code Structure
- **`main.f90`**: Entry point of the program.
- **`modules/`**: Contains reusable modules for numerical methods, material properties, and boundary conditions.
- **`sources/`**: Handles the source terms for wave excitation.
- **`boundaries/`**: Implements user-defined boundary conditions.
- **`output/`**: Contains routines for writing simulation results to files.

## Customization
### Adding New Boundary Conditions
1. Open `boundaries/user_defined_boundary.f90`.
2. Implement the desired boundary condition in the provided template.
3. Recompile the code using `make`.

### Modifying Source Configurations
Edit `source_data.dat` to specify custom source locations and activation times. The format is as follows:
```
Number_of_sources
Source_X Source_Y Activation_Time Amplitude
...
```

## Example
An example setup is provided in the `examples/` directory. Run it using:
```bash
cp examples/input_parameters_example.dat input_parameters.dat
cp examples/source_data_example.dat source_data.dat
./wave_simulation.out
```

## Contributing
Contributions are welcome! Please follow these steps:
1. Fork the repository.
2. Create a new branch for your feature or bug fix.
3. Submit a pull request with a detailed description of the changes.

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact
For questions or issues, please contact:
- **Name**: Vahid Teknik
- **Email**: [vahid.teknik@gmail.com](mailto:vahid.teknik@gmail.com)

Enjoy simulating elastic wave propagation!

