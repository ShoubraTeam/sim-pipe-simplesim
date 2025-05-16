# Understanding how pipelines function using SimpleScalar

## Overview
This project explores and simulates pipeline functionality using SimpleScalar, a popular computer architecture simulator. The simulation helps understand how different pipeline stages work and their impact on processor performance. This includes forwarding and without forwarding compatibility.

## Course Details and Team Members
- **University**: Benha University
- **College**: Shoubra Faculty of Engineering
- **Department**: Computer Engineering
- **Course**: Computer Architecture
- **Professor**: Dr. May Salama
- **Project**: Understanding how pipelines function using SimpleScalar
- **Team Members**:
  - Karim Wael
  - Fady Youssef
  - Abdelrahman Osman
  - Hanin Mustafa
  - Sara Alaa

## Definition of Simulation Problem
The simulation focuses on analyzing and understanding:
- Pipeline stages and their interactions
- Performance metrics including the number of cycles and the number of stalls
- Impact of different pipeline configurations (without forwarding and with forwarding)
- Data hazards (Read After Write — RAW)

## Problems Encountered
1. **Configuration Issues**
   - Initial setup challenges with SimpleScalar
   - Compatibility issues with different benchmark programs (for example, invalid inst)

2. **Performance Analysis**
   - Difficulty in collecting accurate performance metrics
   - Challenges in displaying simulation results in an organized way

3. **Pipeline Hazards**
   - Complex scenarios involving data hazards (Read After Write — RAW)

## How We Solved Them
1. **Configuration Solutions**
   - Added more configuration to support forwarding and without forwarding

2. **Performance Analysis**
   - Created a more organzied format to display the different instructions issuing & cycles they occur in

3. **Pipeline Hazard Resolution**
   - Implemented forwarding paths vs without forwarding to see the difference between the two

## Output Samples
### Sample 1: Without Forwarding
![sample-instructions-1](https://i.ibb.co/qYGZwVQf/image.png)
![sample-stats-1](https://i.ibb.co/Hf4KD9Q6/image.png)

### Sample 2: With Forwarding
![sample-instructions-2](https://i.ibb.co/Swz1rzc4/image.png)
![sample-stats-2](https://i.ibb.co/xSwxTdJ4/image.png)

## How to Run
1. Clone the repository using:
    ```bash
    git clone https://github.com/ShoubraTeam/sim-pipe-simplesim.git
    ```
2. Build the project using:
   ```bash
   make
   ```
3. Run the simulation without forwarding:
   ```bash
   ./sim-pipe2 -v -max:inst 10 -forward false tests-pisa/bin.little/test-math
   ```
4. Run the simulation with forwarding:
   ```bash
   ./sim-pipe2 -v -max:inst 10 -forward true tests-pisa/bin.little/test-math
   ```

## Dependencies
- SimpleScalar v3.0
- GCC compiler
- Make utility

