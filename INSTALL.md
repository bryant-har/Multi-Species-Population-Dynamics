# Addendum for Windows
If you are running on Linux or Mac, skip to [Install Instructions](#install-instructions) 

Otherwise, if you are running on Windows, you need to install XMing to give
WSL something to run the GUI on. Here are instructions for that:

## 1A 
Install XMing.
## 2A
run [export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0.0]

- 3a If Xming still refuses to open the GUI in later steps, you may have to go to X0.hosts in
    C:\Program Files (x86)\Xming and open the XMing logs, find the IP being rejected,
    then add that IP to X0.hosts.

## 3A 
Continue with install instructions below.

# Install Instructions
Steps to launch MS2 Demo:

## 1
Clone the remote repository onto your computer from 
https://github.coecis.cornell.edu/jjg283/cs3110-final-project.git
## 2
cd into the top level of the repository you just cloned.
## 3
run [opam install lablgtk3] to install our GUI dependency.
## 4 
run [make simulate] to launch the simulator.
## 5
Once the graphs are generated in a new window, close the window to quit the simulator.

# Example Inputs
Here are some good example inputs of actual population models!

## 1) Herbivore-Predator Model (with Refuge for Predators)

Number of Species: 2

Species Names: H, P

Parameters: rH, rP, gHP, gPH, R

Diff eqs: 

    dH / dt = rH * H - gHP * (H-R) * P

    dP / dt = gPH * (H-R) * P

Parameter Values: rH= 0.9, rP = 8, gHP = .30, gPH = 0.08, R = 50

Init. Pops: H = 260, P = 2

Duration: 10

Timestep: 0.02

#

## 2) SIR Model for infectious disease (Susceptible, Infectious, Recovered)

Number of Species: 3

Species Names: S, I, R

Parameters: b, m, n, g

Diff eqs:

    dS / dt = - b * S * I - m * S

    dI / dt = b * S * I - (m + n) * I - g * I

    dR / dt = g * I - m * R

Parameter Values: b = 0.0001, m = 0.0001, n= 0.05, g = 0.005

Init. Pops: S = 5000, I = 100, R = 1

Duration: 30

Timestep: 0.02

#

## 3) Ecological model with four species - Herbivores with Refuges, Predator, Competition, Mutualism, and Amensalism.
Number of Species: 4

Species Names: A, B, C, D

Parameters: N/A

Diff eqs:

    dA / dt = 0.9 * A - 0.0005 * A * B + 0.0001 * A * C - 0.20 * (A - 30) * D

    dB / dt = - 0.0003 * A * B + 0.8 * B - 0.15 * (B - 50) * D

    dC / dt = 0.0002 * A * C - 0.0006 * B * C + 0.95 * C - 0.05 * (C - 80) * D

    dD / dt = 0.20 * (A - 30) * D + 0.15 * (B - 50) * D + 0.05 * (C - 80) * D - 15 * D

Parameter Values: N/A

Init. Pops: A = 50, B = 60, C = 100, D = 2

Duration: 5

Timestep: 0.005

#

Have fun demoing!