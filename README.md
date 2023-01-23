# dijkstra-path-finding
Implementing Dijkstra's Algorithm in Software and Hardware 

# How to run

'''
git clone https://github.com/FPGA-Energy/dijkstra-path-finding
'''

## preflight 
'''
cd dijkstra-path-finding/
make clean
make all
'''

## run and collect logs 
Running the following shell script will run the executable files in the folde /executables.
'''
cd src/scripts/
sh test4RasperryPiC.sh 
'''
this will create a log file (CSV-format) in the directory "results/Raspberry Pi/" with the name 
"runtime_{timestamp}.log" (e.g., "runtime_1674480168748365508.log").

'''
[Mon 23 Jan 2023 02:22:48 PM CET]
../../executables/dijkstradd64.exe;9416
../../executables/dijkstradd64.exe;9438
...
'''

# Notes
Dijkstra2.c repeats evaluating a route from index 0 to index n-1 in a graph

It prints out a number (added wight of the route after 500000 iterations
Measured on an ordinary pc (compiled with -O3) it takes 3,5 second corresponding to 6800 ns for a route

Input networks:
- sd: sparse directed
- sud: sparse undirected
- dd: dense directed
- dud: dense undirected

# Compilation
gcc dijkstrasd64.c -O3 -o dijkstrasd64.exe
gcc dijkstradud64.c -O3 -o dijkstradud64.exe
gcc dijkstradd64.c -O3 -o dijkstradd64.exe
gcc dijkstrasud64.c -O3 -o dijkstrasud64.exe

# Execution:
The program will calculate 10 * 500.000 routes switching between the n possible start nodes

It'll output: 
done <accumulated length of routes so result of it is needed and not optimized away>
time <time in sec for 500k routes> <accumulated time in sec> < time for one route in mikroseconds>


