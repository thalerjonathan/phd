carryingCap = [];

for t = 1 : length( dynamics ) 
  carryingCap(end+1, :) = length (dynamics{t}) - 1;
endfor 

% needed for octave on Fedora
graphics_toolkit("gnuplot");

plot(carryingCap);