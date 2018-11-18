popSize = [];

for t = 1 : length(dynamics) 
  popSize(end+1, :) = length(dynamics{t}) - 1;
endfor 

% needed for octave on Fedora
graphics_toolkit("gnuplot");

figure;
plot(popSize);
title ('Population Size');
xlabel('Time');
ylabel('Size');
grid on;