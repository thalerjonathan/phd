t = length(dynamics);
ages = [];

for ai = 2 : length (dynamics{t})
  agentAge = dynamics{t}{ai}(2){1};
  
  ages(end+1,:) = agentAge;
endfor
  
% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

figure;
hist(ages);
title('Age Distribution');