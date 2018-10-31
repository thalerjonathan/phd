% plotting the last 
t = length( dynamics );
ages = [];

for ai = 2 : length (dynamics{t})
  %a = dynamics{t}{ai};
  %agentId = dynamics{t}{ai}(1);
  agentAge = dynamics{t}{ai}(2);
  %agentSug = dynamics{t}{ai}(3);
  %agentMet = dynamics{t}{ai}(4);
  %agentVis = dynamics{t}{ai}(5);
  
  ages(end+1,:) = agentAge;
endfor
  
% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

hist(ages);
