meanWealth = [];

for t = 1 : length( dynamics ) 
  time = dynamics{t}{1};
  mw = [];
  
  for ai = 2 : length (dynamics{t})
    %a = dynamics{t}{ai};
    %agentId = dynamics{t}{ai}(1);
    %agentAge = dynamics{t}{ai}(2);
    agentSug = dynamics{t}{ai}(3);
    %agentMet = dynamics{t}{ai}(4);
    %agentVis = dynamics{t}{ai}(5);
    
    mw(end+1,:) = agentSug;
  endfor
  
  meanWealth(end+1,:) = mean (mw);
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

plot(meanWealth);
title ('Average Wealth');
xlabel ('Time');
ylabel ('Average');
% octave seems to give the lines in the legend different colors... makes it f*** useless
