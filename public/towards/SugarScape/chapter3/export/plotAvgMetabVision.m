meanVision = [];
meanMetab = [];

for t = 1 : length( dynamics ) 
  time = dynamics{t}{1};
  vs = [];
  ms = [];
  
  for ai = 2 : length (dynamics{t})
    %a = dynamics{t}{ai};
    %agentId = dynamics{t}{ai}(1);
    %agentAge = dynamics{t}{ai}(2);
    %agentSug = dynamics{t}{ai}(3);
    agentMet = dynamics{t}{ai}(4);
    agentVis = dynamics{t}{ai}(5);
    
    vs(end+1,:) = agentVis;
    ms(end+1,:) = agentMet;
  endfor
  
  meanVision(end+1,:) = mean (vs);
  meanMetab(end+1,:) = mean (ms);
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

plot(meanVision, "linewidth", 2, "color", 'green');
hold on;
plot(meanMetab, "linewidth", 2, "color", 'blue');
title ('Average Mean and Vision');
xlabel ('Time');
ylabel ('Average');
% octave seems to give the lines in the legend different colors... makes it f*** useless
