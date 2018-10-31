genderRatio = [];

for t = 1 : length( dynamics ) 
  time = dynamics{t}{1};
  female = 0;
  
  for ai = 2 : length (dynamics{t})
    %a = dynamics{t}{ai};
    %agentId = dynamics{t}{ai}(1);
    %agentAge = dynamics{t}{ai}(2);
    %agentSug = dynamics{t}{ai}(3);
    %agentMet = dynamics{t}{ai}(4);
    %agentVis = dynamics{t}{ai}(5);
    agentGen = dynamics{t}{ai}(6);
    
    female = female + agentGen;
  endfor
  
  genderRatio(end+1,:) = female / length (dynamics{t});
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

plot (genderRatio);
title ('Gender Ratio');
xlabel ('Time');
ylabel ('Female Ratio');