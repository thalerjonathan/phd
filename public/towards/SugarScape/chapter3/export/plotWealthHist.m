wealth = {};

for t = 1 : length( dynamics ) 
  time = dynamics{t}{1};
  ws = [];
  
  for ai = 2 : length (dynamics{t})
    %a = dynamics{t}{ai};
    %agentId = dynamics{t}{ai}(1);
    %agentAge = dynamics{t}{ai}(2);
    agentSug = dynamics{t}{ai}(3);
    %agentMet = dynamics{t}{ai}(4);
    %agentVis = dynamics{t}{ai}(5);
    
    ws(end+1,:) = agentSug;
  endfor
  
  wealth{t} = ws;
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

skewness (wealth{length(wealth)})
kurtosis (wealth{length(wealth)})

hist(wealth{length(wealth)});