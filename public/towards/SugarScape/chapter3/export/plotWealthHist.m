wealth = {};

for t = 1 : length( dynamics ) 
  time = dynamics{t}{1};
  ws = [];
  
  for ai = 2 : length (dynamics{t})
    sug = dynamics{t}{ai}(3){1};
    spi = dynamics{t}{ai}(4){1};
    
    ws(end+1,:) = sug + spi;
  endfor
  
  wealth{t} = ws;
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

skewness (wealth{length(wealth)})
kurtosis (wealth{length(wealth)})

figure;
hist(wealth{length(wealth)});
title('Wealth Distribution (Sugar + Spice)');