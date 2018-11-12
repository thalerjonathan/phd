tradingVolume = [];
tMax = length(dynamics);

for t = 1 : tMax
  time = dynamics{t}{1};
  tv = 0;
  
  for ai = 2 : length (dynamics{t})
    a = dynamics{t}{ai};
    agentTrades = dynamics{t}{ai}(8){1};
    
    tv = tv + length (agentTrades);
  endfor
  
  tradingVolume(end+1,:) = tv;
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

plot(tradingVolume);

title ('Trading Volume');
xlabel ('Time');
ylabel ('Volume');