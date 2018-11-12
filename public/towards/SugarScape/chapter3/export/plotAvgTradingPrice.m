meanPrice = [];
tMax = length(dynamics);

for t = 1 : tMax
  time = dynamics{t}{1};
  mp = [];
  
  for ai = 2 : length (dynamics{t})
    a = dynamics{t}{ai};
    agentTrades = dynamics{t}{ai}(8){1};
    
    agentAvgPrice = 0;
    n = length (agentTrades);
    
    if n > 0 
      for at = 1 : n
        at = agentTrades(1);
        agentAvgPrice = agentAvgPrice + abs(at);
      endfor
      
      mp(end+1,:) = agentAvgPrice / length (agentTrades);
    endif
  endfor
  
  meanPrice(end+1,:) = mean (mp);
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

x = [1:tMax];
scatter(x, meanPrice, 'filled');

title ('Average Trading Prices');
xlabel ('Time');
ylabel ('Average');