stdPrice = [];
tMax = length(dynamics);

for t = 1 : tMax
  avgPrices = [];
  
  for ai = 2 : length(dynamics{t})
    agentTradeInfo = dynamics{t}{ai}(9){1};
   
    tradeInfoSize = size(agentTradeInfo, 2);
    % a trade has 3 elements: price, sugar and spice exchanged => tradeInfoSize
    % must always come in multiples of 3
    trades = tradeInfoSize / 3;
    
    if trades > 0 
      agentAvgPrice = 0;
      
      % iterate through trading infos, steps of 3, take first element: price
      for i = 1 : 3 : tradeInfoSize
        price = agentTradeInfo(i);
        agentAvgPrice = agentAvgPrice + price;
      endfor
      
      avgPrices(end+1,:) = agentAvgPrice / trades;
    endif
  endfor
 
  if length(avgPrices) == 0
    stdPrice(end+1,:) = nan; % no trades amongst agents, add nan for this time-step
  else
    stdPrice(end+1,:) = std(avgPrices);
  endif
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

breaks = ceil(sqrt(tMax))
x = [1 : tMax];
stdTrend = splinefit(x, stdPrice, breaks, "order", 2);
stdTrendY = ppval(stdTrend, x);

figure;
scatter(x, stdPrice, 'filled');
hold on;
plot(stdTrendY, "linewidth", 2, "color", 'red');
title ('Standard Deviation Trading Prices');
xlabel('Time');
ylabel('Std');
legend('Prices', 'Trend');
grid on;