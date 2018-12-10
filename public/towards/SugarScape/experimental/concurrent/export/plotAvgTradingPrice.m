meanPrice = [];
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
    meanPrice(end+1,:) = nan; % no trades amongst agents, add nan for this time-step
  else
    meanPrice(end+1,:) = mean(avgPrices);
  endif
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

breaks = floor(tMax / 10); % ceil(sqrt(tMax))
x = [1 : tMax];
meanTrend = splinefit(x, meanPrice, breaks, "order", 2);
meanTrendY = ppval(meanTrend, x);

figure;
scatter(x, meanPrice, 'filled');
hold on;
plot(meanTrendY, "linewidth", 2, "color", 'red');
title ('Average Trading Prices');
xlabel('Time');
ylabel('Average');
legend('Prices', 'Trend');
grid on;