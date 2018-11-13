tradingVolumeSugar = [];
tradingVolumeSpice = [];
tMax = length(dynamics);

for t = 1 : tMax
  time = dynamics{t}{1};
  tvSug = 0;
  tvSpi = 0;
  
  for ai = 2 : length (dynamics{t})
    agentTradeInfo = dynamics{t}{ai}(9){1};
    
    tradeInfoSize = size(agentTradeInfo, 2);
    % a trade has 3 elements: price, sugar and spice exchanged => tradeInfoSize
    % must always come in multiples of 3
    trades = tradeInfoSize / 3;
    
    if trades > 0 
      for i = 1 : 3 : tradeInfoSize
        sug = agentTradeInfo(i+1);
        spi = agentTradeInfo(i+2);
 
        % note: must take absolute values because its always from the agents
        % perspective, so can be negative to indicate 'selling' of sugar / spice
 
        tvSug = tvSug + abs(sug); 
        tvSpi = tvSpi + abs(spi);
      endfor
    endif
  endfor
  
  tradingVolumeSugar(end+1,:) = tvSug;
  tradingVolumeSpice(end+1,:) = tvSpi;
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

figure;
plot(tradingVolumeSugar);
hold on;
plot(tradingVolumeSpice);
title('Trading Volume');
xlabel('Time');
ylabel('Volume');
legend('Sugar', 'Spice');
grid on;