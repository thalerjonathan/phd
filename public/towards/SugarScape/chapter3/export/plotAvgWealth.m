meanSugWealth = [];
meanSpiWealth = [];

for t = 1 : length( dynamics ) 
  msug = [];
  mspi = [];
  
  for ai = 2 : length (dynamics{t})
    sug = dynamics{t}{ai}(3){1};
    spi = dynamics{t}{ai}(4){1};
    
    msug(end+1,:) = sug;
    mspi(end+1,:) = spi;
  endfor
  
  meanSugWealth(end+1,:) = mean(msug);
  meanSpiWealth(end+1,:) = mean(mspi);
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

figure;
plot(meanSugWealth, "linewidth", 2, "color", 'green');
hold on;
plot(meanSpiWealth, "linewidth", 2, "color", 'blue');
title('Average Wealth');
xlabel('Time');
ylabel('Average');
legend('Sugar', 'Spice');
grid on;