meanVision = [];
meanMetab = [];

for t = 1 : length(dynamics) 
  vs = [];
  ms = [];
  
  for ai = 2 : length(dynamics{t})
    agentVis = dynamics{t}{ai}(5){1};
    agentMet = dynamics{t}{ai}(6){1};

    vs(end+1,:) = agentVis;
    ms(end+1,:) = agentMet;
  endfor
  
  meanVision(end+1,:) = mean(vs);
  meanMetab(end+1,:) = mean(ms);
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

figure;
plot(meanVision, "linewidth", 2, "color", 'green');
hold on;
plot(meanMetab, "linewidth", 2, "color", 'blue');
title ('Average Mean and Vision');
xlabel('Time');
ylabel('Average');
legend('Metabolism', 'Vision');
grid on;