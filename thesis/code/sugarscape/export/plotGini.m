gini = [];
tSteps = 50; % WARNING: calculating gini for each step (=1) takes ages
tMax = length(dynamics)

for t = 1 : tSteps : tMax
  aw = [];

  for ai = 2 : length (dynamics{t})
    sug = dynamics{t}{ai}(3){1};
    spi = dynamics{t}{ai}(4){1};
    
    aw(end+1,:) = sug + spi;
  endfor
  
  awTrans = aw';
  n = length(awTrans);
  
  numer = 0;
  denom = 2.0 * n * sum(aw);
  
  for xi = awTrans
    for xj = awTrans
      numer = numer + abs(xi - xj);
    endfor
  endfor

  g = numer / denom;
 
  gini(end+1,:) = g;
endfor

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

figure;
plot(gini);
title('Gini Coefficient');
xlabel('Time');
ylabel('Coefficient');
grid on;