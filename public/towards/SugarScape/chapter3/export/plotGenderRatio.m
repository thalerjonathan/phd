genderRatio = [];

for t = 1 : length( dynamics ) 
  female = 0;
  
  for ai = 2 : length (dynamics{t})
    agentGen = dynamics{t}{ai}(7){1};
    
    female = female + agentGen;
  endfor
  
  genderRatio(end+1,:) = female / length (dynamics{t});
endfor 

% needed for octave on Fedora, otherwise only black output
graphics_toolkit("gnuplot");

figure;
plot (genderRatio);
title ('Gender Fraction');
xlabel('Time');
ylabel('Female Ratio');
grid on;