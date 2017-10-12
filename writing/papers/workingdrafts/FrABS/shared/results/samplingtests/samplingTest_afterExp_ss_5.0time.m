dtAvgs = [
1,5.507;
2,5.247;
5,5.095;
10,5.044;
100,4.999;
1000,4.995;
];
eventTime = 5.0;
ss = dtAvgs (:, 1);
avg = dtAvgs (:, 2);
n = length (ss);
ecsTheoryLinePoints = n + 1;
ecsTheoryLineX = [0; ss];
ecsTheoryLineY = ones(ecsTheoryLinePoints, 1) * eventTime;
figure;
semilogx (ss, avg, 'color', 'blue', 'linewidth', 2);
hold on
plot (ecsTheoryLineX, ecsTheoryLineY, 'color', 'red', 'linewidth', 2);
xLabels = cellstr(num2str(ss));
yLabels = cellstr(num2str(avg));
set(gca,'YTick', avg);
set(gca,'XTick', ss);
set(gca, 'xticklabel', xLabels);
set(gca, 'yticklabel', yLabels);
xlabel ('Super Samples');
ylabel ('Average Time-Out');
legend ('Average Time-Out per Super Samples', 'Theoretical Maximum');
title ('Super-Sampling afterExp with Time-Delta of 1.0 and average timeout of 5.0 time-units');
