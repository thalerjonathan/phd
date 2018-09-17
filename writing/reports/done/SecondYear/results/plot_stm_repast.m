x = [2601, 10201, 40401];
ystm = [21, 92, 305];
y_rep = [10, 110, 1260];

figure;
stmplot = plot(x, ystm);
hold on
repplot = plot(x, y_rep);

set (stmplot, 'linewidth', 2);
set (repplot, 'linewidth', 2);

set (stmplot, 'color', [0 1 0]);
set (repplot, 'color', [1 0 0]);

xlabel('Agents');
ylabel('Duration');
legend('4-Core STM', '1-Core Repast');
title('Performance STM vs. Repast');