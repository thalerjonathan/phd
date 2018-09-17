x = [1, 2, 3, 4];
ystm = [52, 28, 22, 21];
y_io = [61, 45, 40, 38];

figure;
stmplot = plot(x, ystm);
hold on
ioplot = plot(x, y_io);

set (stmplot, 'linewidth', 2);
set (ioplot, 'linewidth', 2);

set (stmplot, 'color', [0 1 0]);
set (ioplot, 'color', [1 0 0]);

xlabel('Cores');
ylabel('Duration');
legend('STM', 'IO');
title('Performance Scaling STM vs. IO');