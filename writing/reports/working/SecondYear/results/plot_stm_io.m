x = [2601, 10201, 22801, 40401, 63001];
ystm = [21, 92, 188, 305, 530];
y_io = [40, 158, 370, 661, 1154];

figure;
stmplot = plot(x, ystm);
hold on
ioplot = plot(x, y_io);

set (stmplot, 'linewidth', 2);
set (ioplot, 'linewidth', 2);

set (stmplot, 'color', [0 1 0]);
set (ioplot, 'color', [1 0 0]);

xlabel('Agents');
ylabel('Duration');
legend('STM', 'IO');
title('Performance STM vs. IO');