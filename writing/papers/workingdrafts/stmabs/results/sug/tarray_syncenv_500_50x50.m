stm = [50.796, 68.788, 77.354, 86.925];
stm_retries = [0.001, 0.01, 0.02, 0.03];
cores = [1, 2, 3, 4];

figure;
hold on;
yyaxis left
plot(cores, stm, 'Color', [0, 0.7, 0]);

yyaxis right
plot (cores, stm_retries, 'Color', [0.7, 0.0, 0]);

legend ("STM", "STM Retries");
grid on;