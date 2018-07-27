stm = [52.8, 51.1, 62.2, 65.0];
stm_retries = [0.002, 0.05, 0.03, 0.04];
cores = [1, 2, 3, 4];

figure;
hold on;
yyaxis left
plot(cores, stm, 'Color', [0, 0.7, 0]);

yyaxis right
plot (cores, stm_retries, 'Color', [0.7, 0.0, 0]);

legend ("STM", "STM Retries");
grid on;