stm = [31.1, 35.4, 38.8, 37.4];
stm_retries = [0.004, 1.1 , 2.1, 3.4];
cores = [1, 2, 3, 4];

figure;
hold on;
yyaxis left
plot(cores, stm, 'Color', [0, 0.7, 0]);

yyaxis right
plot (cores, stm_retries, 'Color', [0.7, 0.0, 0]);

legend ("STM", "STM Retries");
grid on;