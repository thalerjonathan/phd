sequential = [28.2, 28.2, 28.2, 28.2];
stm = [30.9, 35.5, 38.5, 37.3];
stm_retries = [0.004, 1.0 , 2.2, 3.4];
cores = [1, 2, 3, 4];

figure;
hold on;
yyaxis left
plot(cores, sequential, 'Color', [0, 0, 0.7]);
plot(cores, stm, 'Color', [0, 0.7, 0]);

yyaxis right
plot (cores, stm_retries, 'Color', [0.7, 0.0, 0]);

legend ("Seqeuential", "STM", "STM Retries");
grid on;