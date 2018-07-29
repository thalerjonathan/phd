sequential = [39.4, 39.4, 39.4, 39.4];

io = [43.0, 51.8, 57.4, 58.1];

stm_tvar = [47.3, 53.5, 57.1, 53.0];
stm_tvar_retries = [0.0, 1.1, 2.2, 3.2];

stm_tarray = [45.4, 65.3, 75.7, 84.4];
stm_tarray_retries = [0, 0.02, 0.04, 0.05];

cores = [1, 2, 3, 4];

figure;
hold on;
yyaxis left
plot(cores, sequential, 'Color', [0.7, 0, 0], 'LineWidth', 2);
plot(cores, io, 'Color', [0, 0, 0.8], 'LineWidth', 2);
plot(cores, stm_tvar, 'Color', [0, 0.7, 0], 'LineWidth', 2);
plot(cores, stm_tarray, 'Color', [0, 0.7, 0], 'LineWidth', 2);
ylabel ("Steps per second");

yyaxis right
plot (cores, stm_tvar_retries, 'Color', [0.9, 0.7, 0], 'LineWidth', 2);
plot (cores, stm_tarray_retries, 'Color', [0.9, 0.7, 0], 'LineWidth', 2);
ylabel ("Retries");

legend ("Sequential", "Lock-Based", "STM TVar", "STM TArray", "STM TVar Retries", "STM TArray Retries");
xlabel ("Cores");
grid on;