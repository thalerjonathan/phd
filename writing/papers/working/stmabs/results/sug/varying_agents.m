sequential = [14.4, 6.8, 4.7, 4.4, 5.30];
io = [20.2, 10.8, 8.1, 7.6, 5.4];
stm_tvar_3 = [20.1, 10.4, 7.9, 7.4, 9.2];
stm_tvar_4 = [18.5, 9.5, 7.3, 6.7, 8.9];
stm_tarray = [71.9, 54.8, 44.1, 37.0, 33.3];

agents = [500, 1000, 1500, 2000, 2500];

figure;
hold on;

plot(agents, sequential, '--', 'Color', [0.8, 0.0, 0]);
plot(agents, io, '-.', 'Color', [0, 0, 0.8]);
plot(agents, stm_tvar_3, 'Color', [0, 0.8, 0]);
%plot(agents, stm_tvar_4, 'Color', [0, 0.8, 0.8]);
plot(agents, stm_tarray, 'Color', [0.9, 0.7, 0]);

legend ("Sequential", "Lock-Based", "STM TVar 3 cores", "STM TArray");
xlabel ("Agents");
ylabel ("Steps per second");
grid on;
