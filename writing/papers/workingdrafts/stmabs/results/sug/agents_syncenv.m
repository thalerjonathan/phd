sequential = [14.121, 6.841, 4.5, 3.302, 2.641];
tvar = [21.1, 11.3, 8.1, 6.2, 5.2];
tarray = [74.4, 56.8, 45.2, 37.0, 31.7];

agents = [500, 1000, 1500, 2000, 2500];

figure;
hold on;

plot(agents, sequential, 'Color', [0.8, 0.0, 0], 'LineWidth', 2);
plot(agents, tvar, 'Color', [0, 0, 0.8], 'LineWidth', 2);
plot(agents, tarray, 'Color', [0, 0.8, 0], 'LineWidth', 2);

legend ("Sequential", "TVar", "TArray");
xlabel ("Agents");
ylabel ("Steps per second");
grid on;
