stm = [53.182, 27.817, 21.776, 20.201];
io = [60.564, 42.779, 38.586, 41.555];
cores = [1, 2, 3, 4];

figure;
loglog (cores, stm, '-', "color", [0, 0.7, 0]);
hold on;
loglog (cores, io, '--', "color", [0.7, 0, 0]);

legend ("STM", "IO");
xlabel ("Cores");
ylabel ("Average Time sec.");

grid on;

%title ("Performance STM vs IO on 51x51 Grid with varying Cores");