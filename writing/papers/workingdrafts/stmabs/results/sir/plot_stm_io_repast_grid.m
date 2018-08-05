stm = [20.201, 74.493, 168.47, 302.43, 495.73];
io4 = [41.914, 170.55, 376.89, 672.01, 1027.27];
io3 = [38.614, 171.61, 404.11, 720.65, 1117.27];
rep = [10.822, 107.40, 464.017, 1227.68, 3283.63];

agents = [2601, 10201, 22801, 40401, 63001];

figure;
loglog (agents, stm, '-', "color", [0, 0.7, 0]);
hold on;
loglog (agents, io4, ':', "color", [0.7, 0, 0]);
hold on;
loglog (agents, io3, '--', "color", [0.7, 0.7, 0]);
hold on;
loglog (agents, rep, '-.', "color", [0, 0, 0.7]);

legend ("STM 4 cores", "IO 4 cores", "IO 3 cores", "RePast 1 core");
xlabel ("Agents");
ylabel ("Average Time sec.");

grid on;

%title ("Performance STM vs IO vs RePast with varying Agents");