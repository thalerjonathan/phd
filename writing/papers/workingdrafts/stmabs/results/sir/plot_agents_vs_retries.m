retries = [1306, 3712, 8189, 13285, 21217];
agents = [2601, 10201, 22801, 40401, 63001];
retries_ratio = agents ./  retries;

figure;
plot (agents, retries_ratio, "linewidth", 2, "color", [0, 0.7, 0]);

xlabel ("Agents");
ylabel ("Ratio Agents / Retries");

title ("Ratio of Agents to Retries");