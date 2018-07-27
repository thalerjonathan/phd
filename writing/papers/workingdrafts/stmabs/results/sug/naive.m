sequential = [28.2, 28.2, 28.2, 28.2];
stm = [30.9, 35.5, 38.5, 37.3];
stm_retries = [0.004, 1.0 , 2.2, 3.4];
cores = [1, 2, 3, 4];

ax = plotyy (cores, stm, cores, stm_retries);
xlabel ("Cores");
ylabel (ax(1), "Steps per second");
ylabel (ax(2), "Retry-Ratio");

legend ("STM");
title ("bla");