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

  	\begin{tabular}{ c || c | c | c }
        Agents  & Sequential     & TVar       & TArray     \\ \hline \hline 
    	500     & 14.121 (0.096) & 21.1 (0.2) & \textbf{74.4} (1.1) \\ \hline
   		1,000   & 6.841 (0.092)  & 11.3 (0.3) & \textbf{56.8} (0.2) \\ \hline
   		1,500   & 4.5 (0.116)    & 8.1 (0.03) & \textbf{45.2} (0.2) \\ \hline
   		2,000   & 3.302 (0.07)   & 6.2 (0.01) & \textbf{37.0} (0.2) \\ \hline 
   		2,500   & 2.641 (0.129)  &5.2 (0.02)  & \textbf{31.7} (0.3)
   	\end{tabular}