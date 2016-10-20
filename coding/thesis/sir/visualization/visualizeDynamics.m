addpath('jsonlab-1.2');
file='dynamics.json'; 
data=loadjson(file);

figure
hold on

p1 = plot( data(:,1), 'linewidth', 5 );
set( p1,'Color', 'blue' );
p2 = plot( data(:,2), 'linewidth', 5 );
set( p2,'Color', 'red' );
p3 = plot( data(:,3), 'linewidth', 5 );
set( p3,'Color', [0.0 0.5 0.0] );

xlabel( "Time" );
ylabel( "Number of Agents" );
legend ( "Susceptible", "Infected", "Recovered" );