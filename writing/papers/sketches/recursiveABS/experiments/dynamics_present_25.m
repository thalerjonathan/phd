dynamics = [
NaN,NaN,NaN;
0.8657314629258517,0.5871155008429557,0.5871155008429557;
0.7414829659318637,0.6426885517065873,5.557305086363167e-2;
0.687374749498998,0.6644471482647838,2.175859655819638e-2;
0.6432865731462926,0.6942313197824221,2.9784171517638368e-2;
0.5891783567134269,0.7154046187613323,2.1173298978910227e-2;
0.5290581162324649,0.7267519165314758,1.13472977701434e-2;
0.46092184368737477,0.7601146737920286,3.3362757260552864e-2;
0.4529058116232465,0.7744640073798398,1.434933358781122e-2;
0.40480961923847697,0.787383497153036,1.2919489773196232e-2;
0.3907815631262525,0.7998544708464548,1.2470973693418687e-2;
0.35070140280561124,0.8168813818112411,1.7026910964786365e-2;
0.3346693386773547,0.8192305245411453,2.3491427299042428e-3;
0.3306613226452906,0.8262071762572771,6.976651716131788e-3;
0.2785571142284569,0.8534457804497885,2.7238604192511374e-2;
0.27655310621242485,0.8516405827528066,-1.8051976969818788e-3;
0.22845691382765532,0.8645735916276989,1.2933008874892288e-2;
0.18637274549098196,0.8813006966313576,1.6727105003658704e-2;
0.1903807615230461,0.888418106053376,7.117409422018389e-3;
0.17034068136272545,0.8920754206826342,3.6573146292582666e-3;
0.1623246492985972,0.8987109138912741,6.635493208639883e-3;
0.17034068136272545,0.9028374208734927,4.126506982218504e-3;
0.15831663326653306,0.9038823679104241,1.0449470369314053e-3;
0.14428857715430862,0.9065217737061423,2.639405795718174e-3;
0.13026052104208416,0.9153783757992174,8.856602093075224e-3;
0.12024048096192384,0.9245252409581072,9.146865158889725e-3;
0.11623246492985972,0.9252679963100797,7.427553519725492e-4;
0.10420841683366733,0.9307909469733118,5.522950663232076e-3;
9.018036072144289e-2,0.9365985940134232,5.807647040111299e-3;
9.018036072144289e-2,0.9405199287463814,3.921334732958215e-3;
8.016032064128256e-2,0.9475093043229307,6.989375576549372e-3;
7.014028056112225e-2,0.9534052231447021,5.895918821771417e-3;
4.809619238476954e-2,0.9645409867353752,1.1135763590673123e-2;
4.008016032064128e-2,0.9698174126029835,5.276425867608229e-3;
3.406813627254509e-2,0.9735097178483951,3.6923052454116093e-3;
3.006012024048096e-2,0.9750238572382858,1.5141393898907309e-3;
2.2044088176352707e-2,0.9791193498107323,4.095492572446575e-3;
1.8036072144288578e-2,0.9822748671947066,3.155517383974173e-3;
1.2024048096192385e-2,0.9842502465247955,1.9753793300889377e-3;
8.016032064128256e-3,0.9860999777332441,1.84973120844861e-3;
6.012024048096192e-3,0.9855178611190634,-5.821166141807012e-4;
6.012024048096192e-3,0.9855202468428921,2.385723828688798e-6;
6.012024048096192e-3,0.9856514616534655,1.312148105734412e-4;
4.008016032064128e-3,0.9868371663962845,1.185704742818918e-3;
4.008016032064128e-3,0.9868371663962846,1.1391466705572748e-16;
4.008016032064128e-3,0.9868371663962845,-1.1391466705572748e-16;
2.004008016032064e-3,0.9894169290962876,2.5797627000032322e-3;
0.0,0.9910869357763142,1.6700066800265682e-3;
];

unsatisfiedRatio = dynamics (:, 1);
similarity = dynamics (:, 2);
similarityChange = dynamics (:, 3);

figure
plot (unsatisfiedRatio.', 'color', 'green');
hold on
plot (similarity.', 'color', 'red');
hold on
plot (similarityChange.', 'color', 'blue');

xlabel ('steps');
ylabel ('satisfation');

title ('Present dynamics 25x25');