cls
Rem *** This batch file will compile all necessary java files
Rem ***---------------------------------------------------------

del Sugarscape.class
del Sugarscape\*.class
javac Sugarscape.java
pause
appletviewer Sugarscape.html