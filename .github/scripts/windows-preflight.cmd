@echo off
setlocal

echo Running Windows wrapper preflight...
call .\mvnw.cmd --version
if errorlevel 1 exit /b 1

echo Running lightweight Maven validation...
call .\mvnw.cmd --batch-mode -q -Dinvoker.skip=true -DskipTests -DskipITs validate
if errorlevel 1 exit /b 1
