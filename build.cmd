@echo off
setlocal

.paket\paket.exe update -g wsbuild
if errorlevel 1 exit /b %errorlevel%

set DOTNETSOLUTION="WebSharper.sln"
call paket-files\wsbuild\github.com\dotnet-websharper\build-script\WebSharper.Fake.cmd %*
