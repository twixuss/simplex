@echo off
set start_time=%time%
set bar=
set iters=0
set errorlevel=0
:loop
cls
echo Started at %start_time%
echo     Now is %time%
set /a iters=%iters%+1
echo %iters% %bar%
set bar=%bar%.
if "%bar%" equ "................................................................" (
    set bar=
)
bin-Debug\tests
:: For some reason errorlevel might be set to some seemingly random value here. 
:: For example it just printed: All 6 tests succeeded. Exiting. errorlevel=-1073741819
:: Idk why.
if "%errorlevel%" equ "0" (
    goto loop
)
echo Exiting. errorlevel=%errorlevel%
exit /b %errorlevel%