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
bin-Debug\simplex "tests\implicit casts\integers\valid.sp"
if "%errorlevel%" equ "0" (
    goto loop
)
echo Exiting. errorlevel=%errorlevel%
exit /b %errorlevel%