@echo off

if "%1" == "all" (
	for /f %%d in ('dir /ad /b graphs') do (
		echo %%d 
		for %%f in (graphs/%%d/*.gv) do (
					dot -Tpng -O "graphs/%%d/%%~nf.gv"     
		)
	)
) else (
	for %%f in (graphs/%1/*.gv) do (
			dot -Tpng -O "graphs/%1/%%~nf.gv"     
	)
)
