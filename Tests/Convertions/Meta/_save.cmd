@echo off

for /R %%i in (meta_*.yrd.ans) do (
	cp %%i %%i.old
)

