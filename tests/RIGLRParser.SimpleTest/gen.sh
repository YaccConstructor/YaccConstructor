
[ -f log.txt ] && rm log.txt

for i in Chaos Expr Brackets Grammar7
do
    echo . >> log.txt
    echo $i >> log.txt
    mono ../../Bin/Release/v40/YC.YaccConstructor.exe -i $i.yrd -c ExpandEbnf -c ExpandMeta -g "RIGLRGenerator -token int -module RIGLR.$i" >> log.txt
done