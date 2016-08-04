
all: guide

guide: Guide.lhs
	lhs2TeX -o out/Guide.tex Guide.lhs
	dos2unix out/*.tex
	latex --output-directory=./out Guide.tex
	dvips -o ./out/Guide.ps ./out/Guide.dvi
	dvipdfm -o ./out/Guide.pdf ./out/Guide.dvi 
	cp ./out/Guide.pdf ../doc

worldframe: WorldFrame.hs
	runhaskell -i../src WorldFrame.hs