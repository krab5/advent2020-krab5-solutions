HS=stack ghc --
TEX=pdflatex
TARGET=buses

all:
	$(HS) Main.hs -o $(TARGET)

doc:
	$(TEX) -output-directory out main.tex

clean:
	rm $(TARGET)
	rm *.hi
	rm *.o



