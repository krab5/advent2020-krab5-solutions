HS=stack ghc --
TARGET=game

all: $(TARGET)

$(TARGET):
	$(HS) Main.hs -o $(TARGET)

.PHONY: clean challenge

clean:
	rm $(TARGET)
	rm *.hi
	rm *.o

challenge: $(TARGET)
	./$(TARGET) "[2,0,6,12,1,3]" 30000000



