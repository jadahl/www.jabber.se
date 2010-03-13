compile: 
	erl -make
	
clean:
	rm -rf ./ebin/*.beam

all: compile
