STACK=/home/students/inf/PUBLIC/MRJP/Stack/stack
all:
	$(STACK) setup
	$(STACK) build
	$(STACK) install --local-bin-path .


