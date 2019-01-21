#STACK=/home/students/inf/PUBLIC/MRJP/Stack/stack
STACK=stack
all: runtime
	$(STACK) setup
	$(STACK) build
	$(STACK) install --local-bin-path .

runtime:
	clang -S -emit-llvm ./lib/runtime.c -o ./lib/runtime.ll > /dev/null
