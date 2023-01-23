MAIN_C_DIR := src/main/c/

BUILD_DIR := executables

all:	c-sparse-directed c-dense-undirected c-dense-directed c-sparse-undirected

c-sparse-directed:
	gcc $(MAIN_C_DIR)/dijkstrasd64.c -O3 -o $(BUILD_DIR)/dijkstrasd64.exe

c-dense-undirected:
	gcc $(MAIN_C_DIR)/dijkstradud64.c -O3 -o $(BUILD_DIR)/dijkstradud64.exe

c-dense-directed:
	gcc $(MAIN_C_DIR)/dijkstradd64.c -O3 -o $(BUILD_DIR)/dijkstradd64.exe

c-sparse-undirected:
	gcc $(MAIN_C_DIR)/dijkstrasud64.c -O3 -o $(BUILD_DIR)/dijkstrasud64.exe

clean:
	rm $(BUILD_DIR)/*
