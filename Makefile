bin-Debug/simplex: src/main.cpp
	gcc src/main.cpp -o bin-Debug/simplex -DBUILD_DEBUG=1 -std=c++23 -I./dep/tl/include -fpermissive -w -mpopcnt -mlzcnt -lm -ldl