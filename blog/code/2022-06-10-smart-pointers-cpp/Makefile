


test: test_shared test_unique

test_unique: setup
	g++ -std=c++17 -lgtest unique_ptr_test.cpp -o out/unique_ptr_test
	out/./unique_ptr_test

test_shared: setup
	g++ -std=c++17 -lgtest shared_ptr_test.cpp -o out/shared_ptr_test
	out/./shared_ptr_test

setup:
	mkdir -p out

clean:
	rm out/*
