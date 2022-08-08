build:
	/* Find latest version via xcodebuild -sdk -version */
	clang++ main.cpp -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk --std=c++17 -framework CoreAudio -framework AudioToolbox
clean:
	rm a.out samples.csv
