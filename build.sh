if [ ! -d build ]; then
    mkdir build
fi
cd build

if [ "$1" == "release" ]; then
    echo "Building in release mode..."
    cmake -DCMAKE_BUILD_TYPE=Release ..
else
    echo "Building in debug mode..."
    cmake -DCMAKE_BUILD_TYPE=Debug ..
fi

make

