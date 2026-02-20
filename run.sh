./bin/MyExecutable $1
if [ $? -ne 0 ]; then
    exit 1
fi
gcc -o output output.s -no-pie
./output 
echo "exited with code $?"