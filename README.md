# APAC Test 2016
## Reference 
 Here https://code.google.com/codejam/contest/8264486/dashboard#s=p1

## Projects
- bee : Problem A. Lazy Spelling Bee 
- robot : Problem B. Robot Rock Band
 
## Install accelerate for Haskell under windows
- Download accelerate and apply patch.accelerate (Proces Id, ...)
- Download accelerate-cuda and apply patch.accelerate-cuda
- Dowload and install CUDA
- Dowload and install VS 2015 "c:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" (You need VC++, Microsoft Web Developer Tools & Universal Windows App Developement Tools)
- Create 2 properties
1. LIB="C:\Program Files (x86)\Windows Kits\10\Lib\10.0.14393.0\ucrt\x64;C:\Program Files (x86)\Windows Kits\10\Lib\10.0.14393.0\um\x64"
2. INCLUDE="C:\Program Files (x86)\Windows Kits\10\Include\10.0.14393.0\ucrt"
- Run c:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat in your cmd before running your program