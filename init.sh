#!/usr/bin/env bash

# defining ambient
set -e
mkdir -p c
mkdir -p haskell

if command -v tput >/dev/null 2>&1; then
  tput=$(tput setaf 2)
  tput civis
else
  echo "+ Installing dependencies"
  sudo apt-get update > /dev/null 2>&1
  sudo apt-get install -y ncurses-bin build-essential ghc > /dev/null 2>&1
  echo "└ + Installed dependencies!"
  sleep 2
  tput cuu1; tput el; tput cuu1; tput el
fi


if [ ! -x c/token ]; then
  echo "+ Compiling tokenization file './c/token.c'"
  gcc -O2 -std=c99 -o c/token c/token.c  > /dev/null 2>&1;
  sleep 0.5
  tput cuu1; tput el
else
  echo "+ Compiled file: './c/token.c'!"
  sleep 1
  tput cuu1; tput el
fi

if [ ! -x c/autobuilder ]; then
  echo "+ Compiling autobuilder file './c/autobuilder.c'"
  gcc -O2 -std=c99 -o c/autobuilder c/autobuilder.c  > /dev/null 2>&1;
  sleep 0.5
  tput cuu1; tput el
else
   echo "+ Compiled file './c/autobuilder.c'"
fi

if [ ! -x haskell/main ]; then
  echo "+ Compiling './haskell/*.hs' components"
  ghc -O2 -outputdir haskell -o haskell/main \
    haskell/main.hs haskell/builder.hs haskell/process.hs  > /dev/null 2>&1;
  sleep 0.1
  tput cuu1; tput el
else
  echo "+ Compiled components! './haskell/*.hs'"
  sleep 0.1
  tput cuu1; tput el
fi

echo "+ Creating construction tokens"
./c/token > ./haskell/tokens.txt
cat ./haskell/tokens.txt
sleep 1
for i in {1..11}; do tput cuu1; tput el; done
sleep 1
tput cuu1; tput el; tput cuu1; tput el
echo "+ Running Haskell algorithm"
sleep 1
tput cuu1; tput el
cd haskell
./main < tokens.txt > build.txt > /dev/null 2>&1;
tput cnorm
