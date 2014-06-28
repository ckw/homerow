###BF interpreter

#####notes
* memory expands indefinitely in both directions
* cell size is one byte
* to send input to a program, pipe it in. E.g. echo "racecar" | ./homerow reverse.bf
* input instruction zeroes the byte under the pointer on EOF
* this is not a reasonable approach to this problem; I just wanted to mess around with Parsec


#####To run hello\_world.bf
```
git clone git@github.com:ckw/homerow.git
cd homerow
ghc --make homerow.hs
./homerow hello_world.bf
```
