

ip ?

Addition:

1  addr (add register) stores into register C the result of adding register A and register B.
2  addi (add immediate) stores into register C the result of adding register A and value B.

Equality testing:
3  eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.

Greater-than testing:
4  gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.

Multiplication:

5  mulr (multiply register) stores into register C the result of multiplying register A and register B.
6  muli (multiply immediate) stores into register C the result of multiplying register A and value B.
Bitwise AND:

Assignment:

7 setr (set register) copies the contents of register A into register C. (Input B is ignored.)
8 seti (set immediate) stores value A into register C. (Input B is ignored.)


------------------------------


banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
Bitwise OR:

borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
Assignment:

setr (set register) copies the contents of register A into register C. (Input B is ignored.)
seti (set immediate) stores value A into register C. (Input B is ignored.)
Greater-than testing:

gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
Equality testing:

eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
