# arm-vm

Implements a simple virtual machine for a subset of the ARM instruction set

## Running
> If you do not have stack installed, see the section on [External dependencies](#external-dependencies).

Assuming you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed, you can easily run the project as follows:
```
stack run -- [--repl | --interpreter]
```
Passing `--repl` will provide you with a Read-eval-print loop, allowing you to execute assembly instructions one by one, and watch how it affects the registers, memory etc. On the other hand, running `--interpreter` will allow you to pass a prewritten assembly file, enabling you to step through the program one instruction at a time.

### REPL
As mentioned earlier, you can start the REPL by running the following command from the project root folder:
```
stack run -- --repl
```
This should provide you with the following view:
```
Available instructions (R = register, I = immediate):
add R, R, R    ; dst <- lhs + rhs
sub R, R, R    ; dst <- lhs - rhs
mul R, R, R    ; dst <- lhs * rhs
cmp R, R, R    ; set bits [0..2] based on <, >, =
load R, R      ; dst <- memory pointed to be src
store R, R     ; val -> memory pointed to by dst
ldi R, I       ; dst <- immediate

Memory
0x000000f0: 0x00000000
0x000000f4: 0x00000000
0x000000f8: 0x00000000
0x000000fc: 0x00000000
0x00000100: 0x00000000
0x00000104: 0x00000000


Registers
%r0 = 0x00000000        %r2 = 0x00000000
%r1 = 0x00000000        %r3 = 0x00000000

REPL> 
```
This allows you to execute instructions one at a time. For example, running these commands in sequence will load the number 1 into `%r0` and then add `%r0` and `%r1` together, storing the result in `%r1`:
```
ldi %r0, 1
```
```
add %r1, %r1, %r0
```

### Interpreter
As mentioned, you run the interpreter by executing this command from the projects root folder:
```
stack run -- --interpreter
```
This will then prompt you for a file, which ought to contain a header consisting of the memory region you want available during execution, followed by lines of assembly instructions to be executed.

For instance, consider the program below, which will iteratively calculate the first `n` (100 in this case) fibonacci numbers:
```
0x0 0x4
ldi %r9, 0  ; How many how many iterations we've performed
ldi %r8, 100 ; How many Fibonacci numbers to calculate
ldi %r7, 1  ; Used for decrement
ldi %r0, 0  ; The n-th Fibonacci number
ldi %r1, 1  ; The (n+1)-th Fibonacci number
ldi %r2, 1  ; The (n+2)-th Fibonacci number
sub %r0, %r0, %r0 ; Zero r2
add %r0, %r0, %r1 ; Move r3 -> r2 (increase fib)
sub %r1, %r1, %r1 ; Zero r3
add %r1, %r1, %r2 ; Move r4 -> r3 (increase fib)
add %r2, %r2, %r0 ; Calculate next Fibonacci number
add %r9, %r9, %r7 ; Increment the loop counter
cmp %r12, %r8, %r9 ; r8 > r9? Branch back if true
jgt %r12, -32
```
Here, the first line represents the memory region that ought to be available to the program (mandatory). The rest of the lines contain the instruction stream.
This should result in the output presented below, with the `>` denoting the program counter.
```
'r' for restart / ny innlasting av fil, CMD + . for å avslutte

Memory
0x00000000: 0x00000000
0x00000004: 0x00000000


Registers
%r0 = 0x00000000	%r8 = 0x00000000
%r1 = 0x00000000	%r9 = 0x00000000
%r2 = 0x00000000	%r10 = 0x00000000
%r3 = 0x00000000	%r11 = 0x00000000
%r4 = 0x00000000	%r12 = 0x00000000
%r5 = 0x00000000	sp = 0x00000000
%r6 = 0x00000000	lr = 0x00000000
%r7 = 0x00000000	pc = 0x00000000


Instruction stream
 > 0x00000000:  ldi %r9, 0
   0x00000004:  ldi %r8, 100
   0x00000008:  ldi %r7, 1
   0x0000000c:  ldi %r0, 0
   0x00000010:  ldi %r1, 1
   0x00000014:  ldi %r2, 1
   0x00000018:  sub %r0, %r0, %r0
   0x0000001c:  add %r0, %r0, %r1
   0x00000020:  sub %r1, %r1, %r1
   0x00000024:  add %r1, %r1, %r2
   0x00000028:  add %r2, %r2, %r0
   0x0000002c:  add %r9, %r9, %r7
   0x00000030:  cmp %r12, %r8, %r9
   0x00000034:  jgt %r12, -32
```
From here, you can gradually step through instructions one by one by pressing enter.


## External dependencies
This project depends on the [Stack](https://docs.haskellstack.org/en/stable/README/) build tool for building. This is most easily installed through [Homebrew](https://brew.sh/index_nb) as follows:
```
brew install haskell-stack
```
This will download the last version of stack available from brew. After this, I would reccommend you to upgrade to the latest version by running `stack upgrade`.
