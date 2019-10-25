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
