#
# branch-test.s - Simple branch test program for SimpleScalar
#

.text
.align 2
.globl main

main:
        # Initialize registers
        addiu $4, $0, 0       # i = 0
        addiu $5, $0, 10      # count = 10
        addiu $6, $0, 0       # sum = 0

loop:
        # Check if i < count
        slt   $7, $4, $5      # $7 = (i < count)
        beq   $7, $0, end     # if !(i < count) goto end

        # Conditional branches within the loop
        andi  $8, $4, 1       # $8 = i & 1 (check if i is odd)
        beq   $8, $0, even    # if i is even, goto even

odd:
        # For odd numbers, add i*2 to sum
        addu  $9, $4, $4      # $9 = i*2
        addu  $6, $6, $9      # sum += i*2
        j     next            # jump to next

even:
        # For even numbers, add i to sum
        addu  $6, $6, $4      # sum += i

next:
        # Increment i and loop
        addiu $4, $4, 1       # i++
        j     loop            # goto loop

end:
        # Return sum
        addu  $2, $0, $6      # return sum value

        # Exit program
        jr    $31             # return to caller 