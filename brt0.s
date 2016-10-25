.globl _start
_start:
    xor %rbp, %rbp

    callq main

    # linux syscall exit with main's return value
    mov %rax, %rdi
    mov $60, %rax
    syscall
