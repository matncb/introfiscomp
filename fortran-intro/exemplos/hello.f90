program hello

    print * , 'hello world' ! * para indicar formatação livre
    
end program hello

! Para compilar, gfortran hello.f90 -o ../builds/hello
! -o output, indica arquivo de saída

! Em assembly
! gfortran hello.f90 -S -o ../builds/hello.s