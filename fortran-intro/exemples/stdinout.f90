program stdinout
    implicit none
    real :: x, y

    print *, 'Saída padrão. Entre duas variáveis' ! * indica formatação livre, determinada pelo compilador
    read(*,*) x, y ! * indica entrada padrão (teclado)
    print *, 'Variáveis' , x, y

end program stdinout