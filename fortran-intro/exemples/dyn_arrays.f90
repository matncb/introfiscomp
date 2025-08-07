program dyn_arrays
    ! Sintaxe básica: uso do atributo allocatable
    ! O operador (:) indica dimensões a serem definidas posteriormente

    real, dimension(:), allocatable :: vec      ! Vetor 1D
    integer, allocatable :: matrix(:,:)         ! Matriz 2D
    character(len=:), allocatable :: dyn_string ! String dinâmica

    real, allocatable :: init_arr(:) ! Vamos usar depois
    real, allocatable :: data(:), temp(:) ! Vamos usar para realocar depois

    ! Com inicialização (Fortran 2003+)
    ! real, allocatable :: arr(:) = [1.0, 2.0, 3.0]  ! Alocação automática

    ! Para alocar memória --> allocate
    ! stat: Variável inteira que retorna o status da operação
    ! 0: Sucesso
    ! >0: Erro (código específico do compilador)
    ! <0: Não suportado (raro)

    integer :: ierr
    ! Alocar vetor com 1000000 elementos
    allocate(vec(1000000), stat=ierr) ! Saída da varíavel stat --> ierr

    if (ierr /= 0) then
        print *, "Falha na alocação: erro", ierr
        stop ! Encerra a execução do programa todo
    end if

    ! Posso alocar sem verificar também, confiando que vai dar tudo certo
    allocate(matrix(5,10))         ! Aloca matriz 5x10
    allocate(init_arr(5), source=0.0)  ! Todos elementos = 0.0

    ! Verificar se está alocado: função alocated --> retorna logical
    if (allocated(vec)) then
        print *, "Vetor está alocado"
    else
        print *, "Vetor NÃO está alocado"
    end if

    ! Depois de usar, desalocar vetor ---> deallocate
    deallocate(vec, stat=ierr)
    if (ierr /= 0) print *, "Erro na desalocação:", ierr

    ! Vamos verificar novamente
    if (allocated(vec)) then
        print *, "Vetor está alocado"
    else
        print *, "Vetor NÃO está alocado"
    end if

    !! REALOCAÇÃO

    ! Alocação inicial
    allocate(data(100))
    data = [(i, i=1,100)]  ! Preenche valores

    ! Redimensiona para 200 elementos
    call move_alloc(data, temp)    ! 1. Move dados para temporário
    allocate(data(200))            ! 2. Aloca novo tamanho
    data(1:100) = temp             ! 3. Copia dados antigos
    data(101:200) = 0.0            ! 4. Preenche novo espaço
    ! temp é automaticamente desalocado

end program dyn_arrays