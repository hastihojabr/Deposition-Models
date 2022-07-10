! random.f90
program main
    real :: r(1),i
    integer L,T
    integer,dimension(L):: h
    integer,dimension(L):: l
    integer,dimension(T):: x
    integer,dimension(T):: y
    call random_seed()
    call random_number(r)

    write(*,*) 'Give lattice size and number of time steps.'
    read(*,*) L,T

    do i=1,L
      l(i+1) = l(i)+1
    enddo

    L=5
    do t=0, 100:
      i = modulo(int(r*5+1),5)
      h(modulo(i,L)) = maxval((/h(modulo(i-1,L)),h(modulo(i,L)),h(modulo(i+1,L))/))+1
      x(t)= l(i)
      y(t)= h(modulo(i,5))
    end do

    write(*,*) x
    write(*,*) y
end program main
