!!This program is for 10000 random walks, each random walk has a different seed between 1 to 10000
program ih
    implicit none
    character(len=6) :: name
    integer(kind=4)::sized,i,k,j,minmax,binsize
    real(kind=8) ::  rand(2,10), randomnumber100000(100000) !float is kind=8
    real(kind=16):: sum,average(100000) ! double is kind=16
    integer, allocatable:: seedbyme(:),x(:),y(:) !allocate helps define  an array of undefined size
    call random_seed(size=sized)!size of seed get allocated to sized
    allocate(seedbyme(sized)) !defining size of array seed
    seedbyme=1 !all elements of seedbyme is 1
    call random_seed(put=seedbyme) !assigning value of seedbyme to seed of random generator
    call random_number(rand(1,:))
    sum=0
     do j=1,100000
        seedbyme=j !all elements of seedbyme is 1
        call random_seed(put=seedbyme) !assigning value of seedbyme to seed of random generator
        call random_number(randomnumber100000)
        sum=0
        do i=1,10000
            if(randomnumber100000(i)>=0.5) then
               sum=sum+1
            else
                sum=sum-1
            end if
        end do
        average(j)=sum/100000.0
     end do

     !making bins for plotting histogram later on
     binsize=10
     minmax=995
     allocate(x(2*minmax/binsize))
     x=0
     allocate(y(2*minmax/binsize))
     do k=(-1*minmax),(minmax-binsize),binsize
      do i=1,100000
        if(((average(i)*100000.0)<(k+binsize)) .AND. ((average(i)*100000.0)>=k)) then
          x(((k+minmax)/binsize)+1)= x(((k+minmax)/binsize)+1)+1
        end if
        y(((k+minmax)/binsize)+1)= k+(binsize/2)
      end do
     end do

     open(2, file = 'plot_avg_100000steps_100000(processed).dat', status='new')
      do i=1,(2*minmax/binsize)
        write(2,*)x(i), y(i)
      end do
     close(2)

     open(6, file = 'plot_avg_100000steps_discrete100000.dat', status='new')
      do i=1,100000
        write(6,*)average(i)
      end do
     close(6)
    end program ih
