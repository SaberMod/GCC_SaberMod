! { dg-do compile } 
! { dg-additional-options "-fdump-tree-original -std=f2008" } 

! test for tree-dump-original and spaces-commas

! This error is temporary.  Remove when support is added for these clauses
! in the middle end.
! { dg-prune-output "sorry, unimplemented" }
! { dg-prune-output "Error: work-sharing region" }

program test
  implicit none
  integer :: i, j, k, m, sum
  REAL :: a(64), b(64), c(64)

  !$acc kernels 
  !$acc loop collapse(2)
  DO i = 1,10
    DO j = 1,10
    ENDDO
  ENDDO

  !$acc loop independent gang (3)
  DO i = 1,10
    !$acc loop worker(3)
    DO j = 1,10
      !$acc loop vector(5)
      DO k = 1,10
      ENDDO
    ENDDO
  ENDDO
  !$acc end kernels

  sum = 0
  !$acc parallel
  !$acc loop private(m) reduction(+:sum)
  DO i = 1,10
    sum = sum + 1
  ENDDO
  !$acc end parallel

end program test
! { dg-final { scan-tree-dump-times "pragma acc loop" 5 "original" } } 

! { dg-final { scan-tree-dump-times "collapse\\(2\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "independent" 1 "original" } } 
! { dg-final { scan-tree-dump-times "gang\\(num: 3\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "worker\\(3\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "vector\\(5\\)" 1 "original" } } 

! { dg-final { scan-tree-dump-times "private\\(m\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "reduction\\(\\+:sum\\)" 1 "original" } } 
