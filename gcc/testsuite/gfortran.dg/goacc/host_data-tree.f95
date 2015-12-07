! { dg-do compile } 
! { dg-additional-options "-fdump-tree-original" } 

program test
  implicit none
  integer :: i

  !$acc host_data use_device(i)
  !$acc end host_data
end program test
! { dg-prune-output "unimplemented" }
! { dg-final { scan-tree-dump-times "pragma acc host_data use_device\\(i\\)" 1 "original" } } 
! { dg-final { cleanup-tree-dump "original" } } 
