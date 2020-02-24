      PROGRAM   MAIN

! Compile and Link Your Code

! Intel MKL provides many options for creating code for multiple processors and operating systems, compatible with different compilers and third-party libraries, and with different interfaces.
!
! Define directory where mkl libraries are installed, e.g.
! I have MKL in HOMEMKL=/opt/intel/mkl2020
! and then
!  source $HOMEMKL/bin/compilervars.csh intel64 -platform linux
!
! To compile and link the exercises in this tutorial with Intel® Composer XE, type
!
! Windows* OS: ifort /Qmkl dgemm_example.f
! Linux* OS, OS X*: ifort -mkl dgemm_example.f
! NOTE
! This assumes that you have installed Intel MKL and set environment variables as described in http://software.intel.com/en-us/articles/intel-mkl-111-getting-started/.
!
! For other compilers, use the Intel MKL Link Line Advisor to generate a command line to compile and link the exercises in this tutorial: http://software.intel.com/en-us/articles/intel-mkl-link-line-advisor/.
!  gfortran  -fdefault-integer-8 -I${F95ROOT}/include/intel64/ilp64 -m64 -I${MKLROOT}/include dgemm_example.f -o tdgemm.exe  ${F95ROOT}/lib/intel64/libmkl_lapack95_ilp64.a -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_ilp64.a ${MKLROOT}/lib/intel64/libmkl_sequential.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -lpthread -lm -ldl


      IMPLICIT NONE

      DOUBLE PRECISION ALPHA, BETA
      INTEGER          M, K, N, I, J
      PARAMETER        (M=2000, K=200, N=1000)
      DOUBLE PRECISION A(M,K), B(K,N), C(M,N)

      PRINT *, "This example computes real matrix C=alpha*A*B+beta*C"
      PRINT *, "using Intel® MKL function dgemm, where A, B, and C"
      PRINT *, "are matrices and alpha and beta are double precision "
      PRINT *, "scalars"
      PRINT *, ""

      PRINT *, "Initializing data for matrix multiplication C=A*B for "
      PRINT 10, " matrix A(",M," x",K, ") and matrix B(", K," x", N, ")"
10    FORMAT(a,I5,a,I5,a,I5,a,I5,a)
      PRINT *, ""
      ALPHA = 1.0
      BETA = 0.0

      PRINT *, "Intializing matrix data"
      PRINT *, ""
      DO I = 1, M
        DO J = 1, K
          A(I,J) = (I-1) * K + J
        END DO
      END DO

      DO I = 1, K
        DO J = 1, N
          B(I,J) = -((I-1) * N + J)
        END DO
      END DO

      DO I = 1, M
        DO J = 1, N
          C(I,J) = 0.0
        END DO
      END DO

      PRINT *, "Computing matrix product using Intel® MKL DGEMM "
      PRINT *, "subroutine"
      CALL DGEMM('N','N',M,N,K,ALPHA,A,M,B,K,BETA,C,M)

! The arguments provide options for how Intel MKL performs the operation. In this case:
!
! 'N'
! Character indicating that the matrices A and B should not be transposed or conjugate transposed before multiplication.
!
! M, N, K
! Integers indicating the size of the matrices:
!
! A: M rows by K columns
!
! B: K rows by N columns
!
! C: M rows by N columns
!
! ALPHA
! Real value used to scale the product of matrices A and B.
!
! A
! Array used to store matrix A.
!
! M
! Leading dimension of array A, or the number of elements between successive columns (for column major storage) in memory. In the case of this exercise the leading dimension is the same as the number of rows.
!
! B
! Array used to store matrix B.
!
! K
! Leading dimension of array B, or the number of elements between successive columns (for column major storage) in memory. In the case of this exercise the leading dimension is the same as the number of rows.
!
! BETA
! Real value used to scale matrix C.
!
! C
! Array used to store matrix C.
!
! M
! Leading dimension of array C, or the number of elements between successive columns (for column major storage) in memory. In the case of this exercise the leading dimension is the same as the number of rows.

      PRINT *, "Computations completed."
      PRINT *, ""

      PRINT *, "Top left corner of matrix A:"
      PRINT 20, ((A(I,J), J = 1,MIN(K,6)), I = 1,MIN(M,6))
      PRINT *, ""

      PRINT *, "Top left corner of matrix B:"
      PRINT 20, ((B(I,J),J = 1,MIN(N,6)), I = 1,MIN(K,6))
      PRINT *, ""

 20   FORMAT(6(F12.0,1x))

      PRINT *, "Top left corner of matrix C:"
      PRINT 30, ((C(I,J), J = 1,MIN(N,6)), I = 1,MIN(M,6))
      PRINT *, ""

 30   FORMAT(6(ES12.4,1x))

      PRINT *, "Example completed."
      STOP

      END PROGRAM MAIN
