!===============================================================================
! Copyright 2004-2019 Intel Corporation.
!
! This software and the related documents are Intel copyrighted  materials,  and
! your use of  them is  governed by the  express license  under which  they were
! provided to you (License).  Unless the License provides otherwise, you may not
! use, modify, copy, publish, distribute,  disclose or transmit this software or
! the related documents without Intel's prior written permission.
!
! This software and the related documents  are provided as  is,  with no express
! or implied  warranties,  other  than those  that are  expressly stated  in the
! License.
!===============================================================================

*   Content : Intel(R) MKL PARDISO Fortran example
*
********************************************************************************
C----------------------------------------------------------------------
C Example program to show the use of the "PARDISO" routine
C for nonsymmetric linear systems
C---------------------------------------------------------------------
C This program can be downloaded from the following site:
C www.pardiso-project.org
C
C (C) Olaf Schenk, Department of Computer Science,
C University of Basel, Switzerland.
C Email: olaf.schenk@unibas.ch
C
C Modified by Sergei Blinnikov on 11 Sep 2020 to test TridiagN results
C reading form files like SBlcsrTridiag{N}.dat, rename it to
!    SBlcsrTridiagN.dat
C
C---------------------------------------------------------------------
      PROGRAM pardiso_tridiagN
        IMPLICIT NONE
        include 'mkl_pardiso.fi'
C.. Internal solver memory pointer for 64-bit architectures
C.. INTEGER*8 pt(64)
C.. Internal solver memory pointer for 32-bit architectures
C.. INTEGER*4 pt(64)
C.. This is OK in both cases
        TYPE(MKL_PARDISO_HANDLE) pt(64)
C.. All other variables
        INTEGER maxfct, mnum, mtype, phase, n, nnz, nrhs, error, msglvl
        INTEGER iparm(64)
        character(len=1) :: char
        integer(4),dimension(:),allocatable :: ia,ja
        real(8),dimension(:),allocatable :: a,b,x
        INTEGER i, idum(1)
        REAL*8  ddum(1)
C.. Fill all arrays containing matrix data.
        DATA nrhs /1/, maxfct /1/, mnum /1/

      open(11,file='SBlcsrTridiagN.dat',action='read')
         ! matrix reading in Compressed Row Storage (CRS) format

      read(11,*)n,nnz	! reading from the input file
        !  n is the number of rows,
        !  nnz is the number of nonzero elements of the matrix
      write(*,*)n,nnz

! allocate the memory needed to store arrays read from the input file

      allocate(ia(1:n+1)) ! this is an array that stores the positions
!       in a of the first nonzero elements in the rows of the matrix
      allocate(a(1:nnz)) ! it is an array of non-zero elements,
!       written line by line from right to left from top to bottom
      allocate(ja(1:nnz)) ! it is an array of column numbers of nonzero
!       elements, written in the order above
      allocate(b(1:n)) ! it is a vector of right-hand sides
      read(11,'(a)')char ! this string reads 'anz'
      read(11,*)(a(i),i=1,nnz)   ! read an array of nonzero elements
      write(*,'(10f8.3)')(a(i),i=1,nnz)
      read(11,'(a)')char ! this string reads 'inoti'-- delete in future
      read(11,'(a)')char ! this string reads 'rhs b'
      read(11,*)(b(i),i=1,n) ! read an array of right-hand sides
      write(*,'(10f8.3)')(b(i),i=1,n)
      read(11,'(a)')  ! this string reads 'ianz'
      read(11,*)(ia(i),i=1,n+1) ! read the array of the first nonzero positions of elements in line
      write(*,'(10i5)')(ia(i),i=1,n+1)
      read(11,'(a)')char         ! this string reads 'janz'
      read(11,*)(ja(i),i=1,nnz)  ! read an array of column numbers
      write(*,'(10i5)')(ja(i),i=1,nnz)
C      pause
      close(11)
      write(*,'(a,(10f8.3))')'a=',a
      write(*,'(a,(10i5))')'ia=',ia
      write(*,'(a,(10i5))')'ja=',ja

C..
C.. Setup PARDISO control parameter
C..
        DO i = 1, 64
           iparm(i) = 0
        END DO
        iparm(1) = 1 ! no solver default
        iparm(2) = 2 ! fill-in reordering from METIS
        iparm(3) = 1 ! numbers of processors
        iparm(4) = 0 ! no iterative-direct algorithm
        iparm(5) = 0 ! no user fill-in reducing permutation
        iparm(6) = 0 ! =0 solution on the first n components of x
        iparm(7) = 0 ! not in use
        iparm(8) = 9 ! numbers of iterative refinement steps
        iparm(9) = 0 ! not in use
        iparm(10) = 13 ! perturb the pivot elements with 1E-13
        iparm(11) = 1 ! use nonsymmetric permutation and scaling MPS
        iparm(12) = 0 ! not in use
        iparm(13) = 1 ! maximum weighted matching algorithm is switched-on (default for non-symmetric)
        iparm(14) = 0 ! Output: number of perturbed pivots
        iparm(15) = 0 ! not in use
        iparm(16) = 0 ! not in use
        iparm(17) = 0 ! not in use
        iparm(18) = -1 ! Output: number of nonzeros in the factor LU
        iparm(19) = -1 ! Output: Mflops for LU factorization
        iparm(20) = 0 ! Output: Numbers of CG Iterations
        error = 0 ! initialize error flag
        msglvl = 1 ! print statistical information
        mtype = 11 ! real unsymmetric
C.. Initialize the internal solver memory pointer. This is only
C necessary for the FIRST call of the PARDISO solver.
        DO i = 1, 64
            pt(i)%DUMMY = 0
        END DO
C.. Reordering and Symbolic Factorization, This step also allocates
C   all memory that is necessary for the factorization
        phase = 11 ! only reordering and symbolic factorization
        CALL pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja,
     &  idum, nrhs, iparm, msglvl, ddum, ddum, error)
        WRITE(*,*) 'Reordering completed ... '
        IF (error .NE. 0) THEN
            WRITE(*,*) 'The following ERROR was detected: ', error
            STOP 1
        END IF
        WRITE(*,*) 'Number of nonzeros in factors = ',iparm(18)
        WRITE(*,*) 'Number of factorization MFLOPS = ',iparm(19)
C.. Factorization.
        phase = 22 ! only factorization
        CALL pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja,
     &  idum, nrhs, iparm, msglvl, ddum, ddum, error)
        WRITE(*,*) 'Factorization completed ... '
        IF (error .NE. 0) THEN
            WRITE(*,*) 'The following ERROR was detected: ', error
            STOP 1
        END IF
C.. Back substitution and iterative refinement
        iparm(8) = 2 ! max numbers of iterative refinement steps
        phase = 33 ! only factorization
      write(*,'(a,10f8.3)')'b=',b
        CALL pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja,
     &  idum, nrhs, iparm, msglvl, b, x, error)
        WRITE(*,*) 'Solve completed ... '
        WRITE(*,*) 'The solution of the system is '
        DO i = 1, n
            WRITE(*,*) ' x(',i,') = ', x(i)
        END DO
C.. Termination and release of memory
        phase = -1 ! release internal memory
        CALL pardiso (pt, maxfct, mnum, mtype, phase, n, ddum, idum,
     &  idum, idum, nrhs, iparm, msglvl, ddum, ddum, error)
      END PROGRAM pardiso_tridiagN
