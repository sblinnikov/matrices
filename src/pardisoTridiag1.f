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
C Modified by Sergei Blinnikov on 11 Sep 2020 to test Tridiag1 results
C
C---------------------------------------------------------------------
      PROGRAM pardiso_tridiag1
        IMPLICIT NONE
        include 'mkl_pardiso.fi'
C.. Internal solver memory pointer for 64-bit architectures
C.. INTEGER*8 pt(64)
C.. Internal solver memory pointer for 32-bit architectures
C.. INTEGER*4 pt(64)
C.. This is OK in both cases
        TYPE(MKL_PARDISO_HANDLE) pt(64)
C.. All other variables
        INTEGER maxfct, mnum, mtype, phase, n, nrhs, error, msglvl
        INTEGER iparm(64)
        INTEGER ia(11)
        INTEGER ja(28)
        REAL*8 a(28)
        REAL*8 b(10)
        REAL*8 x(10)
        INTEGER i, idum(1)
        REAL*8  ddum(1)
C.. Fill all arrays containing matrix data.
        DATA n /10/, nrhs /1/, maxfct /1/, mnum /1/
        DATA ia /1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 29/
        DATA ja
     1  /1, 2, 1, 2, 3, 2, 3, 4, 3, 4,
     2   5, 4, 5, 6, 5, 6, 7, 6, 7, 8,
     3   7, 8, 9, 8, 9,10, 9,10/
        DATA a
     1  /1.000E-03, 2.000E-01, 1.000E-01, 1.000E-03, 2.000E-01,
     2   1.000E-01, 1.000E-03, 2.000E-01, 1.000E-01, 1.000E-03,
     3   2.000E-01, 1.000E-01, 1.000E-03, 2.000E-01, 1.000E-01,
     4   1.000E-03, 2.000E-01, 1.000E-01, 1.000E-03, 2.000E-01,
     5   1.000E-01, 1.000E-03, 2.000E-01, 1.000E-01, 1.000E-03,
     6   2.000E-01, 1.000E-01, 1.000E-03/
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
        b(1) =  0.201
        DO i = 2, n-1
            b(i) = 0.301
        END DO
        b(n) =  0.101
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
      END PROGRAM pardiso_tridiag1
