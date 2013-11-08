  SUBROUTINE DCG(N,g,W,ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT,Nx,Ny,Nz,IND,lev,direc)

!***BEGIN PROLOGUE  DCG
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DCG-D),
!             Symmetric Linear system, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned Conjugate Gradient iterative Ax=b solver.
!            Routine to  solve a  symmetric positive definite linear
!            system    Ax = b    using the Preconditioned  Conjugate
!            Gradient method.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER  ITER, IERR, IUNIT, IWORK(USER DEFINABLE)
!     DOUBLE PRECISION g(N), W(N), A(NELT), TOL, ERR, R(N), Z(N)
!     DOUBLE PRECISION P(N), DZ(N), RWORK(USER DEFINABLE)
!     EXTERNAL MATVEC, MSOLVE
!
!     CALL DCG(N, g, W, NELT, IA, JA, A, ISYM, MATVEC, MSLOVE,
!    $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ,
!    $     RWORK, IWORK )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! g      :IN       Double Precision g(N).
!         Right-hand side vector.
! W      :INOUT    Double Precision W(N).
!         On input W is your initial guess for solution vector.
!         On output W is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Integer A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See ``Description'', below
!         for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which performs the matrix vector multiply
!         Y = A*W given A and W.  The name of the MATVEC routine must
!         be declared external in the calling program.  The calling
!         sequence to MATVEC is:
!
!             CALL MATVEC( N, W, Y, NELT, IA, JA, A, ISYM )
!
!         Where N is the number of unknowns, Y is the product A*W
!         upon return W is an input vector, NELT is the number of
!         non-zeros in the SLAP IA, JA, A storage for the matrix A.
!         ISYM is a flag which, if non-zero, denotest that A is
!         symmetric and only the lower or upper triangle is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R for
!         Z given R with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays).  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSLOVE is:
!
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and Z is the solution upon return.  RWORK is a double
!         precision
!         array that can be used to pass necessary preconditioning
!         information and/or workspace to MSOLVE.  IWORK is an integer
!         work array for the same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :WORK     Double Precision R(N).
! Z      :WORK     Double Precision Z(N).
! P      :WORK     Double Precision P(N).
! DZ     :WORK     Double Precision DZ(N).
! RWORK  :WORK     Double Precision RWORK(USER DEFINABLE).
!         Double Precision array that can be used by  MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINABLE).
!         Integer array that can be used by  MSOLVE.
!
! *Description
!       This routine does  not care  what matrix data   structure is
!       used for  A and M.  It simply   calls  the MATVEC and MSOLVE
!       routines, with  the arguments as  described above.  The user
!       could write any type of structure and the appropriate MATVEC
!       and MSOLVE routines.  It is assumed  that A is stored in the
!       IA, JA, A  arrays in some fashion and  that M (or INV(M)) is
!       stored  in  IWORK  and  RWORK   in  some fashion.   The SLAP
!       routines DSDCG and DSICCG are examples of this procedure.
!
!       Two  examples  of  matrix  data structures  are the: 1) SLAP
!       Triad  format and 2) SLAP Column format.
!
!       =================== S L A P Triad format ===================
!
!       In  this   format only the  non-zeros are  stored.  They may
!       appear  in *ANY* order.   The user  supplies three arrays of
!       length NELT, where  NELT  is the number  of non-zeros in the
!       matrix:  (IA(NELT), JA(NELT),  A(NELT)).  For each  non-zero
!       the  user puts   the row  and  column index   of that matrix
!       element in the IA and JA arrays.  The  value of the non-zero
!       matrix  element is  placed in  the corresponding location of
!       the A  array.  This is  an extremely easy data  structure to
!       generate.  On  the other hand it  is  not too  efficient  on
!       vector  computers   for the  iterative  solution  of  linear
!       systems.  Hence, SLAP  changes this input  data structure to
!       the SLAP   Column  format for the  iteration (but   does not
!       change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix      SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *See Also:
!       DSDCG, DSICCG
!***REFERENCES  1. Louis Hageman \& David Young, ``Applied Iterative
!                 Methods'', Academic Press, New York (1981) ISBN
!                 0-12-313340-8.
!
!               2. Concus, Golub \& O'Leary, ``A Generalized Conjugate
!                 Gradient Method for the Numerical Solution of
!                 Elliptic Partial Differential Equations,'' in Sparse
!                 Matrix Computations (Bunch \& Rose, Eds.), Academic
!                 Press, New York (1979).
!***ROUTINES CALLED  MATVEC, MSOLVE, ISDCG, DCOPY, DDOT, DAXPY, D1MACH
!***END PROLOGUE  DCG

      !IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER i,ii,N, ITOL, ITMAX, ITER, lev
      INTEGER IUNIT, IERR, direc
      DOUBLE PRECISION   TOL, ERR
	  DOUBLE PRECISION, allocatable, dimension(:) :: DZ, R, Z, P
	  DOUBLE PRECISION g(N),W(N), MOY	, AK, VAR
!
	  allocate(R(N))
	  allocate(Z(N))
	  allocate(P(N))
	  allocate(DZ(N))	

!         Check some of the input data.
!***FIRST EXECUTABLE STATEMENT  DCG
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
!
!         Calculate initial residual and pseudo-residual, and check
!         stopping criterion.
		
	if(direc.eq.2) CALL MULTMATVEC_2D(W,R,Nx,Ny,IND,lev)
	if(direc.eq.3) CALL MULTMATVEC_3D(W,R,Nx,Ny,Nz,IND,lev)
		do i= 1,N
			R(i) = g(i) - R(i)
		enddo
		CALL DCOPY(N, R, 1, Z, 1)
      IF(ISDCG(N,g,ITOL,TOL, ITER, ERR, IERR, IUNIT, R, BNRM).NE. 0 )  then
			GO TO 200
	  endif

         ERR = DNRM2(N, Z, 1)/BNRM
	     !PRINT *, 'err = ', err

      IF( IERR.NE.0 ) RETURN
!
!         ***** Iteration loop *****
!
      DO 100 K=1,ITMAX
         ITER = K
!
!         Calculate coefficient bk and direction vector p.
         BKNUM = DDOT(N, Z, 1, R, 1)
         IF( BKNUM.LE.0.0D0 ) THEN
            IERR = 5
            RETURN
         end if
         IF(ITER .EQ. 1) THEN
            CALL DCOPY(N, Z, 1, P, 1)
         ELSE
            BK = BKNUM/BKDEN
            DO 20 I = 1, N
               P(I) = Z(I) + BK*P(I)
 20         CONTINUE
         end if
         BKDEN = BKNUM
!
!         Calculate coefficient ak, new iterate W, new residual r,
!         and new pseudo-residual z.

			if(direc.eq.2) CALL MULTMATVEC_2D(P,Z,Nx,Ny,IND,lev)
			if(direc.eq.3) CALL MULTMATVEC_3D(P,Z,Nx,Ny,Nz,IND,lev)

         AKDEN = DDOT(N, P, 1, Z, 1)
!         IF( AKDEN.LE.0.0D0 ) THEN
!            IERR = 6
!            RETURN
!         end if
         AK = BKNUM/AKDEN

         CALL DAXPY(N, AK, P, 1, W, 1)

		 if(IND.eq.2) then
			MOY = sum(W)/N
			 do ii=1,N
			   W(ii) = W(ii) - MOY	
			 end do	
		end if

         CALL DAXPY(N, -AK, Z, 1, R, 1)
		 CALL DCOPY(N, R, 1, Z, 1)
         ERR = DNRM2(N, Z, 1)/BNRM
!	     PRINT *, 'ITER,ERR = ', K, ERR
!         check stopping criterion.
     IF(ISDCG(N,g,ITOL,TOL, ITER, ERR, IERR, IUNIT, R, BNRM).NE. 0 )  then
			   GO TO 200
	      endif
!
 100  CONTINUE
!
!         *****   end of loop  *****
!
!         stopping criterion not satisfied.
      ITER = ITMAX + 1
      IERR = 2
!
 200  RETURN

      END
