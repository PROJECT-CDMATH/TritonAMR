FUNCTION ISDCG(N,g,ITOL,TOL, ITER, ERR, IERR, IUNIT, R, BNRM)
!***BEGIN PROLOGUE  ISDCG
!***REFER TO  DCG, DSDCG, DSICCG
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(ISDCG-D),
!             Linear system, Sparse, Stop Test
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned Conjugate Gradient Stop Test.
!            This routine calculates the stop test for the Conjugate
!            Gradient iteration scheme.  It returns a nonzero if the
!            error estimate (the type of which is determined by ITOL)
!            is less than the user specified tolerance TOL.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
!     INTEGER IERR, IUNIT, IWORK(USER DEFINED)
!     DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, R(N), Z(N)
!     DOUBLE PRECISION P(N), DZ(N), RWORK(USER DEFINED), AK, BK
!     DOUBLE PRECISION BNRM, SOLNRM
!     EXTERNAL MSOLVE
!
!     IF( ISDCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ, RWORK, IWORK,
!    $     AK, BK, BNRM, SOLNRM) .NE. 0 ) THEN ITERATION DONE
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :IN       Double Precision X(N).
!         The current approximate solution vector.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See ``Description''
!         in the DCG, DSDCG or DSICCG routines.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R for
!         Z given R with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays).  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSLOVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
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
!         side is less than tol, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the ``exact''
!         solution or a very accurate approximation (one with an error
!         much less than tol) through a common block,
!         COMMON /SOLBLK/ SOLN( )
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than tol.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :IN       Integer.
!         The iteration for which to check for convergence.
! ERR    :OUT      Double Precision.
!         Error estimate of error in the X(N) approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Error flag.  IERR is set to 3 if ITOL is not on of the
!         acceptable values, see above.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :IN       Double Precision R(N).
!         The residual R = B-AX.
! Z      :WORK     Double Precision Z(N).
!         Workspace used to hold the pseudo-residual M Z = R.
! P      :IN       Double Precision P(N).
!         The conjugate direction vector.
! DZ     :WORK     Double Precision DZ(N).
!         Workspace used to hold temporary vector(s).
! RWORK  :WORK     Double Precision RWORK(USER DEFINABLE).
!         Double Precision array that can be used by MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINABLE).
!         Integer array that can be used by MSOLVE.
! BNRM   :INOUT    Double Precision.
!         Norm of the right hand side.  Type of norm depends on ITOL.
!         Calculated only on the first call.
! SOLNRM :INOUT    Double Precision.
!         2-Norm of the true solution, SOLN.  Only computed and used
!         if ITOL = 11.
!
! *Function Return Values:
!       0 : Error estimate (determined by ITOL) is *NOT* less than the
!           specified tolerance, TOL.  The iteration must continue.
!       1 : Error estimate (determined by ITOL) is less than the
!           specified tolerance, TOL.  The iteration can be considered
!           complete.
!
! *Precision:           Double Precision
! *See Also:
!       DCG, DSDCG, DSICCG
!
! *Cautions:
!     This routine will attempt to write to the fortran logical output
!     unit IUNIT, if IUNIT .ne. 0.  Thus, the user must make sure that
!     this  logical  unit  must  be  attached  to  a  file or terminal
!     before calling this routine with a non-zero  value  for   IUNIT.
!     This routine does not check for the validity of a non-zero IUNIT
!     unit number.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MSOLVE, DNRM2
!***COMMON BLOCKS    SOLBLK
!***END PROLOGUE  ISDCG

      !IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, ITOL, ITMAX
      INTEGER ITER, IERR, IUNIT
      DOUBLE PRECISION  TOL, ERR
      DOUBLE PRECISION Z(N), P(N), DZ(N),R(N)
!      EXTERNAL MSOLVE

!
!***FIRST EXECUTABLE STATEMENT  ISDCG
      ISDCG = 0
!
      IF( ITOL.EQ.1 ) THEN
!         err = ||Residual||/||RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) BNRM = DNRM2(N, g, 1)
         ERR = DNRM2(N, R, 1)/BNRM
      end if
!
      IF(IUNIT .NE. 0) THEN
         IF( ITER.EQ.0 ) THEN
            WRITE(IUNIT,1000) N, ITOL
         end if
         WRITE(IUNIT,1010) ITER, ERR
      end if
      IF(ERR .LE. TOL) ISDCG = 1
      RETURN
 1000 FORMAT(' Preconditioned Conjugate Gradient for ', &
           'N, ITOL = ',I5, I5, &
           /' ITER','   Error Estimate','            Alpha', &
           '             Beta')
 1010 FORMAT(1X,I4,1X,E16.7,1X,E16.7,1X,E16.7)
!------------- LAST LINE OF ISDCG FOLLOWS ------------------------------
      END