!: 	EDIT THIS PARAMETER FILE IF PROGRAM DOES NOT COMPILE BECAUSE THE 
!:	STORAGE REQUIREMENTS ARE TOO GREAT (DECREASE SIZES) OR IF YOUR RUN
!:	PARAMETERS RESULT IN A "STORAGE EXCEEDED" MESSAGE (INCREASE SIZES).
!: NLMAX =	GEOACOUSTIC PARAMETER ARRAYS
!:		(total # layers in SVP, bottom, and top + 2)
!: NTLMAX = 	TRANSMISSION LOSS ARRAYS FOR CW CALCULATIONS
!: 		(# src depths) * (# rec depths) * (# ranges)
!:		REFLECTION COEFF ARRAYS FOR REFLECTION COEFF CALCULATIONS
!:		(# plane wave angles) * (# frequencies)
!:		R1R2 ARRAYS FOR K-PLANE IMAGES
!:		(# points on real k-axis) * (# points on imag k-axis)
!: NSRMAX = 	SOURCE AND RECEIVER DEPTH ARRAYS
!:		(# source depths + # receiver depths)
!: 		GRAZING ANGLE AXIS FOR REFLECTION COEFF CALCULATIONS
!:		(# grazing angles)
!: NRNGMAX =	SOURCE-RECEIVER RANGES OR SOURCE TRACK POINTS
!:		(# source-receiver ranges) or (# points on source track)
!: NSNRMAX =	SOURCE-RECEIVER RANGE ARRAYS 
!:		(# receivers) * (# ranges)
!: NSEGMAX = 	SOURCE TRACK ARRAYS
!:		(# source track segments specified for iitl=2 or iifft=2)
!: NM_MAX =	MODE CHARACTERISTICS ARRAYS
!:		(# modes)
!: NSR_NM_MAX =	MODE FUNCTION ARRAYS
!:		(# source depths + # receiver depths) * (# modes)
!: NTFMAX =	TRANSFER FUNCTION ARRAY FOR BROADBAND CALCULATIONS
!: 		(# FFT freqs)*(# rec depths)*(# src-rec ranges)
!: NFBBMAX =	FREQUENCY AXIS ARRAY FOR BROADBAND CALCULATIONS
!:		[# freqs to compute transfer function=(fmax-fmin)/(fs/nfft)]
!: NFCWMAX =	FREQUENCY AXIS ARRAY FOR CW & REFL COEFF CALCULATIONS
!:		(# frequencies)
!: NM_NF_MAX = 	EIGENVALUE ARRAYS FOR BROADBAND CALCULATIONS
!:		(# modes at fmax) * (# FFT freqs)
!: NZ_NF_MAX = 	MODE FUNCTION ARRAYS FOR BROADBAND CALCULATIONS
!:		(# src/rec depths ) * (# FFT freqs)
!: NDMAX =      MAXIMUM NUMBER OF DUCTS IN A PROFILE
!: NPMAX =      MAXIMUM NUMBER OF ENVIRONMENTAL PARAMETERS TO VARY
!: NRECL =      DIRECT ACCESS FILE CONVENTION FOR recl=lenrec COMMAND:
!:              SET NRECL=1 FOR LENREC IN WORDS (SGI,ALPHA),
!:              NRECL=4 FOR LENREC IN BYTES (SUN).
!: SUFX =	OUTPUT FILE SUFFIX DELIMITER ('_' or '.', FOR EXAMPLE)
module Parms_com
implicit none
save

integer*4 :: NTLMAX
integer*4 :: NTFMAX,NFBBMAX,NM_NF_MAX,NZ_NF_MAX,NSNRMAX
integer*4 :: NHDFMAX
character(len=1), parameter :: SUFX='_'
character*128 MLINE
integer*4, parameter :: NLMAX=10000,NSRMAX=10000,NRNGMAX=2,&
	NFCWMAX=5001,NM_MAX=50,NSR_NM_MAX=20000000,NPMAX=1,NRECL=4,&
 	LML=67
integer*4, parameter :: NSEGMAX=1,NDMAX=10000
!     parameter(NLMAX=40,NTLMAX=1000000,NSRMAX=3000,
!    .   NRNGMAX=5000,NM_MAX=10000,NSR_NM_MAX=1000000,NTFMAX=40960000,
!    .   NFBBMAX=8192,NM_NF_MAX=2000000,NZ_NF_MAX=200000,NRECL=4,
!    .   NSNRMAX=1000000,NSEGMAX=50,NHDFMAX=max(NTLMAX,NM_NF_MAX),
!    .   NDMAX=40,NPMAX=25,SUFX='_',MLINE='MEMORY LIMIT EXCEEDED!  '//
!    .   'CHECK INPUTS OR CHANGE LIMITS IN Parms_com.',LML=67)
end module Parms_com
