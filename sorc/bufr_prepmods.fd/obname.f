C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      FUNCTION OBNAME(MTYP,MSBT)
 
      CHARACTER*(*) OBNAME
      CHARACTER*8   NAMES(0:255,0:255)
      LOGICAL       FIRST
 
      DATA NAMES /65536*'      '/
      DATA FIRST /.TRUE./
 
      SAVE NAMES,FIRST
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  NON VALID TYPES CAUSE EMPTY RETURN
C  ----------------------------------
 
      OBNAME = ' '
      IF(MTYP.LT.0 .OR. MTYP.GT.255) RETURN
      IF(MSBT.LT.0 .OR. MSBT.GT.255) RETURN
 
C  DEFINITION BY NAME OF THE MODSBUFR DATA TYPES
C  ---------------------------------------------
 
      IF(FIRST) THEN
         NAMES(000,007) = 'METAR   '
         NAMES(001,001) = 'SHIPS   '
         NAMES(001,101) = 'SHIPSB  '
         NAMES(001,002) = 'DBUOY   '
         NAMES(001,102) = 'DBUOYB  '
         NAMES(001,003) = 'MBUOY   '
         NAMES(001,103) = 'MBUOYB  '
         NAMES(001,004) = 'LCMAN   '
         NAMES(001,104) = 'CMANB   '
         NAMES(001,005) = 'TIDEG   '
         NAMES(001,006) = 'SLPBG   '
         NAMES(001,007) = 'CSTGD   '
         NAMES(001,013) = 'SHIPSU  '
         NAMES(001,113) = 'SHIPUB  '
         NAMES(012,010) = 'SSTNV   '
         NAMES(012,011) = 'SSTNS   '
         NAMES(012,012) = 'SSTNVH  '
         NAMES(012,103) = 'SSMIPN  '
         NAMES(031,001) = 'BATHY   '
         NAMES(031,002) = 'TESAC   '
         NAMES(031,003) = 'TRKOB   '
         NAMES(031,011) = 'ERSHAL  '
         NAMES(031,012) = 'TOPHAL  '
         NAMES(031,013) = 'TOPLAL  '
         NAMES(031,014) = 'GFOHAL  '
         NAMES(031,100) = 'NTOPAL  '
         NAMES(031,101) = 'NERSAL  '
         NAMES(031,102) = 'NGFOAL  '
         NAMES(031,103) = 'NTPXAL  '
         NAMES(031,104) = 'NJSNAL  '
         NAMES(031,109) = 'ENVSAL  '
         FIRST = .FALSE.
      ENDIF
 
C  FOR VALID TYPES, RETURN EITHER MODSBUFR TAG OR BLANK
C  ----------------------------------------------------
 
      OBNAME = NAMES(MTYP,MSBT)
 
      RETURN
      END
