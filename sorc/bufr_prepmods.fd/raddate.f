C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE RADDATE(ADATE,DHOUR,BDATE)
 
      DIMENSION   MON(12)
      REAL(8)     ADATE,BDATE
 
      DATA MON/31,28,31,30,31,30,31,31,30,31,30,31/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  ONE WAY OR ANOTHER PARSE A TEN DIGIT DATE INTEGER
C  -------------------------------------------------
 
      KDATE = NINT(ADATE)
      IDATE = I4DY(KDATE)
      IY = MOD(IDATE/1000000,10000)
      IM = MOD(IDATE/10000  ,100  )
      ID = MOD(IDATE/100    ,100  )
      HR = MOD(ADATE        ,100._8 ) + DHOUR
      IF(MOD(IY    ,4).EQ.0) MON(2) = 29
      IF(MOD(IY/100,4).NE.0) MON(2) = 28
 
 
1     IF(HR.LT.0) THEN
         HR = HR+24
         ID = ID-1
         IF(ID.EQ.0) THEN
            IM = IM-1
            IF(IM.EQ.0) THEN
               IM = 12
               IY = IY-1
            ENDIF
            ID = MON(IM)
         ENDIF
         GOTO 1
      ELSEIF(HR.GE.24) THEN
         HR = HR-24
         ID = ID+1
         IF(ID.GT.MON(IM)) THEN
            ID = 1
            IM = IM+1
            IF(IM.GT.12) THEN
               IM = 1
               IY = IY+1
            ENDIF
         ENDIF
         GOTO 1
      ENDIF
 
      BDATE = IY*1000000 + IM*10000 + ID*100
      BDATE = BDATE + HR
 
      RETURN
      END
