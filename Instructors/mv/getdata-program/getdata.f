
      COMMON X(50000)
C
      LENX=50000
C
      NC=25
      NR=40
      NL=5
      ND= NR*NC*NL
      DX=500.
      DY=500.
C
      ISUM=1
      LCHEAD=ISUM
      ISUM=ISUM+ND
      LCBOT=ISUM
      ISUM=ISUM+ND
      LCTOP=ISUM
      ISUM=ISUM+ND
      LCHY=ISUM
      ISUM=ISUM+ND
      LCHYAT=ISUM
      ISUM=ISUM+(NR*NC)
      LCVCAV=ISUM
      ISUM=ISUM+(NR*NC)
      LCZNOD=ISUM
      ISUM=ISUM+NL
      ISUM=ISUM-1
C
      IF(ISUM.GT.LENX) THEN
      WRITE(*,1000) ISUM,LENX
1000  FORMAT(1X,I6,' out of ',I6,' elements used in X array')
      WRITE(*,*) 'Increase size of X array'
      STOP
      END IF
C
      CALL GETDAT(X(LCHEAD),X(LCBOT),X(LCTOP),X(LCHY),X(LCHYAT),
     1  X(LCVCAV),X(LCZNOD),NR,NC,NL,dx,dy)
C
      STOP
      END
           
      SUBROUTINE GETDAT(HEAD,BOT,TOP,HY,HYAT,VCAV,ZNODE,NR,NC,NL,
     1                  DX,DY)
      DIMENSION HEAD(NC,NR,NL),BOT(NC,NR,NL),TOP(NC,NR,NL),
     1    HY(NC,NR,NL),HYAT(NC,NR),VCAV(NC,NR),ZNODE(NL)
      DIMENSION IROW(50),JCOL(50),BOTTOM(50),
     1          HWT(50),HSCR(50),IOPT(50)
      CHARACTER*80 FNAME,STRING
      CHARACTER*75 HEADER
      CHARACTER*3 GROUP(15)
      CHARACTER*10 RQ
C
      RQ= '0123456789'
      GROUP(1)='g01'
      GROUP(2)='g02'
      GROUP(3)='g03'
      GROUP(4)='g04'
      GROUP(5)='g05'
      GROUP(6)='g06'
      GROUP(7)='g07'
      GROUP(8)='g08'
      GROUP(9)='g09'
      GROUP(10)='g10'
      GROUP(11)='g11'
      GROUP(12)='g12'
      GROUP(13)='g13'
      GROUP(14)='g14'
      GROUP(15)='g15'
C
      OPEN(8,FILE='hyaqt.sml',STATUS='OLD')
      OPEN(9,FILE='vcont3.sml',STATUS='OLD')
      OPEN(10,FILE='head.sml',STATUS='OLD')
      OPEN(11,FILE='bot.sml',STATUS='OLD')
      OPEN(12,FILE='hycond.sml',STATUS='OLD')
      OPEN(13,FILE='top4.sml',STATUS='OLD')
C
      DO 1 K=1,NL
      DO 1 I=1,NR
      READ(10,'(15F8.2)') (HEAD(J,I,K),J=1,NC)
      READ(11,'(15F8.2)') (BOT(J,I,K),J=1,NC)
      READ(12,'(15F8.2)') (HY(J,I,K),J=1,NC)
1     CONTINUE
C
      DO 2 I=1,NR
      READ(13,'(15F8.2)') (TOP(J,I,4),J=1,NC)
2     CONTINUE
C
      DO 3 K=2,NL
      IF(K.NE.4) THEN
      DO 4 I=1,NR
      DO 4 J=1,NC
4     TOP(J,I,K)=BOT(J,I,K-1)
      END IF
3     CONTINUE      
C
      DO 5 I=1,NR
      DO 5 J=1,NC
5     TOP(J,I,1)=HEAD(J,I,1)
c
      DO 100 I=1,NR
      READ(8,'(15F8.2)') (HYAT(J,I),J=1,NC)
      READ(9,'(13E10.3)') (VCAV(J,I),J=1,NC)
100   CONTINUE
C
      CLOSE(8)
      CLOSE(9)
      CLOSE(10)
      CLOSE(11)
      CLOSE(12)
      CLOSE(13)
      WRITE(*,*) 'ENTER GROUP NUMBER:'
      READ(*,*) IG
      WRITE(*,*) 'ENTER REQUEST NUMBER:'
      READ(*,*) IRQ
      IRQ=IRQ+1
      IF(IRQ.GT.1) THEN
      IRQM=IRQ-1
      FNAME= 'wells-r'//RQ(IRQM:IRQM)//'.'//GROUP(IG)
      OPEN(10,FILE=FNAME,STATUS='OLD',ERR=5100)
      READ(10,'(A)') HEADER
      READ(10,*) NWELL
      DO 6 N=1,NWELL
      READ(10,*) NN,IROW(N),JCOL(N),HWT(N),HSCR(N),BOTTOM(N),IOPT(N)
6     CONTINUE
      CLOSE(10)
      ELSE
      IRQ=1
      NWELL=0
      END IF
      FNAME= 'wells-r'//RQ(IRQ:IRQ)//'.'//GROUP(IG)
      OPEN(10,FILE=FNAME,STATUS='NEW',ERR=5000)
      FNAME= 'data-r'//RQ(IRQ:IRQ)//'.'//GROUP(IG)
      OPEN(15,FILE=FNAME)
C
      HEADER= GROUP(IG)//' ;  request '//RQ(IRQ:IRQ)
C
      WRITE(15,'(A)') HEADER
      WRITE(15,*) ' '
10    CONTINUE
      WRITE(*,*) 'Select an option:'
      WRITE(*,*) '  1 = new wells'
      WRITE(*,*) '  2 = slug test on existing wells'
      WRITE(*,*) '  3 = seismic section'
      WRITE(*,*) '  4 = quit'
      READ(*,*) IANS
      IF(IANS.LT.1.OR.IANS.GT.4) THEN
      WRITE(*,*) 'Invalid option, choose again...'
      GO TO 10
      END IF
C
      IF(IANS.EQ.1) THEN
      KOUNT=0
      NW=NWELL
11    KOUNT=KOUNT+1
      NW= NW + 1
      IF(KOUNT.EQ.1) THEN
      WRITE(*,*) 
     1'Test Options are: 0 = no test'
      WRITE(*,*)
     1'                  1 = slug test'
      WRITE(*,*)
     1'                  2 = aquifer test'
      WRITE(*,*) 
     1'To end the list & return to menu, enter 0 for all data items'
      WRITE(*,*) ' '
      END IF
      WRITE(*,*) 'ENTER: row, column, bottom elevation, test option'
      READ(*,*) IROW(NW),JCOL(NW),BOTTOM(NW),IOPT(NW)
      IF(IROW(NW).EQ.0.AND.JCOL(NW).EQ.0) THEN
      NW=NW-1
      GO TO 12
      END IF
C--- check that rows and columns are in bounds
      IF(IROW(NW).LT.1.OR.IROW(NW).GT.NR
     1                .OR.JCOL(NW).LT.1.OR.JCOL(NW).GT.NC) THEN
      WRITE(*,*) 'row and/or column are out of bounds'
      NW=NW-1
      GO TO 11
      END IF
C--- check for duplicate well      
      NWM1=NW-1
      DO 45 N=1,NWM1
      IF(IROW(NW).EQ.IROW(N).AND.JCOL(NW).EQ.JCOL(N)) THEN
      WRITE(*,*) 'Well already exists in this location.'
      NW=NW-1
      GO TO 11
      END IF
45    CONTINUE
      IF(HY(JCOL(NW),IROW(NW),1).GT.1.0E+6) THEN
      WRITE(*,*) 'This location is in the lake. Try another location.'
      NW=NW-1
      GO TO 11
      END IF
      IF(KOUNT.EQ.1) THEN
      WRITE(15,*) 
     1'-------------------- < Data for New Wells > --------------------'
      END IF
      CALL WELL(IROW(NW),JCOL(NW),BOTTOM(NW),15,NW,HEAD,TOP,BOT,HY,
     1  ZNODE,NR,NC,NL,IOPT(NW),HWT(NW),HSCR(NW),HYAT,VCAV)
      GO TO 11
12    CONTINUE
      NWELL=NW
      GO TO 10
      ELSE IF(IANS.EQ.2) THEN
      IF(NWELL.EQ.0) THEN
      WRITE(*,*) 'There are no existing wells to test.'
      GO TO 10
      END IF
      WRITE(*,*) 'ENTER: well number'
      READ(*,*) IWELL
      IF(IWELL.LT.1.OR.IWELL.GT.NWELL) THEN
      WRITE(*,*) 'Well number is out of range.'
      GO TO 10
      END IF      
      IF(IOPT(IWELL).EQ.1) THEN
      WRITE(*,*) 'Slug test already exists for this well.'
      WRITE(*,*) ' '
      GO TO 10
      END IF
      IOPT(IWELL)=1
      WRITE(15,*) 
     1'--------------- < Slug Test for Existing Well > ----------------'
      CALL WELL(IROW(IWELL),JCOL(IWELL),BOTTOM(IWELL),15,IWELL,
     1  HEAD,TOP,BOT,HY,ZNODE,NR,NC,NL,IOPT(IWELL),HWT(IWELL),
     2  HSCR(IWELL),HYAT,VCAV)
      WRITE(*,1000) IWELL
1000  FORMAT(1X,'Slug test generated for well ',I2)
      WRITE(*,*) '(results placed in the <data-r#> output file)'
      WRITE(*,*) '(the <wells-r#> file will be updated)'
      WRITE(*,*) ' '
      GO TO 10
      ELSE IF(IANS.EQ.3) THEN
13    WRITE(*,*) 'Is section along a row or along a column?'
      WRITE(*,*) '    (1=row  2=column)'
      READ(*,*) ISEC
      IF(ISEC.LT.1.OR.ISEC.GT.2) GO TO 13
      IF(ISEC.EQ.1) THEN
14    WRITE(*,*) 'ENTER: row number'
      READ(*,*) ISROW
      IF(ISROW.LT.1.OR.ISROW.GT.NR) GO TO 14
      WRITE(15,*) 
     1'--------------- < Seismic Section along a Row > ----------------'
      DO 62 J=1,NC
      WRITE(15,1100) ISROW,J,BOT(J,ISROW,NL)
62    CONTINUE
      ELSE IF(ISEC.EQ.2) THEN
15    WRITE(*,*) 'ENTER: column number'
      READ(*,*) JSCOL
      IF(JSCOL.LT.1.OR.JSCOL.GT.NC) GO TO 15
      WRITE(15,*) 
     1'-------------- < Seismic Section along a Column > --------------'
      DO 64 I=1,NR
      WRITE(15,1100) I,JSCOL,BOT(JSCOL,I,NL)
64    CONTINUE
      END IF
      WRITE(*,*) '(seismic section generated)'
      WRITE(*,*) '(results placed in the <data-r#> output file)'
1100  FORMAT(1X,'Row ',I2,' Column ',I2,'  Bedrock = ',F7.2,' feet') 
      GO TO 10
      END IF
60    CONTINUE
      OPEN(30,FILE='template.dcf')
      FNAME= 'dcf-r'//RQ(IRQ:IRQ)//'.'//GROUP(IG)
      OPEN(31,FILE=FNAME,STATUS='NEW',ERR=5200) 
      IUDCF=31
70    READ(30,'(A)',END=71) STRING
      WRITE(31,'(A)') STRING
      GO TO 70
71    CONTINUE 
      CLOSE(30)   
      WRITE(10,'(A)') HEADER
      WRITE(10,'(I10)') NWELL
      DO 50 N=1,NWELL
      WRITE(10,'(3I10,3F10.2,I3)') N,IROW(N),JCOL(N),
     1        HWT(N),HSCR(N),BOTTOM(N),IOPT(N)
      CALL MAKDCF(IUDCF,N,DX,DY,IROW(N),JCOL(N),NR)
50    CONTINUE      
      RETURN
5000  WRITE(*,*) 'well file for current request already exists'
      STOP
5100  WRITE(*,*) 'well file for previous request does not exist'
      STOP
5200  write(*,*) 'dcf file for current request already exists'
      STOP
      END

      SUBROUTINE WELL(IR,JC,EL,IO,NWELL,HEAD,TOP,BOT,HY,ZNODE,NR,NC,NL,
     1                IOPT,HWT,HSCR,HYAT,VCAV)
      DIMENSION HEAD(NC,NR,NL),TOP(NC,NR,NL),BOT(NC,NR,NL),HY(NC,NR,NL),
     1          HYAT(NC,NR),VCAV(NC,NR)
      DIMENSION ZNODE(NL)
      CHARACTER*80 DES(5)
C
      DES(1)= 
     1' >fine to medium grained sand with interbedded silts'
      DES(2)=
     1' >medium sand with some interbedded fine sand'
      DES(3)=
     1' >coarse sand with occasional stringers of fine sand'
      DES(4)=
     1' >very coarse sand with occasional thin gravel layers'
      DES(5)=
     1' >tight clay'
      ICB=0
      IF(BOT(JC,IR,3).GT.TOP(JC,IR,4)) ICB=1
      IBROCK=0
      IF(EL.LE.BOT(JC,IR,NL)) THEN
      EL=BOT(JC,IR,NL)
      IBROCK=1
      END IF
      IF(EL.LT.BOT(JC,IR,3).AND.EL.GT.TOP(JC,IR,4)) THEN
      EL=EL-10.0
      END IF
C
      DO 1 K=1,NL
      ZNODE(K)= 0.5*TOP(JC,IR,K) + 0.5*BOT(JC,IR,K)
1     CONTINUE
C
      HYSUM=0.0
      DO 2 K=1,NL
      HYSUM=HYSUM+HY(JC,IR,K)
      LBOT=K
      IF(EL.GE.BOT(JC,IR,K)) GO TO 3
2     CONTINUE
3     CONTINUE     
      HYAVE= HYSUM/LBOT
      DO 4 I=1,4
      HYLOW=(I-1)*125.0
      HYBIG=I*125.0
      IF(HYAVE.GE.HYLOW.AND.HYAVE.LE.HYBIG) THEN
      ICOND=I
      GO TO 5
      END IF
4     CONTINUE
      ICOND=5
5     CONTINUE
      CALL LNGSTR(80,DES(ICOND),NLNG)
      CALL LNGSTR(80,DES(5),NLNG5)
C
      HWT=HEAD(JC,IR,1)
      IF(EL.GE.ZNODE(LBOT).AND.LBOT.EQ.1) THEN
        HSCR=HEAD(JC,IR,LBOT)            
        HYCEL=HY(JC,IR,LBOT)
      ELSE IF(EL.LT.ZNODE(LBOT).AND.LBOT.EQ.NL) THEN
        HSCR=HEAD(JC,IR,LBOT)
        HYCEL=HY(JC,IR,LBOT)
      ELSE IF(EL.GE.ZNODE(LBOT)) THEN
        IF(LBOT.EQ.4.AND.ICB.EQ.1) THEN
          HSCR=HEAD(JC,IR,LBOT)
          HYCEL=HY(JC,IR,LBOT)
        ELSE
          ZL=ZNODE(LBOT-1) - ZNODE(LBOT)
          ZFR= EL - ZNODE(LBOT)
          FRAC= ZFR/ZL
          HSCR= FRAC*HEAD(JC,IR,LBOT-1)
     1                    + (1.0-FRAC)*HEAD(JC,IR,LBOT)
          HYCEL=FRAC*HY(JC,IR,LBOT-1)
     1                    + (1.0-FRAC)*HY(JC,IR,LBOT)
        END IF
      ELSE IF(EL.LT.ZNODE(LBOT)) THEN
        IF(LBOT.EQ.3.AND.ICB.EQ.1) THEN
          HSCR= HEAD(JC,IR,LBOT)
          HYCEL=HY(JC,IR,LBOT)
        ELSE
          ZL=ZNODE(LBOT) - ZNODE(LBOT+1)
          ZFR= EL - ZNODE(LBOT+1)
          FRAC= ZFR/ZL
          HSCR= FRAC*HEAD(JC,IR,LBOT) 
     1                    + (1.0-FRAC)*HEAD(JC,IR,LBOT+1)
          HYCEL=FRAC*HY(JC,IR,LBOT)
     1                    + (1.0-FRAC)*HY(JC,IR,LBOT+1)
        END IF
      END IF      
C
C--- write summary of well info and drillers log
C
      WRITE(IO,1000) NWELL
1000  FORMAT(1X,'*********************************** Well ',I2,
     1' *********************************')
      WRITE(IO,1050) IR,JC
1050  FORMAT(1X,'(Row',I3,'  Column',I3,')')
      SCRTOP=EL+5.0
      IF(ICB.EQ.1.AND.LBOT.GT.3.AND.SCRTOP.GT.TOP(JC,IR,4)) THEN
      SCRTOP=TOP(JC,IR,4)
      END IF
      WRITE(IO,1100) SCRTOP,EL
1100  FORMAT(1X,'Top of screen = ',F7.2,' feet; Bottom of screen = ',
     1F7.2,' feet')
      WRITE(IO,1200) HWT,HSCR
1200  FORMAT(1X,'Water Table = ',F7.2,' feet;  Head in screened interval
     1 = ',F7.2)
      IF(IOPT.EQ.1) THEN
      IHYCON= IFIX(HYCEL)
      WRITE(IO,1500) IHYCON
1500  FORMAT(1X,'Horizontal hydraulic conductivity from slug test =',
     1 I4,' feet/day')
      ELSE IF(IOPT.EQ.2) THEN
      IHYAT= IFIX(HYAT(JC,IR))
      WRITE(IO,1600) IHYAT
1600  FORMAT(1X,'Horizontal hydraulic conductivity from aquifer test =',
     1 I4,' feet/day')
      IF(ICB.EQ.1) WRITE(IO,1700) VCAV(JC,IR)
1700  FORMAT(1X,'Estimated (Kv/thickness) of clay = ',E10.3)
      END IF
      WRITE(IO,*) ' '
      WRITE(IO,*) 'Drillers Log : '
      IF(ICB.EQ.1.AND.LBOT.GT.3) THEN
      WRITE(IO,1300) HWT,BOT(JC,IR,3),DES(ICOND)(1:NLNG)
      WRITE(IO,1300) BOT(JC,IR,3),TOP(JC,IR,4),DES(5)(1:NLNG5)
      WRITE(IO,1300) TOP(JC,IR,4),EL,DES(ICOND)(1:NLNG)
      ELSE
      WRITE(IO,1300) HWT,EL,DES(ICOND)(1:NLNG)
      END IF
1300  FORMAT(1X,F7.2,' to ',F7.2,A)
      IF(IBROCK.EQ.1) WRITE(IO,1400) EL
1400  FORMAT(1X,F7.2,' to    ?    >Bedrock')
      WRITE(IO,*) ' '
C
      RETURN
      END

      SUBROUTINE LNGSTR(NSTR,STRING,NLNG)
      CHARACTER*(*) STRING
C
      DO 1 N=NSTR,1,-1
      IF(STRING(N:N).NE.' ') THEN
      NLNG=N
      GO TO 2
      END IF
1     CONTINUE
      NLNG=0
2     RETURN
      END

      
      SUBROUTINE MAKDCF(IUDCF,NWELL,DX,DY,IROW,JCOL,NROW)
      CHARACTER*2 TX
      TX= 'TX'
      LEVEL=4
      ICLIP=1
      ANGLE=0.0
      ITXFIL=0
      X= DX*(0.5 + FLOAT(JCOL-1))
      Y= DY*(0.5 + FLOAT(NROW-IROW))
      WRITE(IUDCF,1000) TX,X,Y,LEVEL,ICLIP,ANGLE,ITXFIL
      IF(NWELL.LT.10) WRITE(IUDCF,'(I1)') NWELL
      IF(NWELL.GE.10) WRITE(IUDCF,'(I2)') NWELL
1000  FORMAT(A2,8X,2F10.1,2I10,F10.1,I10)
      RETURN
      END