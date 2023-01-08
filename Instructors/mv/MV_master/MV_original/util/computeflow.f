
      DIMENSION Q(100)
      CHARACTER*132 FNAME
      CHARACTER*100 STRING
C
      WRITE(*,*) 'ENTER INPUT FILE NAME:'
      READ(*,'(A)') FNAME
      OPEN(10,FILE=FNAME)
C
      OPEN(12,FILE='streamflow')
C
     
      N=0
5     READ(10,'(A)',END=6) STRING
      N=N+1
      GO TO 5
6     CONTINUE
      REWIND(10)
      WRITE(*,*) N
10    READ(10,'(A)',END=100) STRING
      READ(STRING(91:100),'(F10.0)') Q(N)
      N=N-1
      GO TO 10
100   CONTINUE

      SUM=0.0
      DO 20 N=1,18
      LAYER= 22 + N
      GAIN=0.0
      DO 30 I=1,5
      K= 5*(N-1) + I
      GAIN=GAIN - Q(K)
30    CONTINUE
      SUM=SUM+GAIN
      WRITE(12,1000) LAYER,GAIN,SUM
20    CONTINUE
1000  FORMAT(I5,2F10.0)
      STOP 
      END
     
      
      
           