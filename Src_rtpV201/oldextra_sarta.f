CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      This block of code is intended for use with AIRS RTP files
C      Convert prof.pnote(41:80) to yoff(1:17)
C
       IF (PROF.pnote(41:46) .EQ. 'YOFF17') THEN
          DO II=1:17
             JJ = 45 + II*2
             YOFF(I) = 0.001*FLOAT( C2TOI2(PROF.pnote(JJ:JJ+1)) )
          ENDDO
       ELSE
          DO II=1:17
             YOFF(II) = YOFFST
          ENDDO
       ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
