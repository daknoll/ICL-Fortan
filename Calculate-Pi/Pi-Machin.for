C     Pi-Machin.for from Pi-Machin-04.for
      program machin_pi

      implicit none
      integer*4 i
      real*8 pi, term_005, term_239, prev_pi, diff, sgn, factor

      i          =  0
      prev_pi    = -1.0d0
      pi         =  0.0d0
      sgn        =  1.0d0

      ! Print header
      print *, ' i     Pi Estimate         Difference'

   10 continue
         factor   = dble(2 * i + 1)
         term_005 = sgn / factor /   5.0d0**factor
         term_239 = sgn / factor / 239.0d0**factor
         prev_pi  = pi
         pi = pi + 16.0d0 * term_005 - 4.0d0 * term_239
         diff = abs(pi - prev_pi)
         write(*, 20) i, pi, diff
   20    format(I3, F20.16, F20.16)
         i = i + 1
         sgn = -sgn
         if (pi .ne. prev_pi) goto 10
!
      print *, '------------------------------------------'
      print *, '    3.1415926535897932 = True Pi'

      end
