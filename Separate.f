      program Separate
c
c***********************************************************************
c       program to produce any tsunami source 
c***********************************************************************
c
      real h(3000,3000)
      real z(3000,3000),u(3000,3000),v(3000,3000)
      real dum1,dum2,dum3,dum4,dum5,dum6
      real x,y,xo,yo,r,len,ro,amp,pi,term
      character*30  input
      integer mx,ny,i,j
c
c----------------------------------------------------------------------
c ====> open Surfer ASCII grid file.
c----------------------------------------------------------------------
c
      write(*,*) 'Enter ASCII bathymetry filename'
      read(*,*) input
      write(*,*)
      write(*,*) 'Reading the ASCII grid file'
      write(*,*)
      open (10,file=input)
      read(10,*)
      read(10,*) mx,ny
      read(10,*) dum1,dum2
      read(10,*) dum3,dum4
      read(10,*) dum5,dum6
      do j = 1, ny
        read(10,*) (h(i,j),i=1,mx)
      enddo
      close (10)
c
c-----------------------------------------------------------------------
c ====> write the new ASCII grid file.
c-----------------------------------------------------------------------
c
      xo = -1127400.0
      yo = 9299000.0
      len = 5000.0
      ro = 2500.0
      amp = 45.0
      pi = 4.0*atan(1.0)
      do j = 1, ny
        do i = 1, mx
          x = dum1 + float(i-1)*(dum2-dum1)/float(mx-1)
          y = dum3 + float(j-1)*(dum4-dum3)/float(ny-1)
          r = sqrt((x-xo)*(x-xo)+(y-yo)*(y-yo))
          if (h(i,j).gt.0.0) then
            term = abs(z(i,j)/h(i,j))
            if (term.gt.1.0) term = 1.0
          else
            term = 0.0
          endif
          if (x.gt.xo) then
            ang = atan((y-yo)/(x-xo))
          endif
          if (x.eq.xo) then
            if (y.gt.yo) ang = pi/2.0
            if (y.eq.yo) ang = 0.0
            if (y.lt.yo) ang = -pi/2.0
          endif
          if (x.lt.xo) then
            ang = atan((y-yo)/(x-xo))-pi
          endif
          if (r.gt.(len+ro)) then
            z(i,j) = 0.0
            u(i,j) = 0.0
            v(i,j) = 0.0
          endif
          if ((r.le.(len+ro)).and.(r.gt.ro)) then
            z(i,j) = amp*((len+ro)-r)/len
            u(i,j) = cos(ang)*sqrt(9.81*abs(z(i,j)+h(i,j)))*term
            v(i,j) = sin(ang)*sqrt(9.81*abs(z(i,j)+h(i,j)))*term
          endif
          if ((r.le.ro).and.(r.gt.(ro-500))) then
            z(i,j) = amp*(500-(ro-r))/500-h(i,j)
            u(i,j) = cos(ang)*sqrt(9.81*abs(z(i,j)+h(i,j)))*term
            v(i,j) = sin(ang)*sqrt(9.81*abs(z(i,j)+h(i,j)))*term
          endif
          if (r.le.(ro-500)) then
            z(i,j) = -h(i,j)
            u(i,j) = 0.0
            v(i,j) = 0.0
          endif
        enddo
      enddo
c
c-----------------------------------------------------------------------
c ====> write the new ASCII grid file.
c-----------------------------------------------------------------------
c
      write(*,*)
      write(*,*) 'Writing the tsunami source ASCII grid files'
      write(*,*)
      write(*,*) 'Free surface elevation'
c
      dum5 = 10000.0
      dum6 = -10000.0
      do j = 1, ny
        do i = 1, mx
          if (z(i,j).lt.dum5) dum5 = z(i,j)
          if (z(i,j).gt.dum6) dum6 = z(i,j)
        enddo
      enddo
c
      open (20,file="surface.grd")
      write(20,208)
      write(20,211) mx,ny
      write(20,210) dum1,dum2
      write(20,210) dum3,dum4
      write(20,210) dum5,dum6
      do j = 1, ny
        write(20,209) (z(i,j),i=1,mx)
      enddo
      close (20)
      write(*,*)
      write(*,*) 'U velocity'
c
      dum5 = 10000.0
      dum6 = -10000.0
      do j = 1, ny
        do i = 1, mx
          if (u(i,j).lt.dum5) dum5 = u(i,j)
          if (u(i,j).gt.dum6) dum6 = u(i,j)
        enddo
      enddo
c
      open (30,file="uvel.grd")
      write(30,208)
      write(30,211) mx,ny
      write(30,210) dum1,dum2
      write(30,210) dum3,dum4
      write(30,210) dum5,dum6
      do j = 1, ny
        write(30,209) (u(i,j),i=1,mx)
      enddo
      close (30)
      write(*,*)
      write(*,*) 'V velocity'
c
      dum5 = 10000.0
      dum6 = -10000.0
      do j = 1, ny
        do i = 1, mx
          if (v(i,j).lt.dum5) dum5 = v(i,j)
          if (v(i,j).gt.dum6) dum6 = v(i,j)
        enddo
      enddo
c
      open (40,file="vvel.grd")
      write(40,208)
      write(40,211) mx,ny
      write(40,210) dum1,dum2
      write(40,210) dum3,dum4
      write(40,210) dum5,dum6
      do j = 1, ny
        write(40,209) (v(i,j),i=1,mx)
      enddo
      close (40)
c
  208 format('DSAA')
  209 format(4f15.6)
  210 format(2f20.6)
  211 format(2i20)
c
      pause
      stop
      end




