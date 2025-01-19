c
c??????????????????????????????????????????????????
c Numerical modeling of the elastic wave propogations
c??????????????????????????????????????????????????
c
      Program Geowave
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
c  Parameters relating foremost to TOPICS
c
      real*8 grd,to,tsrc,d,pi,dx,dy,dt,test,dep,lambda
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
c
      integer im,jm,flag,fail,iden,idep,grid,type,nsrc
c
      character input*20,output*4,proper
c
c  Parameters relating foremost to FUNWAVE
c
      real hs,timeo,tout1,tout2,zlim,dpmax,dpmin
      real tinit,per,dsrc,xlm
c
      integer msrc,itype,nt,itdel,itmov,ngage,nlag
      integer ingm,mx,ny,ifail,ibe
c
      character f8n*30,f9n*30
c
c  Parameters relating foremost to Geowave
c
      real c,tmp,timef,tmpmin,tmpmax
c
      integer itmp,quest,i,j,k,l,ntmin,chzibe
c
      dimension c(6)
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
c
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      pi = 4.0D+00 * datan(1.0D+00)
c
      write(*,*)
      write(*,*)
      write(*,*)
      write(*,*) '                
      write(*,*)
      write(*,*) '              
      write(*,*)
      write(*,*)
      write(*,*) 'Developers'
      write(*,*)
      write(*,*)
      write(*,*) 'A wide range of tsunami initial conditions'
      write(*,*) 'Fully nonlinear Boussinesq wave propagation'
      write(*,*) 'Turbulent and breaking wave inundation'
      write(*,*)
      write(*,*)
      write(*,*) 'Please make sure there is a folder Data'
      write(*,*) 'Please make sure there is a folder Grid'
      write(*,*) 'Please make sure there is a folder Movie'
      write(*,*)
      write(*,*)
c
c  You can specify Boussinesq or NSSW simulation equations.  
c 
      write(*,*) 'Type 0 for Boussinesq and 1 for NSWW equations'
      read(*,*) chzibe
      write(*,*)
      if (chzibe.eq.1) then
        ibe=4
      else
        ibe=2
      endif
c
c  You can specify any filename provided the format can be read.  
c 
      write(*,*) 'Enter the bathymetry filename (up to 20 char)'
      read(*,*) input
      write(*,*)
c
c  Specify the format of the bathymetry file.  
c
      write(*,*) 'Type 0 if this is a Surfer ASCII grid file'
      read(*,*) grid
      grid = iabs(grid)
      write(*,*)
c
c  Check the last entry.
c
      if (grid.ne.0) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Specify the format of the initial condition output file.  
c
      write(*,*) 'Type 0 to output Surfer ASCII grid files'
      read(*,*) type
      type = iabs(type)
      write(*,*)
c
c  Check the last entry and specify file extension.
c
      if (type.ne.0) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (type.eq.0) then
        output = '.grd'
      endif
c
c  Read the grid file in the specified format.  
c
      write(*,*) 'Opening the bathymetry file'
      write(*,*)
      if (grid.eq.0) then
        open (202,file=input)
        read(202,*)
        read(202,*) im,jm
        read(202,*) xw,xe
        read(202,*) ys,yn
        read(202,*) dmin,dmax
        if((im.gt.iq).OR.(jm.gt.jq))then
          write(*,*) 'Array parameters are too small'
          write(*,*) 'Program stopped, press return'
          pause
          stop
        endif
        do j=1,jm
          read(202,*)(grd(i,j),i=1,im)
        enddo
        close(202)
      endif
c
c  Positive water depths are used within TOPICS.
c
      write(*,*) 'Type 0 if depths are positive'
      write(*,*) 'Type 1 if depths are negative'
      read(*,*) iden
      iden=iabs(iden)
      write(*,*)
c
c  Check the data for consistency.
c
      if ((iden.ne.0).and.(iden.ne.1)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (xe.le.xw) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'East and west grid edges overlap'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (yn.le.ys) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'North and south grid edges overlap'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (im.le.3) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'Three or fewer columns in the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (jm.le.3) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'Three or fewer rows in the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (im.le.23) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) '23 or fewer columns in the grid'
        write(*,*) 'Simulation domain is sponge layers'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (jm.le.23) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) '23 or fewer columns in the grid'
        write(*,*) 'Simulation domain is sponge layers'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  Calculate the spatial increments of the grid.
c
      xinc = (xe - xw) / dfloat(im - 1)
      yinc = (yn - ys) / dfloat(jm - 1)
c
c  Grid distance must be converted to meters for TOPICS.
c
      write(*,*) 'Type 0 if the grid is in meters'
      write(*,*) 'Type 1 if the grid is in feet'
      write(*,*) 'Type 2 if the grid is in kilometers'
      write(*,*) 'Type 3 if the grid is in miles'
      write(*,*) 'Type 4 if the grid is in decimal degrees'
      read(*,*) flag
      flag = iabs(flag)
      write(*,*)
      if (flag.eq.0) then
        ylat = 1.0D+00
        xlon = 1.0D+00
      endif
      if (flag.eq.1) then
        ylat = 0.3048+00
        xlon = 0.3048D+00
      endif
      if (flag.eq.2) then
        ylat = 1.0D+03
        xlon = 1.0D+03
      endif
      if (flag.eq.3) then
        ylat = 1.609344D+03
        xlon = 1.609344D+03
      endif
      if (flag.eq.4) then
        ylat = 111.265D+03
        xlon = ylat * dcos((yn + ys) * pi / 3.6D+02)
      endif
c
c  Check the last entry.
c
      if ((flag.lt.0).and.(flag.gt.4)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Calculate the vertical and horizontal grid spacing in meters.
c 
      dx = xinc * xlon
      dy = yinc * ylat
c
c  Verify that the grid is close to being uniform, dx=dy.
c
      test = 2.0D+02 * (dabs(dx) - dabs(dy)) / (dabs(dx) + dabs(dy))
      if (dabs(test).gt.5.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The bathy grid is more than 5% nonuniform'
        write(*,*) 'This may make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif 
c
c  Get ready to convert the depths into meters.
c
      write(*,*) 'Type 0 if depth is in meters'
      write(*,*) 'Type 1 if depth is in feet'
      write(*,*) 'Type 2 if depth is in kilometers'
      write(*,*) 'Type 3 if depth is in miles'
      write(*,*) 'Type 4 if depth is in fathoms'
      read(*,*) idep
      idep = iabs(idep)
      write(*,*)
      if (idep.eq.0) then
        dep = 1.0D+00
      endif
      if (idep.eq.1) then
        dep = 0.3048+00
      endif
      if (idep.eq.2) then
        dep = 1.0D+03
      endif
      if (idep.eq.3) then
        dep = 1.609344D+03
      endif
      if (idep.eq.4) then
        dep = 1.8288D+00
      endif
c
c  Check the last entry.
c
      if ((idep.lt.0).and.(idep.gt.4)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Convert grid file max and min to meters.  
c 
      dmin = dmin * dep
      dmax = dmax * dep
c
c  Prepare to render all depths positive.
c 
      if (iden.eq.0) then
        test = 1.0D+00
      else
        test = -1.0D+00
      endif
c
c  Rescale the bathymetry data so that it is positive and in meters.
c
      write(*,*) 'Rescaling the bathymetry grid'
      write(*,*)
      do i=1,im
        do j=1,jm
          grd(i,j) = test * dep * grd(i,j)
        enddo
      enddo
      if (iden.ne.0) then
        test = dmax
        dmax = -dmin
        dmin = -test
      endif
c
c  Convert some variables to FUNWAVE names.
c
      dpmin = dmin
      dpmax = dmax
      mx = im
      ny = jm
      itype = type
c
c  Provide the option of filtering the bathymetry.
c
      write(*,*) 'Smoothing the bathymetry may help convergence'
      write(*,*) 'A 4th order, 5 point Savitsky-Golay filter exists'
      write(*,*)
      write(*,*) 'Type 0 to smooth the bathymetry, 1 otherwise'
      read(*,*) quest
      write(*,*)
      if (quest.eq.0) then
        do j = 1,jm
          do i = 1,im
            hs(i,j)=0.0
          enddo
        enddo
        write(*,*) 'Smoothing the bathymetry data'
        write(*,*)
        tmp = 0.0D+00
        c(0) =  0.333
        c(1) =  0.280
        c(2) =  0.140
        c(3) = -0.023
        c(4) = -0.105
        c(5) =  0.042
        do j = 1,jm
          do i = 1,im
            if (i.le.6) goto 333
            if (j.le.6) goto 333
            if (i.ge.(im-5)) goto 333
            if (j.ge.(jm-5)) goto 333
            do k=-5,5
              do l=-5,5
                if (grd(i+k,j+l).le.tmp) goto 333
                hs(i,j) = hs(i,j) + grd(i+k,j+l) * c(iabs(k)) * 
     &                                             c(iabs(l))
              enddo
            enddo
            goto 222
 333        continue
            hs(i,j) = grd(i,j)
 222        continue
          enddo
        enddo
      else
        do i=1,im
          do j=1,jm
            hs(i,j) = grd(i,j)
          enddo
        enddo
      endif
c
c  Write the smoothed bathy to a file so that it can be checked.
c
      if (quest.eq.0) then
        if (type.eq.0) then
          open (26, file='smoothed'//output)
          tmpmin=1.0e+10
          tmpmax=-1.0e+10
          do j=1,jm
            do i=1,im
              if (hs(i,j).gt.tmpmax) tmpmax=hs(i,j)
              if (hs(i,j).lt.tmpmin) tmpmin=hs(i,j) 
            enddo
          enddo
          write(26,210)
          write(26,211) im,jm
          write(26,212) xw,xe
          write(26,212) ys,yn
          write(26,212) tmpmin,tmpmax
          do j = 1, jm
            write(26,213) (hs(i,j),i=1,im)
          enddo
          close(26)
        endif
      endif
  210 format('DSAA')
  211 format(2i20)
  212 format(2f20.6)
  213 format(4f15.6)
c
c  Specify the number of numerical wave gauges.  
c
      write(*,*) 'Enter the number of numerical wave gauges (0-30)'
      read(*,*) ngage
      ngage = iabs(ngage)
      write(*,*)
c
c  Check the last entry.
c
      if ((ngage.lt.0).and.(ngage.gt.ngm)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  You can specify any filename for the wave gauges.  
c 
      if (ngage.gt.0) then
        write(*,*) 'Enter the wave gauge filename (up to 20 char)'
        read(*,*) f8n
        write(*,*)
      endif
c
c  Specify the number of numerical Lagrangian markers.  
c
      write(*,*) 'Enter the number of Lagrangian markers (0-30)'
      read(*,*) nlag
      nlag = iabs(nlag)
      write(*,*)
c
c  Check the last entry.
c
      if ((nlag.lt.0).and.(nlag.gt.ngm)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  You can specify any filename for the Lagrangian markers.  
c 
      if (nlag.gt.0) then
        write(*,*) 'Enter the markers filename (up to 20 char)'
        read(*,*) f9n
        write(*,*)
      endif
c
c  Find the units for any wave gauges or Lagrangian markers.
c
      if ((nlag.gt.0).or.(ngage.gt.0)) then
        write(*,*) 'Type 0 if locations are in grid units'
        write(*,*) 'Type 1 if locations are in node numbers'
        read(*,*) ingm
        ingm=iabs(ingm)
        write(*,*)
      endif
c
c  Check the data for consistency.
c
      if ((nlag.gt.0).or.(ngage.gt.0)) then
        if ((ingm.ne.0).and.(ingm.ne.1)) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'That was an invalid entry'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
      endif
c
c  Specify the number of tsunami sources.  
c
  666 continue
      write(*,*)
      write(*,*)
      write(*,*) 'Enter the number of tsunami sources (1-9)'
      read(*,*) msrc
      msrc = iabs(msrc)
      write(*,*)
c
c  Check the last entry.
c
      if ((msrc.lt.1).and.(msrc.gt.9)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Start the tsunami source loop.
c
      do i=1,msrc
  667   continue
        nsrc = i
        itmp = mod(nsrc,10)
        proper=char(itmp+48)
        write(*,*)
        write(*,*) nsrc, '  tsunami source ID'
        write(*,*)
        call topics
        dsrc(i) = d
        per(i) = to
        xlm(i) = lambda
        if (fail.eq.0) then
          tinit(i) = tsrc
        else
          tinit(i) = tsrc + to
        endif
        ifail(i) = fail
        write(*,*)
        write(*,*)
        write(*,*) 'PLEASE CHECK THIS TSUNAMI SOURCE NOW'
        write(*,*)
        write(*,*) 'Please check the output text file'
        write(*,*) 'Please plot the free surface shape'
        write(*,*) 'You can redo this tsunami source'
        write(*,*)
        write(*,*) 'Type 0 if tsunami source is OK, 1 otherwise'
        read(*,*) quest
        write(*,*)
        if (quest.ne.0) goto 667
      enddo
c
c  Give the user the chance to reenter all of the tsunami sources.
c
      write(*,*)
      write(*,*)
      write(*,*) 'PLEASE CHECK ALL OF THE TSUNAMI SOURCES NOW'
      write(*,*)
      write(*,*) 'Please check all of the output text files'
      write(*,*) 'Please plot all of the free surface shapes'
      write(*,*) 'You can decide to redo all tsunami sources'
      write(*,*)
      write(*,*) 'Type 0 if all tsunami sources are OK, 1 otherwise'
      read(*,*) quest
      if (quest.ne.0) goto 666
      write(*,*)
c
c  Check some of the TOPICS results.  
c
      if (dt.eq.0) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The time step dt cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Find the overall features of all tsunami sources.
c
  668 continue
      write(*,*)
      timeo = 1.0e+10
      timef = -1.0e+10
      do i=1,msrc
        if (tinit(i).lt.timeo) timeo = tinit(i)
        if (tinit(i).gt.timef) timef = tinit(i)
      enddo
      ntmin = nint((timef - timeo) / dt) + 2
c
c  Specify the number of simulation time steps.  
c
      write(*,*) 'Time of initial tsunami source:  ', timeo
      write(*,*) 'Time of final tsunami source:    ', timef
      write(*,*) 'Simulation time step (s):        ', dt
      write(*,*) 'Minimum number of time steps:    ', ntmin
      write(*,*) 
      write(*,*) 'Enter the total number of simulation time steps'
      read(*,*) nt
      nt = iabs(nt)
      timef = timeo + float(nt) * dt
      write(*,*)
c
c  Check the last entry.
c
      if (nt.lt.ntmin) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Specify the number of time steps between movie grid files.  
c
      write(*,*) 'You can write hundreds of grid files for a movie'
      write(*,*) 'Make sure there is disk space for the files'
      write(*,*) 'The movie grid files are based on time step'
      write(*,*)
      write(*,*) 'Enter the time steps between movie grid files'
      read(*,*) itdel
      itdel = iabs(itdel)
      write(*,*)
c
c  Check the last entry.
c
      if (itdel.eq.0) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Specify the last time step for movie grid files.  
c
      write(*,*) 'Enter the last time step for movie grid files'
      read(*,*) itmov
      itmov = iabs(itmov)
      write(*,*)
c
c  Check the last entry.
c
      if (itmov.le.0) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Specify the start time of output grid files.  
c
      write(*,*) 'Start time of simulation:  ', timeo
      write(*,*) 'End time of simulation:    ', timef
      write(*,*) 
      write(*,*) 'Enter the start time of output grid records'
      read(*,*) tout1
      write(*,*)
c
c  Check the last entry.
c
      if (tout1.ge.timef) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Specify the end time of output grid files.  
c
      write(*,*) 'Enter the end time of output grid records'
      read(*,*) tout2
      write(*,*)
c
c  Check the last entry.
c
      if (tout2.le.tout1) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  Specify the value of dry nodes for grid files.  
c
      write(*,*) 'A specified number can represent missing data'
      write(*,*) 'Or it can represent dry land in grid files'
      write(*,*)
      write(*,*) 'Enter the missing data value for grid file nodes'
      read(*,*) zlim
      write(*,*)
c
c  Start the tsunami propagation and inundation model.
c
      write(*,*) 'Geowave is ready to launch FUNWAVE'
      write(*,*) 'You can decide to redo timing inputs'
      write(*,*)
      write(*,*) 'Type 0 if timing inputs are all OK, 1 otherwise'
      read(*,*) quest
      if (quest.ne.0) goto 668
      write(*,*)
      call funwave
      write(*,*)
      write(*,*) 'FUNWAVE simulation is complete'
      write(*,*) 'Press ENTER to exit Geowave'
      pause
c
      stop
      end
c

c//////////////////////////////////////////////////////////////////
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c//////////////////////////////////////////////////////////////////
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


      subroutine funwave

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      dimension grid(iq,jq)


      write(*,*)
      write(*,*) '        FUlly Nonlinear Boussinesq Wave Model'
      write(*,*)
      write(*,*) '                Welcome to FUNWAVE'
      write(*,*)
      write(*,*)
      write(*,*) '       (c) 1995-2009, University of Delaware'
      write(*,*)
      write(*,*) '    Modified by Applied Fluids Engineering, Inc.'
      write(*,*)
      write(*,*)

      call init
      
      iaver=int(2.0*tpd*rdt)
      ixdel=1
      jydel=1
      itmn=iaver+1
      ictrl=0
      mxdl=m1/ixdel+1
      nydl=n1/jydel+1
      
      ifcntr=49
      ite=0
      
      do 300 it=2,nt
      
      write (*,*) 'Time step    ', it
      t=timeo+float(it-1)*dt
      twrp=tanh(t/(2.*tpd))
      idpc=1
      
      do i=2,m1
      do j=1,ny
      uo(i,j)=a(1,i,j)*un(i-1,j)+b(1,i,j)*un(i,j)+c(1,i,j)*un(i+1,j)
      enddo
      enddo

      do j=2,n1
      do i=1,mx
      vo(i,j)=a(2,i,j)*vn(i,j-1)+b(2,i,j)*vn(i,j)+c(2,i,j)*vn(i,j+1)
      enddo
      enddo

      call unsol
      call vnsol
      
      do i=1,mx
      do j=1,ny
      eto(i,j)=etn(i,j)
      enddo
      enddo

      call etsol
     
      do i=1,mx
      do j=1,ny
      idiff(i,j)=0
      enddo
      enddo

      ite=0
33    continue
      idpc=2
      ite=ite+1

      if(ite.GT.30)then
      write(*,*)
      write(*,*) 'Iterations exceed 30 at ',it
      write(*,*)
      nt=it
      goto 99
      endif
120   format(1x,i6,1x,i6,1x,i6,2x,g11.4,1x,g11.4,1x,g11.4)
      
      do i=1,mx
      do j=1,ny
      et(1,i,j)=etn(i,j)
      u(1,i,j)=un(i,j)
      v(1,i,j)=vn(i,j)
      ete(i,j)=etn(i,j)
      ue(i,j)=un(i,j)
      ve(i,j)=vn(i,j)
      enddo
      enddo

      call eval_e
      call etsol

      call eval_fg
      call unsol
      call vnsol
      
      if(ite.GE.2)then
      do i=1,mx
      do j=1,ny
      etn(i,j)=ete(i,j)+0.2*(etn(i,j)-ete(i,j))
      un(i,j)=ue(i,j)+0.2*(un(i,j)-ue(i,j))
      vn(i,j)=ve(i,j)+0.2*(vn(i,j)-ve(i,j))
      enddo
      enddo
      endif

      call errorcheck

      if(max(etem,uem,vem).GT.1.0e-5) goto 33

      write (*,*) 'Iterations   ', ite
      write (*,*)

      call etatest

      if(idslot.EQ.1)then
      call fltr3(un)
      call fltr3(vn)
      call fltr3(etn)
c      call fltr4(etn)
c      call fltr4(etn)
      call etatest
      endif
      
      if(idft.EQ.1)then
      call fltr1(un)
      call fltr1(vn)
      call fltr1(etn)
      call etatest
      endif

      do j=1,ny
      do i=1,mx
      un(i,j)=un(i,j)*cospg(i,j)
      vn(i,j)=vn(i,j)*cospg(i,j)
      etn(i,j)=etn(i,j)*cospg(i,j)
      enddo
      enddo

c  Add new tsunami sources in the middle of the simulation

      ichange=0
      do k=1,msrc
      ivel=0
      hstp = dt / 2.0D+00
      if ((tinit(k).gt.t-hstp).and.(tinit(k).le.t+hstp)) then
      ichange=1
      itmp=mod(k,10)
      proper=char(itmp+48)
      f1n='surface'//proper//output
      f2n='uvel'//proper//output
      f3n='vvel'//proper//output
      if (ifail(k).ne.0) ivel=1
      open (11,file=f1n)
      if (itype.eq.0) then
      read(11,*)
      read(11,*) idum1,idum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      read(11,*) etamin,etamax
      do j = 1, ny
      read(11,*) (grid(i,j),i=1,mx)
      enddo
      endif
      close (11)
      do j = 1, ny
      do i = 1, mx
      etn(i,j)=etn(i,j)+grid(i,j)
      eto(i,j)=eto(i,j)+grid(i,j)
      et(1,i,j)=et(1,i,j)+grid(i,j)
      et(2,i,j)=et(2,i,j)+grid(i,j)
      et(3,i,j)=et(3,i,j)+grid(i,j)
      et(4,i,j)=et(4,i,j)+grid(i,j)
      enddo
      enddo

      if (ivel.eq.1) then

      write(*,*) 'Adding velocities'

      open (11,file=f2n)
      if (itype.eq.0) then
      read(11,*)
      read(11,*) idum1,idum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      do j = 1, ny
      read(11,*) (grid(i,j),i=1,mx)
      enddo
      endif
      close (11)
      do j = 1, ny
      do i = 1, mx
      un(i,j)=un(i,j)+grid(i,j)
      uo(i,j)=uo(i,j)+grid(i,j)
      u(1,i,j)=u(1,i,j)+grid(i,j)
      u(2,i,j)=u(2,i,j)+grid(i,j)
      u(3,i,j)=u(3,i,j)+grid(i,j)
      u(4,i,j)=u(4,i,j)+grid(i,j)
      enddo
      enddo

      open (11,file=f3n)
      if (itype.eq.0) then
      read(11,*)
      read(11,*) idum1,idum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      do j = 1, ny
      read(11,*) (grid(i,j),i=1,mx)
      enddo
      endif
      close (11)
      do j = 1, ny
      do i = 1, mx
      vn(i,j)=vn(i,j)+grid(i,j)
      vo(i,j)=vo(i,j)+grid(i,j)
      v(1,i,j)=v(1,i,j)+grid(i,j)
      v(2,i,j)=v(2,i,j)+grid(i,j)
      v(3,i,j)=v(3,i,j)+grid(i,j)
      v(4,i,j)=v(4,i,j)+grid(i,j)
      enddo
      enddo
      endif

      endif
      enddo

      if (ichange.eq.1) then
      tmp=1.0D+10
      do i=1,msrc
      if (tinit(i).le.t+hstp) then
      if ((xlm(i)/dsrc(i)).lt.tmp) then
      tmp=xlm(i)/dsrc(i)
      tpd=per(i)
      h0=dsrc(i)
      endif
      endif
      enddo
      write(*,*) h0, '  Wave generation depth'
      write(*,*) tpd, '  Wave time period'
      write(*,*)
      omg=2.*pi/tpd
      call wavenb(omg,wk)
      wl=2.*pi/wk
      cph=omg/wk
      cr=dt*sqrt(rx2**2+ry2**2)*cph
      endif

      call update

      do i=1,mx
      do j=1,ny
      u(1,i,j)=un(i,j)
      v(1,i,j)=vn(i,j)
      et(1,i,j)=etn(i,j)
      enddo
      enddo

      call eval_e
      call eval_fg

      do k=4,2,-1
      do i=1,mx
      do j=1,ny
      e(k,i,j)=e(k-1,i,j)
      f(k,i,j)=f(k-1,i,j)
      g(k,i,j)=g(k-1,i,j)
      f1(k,i,j)=f1(k-1,i,j)
      g1(k,i,j)=g1(k-1,i,j)
      ft(k,i,j)=ft(k-1,i,j)
      gt(k,i,j)=gt(k-1,i,j)
      u(k,i,j)=u(k-1,i,j)
      v(k,i,j)=v(k-1,i,j)
      et(k,i,j)=et(k-1,i,j)
      enddo
      enddo
      enddo

      sum=0.0
      jmass=ny-ispg(4)+1
      do i=ispg(1),imass-1
      do j=ispg(3),jmass-1
      htmp=0.25*(hs(i,j)+hs(i+1,j)+hs(i,j+1)+hs(i+1,j+1))
      etmp=0.25*(etn(i,j)+etn(i+1,j)+etn(i,j+1)+etn(i+1,j+1))
      if(htmp.ge.0.0) sum=sum+dx*dy*etmp
      if(htmp.lt.0.0) then
      if((etmp+htmp).gt.0.0) sum=sum+dx*dy*(etmp+htmp)
      endif
      enddo
      enddo
      uvm(it)=sum
      
      sum=0.0
      i=ispg(1)
      do j=ispg(3),jmass-1
      dep1=etn(i,j)+hs(i,j)
      dep2=etn(i,j+1)+hs(i,j+1)
      if((dep1+dep2)/2.0.gt.0.0) then      
      ftmp=dep1*(un(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_x(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpx(i,j)))/2.0+
     &dep2*(un(i,j+1)+((za(i,j+1)**2/2.0-(hs(i,j+1)**2-
     &hs(i,j+1)*etn(i,j+1)+etn(i,j+1)**2)/6.0)*p_x(i,j+1)+
     &(za(i,j+1)+(hs(i,j+1)-etn(i,j+1))/2.0)*hpx(i,j+1)))/2.0
      sum=sum-dy*dt*ftmp
      endif
      enddo
      i=imass
      do j=ispg(3),jmass-1
      dep1=etn(i,j)+hs(i,j)
      dep2=etn(i,j+1)+hs(i,j+1)
      if((dep1+dep2)/2.0.gt.0.0) then
      ftmp=dep1*(un(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_x(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpx(i,j)))/2.0+
     &dep2*(un(i,j+1)+((za(i,j+1)**2/2.0-(hs(i,j+1)**2-
     &hs(i,j+1)*etn(i,j+1)+etn(i,j+1)**2)/6.0)*p_x(i,j+1)+
     &(za(i,j+1)+(hs(i,j+1)-etn(i,j+1))/2.0)*hpx(i,j+1)))/2.0
      sum=sum+dy*dt*ftmp
      endif
      enddo
      j=ispg(3)
      do i=ispg(1),imass-1
      dep1=etn(i,j)+hs(i,j)
      dep2=etn(i+1,j)+hs(i+1,j)
      if((dep1+dep2)/2.0.gt.0.0) then
      ftmp=dep1*(vn(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_y(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpy(i,j)))/2.0+
     &dep2*(vn(i+1,j)+((za(i+1,j)**2/2.0-(hs(i+1,j)**2-
     &hs(i+1,j)*etn(i+1,j)+etn(i+1,j)**2)/6.0)*p_y(i+1,j)+
     &(za(i+1,j)+(hs(i+1,j)-etn(i+1,j))/2.0)*hpy(i+1,j)))/2.0      
      sum=sum-dx*dt*ftmp
      endif
      enddo
      j=jmass
      do i=ispg(1),imass-1
      dep1=etn(i,j)+hs(i,j)
      dep2=etn(i+1,j)+hs(i+1,j)
      if((dep1+dep2)/2.0.gt.0.0) then
      ftmp=dep1*(vn(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_y(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpy(i,j)))/2.0+
     &dep2*(vn(i+1,j)+((za(i+1,j)**2/2.0-(hs(i+1,j)**2-
     &hs(i+1,j)*etn(i+1,j)+etn(i+1,j)**2)/6.0)*p_y(i+1,j)+
     &(za(i+1,j)+(hs(i+1,j)-etn(i+1,j))/2.0)*hpy(i+1,j)))/2.0
      sum=sum+dx*dt*ftmp
      endif
      enddo
      flux(it)=sum

c      if(mod((it-1),itscr).EQ.0)then
c      write(*,120)it,ite,itwv,uvm(it),flux(it),ass
c      write(*,*)
c      endif
      
      call meanv
      call prnt
      
      if(itbrk.EQ.1)then
      ifcntr=ifcntr+1
      if(mod(ifcntr,50).EQ.0)then
      write(*,*) 'Wave breaking at it = ',it
      write(*,*)
      endif
      else
      ifcntr=49
      endif

      if((mod(it,itftr).EQ.0).OR.(mod(ifcntr,50).EQ.0))then
      write(*,*) 'Filtering the simulation domain'
      write(*,*)
      do k=4,2,-1
      do i=1,mx
      do j=1,ny
      etn(i,j)=et(k,i,j)
      un(i,j)=u(k,i,j)
      vn(i,j)=v(k,i,j)
      enddo
      enddo
      call fltr(etn)
      call etatest
      call fltr(un)
      call fltr(vn)
      call eval_e
      call eval_fg
      do i=1,mx
      do j=1,ny
      e(k,i,j)=e(1,i,j)
      f(k,i,j)=f(1,i,j)
      g(k,i,j)=g(1,i,j)
      f1(k,i,j)=f1(1,i,j)
      g1(k,i,j)=g1(1,i,j)
      et(k,i,j)=etn(i,j)
      u(k,i,j)=un(i,j)
      v(k,i,j)=vn(i,j)
      enddo
      enddo
      enddo
      idpc=1
      call eval_utvt
      endif

300   continue
99    continue
      
      call printing

      return
      end

c  ###########################################################
      
      subroutine init

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      dimension grid(iq,jq)
      character fgage*30,flag*30

      data ga,pi/9.81,3.141593/
      data a1,a2,b1,b2,alpha,alpha1/6*0./
      data p/-25.,48.,-36.,16.,-3.,8.,1./
      data q/-3.,-10.,18.,-6.,1.,0.,0./
      data ta/46.,-32.,10.,9.,19.,-5.,1./
      data td3/1.5,-2.,.5,0.,0.,0.,0.,0./
      data td4/11.,-18.,9.,-2.,2.,3.,-6.,1./
      data cf1/2.,1.,0.,0.,0./
      data cf2/10.,4.,-1.,0.,0./
      data cf3/44.,15.,-6.,1.,0./
      data cf4/186.,56.,-28.,8.,-1./

      data f4n/'wvgauge.out'/
      data f5n/'timesrs.out'/
      data f7n/'fresrfc.out'/

      write(*,*) 'Enter a text file output filename (up to 30 char)'
      read(*,*) f3n
      write(*,*)

      open(292,file=f3n)
      write(292,*) '                GEOWAVE'
      write(292,*)
      write(292,*) '              Version 1.1'
      write(292,*)
      write(292,*)
      write(292,*) 'Copyright, 2002-2009, Geowave Software Developers'
      write(292,*)
      write(292,*)
      write(292,*) ibe, '  Wave equation, 2=Boussinesq, 4=NSWW'
      write(292,*)
      write(292,*) dx, '  Grid step in x direction'
      write(292,*) dy, '  Grid step in y direction'
      write(292,*)
      write(292,*) nt, '  Maximum number of time steps'
      write(292,*) itdel, '  Time steps between movie files'
      write(292,*) itmov, '  Last time step of movie files'
      write(292,*)
      write(292,*) ngage, '  Number of wave gauges'
      write(292,*) nlag, '  Number of Lagrangian markers'
      write(292,*) ingm, '  Units of gauges and markers'
      write(292,*) itype,'  Output grid file format'
      write(292,*)
      write(292,*) zlim, '  No data value'
      write(292,*) tout1, '  Time at start of grid files'
      write(292,*) tout2, '  Time at end of grid files'
      write(292,*)
      write(292,*) f3n, '  Output filename'
      write(292,*) f8n, '  Wave gauge filename'
      write(292,*) f9n, '  Lagrangian marker filename'
      write(292,*) output, '  Grid file extension'  
      write(292,*)

      itbgn=1
      itend=nt
      itftr=5
c      ibe=2
      imch=4
      itscr=10
      idout=0
      idft=1
      idslot=1
      igrid=-1
      ispg(1)=10
      ispg(2)=10
      ispg(3)=10
      ispg(4)=10

      write(292,*) ispg(1), '  Sponge layer thickness at edge'
      write(292,*) ispg(2), '  Sponge layer thickness at edge'
      write(292,*) ispg(3), '  Sponge layer thickness at edge'
      write(292,*) ispg(4), '  Sponge layer thickness at edge'
      write(292,*)

      t=timeo
      cbkv=0.35
      delta=0.08
      slmda=25.0
      cspg=0.0
      cspg2=0.0
      cspg3=0.0
      cbrk=1.2
      ck_bt=0.0025
      c_dm=0.1
      eps=1.0e-4

      ieddy=1
      idsf=9
      it=1

c
c      write(*,*) 'Opening the ASCII grid file'
c      write(*,*)
c      open (10,file=f1n)
c      read(10,*)
c      read(10,*) mx,ny
c      read(10,*) xw,xe
c      read(10,*) ys,yn
c      read(10,*) dpmin,dpmax
c
c      if(mx.GT.iq.OR.ny.GT.jq.OR.nt.GT.nq)then
c      write(*,*) 'Array parameters are too small'
c      write(*,*) 'Program stopped, press return'
c      pause
c      stop
c      endif
c
      if(nt.GT.nq)then
      write(*,*) 'Array parameters are too small'
      write(*,*) 'Program stopped, press return'
      pause
      stop
      endif
c
c      do j = 1, ny
c      read(10,*) (hs(i,j),i=1,mx)
c      enddo
c      close (10)
c      write(*,*) 'Finished reading depth'
c      write(*,*)
c
      write(292,*) t, '  Initial simulation time'
      write(292,*)
      write(292,*) delta, '  Runup slot porosity'
      write(292,*)
      write(292,*) itftr, '  Time steps per filtering'
      write(292,*)
      write(292,*) mx, '  Number of columns'
      write(292,*) ny, '  Number of rows'
      write(292,*)
      write(292,*) xw, '  Western edge in grid units'
      write(292,*) xe, '  Eastern edge in grid units'
      write(292,*) ys, '  Southern edge in grid units'
      write(292,*) yn, '  Northern edge in grid units'
      write(292,*)
      write(292,*) dpmin, '  Minimum depth'
      write(292,*) dpmax, '  Maximum depth'
      write(292,*)

      do j = 1, ny
      do i = 1, mx
      h(i,j)=hs(i,j)
      etn(i,j)=0.
      un(i,j)=0.
      vn(i,j)=0.
      enddo
      enddo

      write(*,*) 'Opening the IC grid files'
      write(*,*)

      do k=1,msrc
      ivel=0
      hstp = dt / 2.0D+00
      if ((tinit(k).gt.t-hstp).and.(tinit(k).le.t+hstp)) then
      itmp=mod(k,10)
      proper=char(itmp+48)
      f1n='surface'//proper//output
      f2n='uvel'//proper//output
      f3n='vvel'//proper//output
      if (ifail(k).ne.0) ivel=1

      open (11,file=f1n)
      if (itype.eq.0) then
      read(11,*)
      read(11,*) idum1,idum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      read(11,*) etamin,etamax
      do j = 1, ny
      read(11,*) (grid(i,j),i=1,mx)
      enddo
      endif
      close (11)
      do j = 1, ny
      do i = 1, mx
      etn(i,j)=etn(i,j)+grid(i,j)
      enddo
      enddo

      if (ivel.eq.1) then
      open (11,file=f2n)
      if (itype.eq.0) then
      read(11,*)
      read(11,*) idum1,idum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      do j = 1, ny
      read(11,*) (grid(i,j),i=1,mx)
      enddo
      endif
      close (11)
      do j = 1, ny
      do i = 1, mx
      un(i,j)=un(i,j)+grid(i,j)
      enddo
      enddo

      open (11,file=f3n)
      if (itype.eq.0) then
      read(11,*)
      read(11,*) idum1,idum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      read(11,*) dum1,dum2
      do j = 1, ny
      read(11,*) (grid(i,j),i=1,mx)
      enddo
      endif
      close (11)
      do j = 1, ny
      do i = 1, mx
      vn(i,j)=vn(i,j)+grid(i,j)
      enddo
      enddo
      endif

      endif
      enddo

      tmp=1.0D+10 
      do i=1,msrc
      if (tinit(i).le.t+hstp) then
      if ((xlm(i)/dsrc(i)).lt.tmp) then
      tmp=xlm(i)/dsrc(i)
      tpd=per(i)
      h0=dsrc(i)
      endif
      endif
      enddo

      write(292,*) 'Tsunami source types'
      write(292,*)
      do i=1,msrc
      write(292,*) ifail(i)
      enddo
      write(292,*)

      write(292,*) 'Tsunami source times'
      write(292,*)
      do i=1,msrc
      write(292,*) tinit(i)
      enddo
      write(292,*)

      write(292,*) 'Tsunami source periods'
      write(292,*)
      do i=1,msrc
      write(292,*) per(i)
      enddo
      write(292,*)

      write(292,*) 'Tsunami source depths'
      write(292,*)
      do i=1,msrc
      write(292,*) dsrc(i)
      enddo
      write(292,*)

      write(292,*) 'Tsunami source wavelengths'
      write(292,*)
      do i=1,msrc
      write(292,*) xlm(i)
      enddo
      write(292,*)

      write(292,*) h0, '  Wave generation depth'
      write(292,*) tpd, '  Wave time period'
      write(292,*)
      write(*,*) h0, '  Wave generation depth'
      write(*,*) tpd, '  Wave time period'
      write(*,*)

      etamax=0.0D+00
      etamin=0.0D+00
      do j=1,ny
      do i=1,mx
      if (etn(i,j).lt.etamin) etamin=etn(i,j)
      if (etn(i,j).gt.etamax) etamax=etn(i,j)
      enddo
      enddo

      write(292,*) etamin, '  Minimum free surface'
      write(292,*) etamax, '  Maximum free surface'

      dum1=0.0D+00
      dum2=0.0D+00
      do j=1,ny
      do i=1,mx
      if (un(i,j).lt.dum1) dum1=un(i,j)
      if (un(i,j).gt.dum2) dum2=un(i,j)
      enddo
      enddo

      write(292,*) dum1, '  Minimum u velocity'
      write(292,*) dum2, '  Maximum u velocity'

      dum1=0.0D+00
      dum2=0.0D+00
      do j=1,ny
      do i=1,mx
      if (vn(i,j).lt.dum1) dum1=vn(i,j)
      if (vn(i,j).gt.dum2) dum2=vn(i,j)
      enddo
      enddo

      write(292,*) dum1, '  Minimum v velocity'
      write(292,*) dum2, '  Maximum v velocity'
      write(292,*)

      if(idum1.ne.mx) then
      write(*,*)
      write(*,*) 'Bathy and IC have different mx'
      write(*,*) 'Program stopped, press return'
      pause
      stop
      endif

      if(idum2.ne.ny) then
      write(*,*)
      write(*,*) 'Bathy and IC have different ny'
      write(*,*) 'Program stopped, press return'
      pause
      stop
      endif

      idum1 = ispg(1) + ispg(2) + 15

      if(idum1.gt.mx) then
      write(*,*)
      write(*,*) 'Domain too small for sponge layers'
      write(*,*) 'Program stopped, press return'
      pause
      stop
      endif

      idum2 = ispg(3) + ispg(4) + 15

      if(idum2.gt.ny) then
      write(*,*)
      write(*,*) 'Domain too small for sponge layers'
      write(*,*) 'Program stopped, press return'
      pause
      stop
      endif

      if(ngm.gt.30) then
      write(*,*)
      write(*,*) 'NGM cannot be greater than 30'
      write(*,*) 'Program stopped, press return'
      pause
      stop
      endif

      if((ingm.lt.0).and.(ingm.gt.1)) then
      write(*,*)
      write(*,*) 'INGM must be either 0 or 1'
      write(*,*) 'Program stopped, press return'
      pause
      stop
      endif

      write(*,102) 'nt, ibe     = ',nt,ibe
      write(*,102) 'ngage   = ',ngage
      write(*,102) 'nlag    = ',nlag
      write(*,102) 'ispg(4)             = ',(ispg(k),k=1,4)
      write(*,103) 'dx, dy = ',dx,dy
      write(*,103) 'cbrk, cspg(3)       = ',cbrk,cspg,cspg2,cspg3
      write(*,*)
102   format(a22,1x,5(i10,1x))
103   format(a22,1x,5(g10.4,1x))

c      dt=0.3*dx/sqrt(ga*dpmax)
      a0=max(abs(etamin),abs(etamax))
      z0=2.2*a0

      call etatest
      call surfgrid

      isltb=1
      islte=mx

      write(292,*) dt, '  Time step'
      write(292,*) a0, '  Characteristic amplitude'
      write(292,*)

      if (ivel.eq.1) then
      do j = 1, ny
      do i = 1, mx
      eto(i,j) = etn(i,j)
      et(3,i,j) = etn(i,j)
      et(4,i,j) = etn(i,j)
      uo(i,j) = un(i,j)
      u(3,i,j) = un(i,j)
      u(4,i,j) = un(i,j)
      vo(i,j) = vn(i,j)
      v(3,i,j) = vn(i,j)
      v(4,i,j) = vn(i,j)
      enddo
      enddo
      else
      do j = 1, ny
      do i = 1, mx
      eto(i,j) = etn(i,j)
      et(3,i,j) = etn(i,j)
      et(4,i,j) = etn(i,j)
      un(i,j) = 0.0
      uo(i,j) = 0.0
      u(3,i,j) = 0.0
      u(4,i,j) = 0.0
      vn(i,j) = 0.0
      vo(i,j) = 0.0
      v(3,i,j) = 0.0
      v(4,i,j) = 0.0
      enddo
      enddo
      endif

      write(*,*) 'Finished reading initial condition'
      write(*,*)

      m1=mx-1
      stp=0.0
      istlm=m1
      do j=1,ny
      istl(j)=m1
      enddo

      islt=min(isltb,mx)
      ntmp=(ny-1)/2+1

      tmp=2.0*a0

      imass=mx-ispg(2)+1
      
      delta=min(0.1,delta)
      slmda=max(1.0,slmda)
      detmp=1.0-delta
      sltmp=1.0/slmda
      dlamda=slmda/z0
      dlts=detmp/dlamda

      do i=1,mx
      do j=1,ny
      wvht(i,j)=0.0
      etmin(i,j)=0.0
      etmax(i,j)=0.0
      tmin(i,j)=0.0
      tmax(i,j)=0.0
      twet(i,j)=0.0
      tdry(i,j)=0.0
      velmx(i,j)=0.0
      velang(i,j)=0.0
      flxmx(i,j)=0.0
      flxang(i,j)=0.0
      wetlnd(i,j)=0.0
      brvis(i,j)=0.0
      brtim(i,j)=dt*float(nt)

      if(hs(i,j).le.tmp)then
      hcr(i,j)=hs(i,j)/detmp-z0*(delta/detmp+sltmp)
      else
      hcr(i,j)=hs(i,j)
      endif

      zdiff(i,j)=0.0
      idiff(i,j)=0
      enddo
      enddo
      
      if(ibe.LE.2)then
      alpha=-0.390
      alpha1=alpha+1./3.
      b2=-1.+sqrt(2.*alpha+1.)
      b1=.5*b2*b2
      a1=b1-1./6.
      a2=b2+.5
      do i=1,mx
      do j=1,ny
      za(i,j)=b2*h(i,j)+(1.0+b2)*etn(i,j)
      enddo
      enddo
      endif

      if(ibe.EQ.3.OR.ibe.EQ.5)then
      alpha=-1./3.
      b1=1./6.
      b2=-0.5
      endif

      clnr=1.0
      if(ibe.EQ.0.OR.ibe.EQ.5)clnr=0.0
      
      m1=mx-1
      m2=mx-2
      m3=mx-3
      m4=mx-4
      m5=mx-5
      m6=mx-6
      n1=ny-1
      n2=ny-2
      n3=ny-3
      n4=ny-4
      n5=ny-5
      n6=ny-6
      rdx=0.5/dx
      rdy=0.5/dy
      rx2=1.0/(dx*dx)
      ry2=1.0/(dy*dy)
      rxy=0.25/(dx*dy)
      xl=float(m1)*dx
      yl=float(n1)*dy
      tl=float(nt-1)*dt
      omg=2.*pi/tpd
      rdt=1./dt
      do j=1,7
      p(j)=p(j)/6.
      q(j)=q(j)/6.
      ta(j)=ta(j)/24.*dt
      enddo
      do j=1,4
      cb(j)=-p(j+1)/p(1)
      enddo
      do j=1,8
      td3(j)=td3(j)*rdt
      td4(j)=td4(j)*rdt/6.
      end do
      do j=1,5
      cf1(j)=cf1(j)/4.
      cf2(j)=cf2(j)/16.
      cf3(j)=cf3(j)/64.
      cf4(j)=cf4(j)/256.
      enddo
      
      do i=1,mx
      do j=1,ny
      ctb(i,j)=0.35
      itb(i,j)=nt
      enddo
      enddo
      
      do i=1,mx
      do j=1,ny
      um(i,j)=0.0
      vm(i,j)=0.0
      em(i,j)=0.0
      enddo
      enddo
      
      itwv=2*(mx-1)*int(dx*rdt/sqrt(ga*h0))
      
      do j=1,ny
      do i=1,mx
      cospg(i,j)=1.0
      enddo
      enddo

      do i=1,mx
      do j=1,ny
      u(1,i,j)=un(i,j)
      v(1,i,j)=vn(i,j)
      et(1,i,j)=etn(i,j)
      u(2,i,j)=uo(i,j)
      v(2,i,j)=vo(i,j)
      et(2,i,j)=eto(i,j)
      enddo
      enddo
      
      idpc=1
      do k=4,1,-1
      do i=1,mx
      do j=1,ny
      un(i,j)=u(k,i,j)
      vn(i,j)=v(k,i,j)
      etn(i,j)=et(k,i,j)
      enddo
      enddo

      call eval_e
      call eval_fg
      do i=1,mx
      do j=1,ny
      e(k,i,j)=e(1,i,j)
      f(k,i,j)=f(1,i,j)
      g(k,i,j)=g(1,i,j)
      f1(k,i,j)=f1(1,i,j)
      g1(k,i,j)=g1(1,i,j)
      enddo
      enddo
      enddo

      sum=0.0
      jmass=ny-ispg(4)+1
      do i=ispg(1),imass-1
      do j=ispg(3),jmass-1
      htmp=0.25*(hs(i,j)+hs(i+1,j)+hs(i,j+1)+hs(i+1,j+1))
      etmp=0.25*(etn(i,j)+etn(i+1,j)+etn(i,j+1)+etn(i+1,j+1))
      if(htmp.ge.0.0) sum=sum+dx*dy*etmp
      if(htmp.lt.0.0) then
      if((etmp+htmp).gt.0.0) sum=sum+dx*dy*(etmp+htmp)
      endif
      enddo
      enddo
      uvm(1)=sum

      sum=0.0
      i=ispg(1)
      do j=ispg(3),jmass-1
      dep1=etn(i,j)+hs(i,j)
      dep2=etn(i,j+1)+hs(i,j+1)
      if((dep1+dep2)/2.0.gt.0.0) then      
      ftmp=dep1*(un(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_x(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpx(i,j)))/2.0+
     &dep2*(un(i,j+1)+((za(i,j+1)**2/2.0-(hs(i,j+1)**2-
     &hs(i,j+1)*etn(i,j+1)+etn(i,j+1)**2)/6.0)*p_x(i,j+1)+
     &(za(i,j+1)+(hs(i,j+1)-etn(i,j+1))/2.0)*hpx(i,j+1)))/2.0
      sum=sum-dy*dt*ftmp
      endif
      enddo
      i=imass
      do j=ispg(3),jmass-1
      dep1=etn(i,j)+hs(i,j)
      dep2=etn(i,j+1)+hs(i,j+1)
      if((dep1+dep2)/2.0.gt.0.0) then
      ftmp=dep1*(un(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_x(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpx(i,j)))/2.0+
     &dep2*(un(i,j+1)+((za(i,j+1)**2/2.0-(hs(i,j+1)**2-
     &hs(i,j+1)*etn(i,j+1)+etn(i,j+1)**2)/6.0)*p_x(i,j+1)+
     &(za(i,j+1)+(hs(i,j+1)-etn(i,j+1))/2.0)*hpx(i,j+1)))/2.0
      sum=sum+dy*dt*ftmp
      endif
      enddo
      j=ispg(3)
      do i=ispg(1),imass-1
      dep1=etn(i,j)+hs(i,j)
      dep2=etn(i+1,j)+hs(i+1,j)
      if((dep1+dep2)/2.0.gt.0.0) then
      ftmp=dep1*(vn(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_y(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpy(i,j)))/2.0+
     &dep2*(vn(i+1,j)+((za(i+1,j)**2/2.0-(hs(i+1,j)**2-
     &hs(i+1,j)*etn(i+1,j)+etn(i+1,j)**2)/6.0)*p_y(i+1,j)+
     &(za(i+1,j)+(hs(i+1,j)-etn(i+1,j))/2.0)*hpy(i+1,j)))/2.0      
      sum=sum-dx*dt*ftmp
      endif
      enddo
      j=jmass
      do i=ispg(1),imass-1
      dep1=etn(i,j)+hs(i,j)
      dep2=etn(i+1,j)+hs(i+1,j)
      if((dep1+dep2)/2.0.gt.0.0) then
      ftmp=dep1*(vn(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_y(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpy(i,j)))/2.0+
     &dep2*(vn(i+1,j)+((za(i+1,j)**2/2.0-(hs(i+1,j)**2-
     &hs(i+1,j)*etn(i+1,j)+etn(i+1,j)**2)/6.0)*p_y(i+1,j)+
     &(za(i+1,j)+(hs(i+1,j)-etn(i+1,j))/2.0)*hpy(i+1,j)))/2.0
      sum=sum+dx*dt*ftmp
      endif
      enddo
      flux(1)=sum

      it=1
      
      call wavenb(omg,wk)
      wl=2.*pi/wk
      cph=omg/wk
      cr=dt*sqrt(rx2**2+ry2**2)*cph

      write(292,*) omg, '  Circular frequency'
      write(292,*) wk, '  Wavenumber'
      write(292,*) wl, '  Wavelength'
      write(292,*) cph, '  Phase celerity'
      write(292,*)
      
      do i=1,mx
      do j=1,ny
      w1(i,j)=0.0
      w2(i,j)=0.0
      w3(i,j)=0.0
      enddo
      enddo

      xtmp=omg/(exp(1.)-1.)
      do j=1,ny
      do i=1,ispg(1)
      xpg=float(ispg(1)-i)/float(ispg(1))
      w1(i,j)=(exp(xpg**2)-1.)*xtmp*cspg
      w2(i,j)=(exp(xpg**2)-1.)*xtmp*cspg2
      w3(i,j)=(exp(xpg**2)-1.)*xtmp*cspg3
      enddo
      xtmp=omg/(exp(1.)-1.)
      itmp=mx-ispg(2)+1
      do i=itmp,mx
      xpg=float(i-itmp)/float(ispg(2))
      w1(i,j)=(exp(xpg**2)-1.)*xtmp*cspg
      w2(i,j)=(exp(xpg**2)-1.)*xtmp*cspg2
      w3(i,j)=-(exp(xpg**2)-1.)*xtmp*cspg3
      enddo
      enddo
      jtmp=ny-ispg(4)+1
      do i=1,mx
      xtmp=omg/(exp(1.)-1.)
      do j=1,ispg(3)
      ypg=float(ispg(3)-j)/float(ispg(3))
      w1(i,j)=w1(i,j)+(exp(ypg**2)-1.)*xtmp*cspg
      w2(i,j)=w2(i,j)+(exp(ypg**2)-1.)*xtmp*cspg2
      w3(i,j)=w3(i,j)+(exp(ypg**2)-1.)*xtmp*cspg3
      enddo
      xtmp=omg/(exp(1.)-1.)
      do j=jtmp,ny
      ypg=float(j-jtmp)/float(ispg(4))
      w1(i,j)=w1(i,j)+(exp(ypg**2)-1.)*xtmp*cspg
      w2(i,j)=w2(i,j)+(exp(ypg**2)-1.)*xtmp*cspg2
      w3(i,j)=w3(i,j)-(exp(ypg**2)-1.)*xtmp*cspg3
      enddo
      enddo

      ralpha=20.0
      rgamma=0.555
      itmp=mx-ispg(2)+1
      jtmp=ny-ispg(4)+1

      do j=1,ny
      do i=1,ispg(1)
      cospg(i,j)=1.0/(ralpha**(rgamma**float(i-1)))
      enddo
      do i=itmp,mx
      cospg(i,j)=1.0/(ralpha**(rgamma**float(mx-i)))
      enddo
      enddo

      do i=1,mx
      do j=1,ispg(3)
      cospg(i,j)=1.0/(ralpha**(rgamma**float(j-1)))
      enddo
      do j=jtmp,ny
      cospg(i,j)=1.0/(ralpha**(rgamma**float(ny-j)))
      enddo
      enddo

      do i=2,m1
      do j=2,n1
      tmp1=abs(hs(i,j)-hs(i+1,j))/dx
      tmp2=abs(hs(i,j)-hs(i-1,j))/dx
      tmp3=abs(hs(i,j)-hs(i,j+1))/dy
      tmp4=abs(hs(i,j)-hs(i,j-1))/dy
      slop=max(tmp1,tmp2,tmp3,tmp4)

c      if(slop.gt.0.75) w1(i,j)=w1(i,j)+0.01*(slop-0.75)

      if((slop.gt.0.75).and.(hs(i,j).le.0.0)) then
c      cospg(i,j)=cospg(i,j)-0.08*slop

      if(i.le.ispg(1)) cospg(i,j)=cospg(i,j)-0.08*slop
      if(i.ge.itmp)    cospg(i,j)=cospg(i,j)-0.08*slop
      if(j.le.ispg(3)) cospg(i,j)=cospg(i,j)-0.08*slop
      if(j.ge.jtmp)    cospg(i,j)=cospg(i,j)-0.08*slop

      if(cospg(i,j).lt.0.01) cospg(i,j)=0.01
      endif
      enddo
      enddo

c      xinc=(xe-xw)/float(mx-1)
c      yinc=(yn-ys)/float(ny-1)

      if((ngage.gt.0).and.(ngage.le.ngm)) then
      open(290,file=f8n)
      do i=1,ngage
      read(290,*) dum1,dum2
      if(ingm.eq.0) then
      ixg(i)=1+nint((dum1-xw)/xinc)
      iyg(i)=1+nint((dum2-ys)/yinc)
      else
      ixg(i)=nint(dum1)
      iyg(i)=nint(dum2)
      endif
      if((ixg(i).lt.2).or.(ixg(i).gt.(mx-1))) then
      ixg(i)=1
      endif
      if((iyg(i).lt.2).or.(iyg(i).gt.(ny-1))) then
      iyg(i)=1
      endif
      enddo
      close(290)
      endif

      if((nlag.gt.0).and.(nlag.le.ngm)) then
      open(291,file=f9n)
      do i=1,nlag
      read(291,*) dum1,dum2
      if(ingm.eq.0) then
      ixl(i)=1+nint((dum1-xw)/xinc)
      iyl(i)=1+nint((dum2-ys)/yinc)
      else
      ixl(i)=nint(dum1)
      iyl(i)=nint(dum2)
      endif
      if((ixl(i).lt.1).or.(ixl(i).gt.mx)) then
      ixl(i)=1
      endif
      if((iyl(i).lt.1).or.(iyl(i).gt.ny)) then
      iyl(i)=1
      endif
      enddo
      close(291)
      endif

      if((ngage.gt.0).and.(ngage.le.ngm)) then
      write(292,*) 'Numerical wave gauge coordinates'
      write(292,*)
      do i=1,ngage
      itmp1 = mod(i,100)/10
      itmp2 = mod(i,10)
      fgage='Data/gage'//char(itmp1+48)//char(itmp2+48)//'.dat'
      iunit=700+i
      open(iunit,file=fgage)
      dep=etn(ixg(i),iyg(i))+hs(ixg(i),iyg(i))
      dum1=xw+float(ixg(i)-1)*dx
      dum2=ys+float(iyg(i)-1)*dy
      write(292,733) ixg(i),iyg(i),dum1,dum2

      if(dep.le.0.0) then
      etmp=0.0
      utmp=0.0
      vtmp=0.0
      fxtmp=0.0
      fytmp=0.0
      else
      etmp=etn(ixg(i),iyg(i))
      utmp=un(ixg(i),iyg(i))
      vtmp=vn(ixg(i),iyg(i))
      fxtmp=dep*(un(ixg(i),iyg(i))+((za(ixg(i),iyg(i))**2/2.0-
     &(hs(ixg(i),iyg(i))**2-hs(ixg(i),iyg(i))*etn(ixg(i),iyg(i))+
     &etn(ixg(i),iyg(i))**2)/6.0)*p_x(ixg(i),iyg(i))+
     &(za(ixg(i),iyg(i))+(hs(ixg(i),iyg(i))-
     &etn(ixg(i),iyg(i)))/2.0)*hpx(ixg(i),iyg(i))))
      fytmp=dep*(vn(ixg(i),iyg(i))+((za(ixg(i),iyg(i))**2/2.0-
     &(hs(ixg(i),iyg(i))**2-hs(ixg(i),iyg(i))*etn(ixg(i),iyg(i))+
     &etn(ixg(i),iyg(i))**2)/6.0)*p_y(ixg(i),iyg(i))+
     &(za(ixg(i),iyg(i))+(hs(ixg(i),iyg(i))-
     &etn(ixg(i),iyg(i)))/2.0)*hpy(ixg(i),iyg(i))))
      endif

      write(iunit,732) t,etmp,utmp,vtmp,fxtmp,fytmp,hs(ixg(i),iyg(i))
      enddo
      endif

732   format(7f15.5)
733   format(2i10,2f20.6)

      if((nlag.gt.0).and.(nlag.le.ngm)) then
      write(292,*)
      write(292,*) 'Numerical Lagrangian marker coordinates'
      write(292,*)
      do i=1,nlag
      itmp1 = mod(i,100)/10
      itmp2 = mod(i,10)
      flag='Data/lag'//char(itmp1+48)//char(itmp2+48)//'.dat'
      iunit=800+i
      xlag(i)=xw+float(ixl(i)-1)*dx
      ylag(i)=ys+float(iyl(i)-1)*dy
      etmp=etn(ixl(i),iyl(i))
      utmp=un(ixl(i),iyl(i))
      vtmp=vn(ixl(i),iyl(i))
      htmp=hs(ixl(i),iyl(i))
      open(iunit,file=flag)
      dep=etmp+htmp
      write(292,833) ixl(i),iyl(i),xlag(i),ylag(i)

      if(dep.le.0.0) then
      etmp=0.0
      utmp=0.0
      vtmp=0.0
      endif

      write(iunit,832) t,xlag(i),ylag(i),etmp,utmp,vtmp,htmp

      if(dep.gt.0.0) then
      xlag(i)=xlag(i)+utmp*dt
      ylag(i)=ylag(i)+vtmp*dt
      endif

      enddo
      endif

832   format(7f15.5)
833   format(2i10,2f20.6)

      close(292)

      call tridag
      call ludec

      return
      end

c  ###########################################################
      
      subroutine tridag

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      do i=2,m1
      do j=1,ny
      rtx=rx2*h(i,j)
      a(1,i,j)=rtx*(b1*h(i,j)+b2*h(i-1,j))
      b(1,i,j)=1.-2.*rtx*(b1*h(i,j)+b2*h(i,j))
      c(1,i,j)=rtx*(b1*h(i,j)+b2*h(i+1,j))
      enddo
      enddo
      
      do j=2,n1
      do i=1,mx
      rty=ry2*h(i,j)
      a(2,i,j)=rty*(b1*h(i,j)+b2*h(i,j-1))
      b(2,i,j)=1.-2.*rty*(b1*h(i,j)+b2*h(i,j))
      c(2,i,j)=rty*(b1*h(i,j)+b2*h(i,j+1))
      enddo
      enddo

      return
      end

c  ###########################################################

      subroutine wavenb(omgn,wkn)

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      tb=omgn*omgn*h0/ga
      if(ibe.LE.2)then
      tc=1.+tb*alpha
      wkn=sqrt((tc-sqrt(tc*tc-4*alpha1*tb))/(2.*alpha1))/h0
      else
      wkn=sqrt(tb/(1.+alpha*tb))/h0
      endif

      return
      end

c  ###########################################################
      
      subroutine ludec

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      one=1.0D+0

      do i=2,m1
      do j=1,ny
      adx(i,j)=a(1,i,j)
      enddo
      enddo

      do j=1,ny
      bdx(2,j)=one/b(1,2,j)
      enddo

      do i=3,m1
      do j=1,ny
      adx(i,j)=adx(i,j)*bdx(i-1,j)
      bdx(i,j)=one/(b(1,i,j)-adx(i,j)*c(1,i-1,j))
      enddo
      enddo
      
      do j=2,n1
      do i=1,mx
      ady(i,j)=a(2,i,j)
      enddo
      enddo

      do i=1,mx
      bdy(i,2)=one/b(2,i,2)
      enddo

      do j=3,n1
      do i=1,mx
      ady(i,j)=ady(i,j)*bdy(i,j-1)
      bdy(i,j)=one/(b(2,i,j)-ady(i,j)*c(2,i,j-1))
      enddo
      enddo

      return
      end

c  ###########################################################
      
      subroutine etsol

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      if(idpc.EQ.1)then
      do i=1,mx
      do j=1,ny
      etn(i,j)=eto(i,j)+ta(1)*e(2,i,j)+ta(2)*e(3,i,j)+ta(3)*e(4,i,j)
      enddo
      enddo
      endif

      if(idpc.EQ.2)then
      do i=1,mx
      do j=1,ny
      etn(i,j)=eto(i,j)+ta(4)*e(1,i,j)+ta(5)*e(2,i,j)+ta(6)*
     &e(3,i,j)+ta(7)*e(4,i,j)
      enddo
      enddo
      endif

      call bcet

      return
      end

c  ###########################################################
      
      subroutine unsol

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      if(idpc.EQ.1)then
      do i=2,m1
      do j=1,ny
      un(i,j)=uo(i,j)+ta(1)*f(2,i,j)+ta(2)*f(3,i,j)+ta(3)*f(4,i,j)+
     &2.*f1(2,i,j)-3.*f1(3,i,j)+f1(4,i,j)
      enddo
      enddo
      
      if(ibe.EQ.2)then
      do j=2,n1
      do i=1,mx
      un(i,j)=un(i,j)+ta(1)*ft(2,i,j)+ta(2)*ft(3,i,j)+ta(3)*ft(4,i,j)
      enddo
      enddo
      endif
      endif
      
      if(idpc.EQ.2)then
      do i=2,m1
      do j=1,ny
      un(i,j)=uo(i,j)+ta(4)*f(1,i,j)+ta(5)*f(2,i,j)+ta(6)*f(3,i,j)+
     &ta(7)*f(4,i,j)+f1(1,i,j)-f1(2,i,j)
      enddo
      enddo
      
      if(ibe.EQ.2)then
      do j=2,n1
      do i=1,mx
      un(i,j)=un(i,j)+ta(4)*ft(1,i,j)+ta(5)*ft(2,i,j)+ta(6)*ft(3,i,j)+
     &ta(7)*ft(4,i,j)
      enddo
      enddo
      endif
      endif
      
      call bcu
      do j=1,ny
      un(2,j)=un(2,j)-a(1,2,j)*un(1,j)
      un(m1,j)=un(m1,j)-c(1,m1,j)*un(mx,j)
      enddo
      
      do i=3,m1
      do j=1,ny
      un(i,j)=un(i,j)-adx(i,j)*un(i-1,j)
      enddo
      enddo
      do j=1,ny
      un(m1,j)=un(m1,j)*bdx(m1,j)
      enddo
      do i=m2,2,-1
      do j=1,ny
      un(i,j)=(un(i,j)-c(1,i,j)*un(i+1,j))*bdx(i,j)
      enddo
      enddo

      call bcu

      return
      end

c  ###########################################################
      
      subroutine vnsol

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      if(idpc.EQ.1)then
      do j=2,n1
      do i=1,mx
      vn(i,j)=vo(i,j)+ta(1)*g(2,i,j)+ta(2)*g(3,i,j)+ta(3)*g(4,i,j)+
     &2.*g1(2,i,j)-3.*g1(3,i,j)+g1(4,i,j)
      enddo
      enddo
      
      if(ibe.EQ.2)then
      do j=2,n1
      do i=1,mx
      vn(i,j)=vn(i,j)+ta(1)*gt(2,i,j)+ta(2)*gt(3,i,j)+ta(3)*gt(4,i,j)
      enddo
      enddo
      endif
      endif
      
      if(idpc.EQ.2)then
      do j=2,n1
      do i=1,mx
      vn(i,j)=vo(i,j)+ta(4)*g(1,i,j)+ta(5)*g(2,i,j)+ta(6)*g(3,i,j)+
     &ta(7)*g(4,i,j)+g1(1,i,j)-g1(2,i,j)
      enddo
      enddo
      
      if(ibe.EQ.2)then
      do j=2,n1
      do i=1,mx
      vn(i,j)=vn(i,j)+ta(4)*gt(1,i,j)+ta(5)*gt(2,i,j)+ta(6)*gt(3,i,j)+
     &ta(7)*gt(4,i,j)
      enddo
      enddo
      endif
      endif
      
      call bcv
      do i=1,mx
      vn(i,2)=vn(i,2)-a(2,i,2)*vn(i,1)
      vn(i,n1)=vn(i,n1)-c(2,i,n1)*vn(i,ny)
      enddo
      
      do j=3,n1
      do i=1,mx
      vn(i,j)=vn(i,j)-ady(i,j)*vn(i,j-1)
      enddo
      enddo
      do i=1,mx
      vn(i,n1)=vn(i,n1)*bdy(i,n1)
      enddo
      do j=n2,2,-1
      do i=1,mx
      vn(i,j)=(vn(i,j)-c(2,i,j)*vn(i,j+1))*bdy(i,j)
      enddo
      enddo
      call bcv

      return
      end

c  ###########################################################
      
      subroutine bcet

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      do j=1,ny
      etn(1,j)=0.0
      etn(mx,j)=0.0
      enddo
      
      do i=1,mx
      etn(i,1)=0.0
      etn(i,ny)=0.0
      enddo

      return
      end

c  ###########################################################
      
      subroutine bcu

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper


      do j=1,ny
      un(1,j)=0.0
      un(mx,j)=0.0
      enddo
      
      do i=1,mx
      un(i,1)=0.0
      un(i,ny)=0.0
      enddo

      return
      end

c  ###########################################################
      
      subroutine bcv

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      do i=1,mx
      vn(i,1)=0.0
      vn(i,ny)=0.0
      enddo
      
      do j=1,ny
      vn(1,j)=0.0
      vn(mx,j)=0.0
      enddo

      return
      end

c  ###########################################################
      
      subroutine errorcheck

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      do k=4,6
      err_sum(k)=0.0
      enddo

      do i=1,mx
      do j=1,ny
      tmp=abs(ete(i,j)-etn(i,j))
      if((tmp.gt.zdiff(i,j)).and.(ite.gt.1)) then
      idiff(i,j)=idiff(i,j)+1
      endif
      err_sum(4)=err_sum(4)+tmp
      if(ite.lt.4) zdiff(i,j)=tmp
      err_sum(5)=err_sum(5)+abs(ue(i,j)-un(i,j))
      err_sum(6)=err_sum(6)+abs(ve(i,j)-vn(i,j))
      enddo
      enddo
      
      do k=4,6
      err_sum(k)=err_sum(k)/float(mx*ny)
      enddo

      etem=err_sum(4)
      uem=err_sum(5)
      vem=err_sum(6)

      if(ite.eq.28) then
      open (25,file='zdiff'//output)
      tmpmin=1.0e+10
      tmpmax=-1.0e+10

      do j=1,ny
      do i=1,mx
      if (zdiff(i,j).gt.tmpmax) then
      tmpmax=zdiff(i,j)
      itmp=i
      jtmp=j
      endif
      if (zdiff(i,j).lt.tmpmin) tmpmin=zdiff(i,j) 
      enddo
      enddo

      if (itype.eq.0) then
      write(25,310)
      write(25,311) mx,ny
      write(25,312) xw,xe
      write(25,312) ys,yn
      write(25,312) tmpmin,tmpmax
      do j = 1, ny
      write(25,313) (zdiff(i,j),i=1,mx)
      enddo
      endif
      close(25)

      open (25,file='idiff'//output)
      if (itype.eq.0) then
      write(25,310)
      write(25,311) mx,ny
      write(25,312) xw,xe
      write(25,312) ys,yn
      write(25,311) 0,28
      do j = 1, ny
      write(25,314) (idiff(i,j),i=1,mx)
      enddo
      endif
      close(25)

      write(*,*)
      write(*,*) 'Writing zdiff saved from ite=3'
      write(*,*) 'Writing idiff summed to ite=28'
      write(*,*) 'The most unstable node i:  ', itmp
      write(*,*) 'The most unstable node j:  ', jtmp
      endif

  310 format('DSAA')
  311 format(2i20)
  312 format(2f20.6)
  313 format(4f15.9)
  314 format(4i15)

      return
      end

c  ###########################################################

      subroutine meanv

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      if(mod(it-1,iaver).EQ.0.AND.it.GE.itwv)then
      itmn=0
      do i=1,mx
      do j=1,ny
      um(i,j)=(etts(i,j,1)/float(iaver))
      vm(i,j)=(etts(i,j,2)/float(iaver))
      em(i,j)=(etts(i,j,3)/float(iaver))
      etts(i,j,1)=0.0
      etts(i,j,2)=0.0
      etts(i,j,3)=0.0
      enddo
      enddo
      endif
      
      htmp=h0*0.015
      if(itmn.LE.iaver)then
      do i=1,mx,ixdel
      do j=1,ny,jydel
      heij=1.0
      hetp=hcr(i,j)+etn(i,j)
      if(hetp.LT.htmp)heij=delta
      etts(i,j,1)=etts(i,j,1)+un(i,j)*heij
      etts(i,j,2)=etts(i,j,2)+vn(i,j)*heij
      etts(i,j,3)=etts(i,j,3)+etn(i,j)
      enddo
      enddo
      itmn=itmn+1
      endif

      return
      end

c  ###########################################################

      subroutine printing

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper


      if((ngage.gt.0).and.(ngage.le.ngm)) then
      do i=1,ngage
      iunit=700+i
      close(iunit)
      enddo
      endif

      if((nlag.gt.0).and.(nlag.le.ngm)) then
      do i=1,nlag
      iunit=800+i
      close(iunit)
      enddo
      endif

      if((ite.le.30).and.(t.gt.tout1)) then
      do i=1,mx
      do j=1,ny
      wvht(i,j)=etmax(i,j)-etmin(i,j)
      velang(i,j)=velang(i,j)*180/pi
      flxang(i,j)=flxang(i,j)*180/pi
      enddo
      enddo

      do i=1,mx
      do j=1,ny
      if(wetlnd(i,j).eq.0.0)then
      wvht(i,j)=zlim
      etmin(i,j)=zlim
      etmax(i,j)=zlim
      tmin(i,j)=zlim
      tmax(i,j)=zlim
      twet(i,j)=zlim
      tdry(i,j)=zlim
      velmx(i,j)=zlim
      velang(i,j)=zlim
      flxmx(i,j)=zlim
      flxang(i,j)=zlim
      brtim(i,j)=zlim
      brvis(i,j)=zlim
      endif
      enddo
      enddo

      open (25,file='Grid/zwavht'//output)
      call gridwriter(wvht)
      close (25)
      open (25,file='Grid/zmin'//output)
      call gridwriter(etmin)
      close (25)
      open (25,file='Grid/zmax'//output)
      call gridwriter(etmax)
      close (25)
      open (25,file='Grid/tmin'//output)
      call gridwriter(tmin)
      close (25)
      open (25,file='Grid/tmax'//output)
      call gridwriter(tmax)
      close (25)
      open (25,file='Grid/tlast'//output)
      call gridwriter(twet)
      close (25)
      open (25,file='Grid/tfirst'//output)
      call gridwriter(tdry)
      close (25)
      open (25,file='Grid/curmax'//output)
      call gridwriter(velmx)
      close (25)
      open (25,file='Grid/curang'//output)
      call gridwriter(velang)
      close (25)
      open (25,file='Grid/flxmax'//output)
      call gridwriter(flxmx)
      close (25)
      open (25,file='Grid/flxang'//output)
      call gridwriter(flxang)
      close (25)
      open (25,file='Grid/wetlnd'//output)
      call gridwriter(wetlnd)
      close (25)
      open (25,file='Grid/brwvtim'//output)
      call gridwriter(brtim)
      close (25)
      open (25,file='Grid/brwvvis'//output)
      call gridwriter(brvis)
      close (25)
      open (25,file='Grid/etn'//output)
      call gridwriter(etn)
      close (25)
      open (25,file='Grid/un'//output)
      call gridwriter(un)
      close (25)
      open (25,file='Grid/vn'//output)
      call gridwriter(vn)
      close (25)
      endif

      write(*,*) 'Finished writing files'
      write(*,*)

      return
      end

c  ###########################################################
      
      subroutine prnt

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      integer itrsn(10),jtrsn(10)
      
c      if((ngage.gt.0).and.(ngage.le.ngm)) then
c      open(30,file=f4n,access='direct',recl=imch*(ngage))
c      write(30,rec=it)(etn(ixg(k),iyg(k)),k=1,ngage)
c      close(30)
c      endif

      if((ngage.gt.0).and.(ngage.le.ngm)) then
      do i=1,ngage
      iunit=700+i
      dep=etn(ixg(i),iyg(i))+hs(ixg(i),iyg(i))

      if(dep.le.0.0) then
      etmp=0.0
      utmp=0.0
      vtmp=0.0
      fxtmp=0.0
      fytmp=0.0
      else
      etmp=etn(ixg(i),iyg(i))
      utmp=un(ixg(i),iyg(i))
      vtmp=vn(ixg(i),iyg(i))
      fxtmp=dep*(un(ixg(i),iyg(i))+((za(ixg(i),iyg(i))**2/2.0-
     &(hs(ixg(i),iyg(i))**2-hs(ixg(i),iyg(i))*etn(ixg(i),iyg(i))+
     &etn(ixg(i),iyg(i))**2)/6.0)*p_x(ixg(i),iyg(i))+
     &(za(ixg(i),iyg(i))+(hs(ixg(i),iyg(i))-
     &etn(ixg(i),iyg(i)))/2.0)*hpx(ixg(i),iyg(i))))
      fytmp=dep*(vn(ixg(i),iyg(i))+((za(ixg(i),iyg(i))**2/2.0-
     &(hs(ixg(i),iyg(i))**2-hs(ixg(i),iyg(i))*etn(ixg(i),iyg(i))+
     &etn(ixg(i),iyg(i))**2)/6.0)*p_y(ixg(i),iyg(i))+
     &(za(ixg(i),iyg(i))+(hs(ixg(i),iyg(i))-
     &etn(ixg(i),iyg(i)))/2.0)*hpy(ixg(i),iyg(i))))
      endif

      write(iunit,731) t,etmp,utmp,vtmp,fxtmp,fytmp,hs(ixg(i),iyg(i))
      enddo
      endif

731   format(7f15.5)

      if((nlag.gt.0).and.(nlag.le.ngm)) then
      do i=1,nlag
      iunit=800+i

      itmp=aint((xlag(i)-xw)/dx)+1
      if(itmp.eq.mx) then
      itmp=mx-1
      xtmp=1.0
      else
      xtmp=(xlag(i)-xw)/dx-float(itmp-1)
      endif

      jtmp=aint((ylag(i)-ys)/dy)+1
      if(jtmp.eq.ny) then      
      jtmp=ny-1
      ytmp=1.0
      else
      ytmp=(ylag(i)-ys)/dy-float(jtmp-1)
      endif      

      etmp=(1.0-xtmp)*(1.0-ytmp)*etn(itmp,jtmp)+
     &(1.0-xtmp)*ytmp*etn(itmp,jtmp+1)+
     &xtmp*(1.0-ytmp)*etn(itmp+1,jtmp)+
     &xtmp*ytmp*etn(itmp+1,jtmp+1)
      utmp=(1.0-xtmp)*(1.0-ytmp)*un(itmp,jtmp)+
     &(1.0-xtmp)*ytmp*un(itmp,jtmp+1)+
     &xtmp*(1.0-ytmp)*un(itmp+1,jtmp)+
     &xtmp*ytmp*un(itmp+1,jtmp+1)
      vtmp=(1.0-xtmp)*(1.0-ytmp)*vn(itmp,jtmp)+
     &(1.0-xtmp)*ytmp*vn(itmp,jtmp+1)+
     &xtmp*(1.0-ytmp)*vn(itmp+1,jtmp)+
     &xtmp*ytmp*vn(itmp+1,jtmp+1)
      htmp=(1.0-xtmp)*(1.0-ytmp)*hs(itmp,jtmp)+
     &(1.0-xtmp)*ytmp*hs(itmp,jtmp+1)+
     &xtmp*(1.0-ytmp)*hs(itmp+1,jtmp)+
     &xtmp*ytmp*hs(itmp+1,jtmp+1)
      dep=etmp+htmp

      if(dep.le.0.0) then
      etmp=0.0
      utmp=0.0
      vtmp=0.0
      endif

      write(iunit,831) t,xlag(i),ylag(i),etmp,utmp,vtmp,htmp

      if(dep.gt.0.0) then
      xlag(i)=xlag(i)+utmp*dt
      ylag(i)=ylag(i)+vtmp*dt
      endif

      enddo
      endif

831   format(7f15.5)

      if((t.gt.tout1).and.(t.lt.tout2)) then
      do i=1,mx
      do j=1,ny
      dep=etn(i,j)+hs(i,j)
      
      if(dep.gt.0.0) then
      utmp=sqrt(un(i,j)**2+vn(i,j)**2)
      fxtmp=dep*(un(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_x(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpx(i,j)))
      fytmp=dep*(vn(i,j)+((za(i,j)**2/2.0-(hs(i,j)**2-
     &hs(i,j)*etn(i,j)+etn(i,j)**2)/6.0)*p_y(i,j)+
     &(za(i,j)+(hs(i,j)-etn(i,j))/2.0)*hpy(i,j)))
      ftmp=sqrt(fxtmp**2+fytmp**2)

      if(etn(i,j).gt.etmax(i,j)) then
      etmax(i,j)=etn(i,j)
      tmax(i,j)=t
      endif

      if(etn(i,j).lt.etmin(i,j)) then
      etmin(i,j)=etn(i,j)
      tmin(i,j)=t
      endif

      if((utmp.gt.velmx(i,j)).and.(utmp.ne.0.0)) then
      velmx(i,j)=utmp
      tmp=un(i,j)/utmp
      if(abs(tmp).lt.1.0) then
      if (vn(i,j).ge.0.) velang(i,j)=asin(tmp)
      if (vn(i,j).lt.0.) velang(i,j)=pi-asin(tmp)
      endif
      endif

      if((ftmp.gt.flxmx(i,j)).and.(ftmp.ne.0.0)) then
      flxmx(i,j)=ftmp
      tmp=fxtmp/ftmp
      if(abs(tmp).lt.1.0) then
      if (fytmp.ge.0.) flxang(i,j)=asin(tmp)
      if (fytmp.lt.0.) flxang(i,j)=pi-asin(tmp)
      endif
      endif

      if((etn(i,j).gt.0.0).and.(tdry(i,j).eq.0.0)) then
      tdry(i,j)=t
      endif

      twet(i,j)=t
      wetlnd(i,j)=1.0

      if(edvis(i,j).gt.brvis(i,j)) then
      brvis(i,j)=edvis(i,j)
      endif

      if((float(itb(i,j))*dt).lt.brtim(i,j)) then
      brtim(i,j)=t
      endif

      endif      
      enddo
      enddo
      endif

      iduv=0

      if(iduv.EQ.0)then      
      ntrsx=1
      ntrsy=6
      itrsn(1)=116*2-1
      itrsn(2)=142*2-1
      itrsn(3)=167*2-1
      itrsn(4)=194*2-1
      itrsn(5)=238*2-1
      itrsn(6)=269*2-1
      jtrsn(1)=91
      ntrs=ntrsx*mxdl+ntrsy*nydl
      if(it.GE.itbgn)then
      ittmp=it-itbgn+1
c      open(29,file=f5n,access='direct',recl=imch*ntrs)
c      write(29,rec=ittmp)((etn(i,jtrsn(k)),i=1,mx,ixdel),k=1,ntrsx),
c     &((etn(itrsn(k),j),j=1,ny,jydel),k=1,ntrsy)
c      close(29)
      endif
c      elseif(iduv.EQ.1)then
c      open(30,file=f5n,access='direct',recl=imch*(2*ngage))
c      write(30,rec=it)(un(ixg(k),iyg(k)),k=1,ngage),(vn(ixg(k),iyg(k)),
c     &k=1,ngage)
c      close(30)
      endif
      
c      if(idout.EQ.1)then
c      if(mod(it-1,iaver).EQ.0.AND.it.GE.itbgn)then
c      open(40,file=f7n,access='direct',recl=imch*mxdl*nydl)
c      ictrl=ictrl+1
c      write(40,rec=ictrl)((um(i,j),j=1,ny,jydel),i=1,mx,ixdel)
c      ictrl=ictrl+1
c      write(40,rec=ictrl)((vm(i,j),j=1,ny,jydel),i=1,mx,ixdel)
c      close(40)
c      endif
      if(idout.EQ.0)then
      if(it.GE.itbgn.AND.it.LE.itend)then
      if(mod(it,itdel).eq.0)then
c      write(*,*) 'Writing to file f7n as unit 40'
c      open(40,file=f7n,access='direct',recl=imch*mxdl*nydl)
c      ictrl=ictrl+1
c      write(*,*) 'The value of ictrl is',ictrl
c      write(40,rec=ictrl)((etn(i,j),j=1,ny,jydel),i=1,mx,ixdel)
c      write(*,*) 'Writing free surface shape to file'
c      write(*,*)
c      close(40)
      call surfgrid
      endif
      endif
      endif
130   format(a5,i5.5)

      return
      end

c  ###########################################################

      subroutine fltr(xy1)

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      real xy1(iq,jq),xy2(iq,jq)
      
      do i=5,m4
      do j=1,ny
      xy2(i,j)=cf4(1)*xy1(i,j)+cf4(2)*(xy1(i+1,j)+xy1(i-1,j))+cf4(3)*
     &(xy1(i+2,j)+xy1(i-2,j))+cf4(4)*(xy1(i+3,j)+xy1(i-3,j))+cf4(5)*
     &(xy1(i+4,j)+xy1(i-4,j))
      enddo
      enddo
      do j=1,ny
      xy2(2,j)=cf1(1)*xy1(2,j)+cf1(2)*(xy1(3,j)+xy1(1,j))
      xy2(m1,j)=cf1(1)*xy1(m1,j)+cf1(2)*(xy1(m2,j)+xy1(mx,j))
      xy2(3,j)=cf2(1)*xy1(3,j)+cf2(2)*(xy1(4,j)+xy1(2,j))+cf2(3)*
     &(xy1(5,j)+xy1(1,j))
      xy2(m2,j)=cf2(1)*xy1(m2,j)+cf2(2)*(xy1(m3,j)+xy1(m1,j))+cf2(3)*
     &(xy1(m4,j)+xy1(mx,j))
      xy2(4,j)=cf3(1)*xy1(4,j)+cf3(2)*(xy1(5,j)+xy1(3,j))+cf3(3)*
     &(xy1(6,j)+xy1(2,j))+cf3(4)*(xy1(7,j)+xy1(1,j))
      xy2(m3,j)=cf3(1)*xy1(m3,j)+cf3(2)*(xy1(m4,j)+xy1(m2,j))+cf3(3)*
     &(xy1(m5,j)+xy1(m1,j))+cf3(4)*(xy1(m6,j)+xy1(mx,j))
      enddo

      do j=5,n4
      do i=2,m1
      xy1(i,j)=cf4(1)*xy2(i,j)+cf4(2)*(xy2(i,j+1)+xy2(i,j-1))+cf4(3)*
     &(xy2(i,j+2)+xy2(i,j-2))+cf4(4)*(xy2(i,j+3)+xy2(i,j-3))+cf4(5)*
     &(xy2(i,j+4)+xy2(i,j-4))
      enddo
      enddo
      do i=2,m1
      xy1(i,2)=cf1(1)*xy2(i,2)+cf1(2)*(xy2(i,3)+xy2(i,1))
      xy1(i,n1)=cf1(1)*xy2(i,n1)+cf1(2)*(xy2(i,n2)+xy2(i,ny))
      xy1(i,3)=cf2(1)*xy2(i,3)+cf2(2)*(xy2(i,4)+xy2(i,2))+cf2(3)*
     &(xy2(i,5)+xy2(i,1))
      xy1(i,n2)=cf2(1)*xy2(i,n2)+cf2(2)*(xy2(i,n3)+xy2(i,n1))+cf2(3)*
     &(xy2(i,n4)+xy2(i,ny))
      xy1(i,4)=cf3(1)*xy2(i,4)+cf3(2)*(xy2(i,5)+xy2(i,3))+cf3(3)*
     &(xy2(i,6)+xy2(i,2))+cf3(4)*(xy2(i,7)+xy2(i,1))
      xy1(i,n3)=cf3(1)*xy2(i,n3)+cf3(2)*(xy2(i,n4)+xy2(i,n2))+cf3(3)*
     &(xy2(i,n5)+xy2(i,n1))+cf3(4)*(xy2(i,n6)+xy2(i,ny))
      enddo

      return
      end

c  ###########################################################
      
      subroutine update

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper


      etamax=0.0
      etamin=0.0
      do i=1,mx
      do j=1,ny
      dep=etn(i,j)+hs(i,j)

      if(dep.gt.0.0)then
      za(i,j)=b2*h(i,j)+(1.0+b2)*etn(i,j)
      else
      za(i,j)=b2*h(i,j)
      endif

      if((etn(i,j).gt.etamax).and.(dep.gt.0.0)) etamax=etn(i,j)
      if((etn(i,j).lt.etamin).and.(dep.gt.0.0)) etamin=etn(i,j)
      enddo
      enddo

      ettmp=max(abs(etamin),abs(etamax))
      if(ettmp.gt.a0) a0=ettmp
      tmp=2.0*a0

      detmp=1.0-delta
      sltmp=1.0/slmda
      tmp=2.0*a0
      z0=2.2*a0

      do i=1,mx
      do j=1,ny

      if(-hs(i,j).gt.tmp) then
      etn(i,j)=0.95*etn(i,j)*abs(tmp/hs(i,j))
      endif

      if((-hs(i,j).gt.a0).and.(-hs(i,j).le.tmp)) then
      etn(i,j)=etn(i,j)*(1.0-0.05*abs(-hs(i,j)-a0)/a0)
      endif

      if(hs(i,j).le.tmp)then
      hcr(i,j)=hs(i,j)/detmp-z0*(delta/detmp+sltmp)
      else
      hcr(i,j)=hs(i,j)
      endif

      enddo
      enddo

      return
      end

c  ###########################################################

      subroutine etatest

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper


      eps=1.0e-5
      tmp=2.0*a0

      do i=1,mx
      do j=1,ny

      if(-hs(i,j).gt.tmp) then
      etn(i,j)=0.95*etn(i,j)*abs(tmp/hs(i,j))
      endif

      dep=etn(i,j)+z0
      if(hs(i,j).le.tmp)then
      if(dep.lt.eps) etn(i,j)=1.2e-5-z0
      if(abs(etn(i,j)).lt.eps) etn(i,j)=0.0
      if((etn(i,j)+hs(i,j)).le.0.0) then
      un(i,j)=0.0
      vn(i,j)=0.0
      endif
      endif

      dep=etn(i,j)+hs(i,j)
      if(hs(i,j).gt.tmp)then
      if(dep.lt.eps) then
      etn(i,j)=eps-hs(i,j)
      un(i,j)=0.0
      vn(i,j)=0.0
      endif
      if(abs(etn(i,j)).lt.eps) etn(i,j)=0.0
      endif

      enddo
      enddo

      return
      end

c  ###########################################################

      subroutine fltr1(xy1)

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper
 
      
      real xy1(iq,jq),xy2(iq,jq),cf5(16),cf6(16)
      
      do i=1,16
      cf6(i)=(cos((real(i)-1.0)*3.1416/2.0/15.0))**3
      cf5(i)=1.0-2.0*cf6(i)*(cf4(2)+cf4(3)+cf4(4)+cf4(5))
      enddo
      mtmpx=m4
      mtmpy=m1
      
      do i=5,mtmpx
      do j=1,16
      xy2(i,j)=cf5(j)*xy1(i,j)+cf6(j)*(+cf4(2)*(xy1(i+1,j)+xy1(i-1,j))+
     &cf4(3)*(xy1(i+2,j)+xy1(i-2,j))+cf4(4)*(xy1(i+3,j)+xy1(i-3,j))+
     &cf4(5)*(xy1(i+4,j)+xy1(i-4,j)))
      enddo
      jj=1
      do j=ny,ny-15,-1
      xy2(i,j)=cf5(jj)*xy1(i,j)+cf6(jj)*(+cf4(2)*(xy1(i+1,j)+xy1(i-1,j))
     &+cf4(3)*(xy1(i+2,j)+xy1(i-2,j))+cf4(4)*(xy1(i+3,j)+xy1(i-3,j))+
     &cf4(5)*(xy1(i+4,j)+xy1(i-4,j)))
      jj=jj+1
      enddo
      enddo
      do j=1,ny
      xy2(2,j)=cf1(1)*xy1(2,j)+cf1(2)*(xy1(3,j)+xy1(1,j))
      xy2(m1,j)=cf1(1)*xy1(m1,j)+cf1(2)*(xy1(m2,j)+xy1(mx,j))
      xy2(3,j)=cf2(1)*xy1(3,j)+cf2(2)*(xy1(4,j)+xy1(2,j))+cf2(3)*
     &(xy1(5,j)+xy1(1,j))
      xy2(m2,j)=cf2(1)*xy1(m2,j)+cf2(2)*(xy1(m3,j)+xy1(m1,j))+cf2(3)*
     &(xy1(m4,j)+xy1(mx,j))
      xy2(4,j)=cf3(1)*xy1(4,j)+cf3(2)*(xy1(5,j)+xy1(3,j))+cf3(3)*
     &(xy1(6,j)+xy1(2,j))+cf3(4)*(xy1(7,j)+xy1(1,j))
      xy2(m3,j)=cf3(1)*xy1(m3,j)+cf3(2)*(xy1(m4,j)+xy1(m2,j))+cf3(3)*
     &(xy1(m5,j)+xy1(m1,j))+cf3(4)*(xy1(m6,j)+xy1(mx,j))
      enddo
      
      do j=5,16
      do i=2,mtmpy
      xy1(i,j)=cf5(j)*xy2(i,j)+cf6(j)*(+cf4(2)*(xy2(i,j+1)+xy2(i,j-1))+
     &cf4(3)*(xy2(i,j+2)+xy2(i,j-2))+cf4(4)*(xy2(i,j+3)+xy2(i,j-3))+
     &cf4(5)*(xy2(i,j+4)+xy2(i,j-4)))
      enddo
      enddo
      jj=5
      do j=n4,ny-15,-1
      do i=2,mtmpy
      xy1(i,j)=cf5(jj)*xy2(i,j)+cf6(jj)*(+cf4(2)*(xy2(i,j+1)+xy2(i,j-1))
     &+cf4(3)*(xy2(i,j+2)+xy2(i,j-2))+cf4(4)*(xy2(i,j+3)+xy2(i,j-3))+
     &cf4(5)*(xy2(i,j+4)+xy2(i,j-4)))
      enddo
      jj=jj+1
      enddo
      do i=2,mtmpy
      xy1(i,2)=cf1(1)*xy2(i,2)+cf1(2)*(xy2(i,3)+xy2(i,1))
      xy1(i,n1)=cf1(1)*xy2(i,n1)+cf1(2)*(xy2(i,n2)+xy2(i,ny))
      xy1(i,3)=cf2(1)*xy2(i,3)+cf2(2)*(xy2(i,4)+xy2(i,2))+cf2(3)*
     &(xy2(i,5)+xy2(i,1))
      xy1(i,n2)=cf2(1)*xy2(i,n2)+cf2(2)*(xy2(i,n3)+xy2(i,n1))+cf2(3)*
     &(xy2(i,n4)+xy2(i,ny))
      xy1(i,4)=cf3(1)*xy2(i,4)+cf3(2)*(xy2(i,5)+xy2(i,3))+cf3(3)*
     &(xy2(i,6)+xy2(i,2))+cf3(4)*(xy2(i,7)+xy2(i,1))
      xy1(i,n3)=cf3(1)*xy2(i,n3)+cf3(2)*(xy2(i,n4)+xy2(i,n2))+cf3(3)*
     &(xy2(i,n5)+xy2(i,n1))+cf3(4)*(xy2(i,n6)+xy2(i,ny))
      enddo

      return
      end

c  ###########################################################

      subroutine fltr3(xy1)

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      real xy1(iq,jq),xy2(iq,jq)

      ctmp=0.9

      do i=1,mx
      do j=1,ny
      xy2(i,j)=xy1(i,j)
      enddo
      enddo

      jmass=ny-ispg(4)+1
      tmp=a0/50.0

      do i=ispg(1),imass
      do j=ispg(3),jmass
      dep=etn(i,j)+hs(i,j)
      if(xy1(i,j).ne.0.0)then
      if(abs(xy1(i,j)).lt.0.1)then
      if(dep.lt.tmp)then
      xy2(i,j)=ctmp*xy1(i,j)+0.58*(1.0-ctmp)*
     &(xy1(i+1,j)+xy1(i-1,j)+xy1(i,j+1)+xy1(i,j-1))/4.0+
     &0.42*(1.0-ctmp)*
     &(xy1(i+1,j+1)+xy1(i+1,j-1)+xy1(i-1,j-1)+xy1(i-1,j+1))/4.0
      endif
      endif
      endif
      enddo
      enddo

      do i=1,mx
      do j=1,ny
      xy1(i,j)=xy2(i,j)
      enddo
      enddo
      
      return
      end

c  ###########################################################

      subroutine fltr4(xy1)

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      real xy1(iq,jq),xy2(iq,jq)

      do i=1,mx
      do j=1,ny
      xy2(i,j)=xy1(i,j)
      enddo
      enddo

      jmass=ny-ispg(4)+1
      eps=1.0e-5

      do i=ispg(1),imass
      do j=ispg(3),jmass
      sum1=(xy1(i,j)+xy1(i+1,j)+xy1(i-1,j)+xy1(i,j+1)+
     &xy1(i,j-1)+xy1(i+1,j+1)+xy1(i+1,j-1)+xy1(i-1,j+1)+
     &xy1(i-1,j-1))/9.0
      sum2=(abs(xy1(i,j))+abs(xy1(i+1,j))+abs(xy1(i-1,j))+
     &abs(xy1(i,j+1))+abs(xy1(i,j-1))+abs(xy1(i+1,j+1))+
     &abs(xy1(i+1,j-1))+abs(xy1(i-1,j+1))+abs(xy1(i-1,j-1)))/9.0
      if((sum1.lt.eps).and.(sum2.lt.eps)) xy2(i,j)=0.0
      enddo
      enddo

      do i=1,mx
      do j=1,ny
      xy1(i,j)=xy2(i,j)
      enddo
      enddo
      
      return
      end

c  ###########################################################

      subroutine eval_fg

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      call pre_eval
      
      do j=1,ny
      u_x(1,j)=(p(1)*un(1,j)+p(2)*un(2,j)+p(3)*un(3,j)+p(4)*un(4,j)+
     &p(5)*un(5,j))*rdx
      u_x(2,j)=(q(1)*un(1,j)+q(2)*un(2,j)+q(3)*un(3,j)+q(4)*un(4,j)+
     &q(5)*un(5,j))*rdx
      u_x(mx,j)=-(p(1)*un(mx,j)+p(2)*un(m1,j)+p(3)*un(m2,j)+p(4)*
     &un(m3,j)+p(5)*un(m4,j))*rdx
      u_x(m1,j)=-(q(1)*un(mx,j)+q(2)*un(m1,j)+q(3)*un(m2,j)+q(4)*
     &un(m3,j)+q(5)*un(m4,j))*rdx
      v_x(1,j)=(p(1)*vn(1,j)+p(2)*vn(2,j)+p(3)*vn(3,j)+p(4)*vn(4,j)+
     &p(5)*vn(5,j))*rdx
      v_x(2,j)=(q(1)*vn(1,j)+q(2)*vn(2,j)+q(3)*vn(3,j)+q(4)*vn(4,j)+
     &q(5)*vn(5,j))*rdx
      v_x(mx,j)=-(p(1)*vn(mx,j)+p(2)*vn(m1,j)+p(3)*vn(m2,j)+p(4)*
     &vn(m3,j)+p(5)*vn(m4,j))*rdx
      v_x(m1,j)=-(q(1)*vn(mx,j)+q(2)*vn(m1,j)+q(3)*vn(m2,j)+q(4)*
     &vn(m3,j)+q(5)*vn(m4,j))*rdx
      do i=3,m2
      u_x(i,j)=(p(6)*(un(i+1,j)-un(i-1,j))-p(7)*(un(i+2,j)-un(i-2,j)))*
     &rdx
      v_x(i,j)=(p(6)*(vn(i+1,j)-vn(i-1,j))-p(7)*(vn(i+2,j)-vn(i-2,j)))*
     &rdx
      enddo
      enddo
      do i=1,mx
      u_y(i,1)=(p(1)*un(i,1)+p(2)*un(i,2)+p(3)*un(i,3)+p(4)*un(i,4)+
     &p(5)*un(i,5))*rdy
      u_y(i,2)=(q(1)*un(i,1)+q(2)*un(i,2)+q(3)*un(i,3)+q(4)*un(i,4)+
     &q(5)*un(i,5))*rdy
      u_y(i,ny)=-(p(1)*un(i,ny)+p(2)*un(i,n1)+p(3)*un(i,n2)+p(4)*
     &un(i,n3)+p(5)*un(i,n4))*rdy
      u_y(i,n1)=-(q(1)*un(i,ny)+q(2)*un(i,n1)+q(3)*un(i,n2)+q(4)*
     &un(i,n3)+q(5)*un(i,n4))*rdy
      v_y(i,1)=(p(1)*vn(i,1)+p(2)*vn(i,2)+p(3)*vn(i,3)+p(4)*vn(i,4)+
     &p(5)*vn(i,5))*rdy
      v_y(i,2)=(q(1)*vn(i,1)+q(2)*vn(i,2)+q(3)*vn(i,3)+q(4)*vn(i,4)+
     &q(5)*vn(i,5))*rdy
      v_y(i,ny)=-(p(1)*vn(i,ny)+p(2)*vn(i,n1)+p(3)*vn(i,n2)+p(4)*
     &vn(i,n3)+p(5)*vn(i,n4))*rdy
      v_y(i,n1)=-(q(1)*vn(i,ny)+q(2)*vn(i,n1)+q(3)*vn(i,n2)+q(4)*
     &vn(i,n3)+q(5)*vn(i,n4))*rdy
      do j=3,n2
      u_y(i,j)=(p(6)*(un(i,j+1)-un(i,j-1))-p(7)*(un(i,j+2)-un(i,j-2)))*
     &rdy
      v_y(i,j)=(p(6)*(vn(i,j+1)-vn(i,j-1))-p(7)*(vn(i,j+2)-vn(i,j-2)))*
     &rdy
      enddo
      enddo
      
      do j=1,ny
      eta_x(1,j)=(p(1)*etn(1,j)+p(2)*etn(2,j)+p(3)*etn(3,j)+p(4)*
     &etn(4,j)+p(5)*etn(5,j))*rdx
      eta_x(2,j)=(q(1)*etn(1,j)+q(2)*etn(2,j)+q(3)*etn(3,j)+q(4)*
     &etn(4,j)+q(5)*etn(5,j))*rdx
      eta_x(mx,j)=-(p(1)*etn(mx,j)+p(2)*etn(m1,j)+p(3)*etn(m2,j)+p(4)*
     &etn(m3,j)+p(5)*etn(m4,j))*rdx
      eta_x(m1,j)=-(q(1)*etn(mx,j)+q(2)*etn(m1,j)+q(3)*etn(m2,j)+q(4)*
     &etn(m3,j)+q(5)*etn(m4,j))*rdx
      do i=3,m2
      eta_x(i,j)=(p(6)*(etn(i+1,j)-etn(i-1,j))-p(7)*(etn(i+2,j)-
     &etn(i-2,j)))*rdx
      enddo
      enddo
      do i=1,mx
      do j=1,ny
      f(1,i,j)=-ga*eta_x(i,j)-(un(i,j)*u_x(i,j)+vn(i,j)*u_y(i,j))*
     &clnr-w1(i,j)*un(i,j)+w2(i,j)*(u_xx(i,j)+u_yy(i,j))
      enddo
      enddo
      
      do i=1,mx
      eta_y(i,1)=(p(1)*etn(i,1)+p(2)*etn(i,2)+p(3)*etn(i,3)+p(4)*
     &etn(i,4)+p(5)*etn(i,5))*rdy
      eta_y(i,2)=(q(1)*etn(i,1)+q(2)*etn(i,2)+q(3)*etn(i,3)+q(4)*
     &etn(i,4)+q(5)*etn(i,5))*rdy
      eta_y(i,ny)=-(p(1)*etn(i,ny)+p(2)*etn(i,n1)+p(3)*etn(i,n2)+p(4)*
     &etn(i,n3)+p(5)*etn(i,n4))*rdy
      eta_y(i,n1)=-(q(1)*etn(i,ny)+q(2)*etn(i,n1)+q(3)*etn(i,n2)+q(4)*
     &etn(i,n3)+q(5)*etn(i,n4))*rdy
      do j=3,n2
      eta_y(i,j)=(p(6)*(etn(i,j+1)-etn(i,j-1))-p(7)*(etn(i,j+2)-
     &etn(i,j-2)))*rdy
      enddo
      enddo
      do j=1,ny
      do i=1,mx
      g(1,i,j)=-ga*eta_y(i,j)-(un(i,j)*v_x(i,j)+vn(i,j)*v_y(i,j))*
     &clnr-w1(i,j)*vn(i,j)+w2(i,j)*(v_xx(i,j)+v_yy(i,j))
      enddo
      enddo
      
      do i=1,mx
      do j=1,ny
      f1(1,i,j)=-h(i,j)*(b1*h(i,j)*v_xy(i,j)+b2*hvxy(i,j))
      g1(1,i,j)=-h(i,j)*(b1*h(i,j)*u_xy(i,j)+b2*huxy(i,j))
      enddo
      enddo

      if(cbrk.GE.1e-6)then
      
      izelt=0
      if(izelt.EQ.1)then
      itbrk=0
      et_xc=0.3
      ddx=dx*0.001
      do i=1,mx
      do j=1,ny
      dp=max(ddx,h(i,j))
      et_tc=et_xc*sqrt(ga*dp)
      tmp=e(1,i,j)
      if(tmp.le.et_tc)then
      cbr=0.0
      elseif(tmp.le.2.0*et_tc)then
      cbr=tmp/et_tc-1.0
      itbrk=1
      else
      cbr=1.0
      itbrk=1
      endif
      edvis(i,j)=cbr*(cbrk**2*dp)*e(1,i,j)
      enddo
      enddo
      else
      itbrk=0
      et1=cbkv
      et2=0.15
      coeft=5.0
      ddx=dx*0.05
      ddxy=sqrt(dx*dx+dy*dy)
      
      if(ite.EQ.1.OR.it.EQ.1)then
      do j=2,n1
      do i=2,m1
      etmp=max(0.00001,abs(eta_x(i,j)))
      angle=atan(eta_y(i,j)/etmp)
      atmp=45.0/180*pi
      if(abs(angle).LE.atmp)then
      iage=min(itb(i+1,j),itb(i-1,j))
      iage=min(iage,itb(i,j))
      if(iage.LT.nt)then
      dp=max(ddx,h(i,j))
      trnt=coeft*sqrt(dp/ga)
      et_tc=et1-(et1-et2)*float(it-iage)*dt/trnt
      ctb(i,j)=max(et_tc,et2)
      else
      ctb(i,j)=et1
      endif
      elseif(angle.GT.atmp)then
      iage=min(itb(i+1,j+1),itb(i-1,j-1))
      iage=min(iage,itb(i,j))
      if(iage.LT.nt)then
      dp=max(ddx,h(i,j))
      trnt=coeft*sqrt(dp/ga)
      et_tc=et1-(et1-et2)*float(it-iage)*dt/trnt
      ctb(i,j)=max(et_tc,et2)
      else
      ctb(i,j)=et1
      endif
      elseif(angle.LT.-atmp)then
      iage=min(itb(i-1,j+1),itb(i+1,j-1))
      iage=min(iage,itb(i,j))
      if(iage.LT.nt)then
      dp=max(ddx,h(i,j))
      trnt=coeft*sqrt(dp/ga)
      et_tc=et1-(et1-et2)*float(it-iage)*dt/trnt
      ctb(i,j)=max(et_tc,et2)
      else
      ctb(i,j)=et1
      endif
      endif
      enddo
      enddo
      endif
      
      do j=2,n1
      do i=2,m1
      dp=max(ddx,h(i,j))
      et_tc=ctb(i,j)*sqrt(ga*dp)
      tmp=e(1,i,j)
      if(tmp.LE.et_tc)then
      cbr=0.
      itb(i,j)=nt
      else
      cbr=min(1.0,(tmp/et_tc-1.0))
      itbrk=1
      itb(i,j)=min(itb(i,j),it)
      endif
      
      if(etn(i,j)+hs(i,j).LT.-ddx)then
      cbr=1.0
      dp=max(ddx,abs(etn(i,j)))
      endif
      edvis(i,j)=cbr*cbrk**2*dp*abs(e(1,i,j))
      enddo
      enddo
      endif
      
      ctm=c_dm
      
      if(ieddy.EQ.1)then
      do i=2,m1
      do j=2,n1
      eddy=(um(i+1,j)-um(i-1,j))**2*rdx**2+(vm(i,j+1)-vm(i,j-1))**2*
     &rdy**2+0.5*((um(i,j+1)-um(i,j-1))*rdy+(vm(i+1,j)-vm(i-1,j))*
     &rdx)**2
      if(eddy.GT.1.0e-6)then
      edvis(i,j)=edvis(i,j)+ctm*dx*dy*sqrt(eddy)
      endif
      enddo
      enddo
      endif
      
      call fltr(edvis)
      
      idfull=1

      if(idfull.EQ.0)then
      do i=2,m1
      do j=2,n1
      heun=he(i,j)*un(i,j)
      hevn=he(i,j)*vn(i,j)
      heu_yp=rdy*(he(i,j+2)*un(i,j+2)-heun)
      heu_ym=-rdy*(he(i,j-2)*un(i,j-2)-heun)
      hev_xp=rdx*(he(i+2,j)*vn(i+2,j)-hevn)
      hev_xm=-rdx*(he(i-2,j)*vn(i-2,j)-hevn)
      fbr(i,j)=+rdx*(edvis(i+1,j)*heu_x(i+1,j)-edvis(i-1,j)*
     &heu_x(i-1,j))+rdy*(edvis(i,j+1)*heu_yp-edvis(i,j-1)*heu_ym)
      gbr(i,j)=rdx*(edvis(i+1,j)*hev_xp-edvis(i-1,j)*hev_xm)+rdy*
     &(edvis(i,j+1)*hev_y(i,j+1)-edvis(i,j-1)*hev_y(i,j-1))
      enddo
      enddo
      endif

      if(idfull.EQ.1)then
      j=1
      do i=3,m2
      heun=he(i,j)*un(i,j)
      hevn=he(i,j)*vn(i,j)
      heu_yp=rdy*(he(i,j+2)*un(i,j+2)-heun)
      heu_ym=-rdy*(he(i,3)*un(i,3)-heun)
      hev_xp=rdx*(he(i+2,j)*vn(i+2,j)-hevn)
      hev_xm=-rdx*(he(i-2,j)*vn(i-2,j)-hevn)
      hev_xpy=rdx*(he(i+1,j+1)*vn(i+1,j+1)-he(i-1,j+1)*vn(i-1,j+1))
      hev_xmy=-hev_xpy
      heu_ypx=0.0
      heu_ymx=0.0
      fbr(i,j)=+rdx*(edvis(i+1,j)*heu_x(i+1,j)-edvis(i-1,j)*
     &heu_x(i-1,j))+rdy*0.5*((edvis(i,j+1)*heu_yp-edvis(i,2)*heu_ym)+
     &(edvis(i,j+1)*hev_xpy-edvis(i,2)*hev_xmy))
      gbr(i,j)=+rdx*0.5*((edvis(i+1,j)*hev_xp-edvis(i-1,j)*hev_xm)+
     &(edvis(i+1,j)*heu_ypx-edvis(i-1,j)*heu_ymx))+rdy*(edvis(i,j+1)*
     &hev_y(i,j+1)-edvis(i,2)*hev_y(i,2))
      enddo
      j=2
      do i=3,m2
      heun=he(i,j)*un(i,j)
      hevn=he(i,j)*vn(i,j)
      heu_yp=rdy*(he(i,j+2)*un(i,j+2)-heun)
      heu_ym=-rdy*(he(i,2)*un(i,2)-heun)
      hev_xp=rdx*(he(i+2,j)*vn(i+2,j)-hevn)
      hev_xm=-rdx*(he(i-2,j)*vn(i-2,j)-hevn)
      hev_xpy=rdx*(he(i+1,j+1)*vn(i+1,j+1)-he(i-1,j+1)*vn(i-1,j+1))
      hev_xmy=rdx*(he(i+1,j-1)*vn(i+1,j-1)-he(i-1,j-1)*vn(i-1,j-1))
      heu_ypx=rdy*(he(i+1,j+1)*un(i+1,j+1)-he(i+1,j-1)*un(i+1,j-1))
      heu_ymx=rdy*(he(i-1,j+1)*un(i-1,j+1)-he(i-1,j-1)*un(i-1,j-1))
      fbr(i,j)=+rdx*(edvis(i+1,j)*heu_x(i+1,j)-edvis(i-1,j)*
     &heu_x(i-1,j))+rdy*0.5*((edvis(i,j+1)*heu_yp-edvis(i,j-1)*heu_ym)+
     &(edvis(i,j+1)*hev_xpy-edvis(i,j-1)*hev_xmy))
      gbr(i,j)=+rdx*0.5*((edvis(i+1,j)*hev_xp-edvis(i-1,j)*hev_xm)+
     &(edvis(i+1,j)*heu_ypx-edvis(i-1,j)*heu_ymx))+rdy*(edvis(i,j+1)*
     &hev_y(i,j+1)-edvis(i,j-1)*hev_y(i,j-1))
      enddo
      j=ny
      do i=3,m2
      heun=he(i,j)*un(i,j)
      hevn=he(i,j)*vn(i,j)
      heu_yp=rdy*(he(i,n2)*un(i,n2)-heun)
      heu_ym=-rdy*(he(i,j-2)*un(i,j-2)-heun)
      hev_xp=rdx*(he(i+2,j)*vn(i+2,j)-hevn)
      hev_xm=-rdx*(he(i-2,j)*vn(i-2,j)-hevn)
      hev_xmy=rdx*(he(i+1,j-1)*vn(i+1,j-1)-he(i-1,j-1)*vn(i-1,j-1))
      hev_xpy=-hev_xmy
      heu_ypx=0.0
      heu_ymx=0.0
      fbr(i,j)=+rdx*(edvis(i+1,j)*heu_x(i+1,j)-edvis(i-1,j)*
     &heu_x(i-1,j))+rdy*0.5*((edvis(i,n1)*heu_yp-edvis(i,j-1)*heu_ym)+
     &(edvis(i,n1)*hev_xpy-edvis(i,j-1)*hev_xmy))
      gbr(i,j)=+rdx*0.5*((edvis(i+1,j)*hev_xp-edvis(i-1,j)*hev_xm)+
     &(edvis(i+1,j)*heu_ypx-edvis(i-1,j)*heu_ymx))+rdy*(edvis(i,n1)*
     &hev_y(i,n1)-edvis(i,j-1)*hev_y(i,j-1))
      enddo
      j=n1
      do i=3,m2
      heun=he(i,j)*un(i,j)
      hevn=he(i,j)*vn(i,j)
      heu_yp=rdy*(he(i,n1)*un(i,n1)-heun)
      heu_ym=-rdy*(he(i,j-2)*un(i,j-2)-heun)
      hev_xp=rdx*(he(i+2,j)*vn(i+2,j)-hevn)
      hev_xm=-rdx*(he(i-2,j)*vn(i-2,j)-hevn)
      hev_xpy=rdx*(he(i+1,j+1)*vn(i+1,j+1)-he(i-1,j+1)*vn(i-1,j+1))
      hev_xmy=rdx*(he(i+1,j-1)*vn(i+1,j-1)-he(i-1,j-1)*vn(i-1,j-1))
      heu_ypx=rdy*(he(i+1,j+1)*un(i+1,j+1)-he(i+1,j-1)*un(i+1,j-1))
      heu_ymx=rdy*(he(i-1,j+1)*un(i-1,j+1)-he(i-1,j-1)*un(i-1,j-1))
      fbr(i,j)=+rdx*(edvis(i+1,j)*heu_x(i+1,j)-edvis(i-1,j)*
     &heu_x(i-1,j))+rdy*0.5*((edvis(i,j+1)*heu_yp-edvis(i,j-1)*heu_ym)+
     &(edvis(i,j+1)*hev_xpy-edvis(i,j-1)*hev_xmy))
      gbr(i,j)=+rdx*0.5*((edvis(i+1,j)*hev_xp-edvis(i-1,j)*hev_xm)+
     &(edvis(i+1,j)*heu_ypx-edvis(i-1,j)*heu_ymx))+rdy*(edvis(i,j+1)*
     &hev_y(i,j+1)-edvis(i,j-1)*hev_y(i,j-1))
      enddo
      do i=3,m2
      do j=3,n2
      heun=he(i,j)*un(i,j)
      hevn=he(i,j)*vn(i,j)
      heu_yp=rdy*(he(i,j+2)*un(i,j+2)-heun)
      heu_ym=-rdy*(he(i,j-2)*un(i,j-2)-heun)
      hev_xp=rdx*(he(i+2,j)*vn(i+2,j)-hevn)
      hev_xm=-rdx*(he(i-2,j)*vn(i-2,j)-hevn)
      hev_xpy=rdx*(he(i+1,j+1)*vn(i+1,j+1)-he(i-1,j+1)*vn(i-1,j+1))
      hev_xmy=rdx*(he(i+1,j-1)*vn(i+1,j-1)-he(i-1,j-1)*vn(i-1,j-1))
      heu_ypx=rdy*(he(i+1,j+1)*un(i+1,j+1)-he(i+1,j-1)*un(i+1,j-1))
      heu_ymx=rdy*(he(i-1,j+1)*un(i-1,j+1)-he(i-1,j-1)*un(i-1,j-1))
      fbr(i,j)=+rdx*(edvis(i+1,j)*heu_x(i+1,j)-edvis(i-1,j)*
     &heu_x(i-1,j))+rdy*0.5*((edvis(i,j+1)*heu_yp-edvis(i,j-1)*heu_ym)+
     &(edvis(i,j+1)*hev_xpy-edvis(i,j-1)*hev_xmy))
      gbr(i,j)=+rdx*0.5*((edvis(i+1,j)*hev_xp-edvis(i-1,j)*hev_xm)+
     &(edvis(i+1,j)*heu_ypx-edvis(i-1,j)*heu_ymx))+rdy*(edvis(i,j+1)*
     &hev_y(i,j+1)-edvis(i,j-1)*hev_y(i,j-1))
      enddo
      enddo
      endif

      do i=2,m1
      do j=2,n1
      dp=max(0.001,he(i,j))
      f(1,i,j)=f(1,i,j)+fbr(i,j)/dp
      g(1,i,j)=g(1,i,j)+gbr(i,j)/dp
      enddo
      enddo
      endif
      
      if(idslot.EQ.1)then
      delta2=delta/2.0
      else
      delta2=ddx
      endif

      do i=1,mx
      do j=1,ny
      uvn=un(i,j)*un(i,j)+vn(i,j)*vn(i,j)
      if(uvn.GT.1.0e-8)then
      dp=max(delta2,he(i,j)+delta2)
      uvn=ck_bt*sqrt(uvn)/dp
      f(1,i,j)=f(1,i,j)-uvn*un(i,j)
      g(1,i,j)=g(1,i,j)-uvn*vn(i,j)
      endif
      enddo
      enddo
      
      if(ibe.EQ.2)then
      call eval_f2g2
      do i=1,mx
      do j=1,ny
      f(1,i,j)=f(1,i,j)+f2(i,j)
      g(1,i,j)=g(1,i,j)+g2(i,j)
      enddo
      enddo
      call eval_utvt
      endif

      return
      end

c  ###########################################################
      
      subroutine eval_e

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      call pre_eval

      do i=1,mx
      do j=1,ny
      he(i,j)=hcr(i,j)+etn(i,j)*clnr
      enddo
      enddo

      if(idslot.EQ.1)then
      do j=1,ny
      do i=islt,mx
      tmp=2.0*a0
      z0mh=z0-hcr(i,j)
      z0pe=z0+etn(i,j)

      if(hs(i,j).le.tmp)then
      if(he(i,j).GT.0.0)then
      he(i,j)=he(i,j)+z0mh*delta+dlts*(1.0-exp(-dlamda*z0mh))
      else
      he(i,j)=z0pe*delta+dlts*exp(dlamda*he(i,j))*
     &(1.0-exp(-dlamda*z0pe))
      endif
      endif

      enddo
      enddo
      endif
      
      do j=1,ny
      heu_x(1,j)=(p(1)*he(1,j)*un(1,j)+p(2)*he(2,j)*un(2,j)+p(3)*he(3,j)
     &*un(3,j)+p(4)*he(4,j)*un(4,j)+p(5)*he(5,j)*un(5,j))*rdx
      heu_x(2,j)=(q(1)*he(1,j)*un(1,j)+q(2)*he(2,j)*un(2,j)+q(3)*he(3,j)
     &*un(3,j)+q(4)*he(4,j)*un(4,j)+q(5)*he(5,j)*un(5,j))*rdx
      heu_x(mx,j)=-(p(1)*he(mx,j)*un(mx,j)+p(2)*he(m1,j)*un(m1,j)+p(3)*
     &he(m2,j)*un(m2,j)+p(4)*he(m3,j)*un(m3,j)+p(5)*he(m4,j)*un(m4,j))*
     &rdx
      heu_x(m1,j)=-(q(1)*he(mx,j)*un(mx,j)+q(2)*he(m1,j)*un(m1,j)+q(3)*
     &he(m2,j)*un(m2,j)+q(4)*he(m3,j)*un(m3,j)+q(5)*he(m4,j)*un(m4,j))*
     &rdx
      do i=3,m2
      heu_x(i,j)=rdx*(p(6)*(he(i+1,j)*un(i+1,j)-he(i-1,j)*un(i-1,j))-
     &p(7)*(he(i+2,j)*un(i+2,j)-he(i-2,j)*un(i-2,j)))
      enddo
      enddo
      do i=1,mx
      hev_y(i,1)=(p(1)*he(i,1)*vn(i,1)+p(2)*he(i,2)*vn(i,2)+p(3)*he(i,3)
     &*vn(i,3)+p(4)*he(i,4)*vn(i,4)+p(5)*he(i,5)*vn(i,5))*rdy
      hev_y(i,2)=(q(1)*he(i,1)*vn(i,1)+q(2)*he(i,2)*vn(i,2)+q(3)*he(i,3)
     &*vn(i,3)+q(4)*he(i,4)*vn(i,4)+q(5)*he(i,5)*vn(i,5))*rdy
      hev_y(i,ny)=-(p(1)*he(i,ny)*vn(i,ny)+p(2)*he(i,n1)*vn(i,n1)+p(3)*
     &he(i,n2)*vn(i,n2)+p(4)*he(i,n3)*vn(i,n3)+p(5)*he(i,n4)*vn(i,n4))*
     &rdy
      hev_y(i,n1)=-(q(1)*he(i,ny)*vn(i,ny)+q(2)*he(i,n1)*vn(i,n1)+q(3)*
     &he(i,n2)*vn(i,n2)+q(4)*he(i,n3)*vn(i,n3)+q(5)*he(i,n4)*vn(i,n4))*
     &rdy
      do j=3,n2
      hev_y(i,j)=rdy*(p(6)*(he(i,j+1)*vn(i,j+1)-he(i,j-1)*vn(i,j-1))-
     &p(7)*(he(i,j+2)*vn(i,j+2)-he(i,j-2)*vn(i,j-2)))
      enddo
      enddo
      
      if(idslot.EQ.1)then
      do j=1,ny
      do i=islt,mx
      ttdp=etn(i,j)+hcr(i,j)
      if(ttdp.LE.0.0)then
      sltc=delta+(1.0-delta)*exp(dlamda*ttdp)
      heu_x(i,j)=heu_x(i,j)/sltc
      hev_y(i,j)=hev_y(i,j)/sltc
      endif
      enddo
      enddo
      endif
      
      e(1,1,1)=-heu_x(1,1)-hev_y(1,1)-(-3.*h(1,1)**2*(a1*h(1,1)*p_x(1,1)
     &+a2*hpx(1,1))+4.*h(2,1)**2*(a1*h(2,1)*p_x(2,1)+a2*hpx(2,1))-h(3,1)
     &**2*(a1*h(3,1)*p_x(3,1)+a2*hpx(3,1)))*rdx-(-3.*h(1,1)**2*(a1*h(1,1
     &)*p_y(1,1)+a2*hpy(1,1))+4.*h(1,2)**2*(a1*h(1,2)*p_y(1,2)+a2*hpy(1,
     &2))-h(1,3)**2*(a1*h(1,3)*p_y(1,3)+a2*hpy(1,3)))*rdy
      e(1,mx,1)=-heu_x(mx,1)-hev_y(mx,1)+(-3.*h(mx,1)**2*(a1*h(mx,1)*p_x
     &(mx,1)+a2*hpx(mx,1))+4.*h(m1,1)**2*(a1*h(m1,1)*p_x(m1,1)+a2*hpx(m1
     &,1))-h(m2,1)**2*(a1*h(m2,1)*p_x(m2,1)+a2*hpx(m2,1)))*rdx-(-3.*h(mx
     &,1)**2*(a1*h(mx,1)*p_y(mx,1)+a2*hpy(mx,1))+4.*h(mx,2)**2*(a1*h(mx,
     &2)*p_y(mx,2)+a2*hpy(mx,2))-h(mx,3)**2*(a1*h(mx,3)*p_y(mx,3)+a2*hpy
     &(mx,3)))*rdy
      e(1,1,ny)=-heu_x(1,ny)-hev_y(1,ny)-(-3.*h(1,ny)**2*(a1*h(1,ny)*p_x
     &(1,ny)+a2*hpx(1,ny))+4.*h(2,ny)**2*(a1*h(2,ny)*p_x(2,ny)+a2*hpx(2,
     &ny))-h(3,ny)**2*(a1*h(3,ny)*p_x(3,ny)+a2*hpx(3,ny)))*rdx+(-3.*h(1,
     &ny)**2*(a1*h(1,ny)*p_y(1,ny)+a2*hpy(1,ny))+4.*h(1,n1)**2*(a1*h(1,n
     &1)*p_y(1,n1)+a2*hpy(1,n1))-h(1,n2)**2*(a1*h(1,n2)*p_y(1,n2)+a2*hpy
     &(1,n2)))*rdy
      e(1,mx,ny)=-heu_x(mx,ny)-hev_y(mx,ny)+(-3.*h(mx,ny)**2*(a1*h(mx,ny
     &)*p_x(mx,ny)+a2*hpx(mx,ny))+4.*h(m1,ny)**2*(a1*h(m1,ny)*p_x(m1,ny)
     &+a2*hpx(m1,ny))-h(m2,ny)**2*(a1*h(m2,ny)*p_x(m2,ny)+a2*hpx(m2,ny))
     &)*rdx+(-3.*h(mx,ny)**2*(a1*h(mx,ny)*p_y(mx,ny)+a2*hpy(mx,ny))+4.*h
     &(mx,n1)**2*(a1*h(mx,n1)*p_y(mx,n1)+a2*hpy(mx,n1))-h(mx,n2)**2*(a1*
     &h(mx,n2)*p_y(mx,n2)+a2*hpy(mx,n2)))*rdy
      
      do j=2,n1
      e(1,1,j)=-heu_x(1,j)-hev_y(1,j)-(-3.*h(1,j)**2*(a1*h(1,j)*p_x(1,j)
     &+a2*hpx(1,j))+4.*h(2,j)**2*(a1*h(2,j)*p_x(2,j)+a2*hpx(2,j))-h(3,j)
     &**2*(a1*h(3,j)*p_x(3,j)+a2*hpx(3,j)))*rdx-(h(1,j+1)**2*(a1*h(1,j+1
     &)*p_y(1,j+1)+a2*hpy(1,j+1))-h(1,j-1)**2*(a1*h(1,j-1)*p_y(1,j-1)+a2
     &*hpy(1,j-1)))*rdy
      e(1,mx,j)=-heu_x(mx,j)-hev_y(mx,j)+(-3.*h(mx,j)**2*(a1*h(mx,j)*p_x
     &(mx,j)+a2*hpx(mx,j))+4.*h(m1,j)**2*(a1*h(m1,j)*p_x(m1,j)+a2*hpx(m1
     &,j))-h(m2,j)**2*(a1*h(m2,j)*p_x(m2,j)+a2*hpx(m2,j)))*rdx-(h(mx,j+1
     &)**2*(a1*h(mx,j+1)*p_y(mx,j+1)+a2*hpy(mx,j+1))-h(mx,j-1)**2*(a1*h(
     &mx,j-1)*p_y(mx,j-1)+a2*hpy(mx,j-1)))*rdy
      enddo
      
      do i=2,m1
      e(1,i,1)=-heu_x(i,1)-hev_y(i,1)-(-3.*h(i,1)**2*(a1*h(i,1)*p_y(i,1)
     &+a2*hpy(i,1))+4.*h(i,2)**2*(a1*h(i,2)*p_y(i,2)+a2*hpy(i,2))-h(i,3)
     &**2*(a1*h(i,3)*p_y(i,3)+a2*hpy(i,3)))*rdy-(h(i+1,1)**2*(a1*h(i+1,1
     &)*p_x(i+1,1)+a2*hpx(i+1,1))-h(i-1,1)**2*(a1*h(i-1,1)*p_x(i-1,1)+a2
     &*hpx(i-1,1)))*rdx
      e(1,i,ny)=-heu_x(i,ny)-hev_y(i,ny)+(-3.*h(i,ny)**2*(a1*h(i,ny)*p_y
     &(i,ny)+a2*hpy(i,ny))+4.*h(i,n1)**2*(a1*h(i,n1)*p_y(i,n1)+a2*hpy(i,
     &n1))-h(i,n2)**2*(a1*h(i,n2)*p_y(i,n2)+a2*hpy(i,n2)))*rdy-(h(i+1,ny
     &)**2*(a1*h(i+1,ny)*p_x(i+1,ny)+a2*hpx(i+1,ny))-h(i-1,ny)**2*(a1*h(
     &i-1,ny)*p_x(i-1,ny)+a2*hpx(i-1,ny)))*rdx
      enddo
      
      do i=2,m1
      do j=2,n1
      e(1,i,j)=-heu_x(i,j)-hev_y(i,j)-(h(i+1,j)**2*(a1*h(i+1,j)*p_x(i+1,
     &j)+a2*hpx(i+1,j))-h(i-1,j)**2*(a1*h(i-1,j)*p_x(i-1,j)+a2*hpx(i-1,j
     &)))*rdx-(h(i,j+1)**2*(a1*h(i,j+1)*p_y(i,j+1)+a2*hpy(i,j+1))-h(i,j-
     &1)**2*(a1*h(i,j-1)*p_y(i,j-1)+a2*hpy(i,j-1)))*rdy
      enddo
      enddo
      
      if(ibe.EQ.2)then
      call eval_e2
      do i=1,mx
      do j=1,ny
      e(1,i,j)=e(1,i,j)+e2(i,j)
      enddo
      enddo
      endif

c      amo=uvm(1)      
c      amn=0.0
      aref=0.0
      jmass=ny-ispg(4)+1
c      iat=2*iaver
c      if(it.GE.itwv) then

c      if(it.gt.2) then
c      do i=1,iat
c      itmp=it-1
c      amn=amn+uvm(itmp)
c      enddo
c      uvm(it)=amn
c      ass=(amn-amo)/(float(imass-ispg(1))*dx*float(jmass-ispg(3))*
c     &dy*float(iaver)*dt*float(iat))-aref

c      ass=(uvm(it-1)-(uvm(it-2)-flux(it-2)))/(float(imass-ispg(1))*
c     &dx*float(jmass-ispg(3))*dy*dt)-aref

c      do i=ispg(1),imass
c      do j=ispg(3),jmass
c      e(1,i,j)=e(1,i,j)-ass
c      enddo
c      enddo
c      endif

      return
      end

c  ###########################################################

      subroutine pre_eval

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      do i=1,mx
      do j=1,ny
      hu(i,j)=h(i,j)*un(i,j)
      hv(i,j)=h(i,j)*vn(i,j)
      enddo
      enddo
      
      u_xy(1,1)=(9.*un(1,1)+16.*un(2,2)+un(3,3)-12.*(un(1,2)+un(2,1))+3.
     &*(un(1,3)+un(3,1))-4.*(un(2,3)+un(3,2)))*rxy
      u_xy(1,ny)=-(9.*un(1,ny)+16.*un(2,n1)+un(3,n2)-12.*(un(1,n1)+un(2,
     &ny))+3.*(un(1,n2)+un(3,ny))-4.*(un(2,n2)+un(3,n1)))*rxy
      u_xy(mx,1)=-(9.*un(mx,1)+16.*un(m1,2)+un(m2,3)-12.*(un(mx,2)+un(m1
     &,1))+3.*(un(mx,3)+un(m2,1))-4.*(un(m1,3)+un(m2,2)))*rxy
      u_xy(mx,ny)=(9.*un(mx,ny)+16.*un(m1,n1)+un(m2,n2)-12.*(un(mx,n1)+u
     &n(m1,ny))+3.*(un(mx,n2)+un(m2,ny))-4.*(un(m1,n2)+un(m2,n1)))*rxy
      huxy(1,1)=(9.*hu(1,1)+16.*hu(2,2)+hu(3,3)-12.*(hu(1,2)+hu(2,1))+3.
     &*(hu(1,3)+hu(3,1))-4.*(hu(2,3)+hu(3,2)))*rxy
      huxy(1,ny)=-(9.*hu(1,ny)+16.*hu(2,n1)+hu(3,n2)-12.*(hu(1,n1)+hu(2,
     &ny))+3.*(hu(1,n2)+hu(3,ny))-4.*(hu(2,n2)+hu(3,n1)))*rxy
      huxy(mx,1)=-(9.*hu(mx,1)+16.*hu(m1,2)+hu(m2,3)-12.*(hu(mx,2)+hu(m1
     &,1))+3.*(hu(mx,3)+hu(m2,1))-4.*(hu(m1,3)+hu(m2,2)))*rxy
      huxy(mx,ny)=(9.*hu(mx,ny)+16.*hu(m1,n1)+hu(m2,n2)-12.*(hu(mx,n1)+h
     &u(m1,ny))+3.*(hu(mx,n2)+hu(m2,ny))-4.*(hu(m1,n2)+hu(m2,n1)))*rxy
      v_xy(1,1)=(9.*vn(1,1)+16.*vn(2,2)+vn(3,3)-12.*(vn(1,2)+vn(2,1))+3.
     &*(vn(1,3)+vn(3,1))-4.*(vn(2,3)+vn(3,2)))*rxy
      v_xy(1,ny)=-(9.*vn(1,ny)+16.*vn(2,n1)+vn(3,n2)-12.*(vn(1,n1)+vn(2,
     &ny))+3.*(vn(1,n2)+vn(3,ny))-4.*(vn(2,n2)+vn(3,n1)))*rxy
      v_xy(mx,1)=-(9.*vn(mx,1)+16.*vn(m1,2)+vn(m2,3)-12.*(vn(mx,2)+vn(m1
     &,1))+3.*(vn(mx,3)+vn(m2,1))-4.*(vn(m1,3)+vn(m2,2)))*rxy
      v_xy(mx,ny)=(9.*vn(mx,ny)+16.*vn(m1,n1)+vn(m2,n2)-12.*(vn(mx,n1)+v
     &n(m1,ny))+3.*(vn(mx,n2)+vn(m2,ny))-4.*(vn(m1,n2)+vn(m2,n1)))*rxy
      hvxy(1,1)=(9.*hv(1,1)+16.*hv(2,2)+hv(3,3)-12.*(hv(1,2)+hv(2,1))+3.
     &*(hv(1,3)+hv(3,1))-4.*(hv(2,3)+hv(3,2)))*rxy
      hvxy(1,ny)=-(9.*hv(1,ny)+16.*hv(2,n1)+hv(3,n2)-12.*(hv(1,n1)+hv(2,
     &ny))+3.*(hv(1,n2)+hv(3,ny))-4.*(hv(2,n2)+hv(3,n1)))*rxy
      hvxy(mx,1)=-(9.*hv(mx,1)+16.*hv(m1,2)+hv(m2,3)-12.*(hv(mx,2)+hv(m1
     &,1))+3.*(hv(mx,3)+hv(m2,1))-4.*(hv(m1,3)+hv(m2,2)))*rxy
      hvxy(mx,ny)=(9.*hv(mx,ny)+16.*hv(m1,n1)+hv(m2,n2)-12.*(hv(mx,n1)+h
     &v(m1,ny))+3.*(hv(mx,n2)+hv(m2,ny))-4.*(hv(m1,n2)+hv(m2,n1)))*rxy
      
      do j=2,n1
      u_xy(1,j)=(-3.*(un(1,j+1)-un(1,j-1))+4.*(un(2,j+1)-un(2,j-1))-(un(
     &3,j+1)-un(3,j-1)))*rxy
      u_xy(mx,j)=-(-3.*(un(mx,j+1)-un(mx,j-1))+4.*(un(m1,j+1)-un(m1,j-1)
     &)-(un(m2,j+1)-un(m2,j-1)))*rxy
      huxy(1,j)=(-3.*(hu(1,j+1)-hu(1,j-1))+4.*(hu(2,j+1)-hu(2,j-1))-(hu(
     &3,j+1)-hu(3,j-1)))*rxy
      huxy(mx,j)=-(-3.*(hu(mx,j+1)-hu(mx,j-1))+4.*(hu(m1,j+1)-hu(m1,j-1)
     &)-(hu(m2,j+1)-hu(m2,j-1)))*rxy
      v_xy(1,j)=(-3.*(vn(1,j+1)-vn(1,j-1))+4.*(vn(2,j+1)-vn(2,j-1))-(vn(
     &3,j+1)-vn(3,j-1)))*rxy
      v_xy(mx,j)=-(-3.*(vn(mx,j+1)-vn(mx,j-1))+4.*(vn(m1,j+1)-vn(m1,j-1)
     &)-(vn(m2,j+1)-vn(m2,j-1)))*rxy
      hvxy(1,j)=(-3.*(hv(1,j+1)-hv(1,j-1))+4.*(hv(2,j+1)-hv(2,j-1))-(hv(
     &3,j+1)-hv(3,j-1)))*rxy
      hvxy(mx,j)=-(-3.*(hv(mx,j+1)-hv(mx,j-1))+4.*(hv(m1,j+1)-hv(m1,j-1)
     &)-(hv(m2,j+1)-hv(m2,j-1)))*rxy
      enddo
      
      do i=2,m1
      u_xy(i,1)=(-3.*(un(i+1,1)-un(i-1,1))+4.*(un(i+1,2)-un(i-1,2))-(un(
     &i+1,3)-un(i-1,3)))*rxy
      u_xy(i,ny)=-(-3.*(un(i+1,ny)-un(i-1,ny))+4.*(un(i+1,n1)-un(i-1,n1)
     &)-(un(i+1,n2)-un(i-1,n2)))*rxy
      huxy(i,1)=(-3.*(hu(i+1,1)-hu(i-1,1))+4.*(hu(i+1,2)-hu(i-1,2))-(hu(
     &i+1,3)-hu(i-1,3)))*rxy
      huxy(i,ny)=-(-3.*(hu(i+1,ny)-hu(i-1,ny))+4.*(hu(i+1,n1)-hu(i-1,n1)
     &)-(hu(i+1,n2)-hu(i-1,n2)))*rxy
      v_xy(i,1)=(-3.*(vn(i+1,1)-vn(i-1,1))+4.*(vn(i+1,2)-vn(i-1,2))-(vn(
     &i+1,3)-vn(i-1,3)))*rxy
      v_xy(i,ny)=-(-3.*(vn(i+1,ny)-vn(i-1,ny))+4.*(vn(i+1,n1)-vn(i-1,n1)
     &)-(vn(i+1,n2)-vn(i-1,n2)))*rxy
      hvxy(i,1)=(-3.*(hv(i+1,1)-hv(i-1,1))+4.*(hv(i+1,2)-hv(i-1,2))-(hv(
     &i+1,3)-hv(i-1,3)))*rxy
      hvxy(i,ny)=-(-3.*(hv(i+1,ny)-hv(i-1,ny))+4.*(hv(i+1,n1)-hv(i-1,n1)
     &)-(hv(i+1,n2)-hv(i-1,n2)))*rxy
      enddo
      
      do i=2,m1
      do j=2,n1
      u_xy(i,j)=(un(i+1,j+1)+un(i-1,j-1)-un(i+1,j-1)-un(i-1,j+1))*rxy
      huxy(i,j)=(hu(i+1,j+1)+hu(i-1,j-1)-hu(i+1,j-1)-hu(i-1,j+1))*rxy
      v_xy(i,j)=(vn(i+1,j+1)+vn(i-1,j-1)-vn(i+1,j-1)-vn(i-1,j+1))*rxy
      hvxy(i,j)=(hv(i+1,j+1)+hv(i-1,j-1)-hv(i+1,j-1)-hv(i-1,j+1))*rxy
      enddo
      enddo
      
      do i=2,m1
      do j=2,n1
      u_xx(i,j)=(un(i+1,j)-2.*un(i,j)+un(i-1,j))*rx2
      v_xx(i,j)=(vn(i+1,j)-2.*vn(i,j)+vn(i-1,j))*rx2
      u_yy(i,j)=(un(i,j+1)-2.*un(i,j)+un(i,j-1))*ry2
      v_yy(i,j)=(vn(i,j+1)-2.*vn(i,j)+vn(i,j-1))*ry2
      enddo
      enddo
      
      do j=1,ny
      p_x(1,j)=(2.*un(1,j)-5.*un(2,j)+4.*un(3,j)-un(4,j))*rx2+v_xy(1,j)
      p_x(mx,j)=(2.*un(mx,j)-5.*un(m1,j)+4.*un(m2,j)-un(m3,j))*rx2+
     &v_xy(mx,j)
      hpx(1,j)=(2.*hu(1,j)-5.*hu(2,j)+4.*hu(3,j)-hu(4,j))*rx2+hvxy(1,j)
      hpx(mx,j)=(2.*hu(mx,j)-5.*hu(m1,j)+4.*hu(m2,j)-hu(m3,j))*rx2+
     &hvxy(mx,j)
      do i=2,m1
      p_x(i,j)=(un(i+1,j)-2.*un(i,j)+un(i-1,j))*rx2+v_xy(i,j)
      hpx(i,j)=(hu(i+1,j)-2.*hu(i,j)+hu(i-1,j))*rx2+hvxy(i,j)
      enddo
      enddo
      
      do i=1,mx
      p_y(i,1)=(2.*vn(i,1)-5.*vn(i,2)+4.*vn(i,3)-vn(i,4))*ry2+u_xy(i,1)
      p_y(i,ny)=(2.*vn(i,ny)-5.*vn(i,n1)+4.*vn(i,n2)-vn(i,n3))*ry2+
     &u_xy(i,ny)
      hpy(i,1)=(2.*hv(i,1)-5.*hv(i,2)+4.*hv(i,3)-hv(i,4))*ry2+huxy(i,1)
      hpy(i,ny)=(2.*hv(i,ny)-5.*hv(i,n1)+4.*hv(i,n2)-hv(i,n3))*ry2+
     &huxy(i,ny)
      do j=2,n1
      p_y(i,j)=(vn(i,j+1)-2.*vn(i,j)+vn(i,j-1))*ry2+u_xy(i,j)
      hpy(i,j)=(hv(i,j+1)-2.*hv(i,j)+hv(i,j-1))*ry2+huxy(i,j)
      enddo
      enddo

      return
      end

c  ###########################################################

      subroutine eval_e2

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      do i=1,mx
      do j=1,ny
      eh1=etn(i,j)*(a1*h(i,j)**2+(h(i,j)*h(i,j)-etn(i,j)*etn(i,j))/6.)
      eh2=etn(i,j)*(a2*h(i,j)-0.5*(h(i,j)+etn(i,j)))
      epx(i,j)=eh1*p_x(i,j)+eh2*hpx(i,j)
      eqy(i,j)=eh1*p_y(i,j)+eh2*hpy(i,j)
      enddo
      enddo
      e2(1,1)=-(-3.*epx(1,1)+4.*epx(2,1)-epx(3,1))*rdx-(-3.*eqy(1,1)+4.*
     &eqy(1,2)-eqy(1,3))*rdy
      e2(1,ny)=-(-3.*epx(1,ny)+4.*epx(2,ny)-epx(3,ny))*rdx+(-3.*eqy(1,ny
     &)+4.*eqy(1,n1)-eqy(1,n2))*rdy
      e2(mx,1)=(-3.*epx(mx,1)+4.*epx(m1,1)-epx(m2,1))*rdx-(-3.*eqy(mx,1)
     &+4.*eqy(mx,2)-eqy(mx,3))*rdy
      e2(mx,ny)=(-3.*epx(mx,ny)+4.*epx(m1,ny)-epx(m2,ny))*rdx+(-3.*eqy(m
     &x,ny)+4.*eqy(mx,n1)-eqy(mx,n2))*rdy
      do j=2,n1
      e2(1,j)=-(-3.*epx(1,j)+4.*epx(2,j)-epx(3,j))*rdx-(eqy(1,j+1)-eqy(1
     &,j-1))*rdy
      e2(mx,j)=(-3.*epx(mx,j)+4.*epx(m1,j)-epx(m2,j))*rdx-(eqy(mx,j+1)-e
     &qy(mx,j-1))*rdy
      enddo
      do i=2,m1
      e2(i,1)=-(epx(i+1,1)-epx(i-1,1))*rdx-(-3.*eqy(i,1)+4.*eqy(i,2)-eqy
     &(i,3))*rdy
      e2(i,ny)=-(epx(i+1,ny)-epx(i-1,ny))*rdx+(-3.*eqy(i,ny)+4.*eqy(i,n1
     &)-eqy(i,n2))*rdy
      enddo
      do i=2,m1
      do j=2,n1
      e2(i,j)=-(epx(i+1,j)-epx(i-1,j))*rdx-(eqy(i,j+1)-eqy(i,j-1))*rdy
      enddo
      enddo

      return
      end

c  ###########################################################

      subroutine eval_f2g2

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      fg(1,1)=(za(1,1)-etn(1,1))*(.5*(za(1,1)+etn(1,1))*(un(1,1)*p_x(1,1
     &)+vn(1,1)*p_y(1,1))+un(1,1)*hpx(1,1)+vn(1,1)*hpy(1,1))+.5*(etn(1,1
     &)*((-3.*un(1,1)+4.*un(2,1)-un(3,1))*rdx+(-3.*vn(1,1)+4.*vn(1,2)-vn
     &(1,3))*rdy)+(-3.*hu(1,1)+4.*hu(2,1)-hu(3,1))*rdx+(-3.*hv(1,1)+4.*h
     &v(1,2)-hv(1,3))*rdy)**2
      fg(mx,1)=(za(mx,1)-etn(mx,1))*(.5*(za(mx,1)+etn(mx,1))*(un(mx,1)*p
     &_x(mx,1)+vn(mx,1)*p_y(mx,1))+un(mx,1)*hpx(mx,1)+vn(mx,1)*hpy(mx,1)
     &)+.5*(etn(mx,1)*(-(-3.*un(mx,1)+4.*un(m1,1)-un(m2,1))*rdx+(-3.*vn(
     &mx,1)+4.*vn(mx,2)-vn(mx,3))*rdy)-(-3.*hu(mx,1)+4.*hu(m1,1)-hu(m2,1
     &))*rdx+(-3.*hv(mx,1)+4.*hv(mx,2)-hv(mx,3))*rdy)**2
      fg(1,ny)=(za(1,ny)-etn(1,ny))*(.5*(za(1,ny)+etn(1,ny))*(un(1,ny)*p
     &_x(1,ny)+vn(1,ny)*p_y(1,ny))+un(1,ny)*hpx(1,ny)+vn(1,ny)*hpy(1,ny)
     &)+.5*(etn(1,ny)*((-3.*un(1,ny)+4.*un(2,ny)-un(3,ny))*rdx-(-3.*vn(1
     &,ny)+4.*vn(1,n1)-vn(1,n2))*rdy)+(-3.*hu(1,ny)+4.*hu(2,ny)-hu(3,ny)
     &)*rdx-(-3.*hv(1,ny)+4.*hv(1,n1)-hv(1,n2))*rdy)**2
      fg(mx,ny)=(za(mx,ny)-etn(mx,ny))*(.5*(za(mx,ny)+etn(mx,ny))*(un(mx
     &,ny)*p_x(mx,ny)+vn(mx,ny)*p_y(mx,ny))+un(mx,ny)*hpx(mx,ny)+vn(mx,n
     &y)*hpy(mx,ny))+.5*(etn(mx,ny)*(-(-3.*un(mx,ny)+4.*un(m1,ny)-un(m2,
     &ny))*rdx-(-3.*vn(mx,ny)+4.*vn(mx,n1)-vn(mx,n2))*rdy)-(-3.*hu(mx,ny
     &)+4.*hu(m1,ny)-hu(m2,ny))*rdx-(-3.*hv(mx,ny)+4.*hv(mx,n1)-hv(mx,n2
     &))*rdy)**2
      do j=2,n1
      fg(1,j)=(za(1,j)-etn(1,j))*(.5*(za(1,j)+etn(1,j))*(un(1,j)*p_x(1,j
     &)+vn(1,j)*p_y(1,j))+un(1,j)*hpx(1,j)+vn(1,j)*hpy(1,j))+.5*(etn(1,j
     &)*((-3.*un(1,j)+4.*un(2,j)-un(3,j))*rdx+(vn(1,j+1)-vn(1,j-1))*rdy)
     &+(-3.*hu(1,j)+4.*hu(2,j)-hu(3,j))*rdx+(hv(1,j+1)-hv(1,j-1))*rdy)**
     &2
      fg(mx,j)=(za(mx,j)-etn(mx,j))*(.5*(za(mx,j)+etn(mx,j))*(un(mx,j)*p
     &_x(mx,j)+vn(mx,j)*p_y(mx,j))+un(mx,j)*hpx(mx,j)+vn(mx,j)*hpy(mx,j)
     &)+.5*(etn(mx,j)*(-(-3.*un(mx,j)+4.*un(m1,j)-un(m2,j))*rdx+(vn(mx,j
     &+1)-vn(mx,j-1))*rdy)-(-3.*hu(mx,j)+4.*hu(m1,j)-hu(m2,j))*rdx+(hv(m
     &x,j+1)-hv(mx,j-1))*rdy)**2
      enddo
      do i=2,m1
      fg(i,1)=(za(i,1)-etn(i,1))*(.5*(za(i,1)+etn(i,1))*(un(i,1)*p_x(i,1
     &)+vn(i,1)*p_y(i,1))+un(i,1)*hpx(i,1)+vn(i,1)*hpy(i,1))+.5*(etn(i,1
     &)*(+(un(i+1,1)-un(i-1,1))*rdx+(-3.*vn(i,1)+4.*vn(i,2)-vn(i,3))*rdy
     &)+(hu(i+1,1)-hu(i-1,1))*rdx+(-3.*hv(i,1)+4.*hv(i,2)-hv(i,3))*rdy)*
     &*2
      fg(i,ny)=(za(i,ny)-etn(i,ny))*(.5*(za(i,ny)+etn(i,ny))*(un(i,ny)*p
     &_x(i,ny)+vn(i,ny)*p_y(i,ny))+un(i,ny)*hpx(i,ny)+vn(i,ny)*hpy(i,ny)
     &)+.5*(etn(i,ny)*(+(un(i+1,ny)-un(i-1,ny))*rdx-(-3.*vn(i,ny)+4.*vn(
     &i,n1)-vn(i,n2))*rdy)+(hu(i+1,ny)-hu(i-1,ny))*rdx-(-3.*hv(i,ny)+4.*
     &hv(i,n1)-hv(i,n2))*rdy)**2
      enddo
      do i=2,m1
      do j=2,n1
      fg(i,j)=(za(i,j)-etn(i,j))*(.5*(za(i,j)+etn(i,j))*(un(i,j)*p_x(i,j
     &)+vn(i,j)*p_y(i,j))+un(i,j)*hpx(i,j)+vn(i,j)*hpy(i,j))+.5*(etn(i,j
     &)*(+(un(i+1,j)-un(i-1,j))*rdx+(vn(i,j+1)-vn(i,j-1))*rdy)+(hu(i+1,j
     &)-hu(i-1,j))*rdx+(hv(i,j+1)-hv(i,j-1))*rdy)**2
      enddo
      enddo
      do j=1,ny
      f2(1,j)=-(-3.*fg(1,j)+4.*fg(2,j)-fg(3,j))*rdx
      f2(mx,j)=(-3.*fg(mx,j)+4.*fg(m1,j)-fg(m2,j))*rdx
      do i=2,m1
      f2(i,j)=-(fg(i+1,j)-fg(i-1,j))*rdx
      enddo
      enddo
      do i=1,mx
      g2(i,1)=-(-3.*fg(i,1)+4.*fg(i,2)-fg(i,3))*rdy
      g2(i,ny)=(-3.*fg(i,ny)+4.*fg(i,n1)-fg(i,n2))*rdy
      do j=2,n1
      g2(i,j)=-(fg(i,j+1)-fg(i,j-1))*rdy
      enddo
      enddo

      return
      end

c  ###########################################################
      
      subroutine eval_utvt

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      if(idpc.EQ.1)then
      do i=1,mx
      do j=1,ny
      u_t(2,i,j)=td3(1)*u(2,i,j)+td3(2)*u(3,i,j)+td3(3)*u(4,i,j)
      u_t(3,i,j)=td3(3)*(u(2,i,j)-u(4,i,j))
      u_t(4,i,j)=-(td3(3)*u(2,i,j)+td3(2)*u(3,i,j)+td3(1)*u(4,i,j))
      v_t(2,i,j)=td3(1)*v(2,i,j)+td3(2)*v(3,i,j)+td3(3)*v(4,i,j)
      v_t(3,i,j)=td3(3)*(v(2,i,j)-v(4,i,j))
      v_t(4,i,j)=-(td3(3)*v(2,i,j)+td3(2)*v(3,i,j)+td3(1)*v(4,i,j))
      enddo
      enddo
      call eval_ftgt
      elseif(idpc.EQ.2)then
      
      do i=1,mx
      do j=1,ny
      u_t(1,i,j)=td4(1)*u(1,i,j)+td4(2)*u(2,i,j)+td4(3)*u(3,i,j)+td4(4)*
     &u(4,i,j)
      u_t(2,i,j)=td4(5)*u(1,i,j)+td4(6)*u(2,i,j)+td4(7)*u(3,i,j)+td4(8)*
     &u(4,i,j)
      u_t(3,i,j)=-(td4(8)*u(1,i,j)+td4(7)*u(2,i,j)+td4(6)*u(3,i,j)+
     &td4(5)*u(4,i,j))
      u_t(4,i,j)=-(td4(4)*u(1,i,j)+td4(3)*u(2,i,j)+td4(2)*u(3,i,j)+
     &td4(1)*u(4,i,j))
      v_t(1,i,j)=td4(1)*v(1,i,j)+td4(2)*v(2,i,j)+td4(3)*v(3,i,j)+td4(4)*
     &v(4,i,j)
      v_t(2,i,j)=td4(5)*v(1,i,j)+td4(6)*v(2,i,j)+td4(7)*v(3,i,j)+td4(8)*
     &v(4,i,j)
      v_t(3,i,j)=-(td4(8)*v(1,i,j)+td4(7)*v(2,i,j)+td4(6)*v(3,i,j)+
     &td4(5)*v(4,i,j))
      v_t(4,i,j)=-(td4(4)*v(1,i,j)+td4(3)*v(2,i,j)+td4(2)*v(3,i,j)+
     &td4(1)*v(4,i,j))
      enddo
      enddo
      call eval_ftgt
      endif

      return
      end

c  ###########################################################
      
      subroutine eval_ftgt

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper

      
      do k=(3-idpc),4
      
      fgt(1,1)=et(k,1,1)*(.5*et(k,1,1)*(rdx*(-3.*u_t(k,1,1)+4.*u_t(k,2,1
     &)-u_t(k,3,1))+rdy*(-3.*v_t(k,1,1)+4.*v_t(k,1,2)-v_t(k,1,3)))+rdx*(
     &-3.*h(1,1)*u_t(k,1,1)+4.*h(2,1)*u_t(k,2,1)-h(3,1)*u_t(k,3,1))+rdy*
     &(-3.*h(1,1)*v_t(k,1,1)+4.*h(1,2)*v_t(k,1,2)-h(1,3)*v_t(k,1,3)))
      fgt(1,ny)=et(k,1,ny)*(.5*et(k,1,ny)*(rdx*(-3.*u_t(k,1,ny)+4.*u_t(k
     &,2,ny)-u_t(k,3,ny))-rdy*(-3.*v_t(k,1,ny)+4.*v_t(k,1,n1)-v_t(k,1,n2
     &)))+rdx*(-3.*h(1,ny)*u_t(k,1,ny)+4.*h(2,ny)*u_t(k,2,ny)-h(3,ny)*u_
     &t(k,3,ny))-rdy*(-3.*h(1,ny)*v_t(k,1,ny)+4.*h(1,n1)*v_t(k,1,n1)-h(1
     &,n2)*v_t(k,1,n2)))
      fgt(mx,1)=et(k,mx,1)*(.5*et(k,mx,1)*(-rdx*(-3.*u_t(k,mx,1)+4.*u_t(
     &k,m1,1)-u_t(k,m2,1))+rdy*(-3.*v_t(k,mx,1)+4.*v_t(k,mx,2)-v_t(k,mx,
     &3)))-rdx*(-3.*h(mx,1)*u_t(k,mx,1)+4.*h(m1,1)*u_t(k,m1,1)-h(m2,1)*u
     &_t(k,m2,1))+rdy*(-3.*h(mx,1)*v_t(k,mx,1)+4.*h(mx,2)*v_t(k,mx,2)-h(
     &mx,3)*v_t(k,mx,3)))
      fgt(mx,ny)=et(k,mx,ny)*(.5*et(k,mx,ny)*(-rdx*(-3.*u_t(k,mx,ny)+4.*
     &u_t(k,m1,ny)-u_t(k,m2,ny))-rdy*(-3.*v_t(k,mx,ny)+4.*v_t(k,mx,n1)-v
     &_t(k,mx,n2)))-rdx*(-3.*h(mx,ny)*u_t(k,mx,ny)+4.*h(m1,ny)*u_t(k,m1,
     &ny)-h(m2,ny)*u_t(k,m2,ny))-rdy*(-3.*h(mx,ny)*v_t(k,mx,ny)+4.*h(mx,
     &n1)*v_t(k,mx,n1)-h(mx,n2)*v_t(k,mx,n2)))
      do j=2,n1
      fgt(1,j)=et(k,1,j)*(.5*et(k,1,j)*(rdx*(-3.*u_t(k,1,j)+4.*u_t(k,2,j
     &)-u_t(k,3,j))+rdy*(v_t(k,1,j+1)-v_t(k,1,j-1)))+rdx*(-3.*h(1,j)*u_t
     &(k,1,j)+4.*h(2,j)*u_t(k,2,j)-h(3,j)*u_t(k,3,j))+rdy*(h(1,j+1)*v_t(
     &k,1,j+1)-h(1,j-1)*v_t(k,1,j-1)))
      fgt(mx,j)=et(k,mx,j)*(.5*et(k,mx,j)*(-rdx*(-3.*u_t(k,mx,j)+4.*u_t(
     &k,m1,j)-u_t(k,m2,j))+rdy*(v_t(k,mx,j+1)-v_t(k,mx,j-1)))-rdx*(-3.*h
     &(mx,j)*u_t(k,mx,j)+4.*h(m1,j)*u_t(k,m1,j)-h(m2,j)*u_t(k,m2,j))+rdy
     &*(h(mx,j+1)*v_t(k,mx,j+1)-h(mx,j-1)*v_t(k,mx,j-1)))
      enddo
      do i=2,m1
      fgt(i,1)=et(k,i,1)*(.5*et(k,i,1)*(rdx*(u_t(k,i+1,1)-u_t(k,i-1,1))+
     &rdy*(-3.*v_t(k,i,1)+4.*v_t(k,i,2)-v_t(k,i,3)))+rdx*(h(i+1,1)*u_t(k
     &,i+1,1)-h(i-1,1)*u_t(k,i-1,1))+rdy*(-3.*h(i,1)*v_t(k,i,1)+4.*h(i,2
     &)*v_t(k,i,2)-h(i,3)*v_t(k,i,3)))
      fgt(i,ny)=et(k,i,ny)*(.5*et(k,i,ny)*(rdx*(u_t(k,i+1,ny)-u_t(k,i-1,
     &ny))-rdy*(-3.*v_t(k,i,ny)+4.*v_t(k,i,n1)-v_t(k,i,n2)))+rdx*(h(i+1,
     &ny)*u_t(k,i+1,ny)-h(i-1,ny)*u_t(k,i-1,ny))-rdy*(-3.*h(i,ny)*v_t(k,
     &i,ny)+4.*h(i,n1)*v_t(k,i,n1)-h(i,n2)*v_t(k,i,n2)))
      enddo
      do i=2,m1
      do j=2,n1
      fgt(i,j)=et(k,i,j)*(.5*et(k,i,j)*(rdx*(u_t(k,i+1,j)-u_t(k,i-1,j))+
     &rdy*(v_t(k,i,j+1)-v_t(k,i,j-1)))+rdx*(h(i+1,j)*u_t(k,i+1,j)-h(i-1,
     &j)*u_t(k,i-1,j))+rdy*(h(i,j+1)*v_t(k,i,j+1)-h(i,j-1)*v_t(k,i,j-1))
     &)
      enddo
      enddo
      do j=1,ny
      ft(k,1,j)=(-3.*fgt(1,j)+4.*fgt(2,j)-fgt(3,j))*rdx
      ft(k,mx,j)=-(-3.*fgt(mx,j)+4.*fgt(m1,j)-fgt(m2,j))*rdx
      do i=2,m1
      ft(k,i,j)=(fgt(i+1,j)-fgt(i-1,j))*rdx
      enddo
      enddo
      do i=1,mx
      gt(k,i,1)=(-3.*fgt(i,1)+4.*fgt(i,2)-fgt(i,3))*rdy
      gt(k,i,ny)=-(-3.*fgt(i,ny)+4.*fgt(i,n1)-fgt(i,n2))*rdy
      do j=2,n1
      gt(k,i,j)=(fgt(i,j+1)-fgt(i,j-1))*rdy
      enddo
      enddo
      enddo

      return
      end

c  ###########################################################

      subroutine surfgrid

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper


      character fname*30
      real xy1(iq,jq)
      
      igrid=igrid+1
      eps=1.0e-4

      if((igrid.lt.1000).and.(it.lt.itmov)) then

      itmp1 = mod(igrid,1000)/100
      itmp2 = mod(igrid,100)/10
      itmp3 = mod(igrid,10)

      fname='Movie/'//char(itmp1+48)//char(itmp2+48)//
     &char(itmp3+48)//output

      do i=1,mx
      do j=1,ny
      dep=etn(i,j)+hs(i,j)
      if(dep.lt.0.0) then
      xy1(i,j)=zlim
      else
      xy1(i,j)=etn(i,j)
      endif
      enddo
      enddo

      open (25,file=fname)
      call gridwriter(xy1)
      close (25)

      endif

      return
      end

c  ###########################################################

      subroutine gridwriter(xy1)

      
      implicit real(a-h,o-z)
      implicit integer(i-n)
      parameter(iq=800,jq=800,nq=8001,ngm=30)
      common/input1/a0,h0,tpd,cbkv,delta,slmda,
     &itbgn,itend,itscr,itftr,isltb,islte,imch
      common/input2/cspg,cspg2,cspg3,cbrk,ck_bt,c_dm,
     &ixg(ngm),iyg(ngm),ixl(ngm),iyl(ngm),idout,idft,ispg(4)
      common/input3/f1n,f2n,f3n,f4n,f5n,f6n,f7n
      common/const1/rdx,rdy,rdt,rx2,ry2,rxy,a1,a2,b1,b2,ga,pi
      common/const2/m1,m2,m3,m4,m5,m6,n1,n2,n3,n4,n5,n6,idpc,irdt
      common/const3/p(7),q(7),ta(7),cb(4),td3(8),td4(8)
      common/const5/cf1(5),cf2(5),cf3(5),cf4(5)
      common/depths/h(iq,jq),za(iq,jq),w1(iq,jq),w2(iq,jq),w3(iq,jq)
      common/inits/etn(iq,jq),un(iq,jq),vn(iq,jq)
      common/solus/ete(iq,jq),ue(iq,jq),ve(iq,jq)
      common/solu2/eto(iq,jq),uo(iq,jq),vo(iq,jq)
      common/solu3/et(4,iq,jq),u(4,iq,jq),v(4,iq,jq)
      common/init3/e(4,iq,jq),f(4,iq,jq),g(4,iq,jq)
      common/init4/f1(4,iq,jq),g1(4,iq,jq)
      common/init5/f2(iq,jq),g2(iq,jq),e2(iq,jq)
      common/arrays/hu(iq,jq),hv(iq,jq),he(iq,jq),fb(iq,jq)
      common/array2/u_xy(iq,jq),v_xy(iq,jq),huxy(iq,jq),hvxy(iq,jq)
      common/array3/p_x(iq,jq),hpx(iq,jq),p_y(iq,jq),hpy(iq,jq)
      common/array4/heu_x(iq,jq),hev_y(iq,jq),epx(iq,jq),eqy(iq,jq)
      common/array5/eta_x(iq,jq),eta_y(iq,jq),fgt(iq,jq),fg(iq,jq),
     &u_t(4,iq,jq),v_t(4,iq,jq),ft(4,iq,jq),gt(4,iq,jq),u_x(iq,jq),
     &u_y(iq,jq),v_x(iq,jq),v_y(iq,jq),u_xx(iq,jq),u_yy(iq,jq),
     &v_xx(iq,jq),v_yy(iq,jq),fbr(iq,jq),gbr(iq,jq)
      common/solu4/t,twrp,cph,omg,alpha,alpha1
      common/solu5/it,ite,itbrk,idiff(iq,jq),igrid,ivel
      common/solu7/edvis(iq,jq),zdiff(iq,jq),xlag(ngm),ylag(ngm)
      common/ludecs/a(2,iq,jq),b(2,iq,jq),c(2,iq,jq),d(2,iq,jq)
      common/ludec2/adx(iq,jq),bdx(iq,jq),ady(iq,jq),bdy(iq,jq)
      common/errors/etem,uem,vem,err_sum(6)
      common/etaout/uvm(nq),flux(nq)
      common/coords/x(iq),y(jq),xp(iq),yp(jq),xrot,zrot,ixorigin,
     &iyorigin,ixsize,iysize,igvnewx,igvnewy,igvoldx,igvoldy
      common/sgips/etts(iq,jq,3),ctb(iq,jq),hcr(iq,jq),
     &cospg(iq,jq),um(iq,jq),vm(iq,jq),em(iq,jq),clnr,
     &z0,dlamda,dlts,ass,islt,idslot,imass,itwv,ieddy,idsf,ictrl,
     &iaver,ixdel,jydel,itmn,mxdl,nydl,istl(jq),istlm,itb(iq,jq)
      common/grids/wvht(iq,jq),etmin(iq,jq),etmax(iq,jq),
     &tmax(iq,jq),twet(iq,jq),tdry(iq,jq),velmx(iq,jq),velang(iq,jq),
     &flxmx(iq,jq),flxang(iq,jq),wetlnd(iq,jq),tmin(iq,jq),
     &brtim(iq,jq),brvis(iq,jq)
      common/surfer/etamin,etamax
c
      common/geowv4/hs(iq,jq),per(10),xlm(10),dsrc(10),tinit(10)
      common/geowv5/dpmax,dpmin,timeo,tout1,tout2,zlim
      common/geowv6/ifail(10),mx,ny,msrc,nt,itdel,itmov,ibe
      common/geowv7/ngage,nlag,ingm,itype,f8n,f9n
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
      real*8    xlag,ylag,dx,dy,dt,xw,xe,ys,yn,xinc,yinc
      character f1n*30,f2n*30,f3n*30,f4n*30,f5n*30,f6n*30
      character f7n*30,f8n*30,f9n*30,output*4,proper


      real xy1(iq,jq)

      tmpmin=1.0e+10
      tmpmax=-1.0e+10
      do j=1,ny
      do i=1,mx
      if (xy1(i,j).gt.tmpmax) tmpmax=xy1(i,j)
      if (xy1(i,j).lt.tmpmin) tmpmin=xy1(i,j) 
      enddo
      enddo

      if (itype.eq.0) then
      write(25,210)
      write(25,211) mx,ny
      write(25,212) xw,xe
      write(25,212) ys,yn
      write(25,212) tmpmin,tmpmax
      do j = 1, ny
      write(25,213) (xy1(i,j),i=1,mx)
      enddo
      endif

      write(25,*)
      write(25,*)
      write(25,*) timeo, '  initial time of the simulation'
      write(25,*) t, '  absolute time of grid writing'
      write(25,*) dt, '  time step of grid writing'
      write(25,*) it, '  number of time steps simulated'
      write(25,*) nt, '  total simulation number of time steps'
      write(25,*) itdel, '  time steps between movie grid files'
      write(25,*) itmov, '  maximum time step for movie grid files'
      write(25,*) zlim, '  grid file value of blank nodes'
      write(25,*) tout1, '  time when output grids begin working'
      write(25,*) tout2, '  time when output grids end working'
      write(25,*) f1n, '  bathymetry filename with depth in meters'
      write(25,*) f2n, '  initial condition filename of free surface'
      write(25,*) f3n, '  filename describing simulation details'

  210 format('DSAA')
  211 format(2i20)
  212 format(2f20.6)
  213 format(4f15.6)

      return
      end

c//////////////////////////////////////////////////////////////////
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c//////////////////////////////////////////////////////////////////
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

c  (c) Applied Fluids Engineering, Inc. (2000-2009)
c  Written by Dr. Philip Watts in December, 2000.
c  Modified by Dr. Philip Watts in November, 2001.
c  Modified by Dr. Philip Watts in November, 2002.
c  Modified by Dr. Philip Watts in August, 2006.
c  Modified by Dr. Philip Watts in August, 2009.
c  Version 1.2 of the software package TOPICS:
c  Tsunami Open & Progressive Initial Conditions System

      subroutine topics
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  1.1 Program header.
c
      write(*,*)
      write(*,*) 'Tsunami Open & Progressive Initial Conditions System'
      write(*,*)
      write(*,*) '           Welcome to TOPICS, Version 1.2'
      write(*,*)
      write(*,*) '(c) 2000-2009, Applied Fluids Engineering, Inc.'
      write(*,*)
      write(*,*)
      write(*,*) '           Program errors are indicated by:'
      write(*,*) '           xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      write(*,*)
      write(*,*) '           Program warnings are indicated by:'
      write(*,*) '           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(*,*)
      write(*,*)
      write(*,*) 'TOPICS gives sufficient latitude to make mistakes'
      write(*,*)
      write(*,*) 'TOPICS uses SI units (m, s, kg) exclusively'
      write(*,*) 'TOPICS expects almost all entries to be positive'
      write(*,*) 'TOPICS will make almost all entries positive'
      write(*,*)
c
c  1.2 Assume that failures are on earth and water is sea water.
c
      write(*,*) 'Tsunamis are assumed on earth and in sea water'
      write(*,*)
      pi = 4.0D+00 * datan(1.0D+00)
      g = 9.81D+00
      rhoo = 1.025D+03
c
c  1.3 Choose the use for which you would like to run TOPICS.
c
c      write(*,*)    
c      write(*,*) 'Type 0 for characteristic tsunami amplitudes'
c      write(*,*) 'Type 1 for gridded tsunami initial conditions'
c      read(*,*) itsu
c      write(*,*)
      itsu = 1
c
c  1.4 Check the entry and quit the program if requested.
c
      if ((itsu.ne.0).and.(itsu.ne.1)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  1.5 Run the subroutines needed to calculate characteristic amplitudes.   
c
      if (itsu.eq.0) then
        call choice 
        if (fail.eq.0) call seismic
        if (fail.eq.1) call slide
        if (fail.eq.2) call slump
        if (fail.eq.3) call splash
        if (fail.eq.4) call pyro
        if (fail.eq.0) call charquake
        if (fail.eq.1) call charfail
        if (fail.eq.2) call charfail
        if (fail.eq.3) call charcoast
        if (fail.eq.4) call charflow
      endif
c
c  1.6 Run the subroutines needed to calculate initial conditions.   
c
      if (itsu.eq.1) then
        call choice
        call gridfile
        if (fail.eq.0) call setquake
        if (fail.eq.1) call setfail
        if (fail.eq.2) call setfail
        if (fail.eq.3) call setcoast
        if (fail.eq.4) call setflow
        if (fail.eq.5) call setwave
        if (fail.eq.0) call seismic
        if (fail.eq.1) call slide
        if (fail.eq.2) call slump
        if (fail.eq.3) call splash
        if (fail.eq.4) call pyro
        if (fail.eq.5) call wave
        if (fail.le.5) call gridinit
        if (fail.eq.0) call gridquake
        if (fail.eq.1) call gridfail
        if (fail.eq.2) call gridfail
        if (fail.eq.3) call gridcoast
        if (fail.eq.4) call gridflow
        if (fail.eq.5) call gridwave
        if (fail.eq.6) call separate
      endif
c
c  1.7 Close the properties output file.
c
      close (203)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridfile
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  2.1 You can specify any filename provided the format can be read.  
c 
c      write(*,*) 'Enter the bathymetry filename (up to 20 char)'
c      read(*,*) input
c      write(*,*)
c
c  2.2 Specify the format of the bathymetry file.  
c
c      write(*,*) 'Type 0 if this is a Surfer ASCII grid file'
c      read(*,*) grid
c      write(*,*)
c
c  2.3 Check the last entry.
c
c      if (grid.ne.0) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'That was an invalid entry'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c
c  2.4 Specify the format of the initial condition output file.  
c
c      write(*,*) 'Type 0 to output a Surfer ASCII grid file'
c      read(*,*) type
c      write(*,*)
c
c  2.5 Check the last entry and specify file extension.
c
c      if (type.ne.0) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'That was an invalid entry'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c      if (type.eq.0) then
c        output = '.grd'
c      endif
c
c  2.6 Specify the wave propagation code being run.  
c
c      write(*,*) 'Time steps and grid configurations vary'
c      write(*,*) 
c      write(*,*) 'Type 0 if you are running TUNAMI-N2'
c      write(*,*) 'Type 1 if you are running GEOWAVE'
c      read(*,*) sim
c      write(*,*)
      sim = 1
c
c  2.7 Check the last entry.
c
      if ((sim.ne.0).and.(sim.ne.1)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  2.8 Read the grid file in the specified format.  
c
c      write(*,*) 'Opening the bathymetry file'
c      write(*,*)
c      if (grid.eq.0) then
c        open (202,file=input)
c        read(202,*)
c        read(202,*) im,jm
c        read(202,*) xw,xe
c        read(202,*) ys,yn
c        read(202,*) dmin,dmax
c        do 24 j=1,jm
c          read(202,*)(grd(i,j),i=1,im)
c  24    continue
c      endif
c      close (202)
c
c  2.9 Positive water depths are used within TOPICS.
c
c      write(*,*) 'Type 0 if depths are positive'
c      write(*,*) 'Type 1 if depths are negative'
c      read(*,*) iden
c      write(*,*)
c
c  2.10 Check the data for consistency.
c
c      if ((iden.ne.0).and.(iden.ne.1)) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'That was an invalid entry'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c      if (xe.le.xw) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'East and west grid edges overlap'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c      if (yn.le.ys) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'North and south grid edges overlap'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c      if (im.le.3) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'Three or fewer columns in the grid'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c      if (jm.le.3) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'Three or fewer rows in the grid'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c
c  2.11 Calculate the spatial increments of the grid.
c
c      xinc = (xe - xw) / dfloat(im - 1)
c      yinc = (yn - ys) / dfloat(jm - 1)
c
c  2.12 Grid distance must be converted to meters for TOPICS.
c
c      write(*,*) 'Type 0 if the grid is in meters'
c      write(*,*) 'Type 1 if the grid is in feet'
c      write(*,*) 'Type 2 if the grid is in kilometers'
c      write(*,*) 'Type 3 if the grid is in miles'
c      write(*,*) 'Type 4 if the grid is in decimal degrees'
c      read(*,*) flag
c      write(*,*)
c      if (flag.eq.0) then
c        ylat = 1.0D+00
c        xlon = 1.0D+00
c      endif
c      if (flag.eq.1) then
c        ylat = 0.3048+00
c        xlon = 0.3048D+00
c      endif
c      if (flag.eq.2) then
c        ylat = 1.0D+03
c        xlon = 1.0D+03
c      endif
c      if (flag.eq.3) then
c        ylat = 1.609344D+03
c        xlon = 1.609344D+03
c      endif
c      if (flag.eq.4) then
c        ylat = 111.265D+03
c        xlon = ylat * dcos((yn + ys) * pi / 3.6D+02)
c      endif
c
c  2.13 Check the last entry.
c
c      if ((flag.lt.0).and.(flag.gt.4)) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'That was an invalid entry'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c
c  2.14 Calculate the vertical and horizontal grid spacing in meters.
c 
c      dx = xinc * xlon
c      dy = yinc * ylat
c
c  2.15 Verify that the grid is close to being uniform, dx=dy.
c
c      test = 2.0D+02 * (dabs(dx) - dabs(dy)) / (dabs(dx) + dabs(dy))
c      if (dabs(test).gt.5.0D+00) then
c        write(*,*)
c        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
c        write(*,*) 'The bathy grid is more than 5% nonuniform'
c        write(*,*) 'This may make the results questionable'
c        write(*,*) 'Type 0 if you wish to continue'
c        read(*,*) quest
c        if (quest.eq.0) then
c          write(*,*)
c          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
c          write(203,*) 'The grid is more than 5% nonuniform'
c          write(203,*) 'This may make the results questionable'
c          write(203,*)
c        else
c          write(*,*) 'Program stopped, press return'
c          pause
c          read(*,*)
c          stop            
c        endif
c      endif 
c
c  2.16 Get ready to convert the depths into meters.
c
c      write(*,*) 'Type 0 if depth is in meters'
c      write(*,*) 'Type 1 if depth is in feet'
c      write(*,*) 'Type 2 if depth is in kilometers'
c      write(*,*) 'Type 3 if depth is in miles'
c      write(*,*) 'Type 4 if depth is in fathoms'
c      read(*,*) idep
c      write(*,*)
c      if (idep.eq.0) then
c        dep = 1.0D+00
c      endif
c      if (idep.eq.1) then
c        dep = 0.3048+00
c      endif
c      if (idep.eq.2) then
c        dep = 1.0D+03
c      endif
c      if (idep.eq.3) then
c        dep = 1.609344D+03
c      endif
c      if (idep.eq.4) then
c        dep = 1.8288D+00
c      endif
c
c  2.17 Check the last entry.
c
c      if ((idep.lt.0).and.(idep.gt.4)) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'That was an invalid entry'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c
c  2.18 Convert grid file max and min to meters.  
c 
c      dmin = dmin * dep
c      dmax = dmax * dep
c
c  2.19 Compute the time step specific to the propogation code.
c
c      if ((iden.eq.0).and.(dmax.le.0.0D+00)) then
      if (dmax.le.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The maximum depth is not positive'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif 
c      if ((iden.eq.0).and.(dmax.gt.0.0D+00)) then
        if (sim.eq.0) then
          dt = 0.95D+00 * min(dx,dy) / dsqrt(2.0D+00 * g * dmax)
        endif
        if (sim.eq.1) then
          dt = 0.3D+00 * min(dx,dy) / dsqrt(g * dmax)
        endif
c      endif
c      if ((iden.eq.1).and.(dmin.ge.0.0D+00)) then
c        write(*,*)
c        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
c        write(*,*) 'The minimum depth is not negative'
c        write(*,*) 'Program stopped, press return'
c        pause
c        read(*,*)
c        stop
c      endif
c      if ((iden.eq.1).and.(dmin.lt.0.0D+00)) then
c        if (sim.eq.0) then
c          dt = 0.95D+00 * min(dx,dy) / dsqrt(-2.0D+00 * g * dmin)
c        endif
c        if (sim.eq.1) then
c          dt = 0.3D+00 * min(dx,dy) / dsqrt(-g * dmin)
c        endif
c      endif
c
c  2.20 Prepare to render all depths positive.
c 
c      if (iden.eq.0) then
c        test = 1.0D+00
c      else
c        test = -1.0D+00
c      endif
c
c  2.21 Rescale the bathymetry data so that it is positive and in meters.
c
c      write(*,*) 'Rescaling the bathymetry grid'
c      write(*,*)
c      do 27 i=1,im
c        do 27 j=1,jm
c          grd(i,j) = test * dep * grd(i,j)
c  27  continue
c      if (iden.ne.0) then
c        test = dmax
c        dmax = -dmin
c        dmin = -test
c      endif
c
c  2.22 Write data to the properties file.
c
      write(203,*) input
      write(203,*)
      write(203,*) grid, '  grid file format'
      write(203,*) type, '  output file format'
      write(203,*) sim, '  wave propagation code'
      write(203,*) iden, '  sign of depth data'
      write(203,*) flag, '  units of grid data'
      write(203,*) idep, '  units of depth data'
      write(203,*)
      write(203,*) im, '  number of columns'
      write(203,*) jm, '  number of rows'
      write(203,*)
      write(203,*) xw, '  grid west edge'
      write(203,*) xe, '  grid east edge'
      write(203,*) ys, '  grid south edge'
      write(203,*) yn, '  grid north edge'
      write(203,*) dmin, '  minimum depth'
      write(203,*) dmax, '  maximum depth'
      write(203,*)
      write(203,*) xinc, '  horizontal increment'
      write(203,*) yinc, '  vertical increment'
      write(203,*) dx, '  horizontal dx'
      write(203,*) dy, '  vertical dy'
      write(203,*) dt, '  simulation time step'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine choice
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  3.1 Choose the desired tsunami source and check the entry.  
c
      if (itsu.eq.0) then
        write(*,*) 'Type 0 for a coseismic displacement source'
        write(*,*) 'Type 1 for a translational slide source'
        write(*,*) 'Type 2 for a rotational slump source'
        write(*,*) 'Type 3 for a subaerial landslide source'
        write(*,*) 'Type 4 for a pyroclastic flow source'
        read(*,*) fail
        write(*,*)
        if ((fail.lt.0).or.(fail.gt.4)) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'That was an invalid entry'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
      endif
c
c  3.2 Repeat this for gridded tsunami sources.
c
      if (itsu.eq.1) then
        write(*,*) 'Type 0 for a coseismic displacement source'
        write(*,*) 'Type 1 for a translational slide source'
        write(*,*) 'Type 2 for a rotational slump source'
        write(*,*) 'Type 3 for a subaerial landslide source'
        write(*,*) 'Type 4 for a pyroclastic flow source'
        write(*,*) 'Type 5 for a design wave train source'
        write(*,*) 'Type 6 for a separate tsunami source'
        read(*,*) fail
        write(*,*)
        if ((fail.lt.0).or.(fail.gt.6)) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'That was an invalid entry'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
      endif
c
c  3.3 You choose a specific ID number for this tsunami source.
c
c      write(*,*) 'Enter a single digit source ID number'
c      read(*,*) nsrc
c      write(*,*)
c      itmp = mod(nsrc,10)
c      proper=char(itmp+48)
c
c  3.4 You choose the absolute time at which tsunami generation begins.
c
      write(*,*) 'Enter beginning time of tsunami generation (s)'
      read(*,*) tsrc
      write(*,*)
c
c  3.5 Open the properties file.
c
      write(*,*) 'Opening the properties file'
      write(*,*)
      open (203,file='out'//proper//'.txt')
c
c  3.6 Write data to the properties file.
c
      write(203,*) '          TOPICS Version 1.2'
      write(203,*) '            (c) 2000-2009'
      write(203,*) '     Applied Fluids Engineering, Inc.'
      write(203,*)
      write(203,*) fail, '  tsunami source'
      write(203,*) nsrc, '  source ID number'
      write(203,*)
      write(203,*) proper
      write(203,*)
      write(203,*) tsrc, '  start tsunami generation'
      write(203,*)
c
c  3.7 Choose the way to describe the earthquake fault geometry.
C
      if (fail.eq.0) then
        write(*,*) 'There are two ways to describe a planar trapezoid'
        write(*,*)
        write(*,*) 'Type 0 for a rectangle from earthquake parameters'
        write(*,*) 'Type 1 for four fault vertices and parameters'
        read(*,*) trap
        write(*,*)
c
c  3.8 Check the entry for validity.
c
        if ((trap.ne.0).and.(trap.ne.1)) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'That was an invalid entry'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
        write(203,*) trap, '  planar trapezoid'
        write(203,*)
      endif
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setfail
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  4.1 Identify the landslide center of mass position on the grid.  
c
      write(*,*) 'The next coordinate pair is in the grid units'
      write(*,*)
      write(*,*) 'Enter initial x-axis mass failure center xo'
      read(*,*) xo
      write(*,*) 'Enter initial y-axis mass failure center yo'
      read(*,*) yo
      write(*,*) 'The next two angles are measured counterclockwise'
      write(*,*)
      write(*,*) 'Enter CCW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CCW angle of failure in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  4.2 Verify that the landslide is within the grid and underwater.
c
      if ((xo.gt.xe).or.(xo.lt.xw)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of xo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((yo.gt.yn).or.(yo.lt.ys)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of yo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      ic = nint((xo - xw) / xinc)
      jc = nint((yo - ys) / yinc)
      if (grd(ic,jc).le.0.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The mass failure is mostly subaerial'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The mass failure is mostly subaerial'
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  4.3 Convert degrees to radians and compute the cosine and sine. 
c
      alpha = alpha * pi / 1.8D+02
      alfao = alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  4.4 Write data to properties file.
c
      write(203,*) xo, '  failure longitude'
      write(203,*) yo, '  failure latitude'
      write(203,*) 1.8D+02 * alfao / pi, '  orientation of north'
      write(203,*) 1.8D+02 * alpha / pi, '  orientation from north'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine slide
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  5.1 Help define the landslide and source geometry. 
C
      write(*,*) 'Slide motion follows Watts and Grilli (2002)'
      write(*,*)
      write(*,*) 'The positive x-axis is in the failure direction'
      write(*,*) 'The x-axis is also a line of mirror symmetry'
      write(*,*)
      write(*,*) 'The depth is an effective mean depth of slide'
      write(*,*) 'An effective slope can mimic basal friction'
      write(*,*) 'The cutoff applies Gaussian decay at y=cut'
      write(*,*)
      write(*,*) 'The source should be in front of the slide'
      write(*,*) 'The xo,yo position controls source location'
      write(*,*)
      write(203,*) 'Slide motion follows Watts and Grilli (2002)'
      write(203,*)
      write(203,*) 'The positive x-axis is in the failure direction'
      write(203,*) 'The x-axis is also a line of mirror symmetry'
      write(203,*)
      write(203,*) 'The depth is an effective mean depth of slide'
      write(203,*) 'An effective slope can mimic basal friction'
      write(203,*) 'The cutoff applies Gaussian decay at y=cut'
      write(203,*)
      write(203,*) 'The source should be in front of the slide'
      write(203,*) 'The xo,yo position controls source location'
      write(203,*)
c
c  5.2 You choose the configuration of the underwater slide. 
c
      write(*,*) 'Enter initial depth of the middle of slide (m)'
      read(*,*) d
      write(*,*) 'Enter mean slope along failure plane (degrees)'
      read(*,*) theta
      write(*,*) 'Enter initial slide length during failure (m)'
      read(*,*) b
      write(*,*) 'Enter maximum initial slide thickness (m)'
      read(*,*) t
      write(*,*) 'Enter maximum initial slide width (m)'
      read(*,*) w
      if (itsu.eq.1) then
        write(*,*) 'Enter a maximum tsunami cutoff width (m)'
        read(*,*) cut
      else
        cut = 2.0D+02 * w
      endif
      write(*,*) 'Enter the slide bulk density (kg/m^3)'
      read(*,*) rhob         
      write(*,*)
c
c  5.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      theta = dabs(theta)
      b = dabs(b)         
      t = dabs(t)
      w = dabs(w)
      cut = dabs(cut)/2.0D+00
      rhob = dabs(rhob)
c
c  5.4 Check that the inputs are physically possible.  
c         
      if (t.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide thickness cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (b.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The initial depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (theta.le.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of theta is zero or negative'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (theta.ge.9.0D+01) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of theta is at or beyond vertical'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rhoo.ge.rhob) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The bulk density is less than water density'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rhob.ge.4.5D+03) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The bulk density is unreasonably large'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  5.5 Convert theta to radians and calculate specific density.
c
      write(*,*) 'Calculating slide properties'
      write(*,*)
      theta = theta * pi / 1.8D+02
      cnt = dcos(theta)
      snt = dsin(theta)
      tnt = dtan(theta)
      gamma = rhob / rhoo
      gmo = gamma - 1.0D+00
c
c  5.6 Compute slide motion and tsunami characteristics.
c
      ao = g * snt * gmo / (gamma + 1.0D+00)
      ut = dsqrt(0.5D+00 * g * b * pi * snt * gmo)
      so = ut**2 / ao
      to = ut / ao
      dist = 0.4338D+00 * so
      lambda = to * dsqrt(g * d)
      hao = lambda / b
      sg = so * snt / d
      fr = ut / dsqrt(g * d)
      eta = 0.723D+00 * so * (4.772D-02 - 3.559D-02 * snt + 
     +      8.13D-03 * snt**2) * (t / b) * (b * snt / d)**1.25 * 
     +      1.18D+00 * (1.0D+00 - dexp(-2.2027D+00 * gmo)) 
      xg = (d + t / cnt) / tnt
      term = eta / (so * snt**1.5)
      xmin = 0.95D+00 * (xg + 0.4338D+00 * so * cnt) - xg
      delx = 0.5D+00 * lambda
c
c  5.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  5.8 Check the calculations for accuracy.
c
      if ((d/b).lt.0.02D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The initial depth is too small for the physics'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif        
      if (cut.lt.(w/3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The cutoff is much less than landslide width'
        write(*,*) 'This cutoff will be increased to 0.66*width'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          cut = 0.33D+00 * w
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The cutoff is much less than landslide width'
          write(203,*) 'This cutoff will be increased to 0.66*width'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (hao.lt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Hammack number is <1  ', hao
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The Hammack number is <1  ', hao
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (sg.gt.0.35D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Submergence number is >0.35  ', sg
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The Submergence number is >0.35  ', sg
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (term.gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The term (eta/so*sinq^1.5) >0.2  ',  term
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The term (eta/so*sinq^1.5) >0.2  ',  term
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = theta * 1.8D+02 / pi
      if (test.gt.3.0D+01) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The incline angle theta >30 degrees ',  test
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The incline angle theta >30 degrees ',  test
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((d/b).lt.0.12D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio d/b <0.12  ',  d / b
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio d/b <0.12  ',  d / b
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((t/b).gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio t/b >0.2  ',  t / b
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio t/b >0.2  ',  t / b
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((t/d).gt.3.33D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio t/d >3.33  ',  t / d
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio t/d >3.33  ',  t / d
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop                    
          endif
      endif
      if ((w/b).lt.0.06D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio w/b <0.1  ',  w / b
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio w/b <0.1  ',  w / b
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((w/b).gt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio w/b >1.0  ',  w / b
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio w/b >1.0  ',  w / b
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  5.9 Calculate quantities that determine tsunami shape. 
c
      zmin = -2.1D+00 * eta
      zmax = 0.64D+00 * eta * (0.8D+00 + 0.2D+00 * d / (b * snt))
      nmin = 1.2D+00 * zmin
      nmax = zmax
      denom = lambda
      shift = delx
      wid = lambda
c
c  5.10 Write data to properties file.
c
      write(203,*) d, '  mean initial depth'
      write(203,*) 1.8D+02 * theta / pi, '  mean incline angle'
      write(203,*) b, '  initial length'
      write(203,*) t, '  initial maximum thickness'
      write(203,*) w, '  initial maximum width'
      if (itsu.eq.1) then
        write(203,*) 2.0D+00 * cut, '  tsunami cutoff width'
      endif
      write(203,*) rhob, '  bulk density'
      write(203,*)
      write(203,*) gamma, '  specific density'
      write(203,*) ao, '  initial acceleration'
      write(203,*) ut, '  terminal velocity'
      write(203,*) so, '  characteristic distance'
      write(203,*) to, '  characteristic time'
      write(203,*) lambda, '  tsunami wavelength'
      write(203,*) dist, '  distance traveled in time to'
      write(203,*)
      write(203,*) hao, '  Hammack number'
      write(203,*) sg, '  Submergence number'
      write(203,*) fr, '  maximum Froude number'
      write(203,*) term, '  Quadratic number'
      write(203,*)
      write(203,*) eta, '  characteristic wave amplitude'      
      write(203,*) xg, '  gauge position'
      write(203,*) xmin, '  trough position'
      write(203,*) delx, '  trough to peak distance'
      write(203,*) zmin, '  trough argument'
      write(203,*) zmax, '  peak argument'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine slump
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  6.1 Help define the landslide and source geometry. 
C
      write(*,*) 'Slump motion follows Watts and Grilli (2002)'
      write(*,*)
      write(*,*) 'The positive x-axis is in the failure direction'
      write(*,*) 'The x-axis is also a line of mirror symmetry'
      write(*,*)
      write(*,*) 'The depth is an effective mean depth of slump'
      write(*,*) 'The slope can also be an effective slope'
      write(*,*) 'The cutoff applies Gaussian decay at y=cut'
      write(*,*)
      write(*,*) 'The source should be in front of the slump'
      write(*,*) 'The xo,yo position controls source location'
      write(*,*)
      write(203,*) 'Slump motion follows Watts and Grilli (2002)'
      write(203,*)
      write(203,*) 'The positive x-axis is in the failure direction'
      write(203,*) 'The x-axis is also a line of mirror symmetry'
      write(203,*)
      write(203,*) 'The depth is an effective mean depth of slump'
      write(203,*) 'The slope can also be an effective slope'
      write(203,*) 'The cutoff applies Gaussian decay at y=cut'
      write(203,*)
      write(203,*) 'The source should be in front of the slump'
      write(203,*) 'The xo,yo position controls source location'
      write(203,*)
c
c  6.2 You choose the configuration of the underwater slump. 
c
      write(*,*) 'Enter initial depth of the middle of slump (m)'
      read(*,*) d
      write(*,*) 'Enter mean slope along failure plane (degrees)'
      read(*,*) theta
      write(*,*) 'Enter initial slump length during failure (m)'
      read(*,*) b
      write(*,*) 'Enter maximum initial slump thickness (m)'
      read(*,*) t
      write(*,*) 'Enter maximum initial slump width (m)'
      read(*,*) w
      if (itsu.eq.1) then
        write(*,*) 'Enter a maximum tsunami cutoff width (m)'
        read(*,*) cut
      else
        cut = 2.0D+02 * w
      endif
      write(*,*) 'Enter distance traveled by center of mass (m)'
      read(*,*) dist
      write(*,*) 'Enter the slump bulk density (kg/m^3)'
      read(*,*) rhob
      write(*,*)
c
c  6.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      theta = dabs(theta)
      b = dabs(b)
      t = dabs(t)
      w = dabs(w)
      cut = dabs(cut)/2.0D+00
      dist = dabs(dist)
      rhob = dabs(rhob)
c
c  6.4 Check that the inputs are physically possible.  
c
      if (t.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The slump thickness cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (b.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The slump length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The initial depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dist.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The slump has to have moved to make a wave'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (theta.le.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of theta is zero or negative'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (theta.ge.9.0D+01) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of theta is at or beyond vertical'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rhoo.ge.rhob) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The bulk density is less than water density'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rhob.ge.4.5D+03) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The bulk density is unreasonably large'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  6.5 Convert theta to radians and calculate specific density.
c
      write(*,*) 'Calculating slump properties'
      write(*,*)
      theta = theta * pi / 1.8D+02
      cnt = dcos(theta)
      snt = dsin(theta)
      tnt = dtan(theta)
      gamma = rhob / rhoo
      gmo = gamma - 1.0D+00
c
c  6.6 Compute slump motion and tsunami characteristics.
c
      r = 0.125D+00 * b**2 / t + t / 2.0D+00
      dphi = dist / r
      so = dist / 2.0D+00
      to = dsqrt((r * (gamma + 1.0D+00)) / (g * gmo))
      ao = so / to**2
      ut = so / to
      lambda = 2.0D+00* to * dsqrt(g * d)
      hao = 0.5D+00 * lambda / b
      sg = so * snt / d
      fr = ut / dsqrt(g * d)
      eta = 0.723D+00 * so * (1.4662D+00 * gmo - 0.3454D+00 * 
     +      gmo**2) * snt**0.22 * (t / b) * (b / d)**1.25 *
     +      dphi**0.39 * (b / r)**0.63 * 0.1309D+00
      xg = (d + t / cnt) / tnt
      xmin = 0.565D+00 * (xg + 0.4597D+00 * so * cnt) - xg
      term = eta / (so * snt**1.5)
      delx = 0.5D+00 * lambda
c
c  6.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  6.8 Check the calculations for accuracy.
c
      if ((d/b).lt.0.02D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The initial depth is too small for the physics'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif        
      if (cut.lt.(w/3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The cutoff is much less than landslide width'
        write(*,*) 'This cutoff will be increased to 0.66*width'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          cut = 0.33D+00 * w
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The cutoff is much less than landslide width'
          write(203,*) 'This cutoff will be increased to 0.66*width'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (hao.lt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Hammack number is <1  ', hao
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The Hammack number is <1  ', hao
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (sg.gt.0.35D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Submergence number is >0.35  ', sg
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The Submergence number is >0.35  ', sg
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (term.gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The term (eta/so*sinq^1.5) >0.2  ',  term
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The term (eta/so*sinq^1.5) >0.2  ',  term
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = theta * 1.8D+02 / pi
      if (test.gt.3.0D+01) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The incline angle >30 degrees ',  test
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The incline angle >30 degrees ',  test
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((d/b).lt.0.12D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio d/b <0.12  ',  d / b
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio d/b <0.12  ',  d / b
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((t/b).gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio t/b >0.2  ',  t / b
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio t/b >0.2  ',  t / b
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((t/d).gt.3.33D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio t/d >3.33  ',  t / d
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio t/d >3.33  ',  t / d
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (dphi.gt.0.53D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The angular motion dphi >0.53 radians  ', dphi
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The angular motion dphi >0.53 radians  ', dphi
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (((r/b).gt.2.0D+00).or.((r/b).lt.1.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio r/d >2 or r/d <1  ',  r / d
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio r/d >2 or r/d <1  ',  r / d
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((w/b).lt.0.25D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio w/b <0.5  ',  w / b
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio w/b <0.5  ',  w / b
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((w/b).gt.4.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio w/b >2.0  ',  w / b
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The ratio w/b >2.0  ',  w / b
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  6.9 Calculate quantities that determine tsunami shape. 
c
      zmin = -eta * ((2.480D+00 * 0.2892D+00 - 0.7904D+00 * sg +
     +               1.3376D+00 * sg**2) / (0.2892D+00 +
     +               0.9163D+00 * sg))
      zmax = eta * ((1.686D+00 * 0.3498D+00 - 0.3531D+00 * sg +
     +               0.6466D+00 * sg**2) / (0.3498D+00 +
     +               1.0257D+00 * sg))
      nmin = 1.22D+00 * 1.15D+00 * zmin
      nmax = 1.22D+00 * zmax
      denom = 0.5D+00 * lambda
      shift = 0.8D+00 * delx
      wid = 0.5D+00 * lambda
c
c  6.10 Write data to properties file.
c
      write(203,*) d, '  mean initial depth'
      write(203,*) 1.8D+02 * theta / pi, '  mean incline angle'
      write(203,*) b, '  initial length'
      write(203,*) t, '  initial maximum thickness'
      write(203,*) w, '  initial maximum width'
      if (itsu.eq.1) then
        write(203,*) 2.0D+00 * cut, '  tsunami cutoff width'
      endif
      write(203,*) dist, '  distance of slump motion'
      write(203,*) rhob, '  bulk density'
      write(203,*)
      write(203,*) gamma, '  specific density'
      write(203,*) r, '  radius of curvature'
      write(203,*) dphi, '  angular rotation'
      write(203,*) ao, '  initial acceleration'
      write(203,*) ut, '  maximum velocity'
      write(203,*) so, '  characteristic distance'
      write(203,*) to, '  characteristic time'
      write(203,*) lambda, '  wavelength'
      write(203,*) dist, '  distance traveled'
      write(203,*) 
      write(203,*) hao, '  Hammack number'
      write(203,*) sg, '  Submergence number'
      write(203,*) fr, '  maximum Froude number'
      write(203,*) term, '  Quadratic number'
      write(203,*)
      write(203,*) eta, '  characteristic wave amplitude'
      write(203,*) xg, '  gauge position'
      write(203,*) xmin, '  trough position'
      write(203,*) delx, '  trough to peak distance'
      write(203,*) zmin, '  trough argument'
      write(203,*) zmax, '  peak argument'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridcheck
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  7.1 Stop the program if the grid size is too small to resolve tsunami.  
c
      if (((delx / 7.0D+00).lt.dx).or.((delx / 7.0D+00).lt.dy)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wavelength is too small for grid'
        write(*,*) 'TOPICS should be stopped immediately'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The wavelength is too small for grid'
          write(203,*) 'TOPICS should be stopped immediately'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  7.2 Warn if the grid size is too small to be accurate.  
c
      if (((delx / 2.0D+01).lt.dx).or.((delx / 2.0D+01).lt.dy)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The grid size is >0.05 times wavelength'
        write(*,*) 'This may lead to simulation errors'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The grid size is >0.05 times wavelength'
          write(203,*) 'This may lead to simulation errors'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridinit
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  8.1 Open the grid files.
c
      write(*,*) 'Opening the grid files'
      write(*,*)
      open (201,file='surface'//proper//output)
      if (fail.ne.0) then
        open (204,file='uvel'//proper//output)
        open (205,file='vvel'//proper//output)
      endif
c
c  8.2 Initialize the grid files to zero.  
c
      do 85 j=1,jm
        do 85 i=1,im
          out(i,j) = 0.0D+00
          uvel(i,j) = 0.0D+00
          vvel(i,j) = 0.0D+00
  85  continue
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridfail
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  9.1 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 91 j=1,jm
        do 91 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  9.2 Evaluate tsunami shape in local x,y coordinates.
c
          term = 1.0D+00 - dexp(-2.0906D+00 * (w / wid) * 
     +          (1.0D+00 + 1.0903D+00 * (w / wid)))
          out(i,j) = term *
     +      (nmin * dexp(-(nmin * (x - xmin) / (denom * nmax))**2) +
     +       nmax * dexp(-((x - xmin - shift) / (denom))**2)) *
     +      (2.0D+00 / (dexp(3.0D+00 * term * y / w) +
     +                 dexp(-3.0D+00 * term * y / w)))**2
          if (dabs(y).gt.cut) then
            out(i,j) = out(i,j) * 
     +                 dexp(-(5.0D+00 * (dabs(y) - cut) / cut)**2)
          endif
c
c  9.3 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
          if ((x.lt.xmin).and.(out(i,j).gt.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((x.gt.(xmin+shift)).and.(out(i,j).lt.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
  91  continue
c
c  9.4 Estimate linear wave quantities. 
c
      kappa = 2.0D+00 * pi / lambda
      term = kappa * d
      omega = dsqrt(g * kappa * dtanh(term))
c
c  9.5 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the u velocities'
      write(*,*)
      do 96 j=1,jm
        do 96 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            xxx = xxx + xinc / 2.0D+00
          endif
          if (sim.eq.1) then
            xxx = xxx
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  9.6 Evaluate tsunami u velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (i.lt.im) then
              fact1 = (out(i+1,j) + out(i,j)) / 2.0D+00
              fact2 = (grd(i+1,j) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i-1,j)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i-1,j)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = fact1 * g * dtanh(term) / (d * omega)
          endif
          if (sim.eq.1) then
            utot = fact1 * g * kappa * dcosh(0.469D+00 * term) / 
     +            (omega * dcosh(term))
          endif
          if (fact1.gt.0.0D+00) then
            uvel(i,j) =  -utot * sna
          else
            uvel(i,j) =  0.0D+00
          endif
c
c  9.7 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = uvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            uvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = 0.0D+00
            endif
          endif
   96 continue
c
c  9.8 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the v velocities'
      write(*,*)
      do 97 j=1,jm
        do 97 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            yyy = yyy + yinc / 2.0D+00
          endif
          if (sim.eq.1) then
            yyy = yyy
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  9.9 Evaluate tsunami v velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (j.lt.jm) then
              fact1 = (out(i,j+1) + out(i,j)) / 2.0D+00
              fact2 = (grd(i,j+1) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i,j-1)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i,j-1)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = fact1 * g * dtanh(term) / (d * omega)
          endif
          if (sim.eq.1) then
            utot = fact1 * g * kappa * dcosh(0.469D+00 * term) / 
     +            (omega * dcosh(term))
          endif
          if (fact1.gt.0.0D+00) then
            vvel(i,j) = utot * cna
          else
            vvel(i,j) =  0.0D+00
          endif
c
c  9.10 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = vvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            vvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = 0.0D+00
            endif
          endif
   97 continue
c
c  9.11 Search for the maxima and minima.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 93 j=1,jm
        do 93 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
   93 continue
      umax = 0.0D+00
      umin = 0.0D+00
      do 94 j=1,jm
        do 94 i=1,im
          if (uvel(i,j).lt.umin) then
            umin = uvel(i,j)
          endif
          if (uvel(i,j).gt.umax) then
            umax = uvel(i,j)
          endif
   94 continue
      vmax = 0.0D+00
      vmin = 0.0D+00
      do 95 j=1,jm
        do 95 i=1,im
          if (vvel(i,j).lt.vmin) then
            vmin = vvel(i,j)
          endif
          if (vvel(i,j).gt.vmax) then
            vmax = vvel(i,j)
          endif
   95 continue
c
c  9.12 Write data to properties file.
c
      write(203,*) fsmin, '  trough amplitude'
      write(203,*) fsmax, '  peak amplitude'
      write(203,*) umin, '  trough u velocity'
      write(203,*) umax, '  peak u velocity'
      write(203,*) vmin, '  trough v velocity'
      write(203,*) vmax, '  peak v velocity'
      write(203,*)
c
c  9.13 Write the initial condition grid file.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine charfail
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  10.1 Calculate the tsunami minimum and maximum amplitude. 
c
      write(*,*) 'Finding free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      ddx = 0.1D-03 * (xmin + 6.0D+00 * shift)
      do 101 j=0,10000
        x = dfloat(j) * ddx - 3.0D+00 * shift
        term = 1.0D+00 - dexp(-2.0906D+00 * (w / wid) * 
     +        (1.0D+00 + 1.0903D+00 * (w / wid)))
        test= term *
     +       (nmin * dexp(-(nmin * (x - xmin) / (denom * nmax))**2) +
     +        nmax * dexp(-((x - xmin - shift) / (denom))**2))
        if (test.lt.fsmin) fsmin = test
        if (test.gt.fsmax) fsmax = test
  101 continue
c
c  10.2 Write data to properties file.
c
      write(203,*) fsmin, '  trough amplitude'
      write(203,*) fsmax, '  peak amplitude'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine seismic
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 dipio,rakeio
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  11.1 Help define the geometry of the fault plane and vertices. 
C
      write(*,*) 'Fault geometry follows the work of Okada (1985)'
      write(*,*) 'The convention used is Aki and Richards (1980)'
      write(*,*) 'The earthquake magnitude provides input checks'
      write(*,*) 'The shear modulus provides the energy release'
      write(*,*)
      write(*,*) 'There is a local axis system for the fault'
      write(*,*) 'The positive x-axis is in the strike direction'
      write(*,*) 'The positive z-axis is pointing up vertically'
      write(*,*) 'The positive y-axis follows from the right hand'
      write(*,*)
      write(*,*) 'Strike is measured clockwise from north'
      write(*,*) 'Dip is measured positive from horizontal'
      write(*,*) 'Rake provides slip direction in fault plane'
      write(*,*) 'Rake increases counterclockwise from strike'
      write(*,*)
      write(*,*) 'The fault plane includes local point 0,0,-D'
      write(*,*)
      write(*,*) 'P1 has positive x and is generally less deep'
      write(*,*) 'P2 has negative x and is generally less deep'
      write(*,*) 'P3 has negative x and is generally more deep'
      write(*,*) 'P4 has positive x and is generally more deep'
      write(*,*)
      write(*,*) 'A Gaussian slip distribution exists about 0,0'
      write(*,*) 'This is specified by the radius for 50% slip'
      write(*,*) 'The singularity at zero depth is also removed'
      write(*,*) 'This is specified by the depth for 1% slip'
      write(*,*)
      write(203,*) 'Fault geometry follows the work of Okada (1985)'
      write(203,*) 'The convention used is Aki and Richards (1980)'
      write(203,*) 'The earthquake magnitude provides input checks'
      write(203,*) 'The shear modulus provides the energy release'
      write(203,*)
      write(203,*) 'There is a local axis system for the fault'
      write(203,*) 'The positive x-axis is in the strike direction'
      write(203,*) 'The positive z-axis is pointing up vertically'
      write(203,*) 'The positive y-axis follows from the right hand'
      write(203,*)
      write(203,*) 'Strike is measured clockwise from north'
      write(203,*) 'Dip is measured positive from horizontal'
      write(203,*) 'Rake provides slip direction in fault plane'
      write(203,*) 'Rake increases counterclockwise from strike'
      write(203,*)
      write(203,*) 'The fault plane includes local point 0,0,-D'
      write(203,*)
      write(203,*) 'P1 has positive x and is generally less deep'
      write(203,*) 'P2 has negative x and is generally less deep'
      write(203,*) 'P3 has negative x and is generally more deep'
      write(203,*) 'P4 has positive x and is generally more deep'
      write(203,*)
      write(203,*) 'A Gaussian slip distribution exists about 0,0'
      write(203,*) 'This is specified by the radius for 50% slip'
      write(203,*) 'The singularity at zero depth is also removed'
      write(203,*) 'This is specified by the depth for 1% slip'
      write(203,*)
c
c  11.2 You choose the parameters of the fault rectangle. 
C
      If (trap.eq.0) then
        write(*,*) 'Enter fault dip from horizontal (0-90 deg)'
        read(*,*) dipio
        write(*,*) 'Enter slip rake from horizontal (0-360 deg)'
        read(*,*) rakeio
        write(*,*) 'Enter earthquake magnitude from some scale'
        read(*,*) mw
        write(*,*) 'Enter maximum value of earthquake slip (m)'
        read(*,*) slip
        write(*,*) 'Enter the centroid fault plane depth (m)'
        read(*,*) depth
        write(*,*) 'Enter the fault length along strike (m)'
        read(*,*) leng
        write(*,*) 'Enter the fault width perpendicular to strike (m)'
        read(*,*) high        
        write(*,*) 'Enter shear modulus in format 4.0D+10 (kg/m-s^2)'
        read(*,*) mu
        write(*,*) 'Enter radius for slip to drop 50% max (m)'
        read(*,*) sharp
        write(*,*) 'Enter depth where slip drops to 1% max (m)'
        read(*,*) sing  
        write(*,*) 'Enter a typical water depth for earthquake (m)'
        read(*,*) d         
        write(*,*)
c
c  11.3 Make sure that all entries are positive and in radians.
c
        dipio = dabs(dipio)
        rakeio = dabs(rakeio)
        if (dipio.gt.90.0D+00) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'The dip cannot be greater than 90 degrees'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
        if (rakeio.gt.360.0D+00) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'The rake cannot be greater than 360 degrees'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
        dip = 1.8D+02 - dipio
        rake = rakeio + 1.8D+02
        dip = dip * pi / 1.8D+02
        rake = rake * pi / 1.8D+02
        mw = dabs(mw)         
        slip = dabs(slip)
        depth = dabs(depth)
        leng = dabs(leng)
        high = dabs(high)
        mu = dabs(mu)
        sharp = dabs(sharp)
        sing = dabs(sing)
        d = dabs(d)
c
c  11.4 Calculate the four corners of the rectangular fault plane.
c
        x1 = 0.5D+00 * leng
        y1 = 0.5D+00 * high * dcos(dip)
        z1 = -depth + 0.5D+00 * high * dsin(dip)
        x2 = -0.5D+00 * leng
        y2 = 0.5D+00 * high * dcos(dip)
        z2 = -depth + 0.5D+00 * high * dsin(dip)
        x3 = -0.5D+00 * leng
        y3 = -0.5D+00 * high * dcos(dip)
        z3 = -depth - 0.5D+00 * high * dsin(dip)
        x4 = 0.5D+00 * leng
        y4 = -0.5D+00 * high * dcos(dip)
        z4 = -depth - 0.5D+00 * high * dsin(dip)
c
c  11.5 You choose the necessary earthquake parameters. 
C
      else
        write(*,*) 'Enter fault dip from horizontal (0-90 deg)'
        read(*,*) dipio
        write(*,*) 'Enter slip rake from horizontal (0-360 deg)'
        read(*,*) rakeio
        write(*,*) 'Enter earthquake magnitude from some scale'
        read(*,*) mw
        write(*,*) 'Enter maximum value of earthquake slip (m)'
        read(*,*) slip
        write(*,*) 'Enter the centroid fault plane depth (m)'
        read(*,*) depth      
        write(*,*) 'Enter shear modulus in format 4.0D+10 (kg/m-s^2)'
        read(*,*) mu
        write(*,*) 'Enter radius for slip to drop 50% max (m)'
        read(*,*) sharp
        write(*,*) 'Enter depth where slip drops to 1% max (m)'
        read(*,*) sing        
        write(*,*) 'Enter a typical water depth for earthquake (m)'
        read(*,*) d
        write(*,*)
c
c  11.6 Make sure that all entries are positive and in radians.
c
        dipio = dabs(dipio)
        rakeio = dabs(rakeio)
        if (dipio.gt.90.0D+00) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'The dip cannot be greater than 90 degrees'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
        if (rakeio.gt.360.0D+00) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'The rake cannot be greater than 360 degrees'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
        dip = 1.8D+02 - dipio
        rake = rakeio + 1.8D+02
        dip = dip * pi / 1.8D+02
        rake = rake * pi / 1.8D+02
        mw = dabs(mw)         
        slip = dabs(slip)
        depth = dabs(depth)
        mu = dabs(mu)
        sharp = dabs(sharp)
        sing = dabs(sing)
        d = dabs(d)
c
c  11.7 You choose the positions of the four trapezoidal vertices.
c
        write(*,*) 'Enter distances of vertices from origin 0,0,-D'
        write(*,*) 'For dip>45 degrees, enter x and z distances'
        write(*,*) 'For lower dip angles, enter x and y distances'
        write(*,*) 'The origin need not be the center of trapezoid'
        write(*,*)
        test = pi / 4.0D+00
        write(*,*) 'Enter the positive value of x1 (m)'
        read(*,*) x1
        x1 = dabs(x1)
        if (dip.lt.test) then
          write(*,*) 'Enter the positive value of y1 (m)'
          read(*,*) y1
          y1 = dabs(y1)
          z1 = dtan(dip) * y1 - depth
        endif
        if (dip.gt.(3.0*test)) then
          write(*,*) 'Enter the negative value of y1 (m)'
          read(*,*) y1
          y1 = -dabs(y1)
          z1 = dtan(dip) * y1 - depth
        endif
        if ((dip.ge.test).and.(dip.le.(3.0*test))) then
          write(*,*) 'Enter the positive value of z1 (m)'
          read(*,*) z1
          z1 = dabs(z1) - depth
          y1 = dcos(dip) * (z1 + depth) / dsin(dip)
        endif
        write(*,*) 'Enter the negative value of x2 (m)'
        read(*,*) x2
        x2 = -dabs(x2)
        if (dip.lt.test) then
          write(*,*) 'Enter the positive value of y2 (m)'
          read(*,*) y2
          y2 = dabs(y2)
          z2 = dtan(dip) * y2 - depth
        endif
        if (dip.gt.(3.0*test)) then
          write(*,*) 'Enter the negative value of y2 (m)'
          read(*,*) y2
          y2 = -dabs(y2)
          z2 = dtan(dip) * y2 - depth
        endif
        if ((dip.ge.test).and.(dip.le.(3.0*test))) then
          write(*,*) 'Enter the positive value of z2 (m)'
          read(*,*) z2
          z2 = dabs(z2) - depth
          y2 = dcos(dip) * (z2 + depth) / dsin(dip)
        endif
        write(*,*) 'Enter the negative value of x3 (m)'
        read(*,*) x3
        x3 = -dabs(x3)
        if (dip.lt.test) then
          write(*,*) 'Enter the negative value of y3 (m)'
          read(*,*) y3
          y3 = -dabs(y3)
          z3 = dtan(dip) * y3 - depth
        endif
        if (dip.gt.(3.0*test)) then
          write(*,*) 'Enter the positive value of y3 (m)'
          read(*,*) y3
          y3 = dabs(y3)
          z3 = dtan(dip) * y3 - depth
        endif
        if ((dip.ge.test).and.(dip.le.(3.0*test))) then
          write(*,*) 'Enter the negative value of z3 (m)'
          read(*,*) z3
          z3 = -dabs(z3) - depth
          y3 = dcos(dip) * (z3 + depth) / dsin(dip)
        endif
        write(*,*) 'Enter the positive value of x4 (m)'
        read(*,*) x4
        x4 = dabs(x4)
        if (dip.lt.test) then
          write(*,*) 'Enter the negative value of y4 (m)'
          read(*,*) y4
          y4 = -dabs(y4)
          z4 = dtan(dip) * y4 - depth
        endif
        if (dip.gt.(3.0*test)) then
          write(*,*) 'Enter the positive value of y4 (m)'
          read(*,*) y4
          y4 = dabs(y4)
          z4 = dtan(dip) * y4 - depth
        endif
        if ((dip.ge.test).and.(dip.le.(3.0*test))) then
          write(*,*) 'Enter the negative value of z4 (m)'
          read(*,*) z4
          z4 = -dabs(z4) - depth
          y4 = dcos(dip) * (z4 + depth) / dsin(dip)
        endif
        write(*,*)
        leng = dabs(x1 + x4 - x2 - x3) / 2.0D+00
        if ((dip.gt.test).and.(dip.lt.(3.0*test))) then
          high = dabs(z1 + z2 - z3 - z4) / (2.0D+00 * dsin(dip))
        else
          high = dabs(y1 + y2 - y3 - y4) / (2.0D+00 * dcos(dip))
        endif
      endif
c
c  11.8 Check that the inputs are physically possible.  
c         
      if (mw.gt.9.5D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The magnitude cannot be greater than 9.5'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (mw.lt.5.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The magnitude cannot be less than 5'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (slip.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The fault slip cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (high.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The fault width cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (leng.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The fault length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (sing.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The surface decay rate cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (sharp.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The slip decay radius cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (z3.ge.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The point P3 cannot be at or above ground'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (z4.ge.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The point P4 cannot be at or above ground'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The water depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  11.9 Establish the characteristic period and size of tsunami.
c
      write(*,*) 'Calculating earthquake properties'
      write(*,*)
      to = min(leng,high) / dsqrt(g * d)
      lambda = min(leng,high)
      delx = leng
      if (high.lt.leng) delx = high
c
c  11.10 Compare the grid size to the fault size.
c
      if (itsu.eq.1) call gridcheck
c
c  11.11 Check the entries for accuracy.
c
      test = high * dsin(dip) / 2.0D+00
      if ((test.gt.depth).and.(trap.eq.1)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The fault depth is less than fault height'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The fault depth is less than fault height'
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((test.gt.depth).and.(trap.eq.0)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The fault depth is less than fault height'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((mu.lt.1.0D+08).or.(mu.gt.1.0D+12)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The shear modulus is unreasonable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif  
      if ((mu.lt.1.0D+10).or.(mu.gt.1.0D+11)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The shear modulus is exceptional', mu
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The shear modulus is exceptional', mu
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (z1.gt.0.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The fault vertex P1 is above ground'
        write(*,*) 'z1 will be reduced to ground level'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
         if ((dip.eq.0.0D+00).or.(dip.eq.pi)) stop
          z1 = 0.0D+00
          y1 = dcos(dip) * (z1 + depth) / dsin(dip)
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The fault vertex P1 is above ground'
          write(203,*) 'z1 will be reduced to ground level'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (z2.gt.0.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The fault vertex P2 is above ground'
        write(*,*) 'The value of z2 will be reduced to 0'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
         if ((dip.eq.0.0D+00).or.(dip.eq.pi)) stop
          z2 = 0.0D+00
          y2 = dcos(dip) * (z2 + depth) / dsin(dip)
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The fault vertex P2 is above ground'
          write(203,*) 'The value of z2 will be reduced to 0'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 10**(0.44D+00 * mw - 2.77D+00)
      if (slip.lt.(test/3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The slip is much less than predicted', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The slip is much less than predicted', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (slip.gt.(test*3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Slip is much greater than predicted', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'Slip is much greater than predicted', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 10**(0.69D+00 * mw - 0.22D+00)
      if (leng.lt.(test/3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The length is less than predicted', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The length is less than predicted', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (leng.gt.(test*3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The length is greater than predicted', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The length is greater than predicted', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 10**(0.47D+00 * mw - 2.95D+00)
      if ((leng/high).lt.(test/3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The fault spect ratio is small', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The fault spect ratio is small', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((leng/high).gt.(test*3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The fault spect ratio is large', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The fault spect ratio is large', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (high.le.leng) test = high / 1.0D+01
      if (high.gt.leng) test = leng / 1.0D+01
      if (sharp.lt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The slip decay radius is unreasonably small'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The slip decay radius is unreasonably small'
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (sing.lt.(5.0D+00*slip)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The slip decay depth is too small'
        write(*,*) 'The depth will be increased to 5*slip'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          sing = 5.0D+00 * slip
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The slip decay depth is too small'
          write(203,*) 'The depth will be increased to 5*slip'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (sing.gt.(5.0D+01*slip)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The slip decay depth is too large'
        write(*,*) 'The depth will be decreased to 50*slip'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          sing = 5.0D+01 * slip
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The slip decay depth is too large'
          write(203,*) 'The depth will be decreased to 50*slip'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  11.12 Write data to properties file.
c
      write(203,*) dipio, '  fault dip angle'
      write(203,*) rakeio, '  slip rake angle'
      write(203,*) mw, '  earthquake magnitude'
      write(203,*) slip, '  maximum fault slip'
      write(203,*) depth, '  centroid depth'
      write(203,*) leng, '  typical fault length'
      write(203,*) high, '  typical fault width'
      write(203,*) mu, '  shear modulus'
      write(203,*) sharp, '  radius for slip to drop 50%'
      write(203,*) sing, '  depth for slip to drop to 1%'
      write(203,*) d, '  typical water depth'
      write(203,*)
      write(203,*) 'These four vertices are in local coordinates:'
      write(203,*)
      write(203,*) x1, '  x-position of P1'
      write(203,*) y1, '  y-position of P1'
      write(203,*) z1, '  z-position of P1'
      write(203,*)
      write(203,*) x2, '  x-position of P2'
      write(203,*) y2, '  y-position of P2'
      write(203,*) z2, '  z-position of P2'
      write(203,*)
      write(203,*) x3, '  x-position of P3'
      write(203,*) y3, '  y-position of P3'
      write(203,*) z3, '  z-position of P3'
      write(203,*)
      write(203,*) x4, '  x-position of P4'
      write(203,*) y4, '  y-position of P4'
      write(203,*) z4, '  z-position of P4'
      write(203,*)
      write(203,*) to, '  typical tsunami period'
      write(203,*) lambda, '  typical wavelength'
      write(203,*) delx, '  typical fault size'
      write(203,*) 
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setquake
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  12.1 Identify the center of the fault and strike on the grid.  
C
      write(*,*) 'The next coordinate pair is in the grid units'
      write(*,*)
      write(*,*) 'Enter x-axis centroid of the fault xo'
      read(*,*) xo
      write(*,*) 'Enter y-axis centroid of the fault yo'
      read(*,*) yo
      write(*,*) 'The next two angles are measured clockwise'
      write(*,*)
      write(*,*) 'Enter CW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CW angle of strike in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  12.2 Verify that the earthquake is within the grid.
c
      if ((xo.gt.xe).or.(xo.lt.xw)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The value of xo is outside the grid'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The value of xo is outside the grid'
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((yo.gt.yn).or.(yo.lt.ys)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The value of yo is outside the grid'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The value of yo is outside the grid'
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  12.3 Convert degrees to radians, CW to CCW, and compute the cosine and sine. 
c
      alpha = 2.0D+00 * pi - alpha * pi / 1.8D+02
      alfao = 2.0D+00 * pi - alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  12.4 Write data to properties file.
c
      write(203,*) xo, '  fault centroid longitude'
      write(203,*) yo, '  fault centroid latitude'
      test = 3.6D+02 - 1.8D+02 * alfao / pi
      write(203,*) test, '  orientation of grid north'
      test = 3.6D+02 - 1.8D+02 * alpha / pi
      write(203,*) test, '  orientation of strike from north'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridquake
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  13.1 Enter the number of fault subdivisions in x,y directions.
c
      write(*,*) 'Enter number of x-direction subdivisions (20-200)'
      read(*,*) nx
      write(*,*) 'Enter number of y-direction subdivisions (20-200)'
      read(*,*) ny
      write(*,*)
      nx = iabs(nx)
      ny = iabs(ny)
      if ((nx.gt.200).or.(nx.lt.20)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of nx is unacceptable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((ny.gt.200).or.(ny.lt.20)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of ny is unacceptable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      write(203,*) nx, '  x-direction divisions'
      write(203,*) ny, '  y-direction divisions'
      write(203,*)
c
c  13.2 Compute some constant values for these calculations.
c
      cdip = dcos(dip)
      sdip = dsin(dip)
      elast = 0.5D+00
      u1var = slip * dcos(rake)
      u2var = slip * dsin(rake)
      bnd12 = dabs(x1 - x2) / dfloat(nx)
      bnd23 = dsqrt((z3 - z2)**2 + (y3 - y2)**2) / dfloat(ny)
      bnd34 = dabs(x3 - x4) / dfloat(nx)
      bnd41 = dsqrt((z1 - z4)**2 + (y1 - y4)**2) / dfloat(ny)
c
c  13.3 Zero the value of the earthquake moment to sum up elements.
c
      momt = 0.0D+00
c
c  13.4 Calculate the vertical coseismic displacement over the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 131 j=1,jm
        if (mod(j,100).eq.0) write(*,*)'  j = ',j
        do 131 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  13.5 Interpolate and sum the point sources from the four fault vertices.
c
          do 132 k=1,nx
            do 132 l=1,ny 
              x12 = x2 + (dfloat(k) - 0.5D+00) * (x1 - x2) / 
     +                    dfloat(nx)
              x34 = x3 + (dfloat(k) - 0.5D+00) * (x4 - x3) / 
     +                    dfloat(nx)
              xp = x12 + (dfloat(l) - 0.5D+00) * (x34 - x12) / 
     +                    dfloat(ny)
              xvar = x - xp
              y12 = y2 + (dfloat(k) - 0.5D+00) * (y1 - y2) / 
     +                    dfloat(nx)
              y34 = y3 + (dfloat(k) - 0.5D+00) * (y4 - y3) / 
     +                    dfloat(nx)
              yp = y12 + (dfloat(l) - 0.5D+00) * (y34 - y12) / 
     +                    dfloat(ny)
              yvar = y - yp
              z12 = z2 + (dfloat(k) - 0.5D+00) * (z1 - z2) / 
     +                    dfloat(nx)
              z34 = z3 + (dfloat(k) - 0.5D+00) * (z4 - z3) / 
     +                    dfloat(nx)
              zp = z12 + (dfloat(l) - 0.5D+00) * (z34 - z12) / 
     +                    dfloat(ny)
              dvar = -zp
              area = (bnd12 + (dfloat(l) - 0.5D+00) * 
     +               (bnd34 - bnd12) / dfloat(ny)) * 
     +               (bnd23 + (dfloat(k) - 0.5D+00) * 
     +               (bnd41 - bnd23) / dfloat(nx))
              if (dvar.le.0.0D+00) goto 133
              rado = dsqrt(xvar**2 + yvar**2 + dvar**2)
              radd = rado + dvar
              pvar = yvar * cdip + dvar * sdip 
              qvar = yvar * sdip - dvar * cdip
              fact1 = dexp(-0.6931D+00 * ((xp**2 + yp**2 + 
     +                     (zp + depth)**2) / sharp**2))
              fact2 = dexp(4.6052D+00 * sing / zp)
              term = xvar * (2.0D+00 * rado + dvar) / 
     +                      (rado**3 * radd**2)
              i4var = -elast * yvar * term 
              i5var = elast * (1.0D+00 / (rado * radd) - xvar * term)
              out(i,j) = out(i,j) - area * fact1 * fact2 * 0.5D+00 *
     +                  (u1var * (3.0D+00 * xvar * dvar * qvar / 
     +                            rado**5 + i4var * sdip) +
     +                   u2var * (3.0D+00 * dvar * pvar * qvar / 
     +                            rado**5 - i5var * sdip * cdip)) / pi
              if ((i.eq.1).and.(j.eq.1)) then
                momt = momt + slip * fact1 * fact2 * area * mu
              endif
  133         continue
  132     continue
c
c  13.6 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
  131 continue
      if (jm.ge.100) write(*,*)
c
c  13.7 Search for the maximum and minimum amplitudes.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 134 j=1,jm
        do 134 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
  134 continue
c
c  13.8 Write data to properties file.
c
      write(203,*) momt, '  earthquake moment'
      write(203,*) fsmin, '  trough amplitude'
      write(203,*) fsmax, '  peak amplitude'
      write(203,*)
c
c  13.9 Write the initial condition grid file.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine charquake
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp 
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  14.1 Enter the number of fault subdivisions in x,y directions.
c
      write(*,*) 'Enter number of x-direction subdivisions (20-200)'
      read(*,*) nx
      write(*,*) 'Enter number of y-direction subdivisions (20-200)'
      read(*,*) ny
      write(*,*)
      nx = iabs(nx)
      ny = iabs(ny)
      if ((nx.gt.200).or.(nx.lt.20)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of nx is unacceptable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((ny.gt.200).or.(ny.lt.20)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of ny is unacceptable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      write(203,*) nx, '  x-direction divisions'
      write(203,*) ny, '  y-direction divisions'
      write(203,*)
c
c  14.2 Compute some constant values for these calculations.
c
      cdip = dcos(dip)
      sdip = dsin(dip)
      elast = 0.5D+00
      u1var = slip * dcos(rake)
      u2var = slip * dsin(rake)
      bnd12 = dabs(x1 - x2) / dfloat(nx)
      bnd23 = dsqrt((z3 - z2)**2 + (y3 - y2)**2) / dfloat(ny)
      bnd34 = dabs(x3 - x4) / dfloat(nx)
      bnd41 = dsqrt((z1 - z4)**2 + (y1 - y4)**2) / dfloat(ny)
c
c  14.3 Estimate the tsunami minimum and maximum amplitude and earthquake moment. 
c
      write(*,*) 'Finding free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      ddx = 1.0D-03 * 2.0D+00 * high
      x = 0.0D+00
      momt = 0.0D+00
      do 141 j=0,1000
        y = -high + dfloat(j) * ddx
        test = 0.0D+00
        do 142 k=1,nx
          do 142 l=1,ny 
            x12 = x2 + (dfloat(k) - 0.5D+00) * (x1 - x2) / 
     +                  dfloat(nx)
            x34 = x3 + (dfloat(k) - 0.5D+00) * (x4 - x3) / 
     +                  dfloat(nx)
            xp = x12 + (dfloat(l) - 0.5D+00) * (x34 - x12) / 
     +                  dfloat(ny)
            xvar = x - xp
            y12 = y2 + (dfloat(k) - 0.5D+00) * (y1 - y2) / 
     +                  dfloat(nx)
            y34 = y3 + (dfloat(k) - 0.5D+00) * (y4 - y3) / 
     +                  dfloat(nx)
            yp = y12 + (dfloat(l) - 0.5D+00) * (y34 - y12) / 
     +                  dfloat(ny)
            yvar = y - yp
            z12 = z2 + (dfloat(k) - 0.5D+00) * (z1 - z2) / 
     +                  dfloat(nx)
            z34 = z3 + (dfloat(k) - 0.5D+00) * (z4 - z3) / 
     +                  dfloat(nx)
            zp = z12 + (dfloat(l) - 0.5D+00) * (z34 - z12) / 
     +                  dfloat(ny)
            dvar = -zp
            area = (bnd12 + (dfloat(l) - 0.5D+00) * 
     +             (bnd34 - bnd12) / dfloat(ny)) * 
     +             (bnd23 + (dfloat(k) - 0.5D+00) * 
     +             (bnd41 - bnd23) / dfloat(nx))
            if (dvar.le.0.0D+00) goto 143
            rado = dsqrt(xvar**2 + yvar**2 + dvar**2)
            radd = rado + dvar
            pvar = yvar * cdip + dvar * sdip 
            qvar = yvar * sdip - dvar * cdip
            fact1 = dexp(-0.6931D+00 * ((xp**2 + yp**2 + 
     +                   (zp + depth)**2) / sharp**2))
            fact2 = dexp(4.6052D+00 * sing / zp)
            term = xvar * (2.0D+00 * rado + dvar) / 
     +                    (rado**3 * radd**2)
            i4var = -elast * yvar * term 
            i5var = elast * (1.0D+00 / (rado * radd) - xvar * term)
            test = test - area * fact1 * fact2 * 0.5D+00 *
     +            (u1var * (3.0D+00 * xvar * dvar * qvar / 
     +                      rado**5 + i4var * sdip) +
     +             u2var * (3.0D+00 * dvar * pvar * qvar / 
     +                      rado**5 - i5var * sdip * cdip)) / pi
            if (j.eq.1) then
              momt = momt + slip * fact1 * fact2 * area * mu
            endif
  143       continue
  142   continue
        if (test.lt.fsmin) fsmin = test
        if (test.gt.fsmax) fsmax = test
  141 continue
c
c  14.4 Write data to properties file.
c
      write(203,*) momt, '  earthquake moment'
      write(203,*) fsmin, '  trough amplitude'
      write(203,*) fsmax, '  peak amplitude'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine splash
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  15.1 Help define the landslide and source characteristics. 
C
      write(*,*) 'Source description follows Walder et al. (2002)'
      write(*,*)
      write(*,*) 'The positive x-axis is in the failure direction'
      write(*,*) 'The x-axis is also a line of mirror symmetry'
      write(*,*)
      write(*,*) 'The depth is a final depth of landslide deposit'
      write(*,*) 'The velocity provides a check on dynamics'
      write(*,*) 'The width provides a check on geometry'
      write(*,*) 'Two tsunami amplitudes are calculated'
      write(*,*) 'The smallest tsunami amplitude is used'
      write(*,*)
      write(*,*) 'The source will be in front of runout length'
      write(*,*) 'The xo,yo position controls source location'
      write(*,*)
      write(203,*) 'Source description follows Walder et al. (2002)'
      write(203,*)
      write(203,*) 'The positive x-axis is in the failure direction'
      write(203,*) 'The x-axis is also a line of mirror symmetry'
      write(203,*)
      write(203,*) 'The depth is a mean depth of landslide deposit'
      write(203,*) 'The velocity provides a check on dynamics'
      write(203,*) 'The width provides a check on geometry'
      write(203,*) 'Two tsunami amplitudes are calculated'
      write(203,*) 'The smallest tsunami amplitude is used'
      write(203,*)
      write(203,*) 'The source will be in front of runout length'
      write(203,*) 'The xo,yo position controls source location'
      write(203,*)
c
c  15.2 You choose the configuration of the subaerial landslide. 
c
      write(*,*) 'Enter the water depth near end of landslide (m)'
      read(*,*) d
      write(*,*) 'Enter the landslide volume within water (m^3)'
      read(*,*) vol
      write(*,*) 'Enter landslide velocity at shoreline (m/s)'
      read(*,*) ut
      write(*,*) 'Enter landslide runout length in water (m)'
      read(*,*) so
      write(*,*) 'Enter landslide runout time in water (s)'
      read(*,*) to         
      write(*,*) 'Enter the landslide width at shoreline (m)'
      read(*,*) w
      write(*,*)
c
c  15.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      vol = dabs(vol)
      ut = dabs(ut)         
      so = dabs(so)
      to = dabs(to)
      w = dabs(w)
c
c  15.4 Check that the inputs are physically possible.  
c         
      if (to.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The runout time cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (so.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The runout length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The water depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (vol.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide volume cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (w.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide width cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (ut.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide velocity cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  15.5 Compute subaerial landslide tsunami characteristics.
c
      write(*,*) 'Calculating landslide properties'
      write(*,*)
      ao = -ut / to
      fr = ut / dsqrt(g * d)
      lambda = 0.27D+00 * to * dsqrt(g * d)
      test = 0.86D+00 * d
      etal = 1.32D+00 * d * (vol * 2.0D+00 * ut / 
     +                      (pi * w * so * d * dsqrt(d * g)))**0.68
      etat = 1.32D+00 * d * (vol / 
     +                      (w * to * d * dsqrt(d * g)))**0.68
      xmin = so + lambda
      delx = lambda
c
c  15.6 Choose the smallest amplitude for two measures of time.
c
      if (etal.lt.etat) then
        eta = etal
        qs = vol * 2.0D+00 * ut / (pi * w * so * d * dsqrt(d * g))
        tm = pi * so * dsqrt(g / d) / (2.0D+00 * ut)
      else
        eta = etat
        qs = vol / (w * to * d * dsqrt(d * g))
        tm = to * dsqrt(g / d)
      endif
c
c  15.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  15.8 Check the calculations for accuracy.
c
      if (eta.gt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave amplitude is excessively large'
        write(*,*) 'The amplitude will be decreased to 0.86*depth'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          eta = test
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The wave amplitude is excessively large'
          write(203,*) 'The amplitude will be decreased to 0.86*depth'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * dabs(etal - etat) / (etal + etat)
      if (test.gt.0.4D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The two wave amplitudes differ more than 40%'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The two wave amplitudes differ more than 40%'
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (fr.gt.4.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Froude number is >4  ', fr
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The Froude number is >4  ', fr
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (fr.lt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Froude number is <1  ', fr
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The Froude number is <1  ', fr
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * to * ut / (so * pi)
      if ((test.lt.0.4D+00).or.(test.gt.2.5D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Runout length and time differ  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'Runout length and time differ  ', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = to / (4.5D+00 * dsqrt(1.0D+01 * dsqrt(vol / w) / g))
      if ((test.lt.0.3D+00).or.(test.gt.3.3D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Runout length and time differ  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'Runout length and time differ  ', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 3.4D+00 * vol**(1.0D+00 / 3.0D+00) / w
      if ((test.lt.0.25D+00).or.(test.gt.4.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Landslide width may be unusual  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'Landslide width may be unusual  ', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  15.9 Calculate quantities that determine tsunami shape. 
c
      zmax = eta
      nmax = zmax
      wid = lambda
c
c  15.10 Write data to properties file.
c
      write(203,*) d, '  typical final depth'
      write(203,*) vol, '  submerged landslide volume'
      write(203,*) ut, '  impact velocity'
      write(203,*) so, '  runout length'
      write(203,*) to, '  runout time'
      write(203,*) w, '  landslide width at shore'
      write(203,*)
      write(203,*) ao, '  landslide deceleration'
      write(203,*) lambda, '  wavelength'
      write(203,*) fr, '  impact Froude number'
      write(203,*) qs, '  nondimensional flux'
      write(203,*) tm, '  nondimensional time'
      write(203,*)
      write(203,*) etal, '  amplitude from runout length'
      write(203,*) etat, '  amplitude from runout time'
      write(203,*) eta, '  characteristic wave amplitude'
      write(203,*) 0.85D+00 * d, '  maximum possible amplitude'
      write(203,*) xmin, '  peak position'
      write(203,*) delx, '  peak width'
      write(203,*) zmax, '  peak argument'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setcoast
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  16.1 Identify the shoreline position of landslide on the grid.  
c
      write(*,*) 'The next coordinate pair is in the grid units'
      write(*,*)
      write(*,*) 'Enter x-axis landslide entry at shoreline xo'
      read(*,*) xo
      write(*,*) 'Enter y-axis landslide entry at shoreline yo'
      read(*,*) yo
      write(*,*) 'The next two angles are measured counterclockwise'
      write(*,*)
      write(*,*) 'Enter CCW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CCW angle of failure in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  16.2 Verify that the landslide is within the grid and underwater.
c
      if ((xo.gt.xe).or.(xo.lt.xw)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of xo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((yo.gt.yn).or.(yo.lt.ys)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of yo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      ic = nint((xo - xw) / xinc)
      jc = nint((yo - ys) / yinc)
      test = dmax / 1.0D+02
      if (test.lt.dabs(dx + dy)) test = dabs(dx + dy)
      if (dabs(grd(ic,jc)).gt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The indicated depth is  ',  grd(ic,jc)
        write(*,*) 'This does not appear to be the shoreline'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The indicated depth is  ',  grd(ic,jc)
          write(203,*) 'This does not appear to be the shoreline'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  16.3 Convert degrees to radians and compute the cosine and sine. 
c
      alpha = alpha * pi / 1.8D+02
      alfao = alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  16.4 Write data to properties file.
c
      write(203,*) xo, '  landslide longitude'
      write(203,*) yo, '  landslide latitude'
      write(203,*) 1.8D+02 * alfao / pi, '  orientation of north'
      write(203,*) 1.8D+02 * alpha / pi, '  orientation from north'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridcoast
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  17.1 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 171 j=1,jm
        do 171 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  17.2 Evaluate tsunami shape in local x,y coordinates.
c
          term = 4.0D+00 * nmax * (w + wid) * delx / vol
          if (((w + wid) / term).lt.delx) then
            term = (w + wid) / delx
            nmax = term * vol / (4.0D+00 * (w + wid) * delx)
          endif
          out(i,j) = nmax *
     +      (2.0D+00 / (dexp(term * y / (w + wid)) +
     +                 dexp(-term * y / (w + wid))))**2 *
     +      (2.0D+00 / (dexp((x - xmin) / delx) +
     +                 dexp(-(x - xmin) / delx)))**2
c
c  17.3 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
  171 continue
c
c  17.4 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the u velocities'
      write(*,*)
      term = nmax / d
      do 176 j=1,jm
        do 176 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            xxx = xxx + xinc / 2.0D+00
          endif
          if (sim.eq.1) then
            xxx = xxx
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  17.5 Evaluate tsunami u velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (i.lt.im) then
              fact1 = (out(i+1,j) + out(i,j)) / 2.0D+00
              fact2 = (grd(i+1,j) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i-1,j)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i-1,j)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 - 0.01334D+00 * term) * fact1 / d
     +                 - 0.97999D+00 * (fact1 / d)**2)
          endif
          if (sim.eq.1) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 + 0.17006D+00 * term) * fact1 / d
     +                 - 1.25509D+00 * (fact1 / d)**2)
          endif
          uvel(i,j) =  -utot * sna
c
c  17.6 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = uvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            uvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = 0.0D+00
            endif
          endif
  176 continue
c
c  17.7 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the v velocities'
      write(*,*)
      do 177 j=1,jm
        do 177 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            yyy = yyy + yinc / 2.0D+00
          endif
          if (sim.eq.1) then
            yyy = yyy
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  17.8 Evaluate tsunami v velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (j.lt.jm) then
              fact1 = (out(i,j+1) + out(i,j)) / 2.0D+00
              fact2 = (grd(i,j+1) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i,j-1)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i,j-1)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 - 0.01334D+00 * term) * fact1 / d
     +                 - 0.97999D+00 * (fact1 / d)**2)
          endif
          if (sim.eq.1) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 + 0.17006D+00 * term) * fact1 / d
     +                 - 1.25509D+00 * (fact1 / d)**2)
          endif
          vvel(i,j) = utot * cna
c
c  17.9 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = vvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            vvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = 0.0D+00
            endif
          endif
  177 continue
c
c  17.10 Search for the maxima and minima.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 173 j=1,jm
        do 173 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
  173 continue
      umax = 0.0D+00
      umin = 0.0D+00
      do 174 j=1,jm
        do 174 i=1,im
          if (uvel(i,j).lt.umin) then
            umin = uvel(i,j)
          endif
          if (uvel(i,j).gt.umax) then
            umax = uvel(i,j)
          endif
  174 continue
      vmax = 0.0D+00
      vmin = 0.0D+00
      do 175 j=1,jm
        do 175 i=1,im
          if (vvel(i,j).lt.vmin) then
            vmin = vvel(i,j)
          endif
          if (vvel(i,j).gt.vmax) then
            vmax = vvel(i,j)
          endif
  175 continue
c
c  17.11 Write data to properties file.
c
      write(203,*) fsmin, '  trough amplitude'
      write(203,*) fsmax, '  peak amplitude'
      write(203,*) umin, '  trough u velocity'
      write(203,*) umax, '  peak u velocity'
      write(203,*) vmin, '  trough v velocity'
      write(203,*) vmax, '  peak v velocity'
      write(203,*)
c
c  17.12 Write the initial condition grid file.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine charcoast
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper 
c
c  --------------------------------------------------------------------
c
c  18.1 Calculate the tsunami minimum and maximum amplitude. 
c
      write(*,*) 'Finding free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      ddx = 0.2D-03 * xmin * 5.0D+00
      do 181 j=0,5000
        x = dfloat(j) * ddx - 2.0D+00 * xmin
        term = 4.0D+00 * nmax * (w + wid) * delx / vol
        if (((w + wid) / term).lt.delx) then
          term = (w + wid) / delx
          nmax = term * vol / (4.0D+00 * (w + wid) * delx)
        endif
        test = nmax *
     +      (2.0D+00 / (dexp((x - xmin) / delx) +
     +                 dexp(-(x - xmin) / delx)))**2
        if (test.lt.fsmin) fsmin = test
        if (test.gt.fsmax) fsmax = test
  181 continue
c
c  18.2 Write data to properties file.
c
      write(203,*) fsmin, '  trough amplitude'
      write(203,*) fsmax, '  peak amplitude'
      write(203,*) 
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridwrite
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  19.1 Write the free surface grid file in the chosen grid format.
c
      write(*,*) 'Writing tsunami grid files'
      write(*,*)
      if (type.eq.0) then
        write(201,208)
        write(201,211) im,jm
        write(201,210) xw,xe
        write(201,210) ys,yn
        write(201,210) fsmin,fsmax
        do 192 j=1,jm
          write(201,209)(out(i,j),i=1,im)
 192    continue
      endif
      close (201)
c
c  19.2 Write the u-velocity grid file in the chosen grid format.
c
      if (fail.ne.0) then
        if (type.eq.0) then
          write(204,208)
          write(204,211) im,jm
          write(204,210) xw,xe
          write(204,210) ys,yn
          write(204,210) umin,umax
          do 194 j=1,jm
            write(204,209)(uvel(i,j),i=1,im)
 194      continue
        endif
        close (204)
      endif
c
c  19.3 Write the v-velocity grid file in the chosen grid format.
c
      if (fail.ne.0) then
        if (type.eq.0) then
          write(205,208)
          write(205,211) im,jm
          write(205,210) xw,xe
          write(205,210) ys,yn
          write(205,210) vmin,vmax
          do 196 j=1,jm
            write(205,209)(vvel(i,j),i=1,im)
 196      continue
        endif
        close (205)
      endif
c
c  19.4 Format statements for the grid
c
  208 format('DSAA')
  209 format(4f15.6)
  210 format(2f20.6)
  211 format(2i20)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setwave
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  20.1 Identify the wave train entrance edge on the grid.  
c
      write(*,*) 'Type 0 for wave train entry at grid top'
      write(*,*) 'Type 1 for wave train entry at grid left'
      write(*,*) 'Type 2 for wave train entry at grid bottom'
      write(*,*) 'Type 3 for wave train entry at grid right'
      read(*,*) itrain
      write(*,*)
      if ((itrain.lt.0).or.(itrain.gt.3)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  20.2 Identify the wave train number of wavelengths.  
c
      write(*,*) 'Enter the number of complete wavelengths'
      read(*,*) nwave
      write(*,*)
      if (nwave.le.0) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  20.3 Identify the wave train leading wave polarity.  
c
      write(*,*) 'Type 0 for a leading depression wave'
      write(*,*) 'Type 1 for a leading elevation wave'
      read(*,*) lwave
      if ((lwave.ne.0).and.(lwave.ne.1)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  20.4 Identify the wave train propagation direction.  
c
      write(*,*) 'The next two angles are measured counterclockwise'
      write(*,*)
      write(*,*) 'Enter CCW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CCW angle of wave front in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  20.5 Verify that the angles are reasonable.
c
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (alpha.lt.0.0D+00) alpha = alpha + 3.6D+02
      test = dfloat(itrain) * 90.0D+00 - 90.0D+00
      if (test.lt.-3.6D+02) test = test + 3.6D+02
      if (test.lt.0.0D+00) test = test + 3.6D+02
      if (alpha.gt.(test-10.0D+00)) then
      if (alpha.lt.(test+10.0D+00)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'Wave front is <10 degrees perpendicular to edge'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      endif
      test = dfloat(itrain) * 90.0D+00 - 270.0D+00
      if (test.lt.-3.6D+02) test = test + 3.6D+02
      if (test.lt.0.0D+00) test = test + 3.6D+02
      if (alpha.gt.(test-10.0D+00)) then
      if (alpha.lt.(test+10.0D+00)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'Wave front is <10 degrees perpendicular to edge'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      endif
      if (itrain.eq.0) then
        if ((alpha.ge.9.0D+01).and.(alpha.lt.1.8D+02)) then
          alpha = alpha + 1.8D+02
        endif
        if ((alpha.ge.1.8D+02).and.(alpha.lt.2.7D+02)) then
          alpha = alpha - 1.8D+02
        endif         
      endif
      if (itrain.eq.1) then
        if (alpha.gt.1.8D+02) then
          alpha = alpha - 1.8D+02
        endif 
      endif
      if (itrain.eq.2) then
        if (alpha.lt.9.0D+01) then
          alpha = alpha + 1.8D+02
        endif
        if (alpha.gt.2.7D+02) then
          alpha = alpha - 1.8D+02
        endif 
      endif
      if (itrain.eq.3) then
        if (alpha.lt.1.8D+02) then
          alpha = alpha + 1.8D+02
        endif 
      endif
c
c  20.6 Convert degrees to radians and compute the cosine and sine. 
c
      alpha = alpha * pi / 1.8D+02
      alfao = alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  20.7 Write data to properties file.
c
      write(203,*) itrain, '  wave train edge index'
      write(203,*) nwave, '  number of wavelengths'
      write(203,*) lwave, '  leading wave polarity'
      write(203,*)
      write(203,*) 1.8D+02 * alfao / pi, '  orientation of north'
      write(203,*) 1.8D+02 * alpha / pi, '  orientation from north'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine wave
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  21.1 Help define the wave train description. 
C
      write(*,*) 'Wave train follows linear theory in Mei (1989)'
      write(*,*)
      write(*,*) 'The wave train has a finite number of waves'
      write(*,*) 'The wave train is entering the domain'
      write(*,*) 'The wave train length is user defined'
      write(*,*)
      write(*,*) 'The depth is a mean depth below waves'
      write(*,*)
      write(*,*) 'The end of the wave train will be at the edge'
      write(*,*) 'The wave train will extend over entire edge'
      write(*,*)
      write(203,*) 'Wave train follows linear theory in Mei (1989)'
      write(203,*)
      write(203,*) 'The wave train has a finite number of waves'
      write(203,*) 'The wave train is entering the domain'
      write(203,*) 'The wave train length is user defined'
      write(203,*)
      write(203,*) 'The depth is a mean depth below waves'
      write(203,*)
      write(203,*) 'The end of the wave train will be at the edge'
      write(203,*) 'The wave train will extend over entire edge'
      write(203,*)
c
c  21.2 You choose the configuration of the wave train. 
c
      write(*,*) 'Enter mean depth below wave train (m)'
      read(*,*) d
      write(*,*) 'Enter maximum wave train amplitude (m)'
      read(*,*) eta
      write(*,*) 'Enter wavelength of a single wave (m)'
      read(*,*) lambda      
      write(*,*) 'Enter period of a single wave (s)'
      read(*,*) to
      write(*,*)
c
c  21.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      eta = dabs(eta)
      lambda = dabs(lambda)
      to = dabs(to)
c
c  21.4 Check that the inputs are physically possible.  
c
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The mean depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (eta.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The amplitude must exist for a wave'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (lambda.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The wavelength cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (to.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The period cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  21.5 Calculate all necessary wave properties.
c
      write(*,*) 'Calculating wave train properties'
      write(*,*)
      kappa = 2.0D+00 * pi / lambda
      w = 2.0D+00 * pi / to
      term = kappa * d
      omega = dsqrt(g * kappa * dtanh(term))
      if (itrain.eq.0) then
        dist = dfloat(nwave) * lambda *dabs(cna) * yinc / dy
        shift = lambda *dabs(cna) * yinc / dy
        xo = (xw + xe) / 2.0D+00 
        yo = yn
      endif
      if (itrain.eq.1) then
        dist = dfloat(nwave) * lambda *dabs(sna) * xinc / dx
        shift = lambda *dabs(sna) * xinc / dx
        xo = xw 
        yo = (yn + ys) / 2.0D+00
      endif
      if (itrain.eq.2) then
        dist = dfloat(nwave) * lambda *dabs(cna) * yinc / dy
        shift = lambda *dabs(cna) * yinc / dy
        xo = (xw + xe) / 2.0D+00 
        yo = ys
      endif
      if (itrain.eq.3) then
        dist = dfloat(nwave) * lambda *dabs(sna) * xinc / dx
        shift = lambda *dabs(sna) * xinc / dx
        xo = xe
        yo = (yn + ys) / 2.0D+00
      endif
      if (lwave.eq.0) dphi = 0.0D+00
      if (lwave.eq.1) dphi = pi
c
c  21.6 Compute some tsunami characteristics.
c
      steep = eta / lambda
      linear = eta / d
      shall = lambda / d
      delx = 0.5D+00 * lambda
c
c  21.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  21.8 Check the calculations for accuracy.
c
      if (linear.gt.0.86D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The amplitude is greater than 0.86*depth'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif        
      if (linear.gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The amplitude is greater than 0.2*depth'
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The amplitude is greater than 0.2*depth'
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * dabs(omega - w) / (omega + w)
      if (test.gt.0.5D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave period is off by more than 50%'
        write(*,*) 'The design wave should be recomputed'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The wave period is off by more than 50%'
          write(203,*) 'The design wave should be recomputed'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      ic = nint((xo - xw) / xinc)
      jc = nint((yo - ys) / yinc)
      if (ic.lt.1) ic = 1
      if (ic.gt.im) ic = im
      if (jc.lt.1) jc = 1
      if (jc.gt.jm) jc = jm
      if (grd(ic,jc).le.0.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The middle of wave train edge on land'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The middle of wave train edge on land'
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (test.gt.0.1D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave period is off by more than 10%'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The wave period is off by more than 10%'
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((dist + shift).ge.dabs(xe - xw)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave train is longer than grid width'
        write(*,*) 'The design wave should be recomputed'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The wave train is longer than grid width'
          write(203,*) 'The design wave should be recomputed'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif   
      if ((dist + shift).ge.dabs(yn - ys)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave train is longer than grid height'
        write(*,*) 'The design wave should be recomputed'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The wave train is longer than grid height'
          write(203,*) 'The design wave should be recomputed'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif   
c
c  21.9 Write data to properties file.
c
      write(203,*) d, '  mean depth'
      write(203,*) eta, '  maximum wave amplitude'
      write(203,*) lambda, '  wavelength'
      write(203,*) to, '  wave period'
      write(203,*)
      write(203,*) kappa, '  wave number'
      write(203,*) omega, '  circular frequency'
      write(203,*) w,'  given frequency'
      write(203,*) dphi, '  wave train phase'
      write(203,*)
      write(203,*) xo, '  wave train x origin'
      write(203,*) yo, '  wave train y origin'
      write(203,*) dist, '  penetration distance'
      write(203,*) shift, '  buffer distance'
      write(203,*)
      write(203,*) steep, '  amplitude over wavelength'
      write(203,*) linear, '  amplitude over depth'
      write(203,*) shall, '  wavelength over depth'
      write(203,*) delx, '  trough to peak distance'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridwave
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  22.1 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 221 j=1,jm
        do 221 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  22.2 Evaluate tsunami shape in local x,y coordinates.
c
          test = -dsin(kappa * x + dphi)
          out(i,j) = eta * test
c
c  22.3 Modify the initial condition based on distance from edge.
c
          test = 1.0D+00
          if (itrain.eq.0) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.1) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.2) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.3) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          out(i,j) = out(i,j) * test
c
c  22.4 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
  221 continue
c
c  22.5 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the u velocities'
      write(*,*)
      do 226 j=1,jm
        do 226 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            xxx = xxx + xinc / 2.0D+00
          endif
          if (sim.eq.1) then
            xxx = xxx
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  22.6 Evaluate tsunami shape in local x,y coordinates.
c
          test = -dsin(kappa * x + dphi)
          if (sim.eq.0) then
            utot = eta * g * dtanh(term) * test / (d * omega)
          endif
          if (sim.eq.1) then
            utot = eta * g * kappa * dcosh(0.469D+00 * term) * 
     +             test / (omega * dcosh(term))
          endif
          uvel(i,j) =  utot * sna
c
c  22.7 Modify the initial condition based on distance from edge.
c
          test = 1.0D+00
          if (itrain.eq.0) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.1) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.2) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.3) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          uvel(i,j) = uvel(i,j) * test
c
c  22.8 Modify the initial condition based on bathy and topo data.
c
          if (sim.eq.0) then
            if (i.lt.im) then
              fact1 = (out(i+1,j) + out(i,j)) / 2.0D+00
              fact2 = (grd(i+1,j) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i-1,j)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i-1,j)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = uvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            uvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = 0.0D+00
            endif
          endif
  226 continue
c
c  22.9 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the v velocities'
      write(*,*)
      do 227 j=1,jm
        do 227 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            yyy = yyy + yinc / 2.0D+00
          endif
          if (sim.eq.1) then
            yyy = yyy
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  22.10 Evaluate tsunami shape in local x,y coordinates.
c
          test = -dsin(kappa * x + dphi)
          if (sim.eq.0) then
            utot = eta * g * dtanh(term) * test / (d * omega)
          endif
          if (sim.eq.1) then
            utot = eta * g * kappa * dcosh(0.469D+00 * term) * 
     +             test / (omega * dcosh(term))
          endif
          vvel(i,j) = -utot * cna
c
c  22.11 Modify the initial condition based on distance from edge.
c
          test = 1.0D+00
          if (itrain.eq.0) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.1) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.2) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.3) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          vvel(i,j) = vvel(i,j) * test
c
c  22.12 Modify the initial condition based on bathy and topo data.
c
          if (sim.eq.0) then
            if (j.lt.jm) then
              fact1 = (out(i,j+1) + out(i,j)) / 2.0D+00
              fact2 = (grd(i,j+1) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i,j-1)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i,j-1)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = vvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            vvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = 0.0D+00
            endif
          endif
  227 continue
c
c  22.13 Search for the maxima and minima.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 223 j=1,jm
        do 223 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
  223 continue
      umax = 0.0D+00
      umin = 0.0D+00
      do 224 j=1,jm
        do 224 i=1,im
          if (uvel(i,j).lt.umin) then
            umin = uvel(i,j)
          endif
          if (uvel(i,j).gt.umax) then
            umax = uvel(i,j)
          endif
  224 continue
      vmax = 0.0D+00
      vmin = 0.0D+00
      do 225 j=1,jm
        do 225 i=1,im
          if (vvel(i,j).lt.vmin) then
            vmin = vvel(i,j)
          endif
          if (vvel(i,j).gt.vmax) then
            vmax = vvel(i,j)
          endif
  225 continue
c
c  22.14 Write data to properties file.
c
      write(203,*) fsmin, '  trough amplitude'
      write(203,*) fsmax, '  peak amplitude'
      write(203,*) umin, '  trough u velocity'
      write(203,*) umax, '  peak u velocity'
      write(203,*) vmin, '  trough v velocity'
      write(203,*) vmax, '  peak v velocity'
      write(203,*)
c
c  22.15 Write the initial condition grid files.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setflow
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  23.1 Identify the shoreline position of flow on the grid.  
c
      write(*,*) 'The next coordinate pair is in the grid units'
      write(*,*)
      write(*,*) 'Enter x-axis of flow entry at shoreline xo'
      read(*,*) xo
      write(*,*) 'Enter y-axis of flow entry at shoreline yo'
      read(*,*) yo
      write(*,*) 'The next two angles are measured counterclockwise'
      write(*,*)
      write(*,*) 'Enter CCW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CCW angle of flow in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  23.2 Verify that the flow is within the grid and underwater.
c
      if ((xo.gt.xe).or.(xo.lt.xw)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of xo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((yo.gt.yn).or.(yo.lt.ys)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of yo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      ic = nint((xo - xw) / xinc)
      jc = nint((yo - ys) / yinc)
      test = dmax / 1.0D+02
      if (test.lt.dabs(dx + dy)) test = dabs(dx + dy)
      if (dabs(grd(ic,jc)).gt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The indicated depth is  ',  grd(ic,jc)
        write(*,*) 'This does not appear to be the shoreline'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The indicated depth is  ',  grd(ic,jc)
          write(203,*) 'This does not appear to be the shoreline'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  23.3 Convert degrees to radians and compute the cosine and sine. 
c
      alpha = alpha * pi / 1.8D+02
      alfao = alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  23.4 Write data to properties file.
c
      write(203,*) xo, '  pyroclastic flow longitude'
      write(203,*) yo, '  pyroclastic flow latitude'
      write(203,*) 1.8D+02 * alfao / pi, '  orientation of north'
      write(203,*) 1.8D+02 * alpha / pi, '  orientation from north'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine pyro
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  24.1 Help define the flow and source characteristics. 
C
      write(*,*) 'Source follows Watts and Waythomas (2002)'
      write(*,*)
      write(*,*) 'The positive x-axis is in the flow direction'
      write(*,*) 'The x-axis is also a line of mirror symmetry'
      write(*,*)
      write(*,*) 'Consider submerged motion of pyroclastic flow'
      write(*,*) 'The depth is a final depth of flow deposit'
      write(*,*) 'The velocity provides a check on dynamics'
      write(*,*) 'The width provides a check on geometry'
      write(*,*) 'Two tsunami amplitudes are calculated'
      write(*,*) 'The smallest tsunami amplitude is used'
      write(*,*)
      write(*,*) 'The source will be in front of runout length'
      write(*,*) 'The xo,yo position controls source location'
      write(*,*)
      write(203,*) 'Source follows Watts and Waythomas (2002)'
      write(203,*)
      write(203,*) 'The positive x-axis is in the flow direction'
      write(203,*) 'The x-axis is also a line of mirror symmetry'
      write(203,*)
      write(203,*) 'Consider submerged motion of pyroclastic flow'
      write(203,*) 'The depth is a mean depth of flow deposit'
      write(203,*) 'The velocity provides a check on dynamics'
      write(203,*) 'The width provides a check on geometry'
      write(203,*) 'Two tsunami amplitudes are calculated'
      write(203,*) 'The smallest tsunami amplitude is used'
      write(203,*)
      write(203,*) 'The source will be in front of runout length'
      write(203,*) 'The xo,yo position controls source location'
      write(203,*)
c
c  24.2 You choose the configuration of the pyroclastic flow. 
c
      write(*,*) 'Enter the water depth near end of flow (m)'
      read(*,*) d
      write(*,*) 'Enter submerged volume in format 1.0D+11 (m^3)'
      read(*,*) vol
      write(*,*) 'Enter flow velocity at shoreline (m/s)'
      read(*,*) ut
      write(*,*) 'Enter flow runout length under water (m)'
      read(*,*) so
      write(*,*) 'Enter flow runout time under water (s)'
      read(*,*) to         
      write(*,*) 'Enter the flow width at the shoreline (m)'
      read(*,*) w
      write(*,*)
c
c  24.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      vol = dabs(vol)
      ut = dabs(ut)         
      so = dabs(so)
      to = dabs(to)
      w = dabs(w)
c
c  24.4 Check that the inputs are physically possible.  
c         
      if (to.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The runout time cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (so.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The runout length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The water depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (vol.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The flow volume cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (w.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The flow width cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (ut.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The flow velocity cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  24.5 Compute the pyroclastic flow tsunami characteristics.
c
      write(*,*) 'Calculating pyroclastic flow properties'
      write(*,*)
      ao = -ut / to
      fr = ut / dsqrt(g * d)
      lambda = 0.27D+00 * to * dsqrt(g * d)
      test = 0.86D+00 * d
      etal = 1.32D+00 * d * (vol * 2.0D+00 * ut / 
     +                      (pi * w * so * d * dsqrt(d * g)))**0.68
      etat = 1.32D+00 * d * (vol / 
     +                      (w * to * d * dsqrt(d * g)))**0.68
      xmin = so + lambda
      delx = lambda
c
c  24.6 Choose the smallest amplitude for two measures of time.
c
      if (etal.lt.etat) then
        eta = etal
        qs = vol * 2.0D+00 * ut / (pi * w * so * d * dsqrt(d * g))
        tm = pi * so * dsqrt(g / d) / (2.0D+00 * ut)
      else
        eta = etat
        qs = vol / (w * to * d * dsqrt(d * g))
        tm = to * dsqrt(g / d)
      endif
c
c  24.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  24.8 Check the calculations for accuracy.
c
      if (eta.gt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave amplitude is excessively large'
        write(*,*) 'The amplitude will be decreased to 0.86*depth'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          eta = test
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The wave amplitude is excessively large'
          write(203,*) 'The amplitude will be decreased to 0.86*depth'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * dabs(etal - etat) / (etal + etat)
      if (test.gt.0.4D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The two wave amplitudes differ more than 40%'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The two wave amplitudes differ more than 40%'
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (fr.gt.4.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Froude number is >4  ', fr
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The Froude number is >4  ', fr
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (fr.lt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Froude number is <1  ', fr
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'The Froude number is <1  ', fr
          write(203,*) 'This will make the amplitude inaccurate'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * to * ut / (so * pi)
      if ((test.lt.0.4D+00).or.(test.gt.2.5D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Runout length and time differ  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'Runout length and time differ  ', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = to / (4.5D+00 * dsqrt(1.0D+01 * dsqrt(vol / w) / g))
      if ((test.lt.0.3D+00).or.(test.gt.3.3D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Runout length and time differ  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'Runout length and time differ  ', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 30.0D+00 * vol**(1.0D+00 / 3.0D+00) / w
      if ((test.lt.0.25D+00).or.(test.gt.4.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Flow width may be unusual  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(203,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(203,*) 'Flow width may be unusual  ', test
          write(203,*) 'This will make the results questionable'
          write(203,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  24.9 Calculate quantities that determine tsunami shape. 
c
      zmax = eta
      nmax = zmax
      wid = lambda
c
c  24.10 Write data to properties file.
c
      write(203,*) d, '  typical final depth'
      write(203,*) vol, '  submerged flow volume'
      write(203,*) ut, '  impact velocity'
      write(203,*) so, '  runout length'
      write(203,*) to, '  runout time'
      write(203,*) w, '  flow width at shore'
      write(203,*)
      write(203,*) ao, '  underwater deceleration'
      write(203,*) lambda, '  wavelength'
      write(203,*) fr, '  impact Froude number'
      write(203,*) qs, '  nondimensional flux'
      write(203,*) tm, '  nondimensional time'
      write(203,*)
      write(203,*) etal, '  amplitude from runout length'
      write(203,*) etat, '  amplitude from runout time'
      write(203,*) eta, '  characteristic wave amplitude'
      write(203,*) 0.85D+00 * d, '  maximum possible amplitude'
      write(203,*) xmin, '  peak position'
      write(203,*) delx, '  peak width'
      write(203,*) zmax, '  peak argument'
      write(203,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridflow
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  25.1 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 251 j=1,jm
        do 251 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  25.2 Evaluate tsunami shape in local x,y coordinates.
c
          term = 4.0D+00 * nmax * (w + wid) * delx / vol
          if (((w + wid) / term).lt.delx) then
            term = (w + wid) / delx
            nmax = term * vol / (4.0D+00 * (w + wid) * delx)
          endif
          out(i,j) = nmax *
     +      (2.0D+00 / (dexp(term * y / (w + wid)) +
     +                 dexp(-term * y / (w + wid))))**2 *
     +      (2.0D+00 / (dexp((x - xmin) / delx) +
     +                 dexp(-(x - xmin) / delx)))**2
c
c  25.3 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
  251 continue
c
c  25.4 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the u velocities'
      write(*,*)
      term = nmax / d
      do 256 j=1,jm
        do 256 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            xxx = xxx + xinc / 2.0D+00
          endif
          if (sim.eq.1) then
            xxx = xxx
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  25.5 Evaluate tsunami u velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (i.lt.im) then
              fact1 = (out(i+1,j) + out(i,j)) / 2.0D+00
              fact2 = (grd(i+1,j) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i-1,j)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i-1,j)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 - 0.01334D+00 * term) * fact1 / d
     +                 - 0.97999D+00 * (fact1 / d)**2)
          endif
          if (sim.eq.1) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 + 0.17006D+00 * term) * fact1 / d
     +                 - 1.25509D+00 * (fact1 / d)**2)
          endif
          uvel(i,j) =  -utot * sna
c
c  25.6 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = uvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            uvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = 0.0D+00
            endif
          endif
  256 continue
c
c  25.7 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the v velocities'
      write(*,*)
      do 257 j=1,jm
        do 257 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            yyy = yyy + yinc / 2.0D+00
          endif
          if (sim.eq.1) then
            yyy = yyy
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  25.8 Evaluate tsunami v velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (j.lt.jm) then
              fact1 = (out(i,j+1) + out(i,j)) / 2.0D+00
              fact2 = (grd(i,j+1) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i,j-1)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i,j-1)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 - 0.01334D+00 * term) * fact1 / d
     +                 - 0.97999D+00 * (fact1 / d)**2)
          endif
          if (sim.eq.1) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 + 0.17006D+00 * term) * fact1 / d
     +                 - 1.25509D+00 * (fact1 / d)**2)
          endif
          vvel(i,j) = utot * cna
c
c  25.9 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = vvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            vvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = 0.0D+00
            endif
          endif
  257 continue
c
c  25.10 Search for the maxima and minima.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 253 j=1,jm
        do 253 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
  253 continue
      umax = 0.0D+00
      umin = 0.0D+00
      do 254 j=1,jm
        do 254 i=1,im
          if (uvel(i,j).lt.umin) then
            umin = uvel(i,j)
          endif
          if (uvel(i,j).gt.umax) then
            umax = uvel(i,j)
          endif
  254 continue
      vmax = 0.0D+00
      vmin = 0.0D+00
      do 255 j=1,jm
        do 255 i=1,im
          if (vvel(i,j).lt.vmin) then
            vmin = vvel(i,j)
          endif
          if (vvel(i,j).gt.vmax) then
            vmax = vvel(i,j)
          endif
  255 continue
c
c  25.11 Write data to properties file.
c
      write(203,*) fsmin, '  trough amplitude'
      write(203,*) fsmax, '  peak amplitude'
      write(203,*) umin, '  trough u velocity'
      write(203,*) umax, '  peak u velocity'
      write(203,*) vmin, '  trough v velocity'
      write(203,*) vmax, '  peak v velocity'
      write(203,*)
c
c  25.12 Write the initial condition grid file.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine charflow
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper 
c
c  --------------------------------------------------------------------
c
c  26.1 Calculate the tsunami minimum and maximum amplitude. 
c
      write(*,*) 'Finding free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      ddx = 0.2D-03 * xmin * 5.0D+00
      do 261 j=0,5000
        x = dfloat(j) * ddx - 2.0D+00 * xmin
        term = 4.0D+00 * nmax * (w + wid) * delx / vol
        if (((w + wid) / term).lt.delx) then
          term = (w + wid) / delx
          nmax = term * vol / (4.0D+00 * (w + wid) * delx)
        endif
        test = nmax *
     +      (2.0D+00 / (dexp((x - xmin) / delx) +
     +                 dexp(-(x - xmin) / delx)))**2
        if (test.lt.fsmin) fsmin = test
        if (test.gt.fsmax) fsmax = test
  261 continue
c
c  26.2 Write data to properties file.
c
      write(203,*) fsmin, '  trough amplitude'
      write(203,*) fsmax, '  peak amplitude'
      write(203,*) 
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine separate
c
      parameter(iq=800,jq=800,nq=8001,ngm=30)
c
      real*8 ttsrc
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,pi,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 dx,dy,wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(iq,jq)
      dimension uvel(iq,jq), vvel(iq,jq)
c
      common/block1/  out,g,so,b,r,t 
      common/block2/  alpha,alfao,xo,yo,cna,sna,x,y,xx,yy,xxx,yyy
c      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt
      common/block7/  term,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm
      common/block15/ ic,jc,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,sim
c      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,itmp
c
      common/geowv1/grd(iq,jq),pi,test,dmax,dmin,to
      common/geowv2/tsrc,d,dep,lambda,xlon,ylat
      common/geowv3/flag,fail,iden,idep,grid,type,nsrc,im,jm,input
      common/geowv8/dx,dy,dt,xw,xe,ys,yn,xinc,yinc,output,proper
c
c  --------------------------------------------------------------------
c
c  27.1 Help define the source requirements. 
C
      write(*,*) 'The tsunami source may be user customized'
      write(*,*) 'Or the source may resume a prior simulation'
      write(*,*) 'Either way three grid files will be needed'
      write(*,*)
      write(*,*) 'The first is a free surface initial condition'
      write(*,*) 'The second is u velocity, positive to right'
      write(*,*) 'The third is v velocity, positive towards up'
      write(*,*) 'The velocity components are mid water column'
      write(*,*) 'If no velocity, then null grid files are needed'
      write(*,*)
      write(*,*) 'The source will need to be characterized'
      write(*,*) 'The time, period, depth, and wavelength'
      write(*,*)
      write(203,*) 'The tsunami source may be user customized'
      write(203,*) 'Or the source may resume a prior simulation'
      write(203,*) 'Either way three grid files will be needed'
      write(203,*)
      write(203,*) 'The first is a free surface initial condition'
      write(203,*) 'The second is u velocity, positive to right'
      write(203,*) 'The third is v velocity, positive towards up'
      write(203,*) 'The velocity components are mid water column'
      write(203,*) 'If no velocity, then null grid files are needed'
      write(203,*)
      write(203,*) 'The source will need to be characterized'
      write(203,*) 'The time, period, depth, and wavelength'
      write(203,*)
c
c  27.2 You choose the tsunami source characteristics. 
c
      write(*,*) 'Added to time of tsunami generation'
      write(*,*) 'Enter time of this tsunami source (s)'
      read(*,*) ttsrc
      write(*,*) 'Enter a typical source period (s)'
      read(*,*) to
      write(*,*) 'Enter a typical source water depth (m)'
      read(*,*) d
      write(*,*) 'Enter a typical source wavelength (m)'
      read(*,*) lambda
c
c  27.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      to = dabs(to)
      lambda = dabs(lambda)         
      ttsrc = dabs(ttsrc)
c
c  27.4 Check that the inputs are physically possible.  
c         
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The water depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (to.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The source period cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (lambda.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The source wavelength cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  27.5 Update the tsunami source time to its total value.
c
      tsrc = tsrc + ttsrc
c
c  27.6 Write data to properties file.
c
      write(203,*) tsrc, '  tsunami source time'
      write(203,*) to, '  typical source period'
      write(203,*) d, '  typical source water depth'
      write(203,*) lambda, '  typical source wavelength'
      write(203,*)
c
c  27.7 Convert the source time because it will be modified later.
c
      tsrc = tsrc - to
c
c  27.8 Create the source grid files.
c
      write(*,*)
      write(*,*) 'Three grid files must be given these names'
      write(*,*) 'Create the file:  surface'//proper//'.grd'
      write(*,*) 'Create the file:  uvel'//proper//'.grd'
      write(*,*) 'Create the file:  vvel'//proper//'.grd'
      write(*,*)
      write(203,*) 'Three grid files must be given these names'
      write(203,*) 'Create the file:  surface'//proper//'.grd'
      write(203,*) 'Create the file:  uvel'//proper//'.grd'
      write(203,*) 'Create the file:  vvel'//proper//'.grd'
      write(203,*)
c
      return
      end
