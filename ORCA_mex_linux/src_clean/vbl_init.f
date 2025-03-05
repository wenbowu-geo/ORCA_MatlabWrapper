      subroutine vbl_init
c
c: Initializes variables in scairy_com and lab_com.
c
      use scairy_com
c yt      use lab_com
c
      ei13=( 0.5D0, 0.8660254037844387D0)
      ei23=(-0.5D0, 0.8660254037844387D0)
      ei16=( 0.8660254037844387D0, 0.5D0)
      ei56=(-0.8660254037844387D0, 0.5D0)
      eim13=( 0.5D0, -0.8660254037844387D0)
      eim23=(-0.5D0, -0.8660254037844387D0)
      eim16=( 0.8660254037844387D0, -0.5D0)
      eim56=(-0.8660254037844387D0, -0.5D0)
      eye=(0.D0,1.D0)
cc    zpt=(-0.9D0,2.8D0)
      pie23=2.094395102393195D0
      pie_x=3.1415926535897932D0
      pie_inv=0.31830988618379D0
      det_bi=(0.31830988618379,0.)
      det_pos=(0.13783222385545,0.07957747154595)
      det_neg=(0.13783222385545,-0.07957747154595)
      sqrt3=1.73205080756888
cc
c yt      flab='Frequency  - Hz'
c yt      rlab='Range  - km'
c yt      mnlab='Mode Number'
c yt      tlab='Time  - s'
c yt      krlab='RE[k]'
c yt      kilab='IM[k]'
c yt      thlab='Grazing Angle  - deg'
c yt      mlab='|R| - dB'
c yt      phlab='Phase of R  - deg'
c yt      dlab='Depth  - m'
c yt      tlclab='Transmission Loss - dB'
c yt      tlilab='Incoherent TL - dB'
c yt      mrlab='RE[Mode Amplitude]'
c yt      milab='IM[Mode Amplitude]'
c yt      malab='Mode Amplitude'
c yt      mplab='Mode Phase'
c yt      dblab='dB'
c yt      mtlab='m'
c yt      kmlab='km'
c yt      z4=0. 
c
      return
      end
