
(in-package #:cl-user)

;;; Nothing special about the "VIGRACL" package.  We're just
;;; using it as a substitute for your own CL package.
(defpackage #:vigracl
    (:use #:common-lisp #:cffi #:waaf-cffi)
    (:export
  	   ;vigracl.config 
       :vigracl-path
       :vigracl-version
       
       ;vigracl.helpers
       
       ;arrays (a.k.a. bands)
       :copy-array 
	   :array-map
	   :array-map!
	   :array-reduce
	   :array-for-each-index
	   :make-band
	   :band-width
	   :band-height
	   
	   ;images
	   :image-map
	   :image-map!
	   :image-reduce
	   :image-for-each-index
	   :make-image
	   :image-width
	   :image-height
	   :image-numbands
	   :image-ref
	   :image-set
	   :copy-image
	   :image->red-band
       :image->red
       :image->green-band
       :image->green
       :image->blue-band
       :image->blue
	   
	   ;vigracl.impex
	   :loadimage 
	   :load-image 
	   :image-load 
	   :saveimage 
	   :save-image 
	   :image-save
	   
	   ;vigracl.filters
	   :convolveimage-band    
	   :convolveimage
	   :separableconvolveimage-band
	   :separableconvolveimage
	   :gsmooth-band
	   :gsmooth
	   :gaussiangradient-band
	   :gaussiangradient
	   :ggradient-band
	   :ggradient
	   :laplacianofgaussian-band
	   :laplacianofgaussian
	   :hessianmatrixofgaussian-band
	   :hessianmatrixofgaussian
	   :structuretensor-band
	   :structuretensor
	   :boundarytensor-band
	   :boundarytensor
	   :boundarytensor1-band
	   :boundarytensor1
	   :tensoreigenrepresentation-band
	   :tensoreigenrepresentation
	   :tensortrace-band
	   :tensortrace
	   :tensortoedgecorner-band
	   :tensortoedgecorner
	   :hourglassfilter-band
	   :hourglassfilter
	   :gsharpening-band
	   :gsharpening
	   :sharpening-band
	   :sharpening
	   :nonlineardiffusion-band 
	   :nonlineardiffusion 
	   :distancetransform-band 
	   :distancetransform
	   
	   ;vigracl.imgproc
	   :resizeimage-band
	   :resizeimage
	   :rotateimage-band
	   :rotateimage
	   :affinewarpimage-band
	   :affinewarpimage
	   :reflectimage-band
	   :reflectimage
	   :fouriertransform-band
	   :fouriertransform
	   
	   ;vigracl.segmentation
	   :labelimage-band
	   :labelimage
	   :watersheds-band
	   :watersheds
	   :cannyedgeimage-band
	   :cannyedgeimage
	   :differenceofexponentialedgeimage-band
	   :differenceofexponentialedgeimage
	   :regionimagetocrackedgeimage-band
	   :regionimagetocrackedgeimage
	   
	   ;vigracl.morphology
	   :erodeimage-band
	   :erodeimage
	   :dilateimage-band
	   :dilateimage
	   :openingimage-band
	   :openingimage
	   :closingimage-band
	   :closingimage
	   
	   ;vigracl.splineimageview
	   :create-splineimageview-band
	   :create-splineimageview
	   :delete-splineimageview-band
	   :delete-splineimageview
	   :splineimageview-value-band
	   :splineimageview-value
	   :splineimageview-dx-band
	   :splineimageview-dx 
	   :splineimageview-dy-band
	   :splineimageview-dy 
	   :splineimageview-dxx-band
	   :splineimageview-dxx
	   :splineimageview-dxy-band
	   :splineimageview-dxy 
	   :splineimageview-dyy-band
	   :splineimageview-dyy 
	   :splineimageview-dx3-band
	   :splineimageview-dx3 
	   :splineimageview-dxxy-band
	   :splineimageview-dxxy
	   :splineimageview-dxyy-band
	   :splineimageview-dxyy 
	   :splineimageview-dy3-band
	   :splineimageview-dy3 
	   :splineimageview-g2-band
	   :splineimageview-g2 
	   :splineimageview-g2x-band
	   :splineimageview-g2x 
	   :splineimageview-g2y-band
	   :splineimageview-g2y 
	   :splineimageview-g2xx-band
	   :splineimageview-g2xx 
	   :splineimageview-g2xy-band
	   :splineimageview-g2xy 
	   :splineimageview-g2yy-band
	   :splineimageview-g2yy ))
