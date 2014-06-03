(in-package #:vigracl)

;###############################################################################
;###################         Resize image                   ####################
(defcfun ("vigra_resizeimage_c" vigra_resizeimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(width2 :int)
	(height2 :int)
	(resize_mode :int))

(defun resizeimage-band (band width2 height2 resize_mode)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width2 height2 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float) )
						(vigra_resizeimage_c ptr_band ptr_band2 width height width2 height2 resize_mode))))
   		(case result
      		((0) band2)
      		((1) (error "Error in vigracl.imgproc:resizeimage: Rotation of image failed!!"))
      		((2) (error "Error in vigracl.imgproc:resizeimage: Resize mode must be in {0,1,2,3,4}!!")))))

(defun resizeimage (image width2 height2 resize_mode)
  (mapcar #'(lambda (arr) (resizeimage-band arr width2 height2 resize_mode)) image))


;###############################################################################
;###################         Rotate image                   ####################
(defcfun ("vigra_rotateimage_c" vigra_rotateimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(angle :float)
	(resize_mode :int))

(defun rotateimage-band (band angle resize_mode)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_rotateimage_c ptr_band ptr_band2 width height angle resize_mode))))
    	(case result
     	 	((0) band2)
     		((1) (error "Error in vigracl.imgproc:rotateimage: Rotation of image failed!!"))
      		((2) (error "Error in vigracl.imgproc:rotateimage: Resize mode must be in {0,1,2,3,4}!!")))))

(defun rotateimage (image angle resize_mode)
	(mapcar #'(lambda (arr) (rotateimage-band arr angle resize_mode)) image))

;###############################################################################
;###################         Affine transform image         ####################
(defcfun ("vigra_affinewarpimage_c" vigra_affinewarpimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(affineMat :pointer)
	(width :int)
	(height :int)
	(resize_mode :int))


(defun affinewarpimage-band (band affineMat resize_mode)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  	ptr_band  	  :float  :lisp-type single-float) 
						 (band2 	ptr_band2 	  :float  :lisp-type single-float) 
						 (affineMat	ptr_affineMat :double :lisp-type double-float))
						(vigra_affinewarpimage_c ptr_band ptr_band2 ptr_affineMat width height resize_mode))))
    	(case result
     		((0) band2)
      		((1) (error "Error in vigracl.imgproc:affinewarpimage: Affine Warp  of image failed!!"))
      		((2) (error "Error in vigracl.imgproc:affinewarpimage: Resize mode must be in {0,1,2,3,4}!!")))))

(defun affinewarpimage (image affinematrix resize_mode)
  	(mapcar #'(lambda (arr) (affinewarpimage-band arr affinematrix resize_mode)) image))

;###############################################################################
;###################         Reflect image                  ####################
(defcfun ("vigra_reflectimage_c" vigra_reflectimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(reflect_mode :int))

(defun reflectimage-band (band reflect_mode)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band 	ptr_band  :float :lisp-type single-float) 
						 (band2	ptr_band2 :float :lisp-type single-float))
						(vigra_reflectimage_c ptr_band ptr_band2 width height reflect_mode))))
   		(case result
     		((0) band2)
      		((1) (error "Error in vigracl.imgproc:reflectimage: Reflection of image failed!!"))
      		((2) (error "Error in vigracl.imgproc:reflectimage: Reflection mode must be in {1 (= horizontal), 2 (= vertical), 3 (=both)}!!")))))
	  
(defun reflectimage (image reflect_mode)
  	(mapcar #'(lambda (arr) (reflectimage-band arr reflect_mode)) image))
	  

;###############################################################################
;###################         Fast Fourier Transform         ####################
(defcfun ("vigra_fouriertransform_c" vigra_fouriertransform_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
	(height :int))

(defun fouriertransform-band (band)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (band3  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band 	ptr_band  :float :lisp-type single-float) 
						 (band2	ptr_band2 :float :lisp-type single-float) 
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_fouriertransform_c ptr_band ptr_band2 ptr_band3 width height))))
    	(case result
     		((0) (list band2 band3))
      		((1) (error "Error in vigracl.imgproc:fouriertransform: FastFourier Transform of image failed!!")))))

(defun fouriertransform (image)
  	(pivot-list (mapcar #'fouriertransform-band image)))
