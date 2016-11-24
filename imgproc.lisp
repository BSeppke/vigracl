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
	(resample_mode :int))

(defun resizeimage-band (band width2 height2 resample_mode)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width2 height2 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float) )
						(vigra_resizeimage_c ptr_band ptr_band2 width height width2 height2 resample_mode))))
   		(case result
      		((0) band2)
      		((1) (error "Error in vigracl.imgproc:resizeimage: Resize of image failed!!"))
      		((2) (error "Error in vigracl.imgproc:resizeimage: Resample mode must be in {0,1,2,3,4}!!")))))

(defun resizeimage (image width2 height2 resample_mode)
  (mapcar #'(lambda (arr) (resizeimage-band arr width2 height2 resample_mode)) image))


;###############################################################################
;###################         Rotate image                   ####################
(defcfun ("vigra_rotateimage_c" vigra_rotateimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(angle :float)
	(resample_mode :int))

(defun rotateimage-band (band angle resample_mode)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_rotateimage_c ptr_band ptr_band2 width height angle resample_mode))))
    	(case result
     	 	((0) band2)
     		((1) (error "Error in vigracl.imgproc:rotateimage: Rotation of image failed!!"))
      		((2) (error "Error in vigracl.imgproc:rotateimage: Resample mode must be in {0,1,2,3,4}!!")))))

(defun rotateimage (image angle resample_mode)
	(mapcar #'(lambda (arr) (rotateimage-band arr angle resample_mode)) image))

;###############################################################################
;###################         Affine transform image         ####################
(defcfun ("vigra_affinewarpimage_c" vigra_affinewarpimage_c) :int
	(band :pointer)
	(affineMat :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(resample_mode :int))


(defun affinewarpimage-band (band affineMat resample_mode)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  	ptr_band  	  :float  :lisp-type single-float) 
						 (affineMat	ptr_affineMat :double :lisp-type double-float)
						 (band2 	ptr_band2 	  :float  :lisp-type single-float) )
						(vigra_affinewarpimage_c ptr_band ptr_affineMat ptr_band2 width height resample_mode))))
    	(case result
     		((0) band2)
      		((1) (error "Error in vigracl.imgproc:affinewarpimage: Affine Warp  of image failed!!"))
      		((2) (error "Error in vigracl.imgproc:affinewarpimage: Resample mode must be in {0,1,2,3,4}!!")))))

(defun affinewarpimage (image affinematrix resample_mode)
  	(mapcar #'(lambda (arr) (affinewarpimage-band arr affinematrix resample_mode)) image))

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

;###############################################################################
;###################        Fast cross correlation          ####################
(defcfun ("vigra_fastcrosscorrelation_c" vigra_fastcrosscorrelation_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
	(height :int)
	(mask_width :int)
	(mask_height :int))

(defun fastcrosscorrelation-band (band mask)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (mask_width  (band-width mask))
		   (mask_height (band-height mask))
	 	   (band2  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band 	ptr_band  :float :lisp-type single-float) 
						 (mask	ptr_mask  :float :lisp-type single-float) 
						 (band2	ptr_band2 :float :lisp-type single-float))
						(vigra_fastcrosscorrelation_c ptr_band ptr_mask ptr_band2 width height mask_width mask_height))))
   		(case result
     		((0) band2)
      		((1) (error "Error in vigracl.imgproc:fastcrosscorrelation: Fast cross-correlation of image failed!!"))
      		((2) (error "Error in vigracl.imgproc:fastcrosscorrelation: Mask size needs to be odd!!")))))
	  
(defun fastcrosscorrelation (image mask)
  	(mapcar #'fastcrosscorrelation-band image mask))
  	
;###############################################################################
;###################   Fast normalized cross correlation    ####################
(defcfun ("vigra_fastnormalizedcrosscorrelation_c" vigra_fastnormalizedcrosscorrelation_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
	(height :int)
	(mask_width :int)
	(mask_height :int))

(defun fastnormalizedcrosscorrelation-band (band mask)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (mask_width  (band-width mask))
		   (mask_height (band-height mask))
	 	   (band2  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band 	ptr_band  :float :lisp-type single-float) 
						 (mask	ptr_mask  :float :lisp-type single-float) 
						 (band2	ptr_band2 :float :lisp-type single-float))
						(vigra_fastnormalizedcrosscorrelation_c ptr_band ptr_mask ptr_band2 width height mask_width mask_height))))
   		(case result
     		((0) band2)
      		((1) (error "Error in vigracl.imgproc:fastnormalizedcrosscorrelation: Fast normalized cross-correlation of image failed!!"))
      		((2) (error "Error in vigracl.imgproc:fastnormalizedcrosscorrelation: Mask size needs to be odd!!")))))
	  
(defun fastnormalizedcrosscorrelation (image mask)
  	(mapcar #'fastnormalizedcrosscorrelation-band image mask))
  
;###############################################################################
;################### Extraction of local maxima of an image ####################
(defcfun ("vigra_localmaxima_c" vigra_localmaxima_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int))

(defun localmaxima-band (band)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band 	ptr_band  :float :lisp-type single-float) 
						 (band2	ptr_band2 :float :lisp-type single-float))
						(vigra_localmaxima_c ptr_band ptr_band2 width height))))
   		(case result
     		((0) band2)
      		((1) (error "Error in vigracl.imgproc:localmaxima: Extraction of local maxima of image failed!!")))))
	  
(defun localmaxima (image)
  	(mapcar #'localmaxima-band image))
  
;###############################################################################
;################### Extraction of local minima of an image ####################
(defcfun ("vigra_localminima_c" vigra_localminima_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int))

(defun localminima-band (band)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band 	ptr_band  :float :lisp-type single-float) 
						 (band2	ptr_band2 :float :lisp-type single-float))
						(vigra_localminima_c ptr_band ptr_band2 width height))))
   		(case result
     		((0) band2)
      		((1) (error "Error in vigracl.imgproc:localminima: Extraction of local minima of image failed!!")))))
	  
(defun localminima (image)
  	(mapcar #'localminima-band image))


;###############################################################################
;###################             subimage                   ####################
(defcfun ("vigra_subimage_c" vigra_subimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(left :int)
	(upper :int)
	(right :int)
	(lower :int))

(defun subimage-band (band left upper right lower)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
           (cut_width (- right left))
           (cut_height (- lower upper))
	 	   (band2  (make-band cut_width cut_height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float) )
						(vigra_subimage_c ptr_band ptr_band2 width height left upper right lower))))
   		(case result
      		((0) band2)
      		((1) (error "Error in vigracl.imgproc:subimage: Subimage creation failed!!"))
      		((2) (error "Error in vigracl.imgproc:subimage: Constraints not fulfilled: left < right, upper < lower, right - left <= width, lower - upper <= height!!")))))

(defun subimage (image left upper right lower)
  (mapcar #'(lambda (arr) (subimage-band arr left upper right lower)) image))

  	

;###############################################################################
;###################             paddimage                   ####################
(defcfun ("vigra_paddimage_c" vigra_paddimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(left :int)
	(upper :int)
	(right :int)
	(lower :int))

(defun paddimage-band (band left upper right lower)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
           (padd_width (+ right width left))
           (padd_height (+ lower height upper))
	 	   (band2  (make-band padd_width padd_height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float) )
						(vigra_paddimage_c ptr_band ptr_band2 width height left upper right lower))))
   		(case result
      		((0) band2)
      		((1) (error "Error in vigracl.imgproc:paddimage: Padded image creation failed!!"))
      		((2) (error "Error in vigracl.imgproc:paddimage: Constraints not fulfilled: left & right >= 0, upper & lower >= 0!!")))))

(defun paddimage (image left upper right lower)
  (mapcar #'(lambda (arr) (paddimage-band arr left upper right lower)) image))