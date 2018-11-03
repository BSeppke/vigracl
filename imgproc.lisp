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

(defun resizeimage-band (band width2 height2  &optional (resample_mode 2)) ; = bi-quadratic interpolation
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

(defun resizeimage (image width2 height2 &optional (resample_mode 2)) ; = bi-quadratic interpolation
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

(defun rotateimage-band (band angle &optional (resample_mode 2)) ; = bi-quadratic interpolation
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

(defun rotateimage (image angle &optional (resample_mode 2)) ; = bi-quadratic interpolation
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


(defun affinewarpimage-band (band affineMat  &optional (resample_mode 2)) ; = bi-quadratic interpolation
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

(defun affinewarpimage (image affinematrix  &optional (resample_mode 2)) ; = bi-quadratic interpolation
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
      		((1) (error "Error in vigracl.imgproc:fouriertransform: Fast Fourier Transform of image failed!!")))))

(defun fouriertransform (image)
  	(pivot-list (mapcar #'fouriertransform-band image)))
	  

;###############################################################################
;###################      Inverse Fast Fourier Transform    ####################
(defcfun ("vigra_fouriertransforminverse_c" vigra_fouriertransforminverse_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(band4 :pointer)
	(width :int)
	(height :int))

(defun fouriertransforminverse-band (band_real band_imag)
  	(let* ((width  (band-width band_real))
		   (height (band-height band_imag))
	 	   (band3  (make-band width height 0.0))
	 	   (band4  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band_real	ptr_band_real :float :lisp-type single-float) 
						 (band_imag	ptr_band_imag :float :lisp-type single-float) 
						 (band3	ptr_band3 :float :lisp-type single-float) 
						 (band4	ptr_band4 :float :lisp-type single-float))
						(vigra_fouriertransforminverse_c ptr_band_real ptr_band_imag ptr_band3 ptr_band4 width height))))
    	(case result
     		((0) (list band3 band4))
      		((1) (error "Error in vigracl.imgproc:fouriertransforminverse: Inverse Fast Fourier Transform of image failed!!")))))

(defun fouriertransforminverse (fft_image)
  	(pivot-list (mapcar #'fouriertransforminverse-band (first fft_image) (second fft_image))))


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
	(height :int)
	(eight_connectivity :boolean)
    (marker :float)
    (threshold :float)
    (allow_at_border :boolean)
    (allow_plateaus :boolean)
    (plateau_epsilon :float))

(defun localmaxima-band (band &optional (eight_connectivity T) (marker 1.0) (threshold MOST-NEGATIVE-SHORT-FLOAT) 
										(allow_at_border NIL) (allow_plateaus NIL) (plateau_epsilon 0.001))
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band 	ptr_band  :float :lisp-type single-float) 
						 (band2	ptr_band2 :float :lisp-type single-float))
						(vigra_localmaxima_c ptr_band ptr_band2 width height eight_connectivity 
											 marker threshold allow_at_border allow_plateaus plateau_epsilon))))
   		(case result
     		((0) band2)
      		((1) (error "Error in vigracl.imgproc:localmaxima: Extraction of local maxima of image failed!!")))))
	  
(defun localmaxima (image &optional (eight_connectivity T) (marker 1.0) (threshold MOST-NEGATIVE-SHORT-FLOAT)
							        (allow_at_border NIL) (allow_plateaus NIL) (plateau_epsilon 0.001))
  	(mapcar #'(lambda (arr) (localmaxima-band arr eight_connectivity marker 
  												  threshold allow_at_border 
  											  	  allow_plateaus plateau_epsilon))
  			image))
  
;###############################################################################
;################### Extraction of local minima of an image ####################
(defcfun ("vigra_localminima_c" vigra_localminima_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(eight_connectivity :boolean)
    (marker :float)
    (threshold :float)
    (allow_at_border :boolean)
    (allow_plateaus :boolean)
    (plateau_epsilon :float))

(defun localminima-band (band &optional (eight_connectivity T) (marker 1.0) (threshold MOST-POSITIVE-SHORT-FLOAT)
										(allow_at_border NIL) (allow_plateaus NIL) (plateau_epsilon 0.001))
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band 	ptr_band  :float :lisp-type single-float) 
						 (band2	ptr_band2 :float :lisp-type single-float))
						(vigra_localminima_c ptr_band ptr_band2 width height eight_connectivity 
											 marker threshold allow_at_border allow_plateaus plateau_epsilon))))
   		(case result
     		((0) band2)
      		((1) (error "Error in vigracl.imgproc:localminima: Extraction of local minima of image failed!!")))))
	  
(defun localminima (image  &optional (eight_connectivity T) (marker 1.0) (threshold MOST-POSITIVE-SHORT-FLOAT)
									 (allow_at_border NIL) (allow_plateaus NIL) (plateau_epsilon 0.001))
  	(mapcar #'(lambda (arr) (localminima-band arr eight_connectivity marker 
  												  threshold allow_at_border 
  											  	  allow_plateaus plateau_epsilon))
  			image))


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

(defun paddimage-band (band left upper right lower &optional (value 0.0)) ; fills border with zeros
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
           (padd_width (+ right width left))
           (padd_height (+ lower height upper))
	 	   (band2  (make-band padd_width padd_height value))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float) )
						(vigra_paddimage_c ptr_band ptr_band2 width height left upper right lower))))
   		(case result
      		((0) band2)
      		((1) (error "Error in vigracl.imgproc:paddimage: Padded image creation failed!!"))
      		((2) (error "Error in vigracl.imgproc:paddimage: Constraints not fulfilled: left & right >= 0, upper & lower >= 0!!")))))

(defun paddimage (image left upper right lower &optional (value '())) ; fills border with zeros
	(let* ((band_count (image-numbands image))
    	   (fill_value (if (null value)
                           (make-list band_count :initial-element 0.0)
                            value)))
  (mapcar #'(lambda (arr value) (paddimage-band arr left upper right lower value)) image fill_value)))
  
  
;###############################################################################
;###################         Clip images to Min...Max       ####################
(defcfun ("vigra_clipimage_c" vigra_clipimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(low :float)
	(upp :float))

(defun clipimage-band (band &optional (low 0.0) (upp 255.0))
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
		   (result (with-arrays-as-foreign-pointers
						((band 	ptr_band  :float :lisp-type single-float) 
						 (band2	ptr_band2 :float :lisp-type single-float))
						(vigra_clipimage_c ptr_band ptr_band2 width height low upp))))
   		(if (= result 0)
     		band2
      		(error "Error in vigracl.imgproc:clipimage: Clipping of image failed!!"))))
	  
(defun clipimage (image  &optional (low 0.0) (upp 255.0))
  	(mapcar #'(lambda (arr) (clipimage-band arr low upp)) image))
  	
  	
  	
;###############################################################################
;###################           Image addition               ####################
(defcfun ("vigra_imageplusimage_c" vigra_imageplusimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
    (height :int))

(defcfun ("vigra_imageplusvalue_c" vigra_imageplusvalue_c) :int
	(band :pointer)
	(band2 :pointer)
	(value :float)
	(width :int)
    (height :int))

(defun image+-band (band1 band2_or_value)
  	(let* ((width  (band-width band1))
		   (height (band-height band1))
	 	   (band3  (make-band width height 0.0))
	 	   (result 
	 	   		(if (numberp band2_or_value)
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imageplusvalue_c ptr_band1 ptr_band3 band2_or_value width height))
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band2_or_value	ptr_band2 :float :lisp-type single-float)
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imageplusimage_c ptr_band1 ptr_band2 ptr_band3 width height)))))
   		(if (= result 0)
     		band3
      		(error "Error in vigracl.imgproc:image+: Addition of images failed!!"))))
	  
(defun image+ (image1 image2)
  	(mapcar #'(lambda (arr1 arr2) (image+-band arr1 arr2)) image1 image2))

;###############################################################################
;###################           Image subtraction            ####################
(defcfun ("vigra_imageminusimage_c" vigra_imageminusimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
    (height :int))

(defcfun ("vigra_imageminusvalue_c" vigra_imageminusvalue_c) :int
	(band :pointer)
	(band2 :pointer)
	(value :float)
	(width :int)
    (height :int))

(defun image--band (band1 band2_or_value)
  	(let* ((width  (band-width band1))
		   (height (band-height band1))
	 	   (band3  (make-band width height 0.0))
	 	   (result 
	 	   		(if (numberp band2_or_value)
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imageminusvalue_c ptr_band1 ptr_band3 band2_or_value width height))
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band2_or_value	ptr_band2 :float :lisp-type single-float)
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imageminusimage_c ptr_band1 ptr_band2 ptr_band3 width height)))))
   		(if (= result 0)
     		band3
      		(error "Error in vigracl.imgproc:image-: Subtraction of images failed!!"))))
	  
(defun image- (image1 image2)
  	(mapcar #'(lambda (arr1 arr2) (image--band arr1 arr2)) image1 image2))


;###############################################################################
;###################           Image multiplication         ####################
(defcfun ("vigra_imagemultimage_c" vigra_imagemultimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
    (height :int))

(defcfun ("vigra_imagemultvalue_c" vigra_imagemultvalue_c) :int
	(band :pointer)
	(band2 :pointer)
	(value :float)
	(width :int)
    (height :int))
    
(defun image*-band (band1 band2_or_value)
  	(let* ((width  (band-width band1))
		   (height (band-height band1))
	 	   (band3  (make-band width height 0.0))
	 	   (result 
	 	   		(if (numberp band2_or_value)
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imagemultvalue_c ptr_band1 ptr_band3 band2_or_value width height))
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band2_or_value	ptr_band2 :float :lisp-type single-float)
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imagemultimage_c ptr_band1 ptr_band2 ptr_band3 width height)))))
   		(if (= result 0)
     		band3
      		(error "Error in vigracl.imgproc:image*: Multiplication of images failed!!"))))
	  
(defun image* (image1 image2)
  	(mapcar #'(lambda (arr1 arr2) (image*-band arr1 arr2)) image1 image2))

;###############################################################################
;###################              Image dividing            ####################
(defcfun ("vigra_imagedivideimage_c" vigra_imagedivideimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
    (height :int))

(defcfun ("vigra_imagedividevalue_c" vigra_imagedividevalue_c) :int
	(band :pointer)
	(band2 :pointer)
	(value :float)
	(width :int)
    (height :int))
    
(defun image/-band (band1 band2_or_value)
  	(let* ((width  (band-width band1))
		   (height (band-height band1))
	 	   (band3  (make-band width height 0.0))
	 	   (result 
	 	   		(if (numberp band2_or_value)
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imagedividevalue_c ptr_band1 ptr_band3 band2_or_value width height))
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band2_or_value	ptr_band2 :float :lisp-type single-float)
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imagedivideimage_c ptr_band1 ptr_band2 ptr_band3 width height)))))
   		(if (= result 0)
     		band3
      		(error "Error in vigracl.imgproc:image/: Division of images failed!!"))))
	  
(defun image/ (image1 image2)
  	(mapcar #'(lambda (arr1 arr2) (image/-band arr1 arr2)) image1 image2))


;###############################################################################
;###################          Image raised to power         ####################
(defcfun ("vigra_imagepowimage_c" vigra_imagepowimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
    (height :int))

(defcfun ("vigra_imagepowvalue_c" vigra_imagepowvalue_c) :int
	(band :pointer)
	(band2 :pointer)
	(value :float)
	(width :int)
    (height :int))
    
(defun image^-band (band1 band2_or_value)
  	(let* ((width  (band-width band1))
		   (height (band-height band1))
	 	   (band3  (make-band width height 0.0))
	 	   (result 
	 	   		(if (numberp band2_or_value)
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imagepowvalue_c ptr_band1 ptr_band3 band2_or_value width height))
		   			(with-arrays-as-foreign-pointers
						((band1 ptr_band1 :float :lisp-type single-float) 
						 (band2_or_value	ptr_band2 :float :lisp-type single-float)
						 (band3	ptr_band3 :float :lisp-type single-float))
						(vigra_imagepowimage_c ptr_band1 ptr_band2 ptr_band3 width height)))))
   		(if (= result 0)
     		band3
      		(error "Error in vigracl.imgproc:image^: Power of images failed!!"))))
	  
(defun image^ (image1 image2)
  	(mapcar #'(lambda (arr1 arr2) (image^-band arr1 arr2)) image1 image2))
