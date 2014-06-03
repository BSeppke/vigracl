(in-package #:vigracl)

;###############################################################################
;############ Getting Dimensions and # of Bands of an image-file    ############

(defcfun ("get_width_c" get_width_c)   		:int 	(filename :string))
(defcfun ("get_height_c" get_height_c) 		:int 	(filename :string))
(defcfun ("get_numbands_c" get_numbands_c)	:int 	(filename :string))


;###############################################################################
;###################             Loading images             ####################

;###############                 Grayscale images                  #############

(defcfun ("vigra_importgrayimage_c" vigra_importgrayimage_c) :int
	(band :pointer)
	(width :int)
	(height :int)
	(filename :string))

(defun loadgrayimage (filename)
	(let* ((width  	(get_width_c filename))
		   (height 	(get_height_c filename))
	 	   (band    (make-band width height 0.0))
	 	   (result 	(with-array-as-foreign-pointer
	 					(band ptr_band :float :lisp-type single-float) 
	 			 		(vigra_importgrayimage_c  ptr_band width height filename))))
		(case result
      		((0) 	(list band))
      		((1) 	(error "Error in vigracl.impex.loadgrayimage: Image cannot be loaded by vigra!"))
      		((2) 	(error "Error in vigracl.impex.loadgrayimage: Image is not grayscale!"))
      		((3)	(error "Error in vigracl.impex.loadgrayimage: Sizes do not match!")))))

;###############                  Color images                     #############
(defcfun ("vigra_importrgbimage_c" vigra_importrgbimage_c) :int
	(band_r :pointer)
	(band_g :pointer)
	(band_b :pointer)
	(width :int)
	(height :int)
	(filename :string))
	      
(defun loadrgbimage (filename)
  	(let* ((width  (get_width_c filename))
	 	   (height (get_height_c filename))
	 	   (band_r (make-band width height 0.0))
	 	   (band_g (make-band width height 0.0))
	 	   (band_b (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
	 					((band_r ptr_band_r :float :lisp-type single-float) 
	 					 (band_g ptr_band_g :float :lisp-type single-float) 
	 			 		 (band_b ptr_band_b :float :lisp-type single-float))
	 					(vigra_importrgbimage_c ptr_band_r ptr_band_g ptr_band_b width height filename))))
    	(case result
      		((0) (list band_r band_g band_b))
     		((1) (error "Error in vigracl.impex.loadrgbimage: Image cannot be loaded by vigra!"))
     		((2) (error "Error in vigracl.impex.loadrgbimage: Image is not of rgb color!"))
      		((3) (error "Error in vigracl.impex.loadrgbimage: Sizes do not match!")))))

;######    Generic (choose automatically if image is gray or colored)    #######

(defun loadimage (filename)
  	(case (get_numbands_c filename)
    	((1) (loadgrayimage filename))
    	((3) (loadrgbimage filename))
    	(else "Error in vigracl.impex.loadimage: Image has neither 1 nor 3 bands and thus cannot be loaded!")))
(defun load-image  (filename) (loadimage filename))
(defun image-load  (filename) (loadimage filename))


;###############################################################################
;###################             Saving images              ####################

;###############                 Grayscale images                  #############
(defcfun ("vigra_exportgrayimage_c" vigra_exportgrayimage_c) :int
	(band :pointer)
	(width :int)
	(height :int)
	(filename :string))

(defun savegrayimage (image filename)
  	(let* ((band   (first image))
	 	   (width  (band-width band))
	 	   (height (band-height band))
		   (result (with-array-as-foreign-pointer
	 					(band ptr_band :float :lisp-type single-float) 
	 			 		(vigra_exportgrayimage_c ptr_band width height filename))))
    	(case result
      		((0) t)
      		((1) (error "Error in vigracl.impex.savegrayimage: Image cannot be saved by vigra!")))))


;###############                RGB-Color images                  #############
(defcfun ("vigra_exportrgbimage_c" vigra_exportrgbimage_c) :int
	(band_r :pointer)
	(band_g :pointer)
	(band_b :pointer)
	(width :int)
	(height :int)
	(filename :string))

(defun savergbimage (image filename)
 	 (let* ((band_r  (first image))
	 		(band_g  (second image))
		 	(band_b  (third image))
			(width  (band-width  band_r))
			(height (band-height band_r))
			(result (with-arrays-as-foreign-pointers
						((band_r ptr_band_r :float :lisp-type single-float) 
						 (band_g ptr_band_g :float :lisp-type single-float) 
						 (band_b ptr_band_b :float :lisp-type single-float))
						(vigra_exportrgbimage_c ptr_band_r ptr_band_g ptr_band_b width height filename))))
		(case result
			((0) T)
			((1) (error "Error in vigracl.impex.savergbimage: Image cannot be saved by vigra!")))))

;######    Generic (choose automatically if image is gray or colored)    #######
(defun saveimage (image filename)
  	(case (length image)
    	((1) (savegrayimage image filename))
    	((3) (savergbimage  image filename))
    	(else (error "Error in vigracl.impex.saveimage: Image has neither 1 nor 3 bands and thus cannot be saved!"))))
(defun save-image  (image filename) (saveimage image filename))
(defun image-save  (image filename) (saveimage image filename))
