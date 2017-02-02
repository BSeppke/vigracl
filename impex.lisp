(in-package #:vigracl)

;###############################################################################
;############ Getting Dimensions and # of Bands of an image-file    ############

(defcfun ("vigra_imagewidth_c" vigra_imagewidth_c)   		:int 	(filename :string))
(defcfun ("vigra_imageheight_c" vigra_imageheight_c) 		:int 	(filename :string))
(defcfun ("vigra_imagenumbands_c" vigra_imagenumbands_c)	:int 	(filename :string))


;###############################################################################
;###################             Loading images             ####################

;###############                 Grayscale images                  #############

(defcfun ("vigra_importgrayimage_c" vigra_importgrayimage_c) :int
	(band :pointer)
	(width :int)
	(height :int)
	(filename :string))

(defun loadgrayimage (filename)
	(let* ((width  	(vigra_imagewidth_c filename))
		   (height 	(vigra_imageheight_c filename))
	 	   (band    (make-band width height 0.0))
	 	   (result 	(with-array-as-foreign-pointer
	 					(band ptr_band :float :lisp-type single-float) 
	 			 		(vigra_importgrayimage_c  ptr_band width height filename))))
		(case result
      		((0) 	(list band))
      		((1) 	(error "Error in vigracl.impex.loadgrayimage: Image cannot be loaded by vigra!"))
      		((2) 	(error "Error in vigracl.impex.loadgrayimage: Image is not grayscale!"))
      		((3)	(error "Error in vigracl.impex.loadgrayimage: Sizes do not match!")))))

;###############               RGB-Color images                     #############
(defcfun ("vigra_importrgbimage_c" vigra_importrgbimage_c) :int
	(band_r :pointer)
	(band_g :pointer)
	(band_b :pointer)
	(width :int)
	(height :int)
	(filename :string))
	      
(defun loadrgbimage (filename)
  	(let* ((width  (vigra_imagewidth_c filename))
	 	   (height (vigra_imageheight_c filename))
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
      		
;###############             RGBA-Color images                     #############
(defcfun ("vigra_importrgbaimage_c" vigra_importrgbaimage_c) :int
	(band_r :pointer)
	(band_g :pointer)
	(band_b :pointer)
	(band_a :pointer)
	(width :int)
	(height :int)
	(filename :string))
	      
(defun loadrgbaimage (filename)
  	(let* ((width  (vigra_imagewidth_c filename))
	 	   (height (vigra_imageheight_c filename))
	 	   (band_r (make-band width height 0.0))
	 	   (band_g (make-band width height 0.0))
	 	   (band_b (make-band width height 0.0))
	 	   (band_a (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
	 					((band_r ptr_band_r :float :lisp-type single-float) 
	 					 (band_g ptr_band_g :float :lisp-type single-float) 
	 			 		 (band_b ptr_band_b :float :lisp-type single-float) 
	 			 		 (band_a ptr_band_a :float :lisp-type single-float))
	 					(vigra_importrgbaimage_c ptr_band_r ptr_band_g ptr_band_b ptr_band_a width height filename))))
    	(case result
      		((0) (list band_r band_g band_b band_a))
     		((1) (error "Error in vigracl.impex.loadrgbaimage: Image cannot be loaded by vigra!"))
     		((2) (error "Error in vigracl.impex.loadrgbaimage: Image is not of rgba color!"))
      		((3) (error "Error in vigracl.impex.loadrgbaimage: Sizes do not match!")))))

;######    Generic (choose automatically if image is gray or colored)    #######

(defun loadimage (filename)
  	(case (vigra_imagenumbands_c filename)
    	((1) (loadgrayimage filename))
    	((3) (loadrgbimage filename))
    	((4) (loadrgbaimage filename))
    	(else "Error in vigracl.impex.loadimage: Image has neither 1 nor 3 nor 4bands and thus cannot be loaded!")))
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


;###############               RGBA-Color images                  #############
(defcfun ("vigra_exportrgbaimage_c" vigra_exportrgbaimage_c) :int
	(band_r :pointer)
	(band_g :pointer)
	(band_b :pointer)
	(band_a :pointer)
	(width :int)
	(height :int)
	(filename :string))

(defun savergbaimage (image filename)
 	 (let* ((band_r  (first image))
	 		(band_g  (second image))
		 	(band_b  (third image))
		 	(band_a  (fourth image))
			(width  (band-width  band_r))
			(height (band-height band_r))
			(result (with-arrays-as-foreign-pointers
						((band_r ptr_band_r :float :lisp-type single-float) 
						 (band_g ptr_band_g :float :lisp-type single-float) 
						 (band_b ptr_band_b :float :lisp-type single-float) 
						 (band_a ptr_band_a :float :lisp-type single-float))
						(vigra_exportrgbaimage_c ptr_band_r ptr_band_g ptr_band_b ptr_band_a width height filename))))
		(case result
			((0) T)
			((1) (error "Error in vigracl.impex.savergbaimage: Image cannot be saved by vigra!")))))

;######    Generic (choose automatically if image is gray or colored)    #######
(defun saveimage (image filename)
  	(case (length image)
    	((1) (savegrayimage image filename))
    	((3) (savergbimage  image filename))
    	((4) (savergbaimage  image filename))
    	(else (error "Error in vigracl.impex.saveimage: Image has neither 1 nor 3 nor 4 bands and thus cannot be saved!"))))
(defun save-image  (image filename) (saveimage image filename))
(defun image-save  (image filename) (saveimage image filename))
