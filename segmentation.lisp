(in-package #:vigracl)

;###############################################################################
;###################         Label image                    ####################
(defcfun ("vigra_labelimage_c" vigra_labelimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int))

(defun labelimage-band (band)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_labelimage_c ptr_band ptr_band2 width height))))
    	(if (= result -1)
			(error "Error in vigracl.segmentation.labelimage: Labeling of image failed!")
     		band2)))

(defun labelimage (image)
  	(mapcar #'labelimage-band image))


;###############################################################################
;###################      Watershed Transform (Union-Find)  ####################
(defcfun ("vigra_watersheds_c" vigra_watersheds_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int))

(defun watersheds-band (band)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_watersheds_c ptr_band ptr_band2 width height))))
    	(if (= result -1)
			(error "Error in vigracl.segmentation.watersheds: Watershed transform of image failed!")
     		band2)))
	  
(defun watersheds (image)
  (mapcar #'watersheds-band image))


;###############################################################################
;###################      Canny Edge-Detection              ####################
(defcfun ("vigra_cannyedgeimage_c" vigra_cannyedgeimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
    (scale :float)
    (gradient_threshold :float)
    (mark :float))

(defun cannyedgeimage-band (band scale gradient_threshold mark)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_cannyedgeimage_c ptr_band ptr_band2 width height scale gradient_threshold mark))))
   		(case result
      		((0) band2)
      		((1) (error "Error in vigracl.segmentation:cannyedgeimage: Canny Edge Detection of image failed!")))))

(defun cannyedgeimage (image scale gradient_threshold mark)
  	(mapcar #'(lambda (arr) (cannyedgeimage-band arr scale gradient_threshold mark)) image))	  


;###############################################################################
;################    Difference of Exponential Edge-Detection  #################
(defcfun ("vigra_differenceofexponentialedgeimage_c" vigra_differenceofexponentialedgeimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
    (scale :float)
    (gradient_threshold :float)
    (mark :float))

(defun differenceofexponentialedgeimage-band (band scale gradient_threshold mark)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_differenceofexponentialedgeimage_c ptr_band ptr_band2 width height scale gradient_threshold mark))))
   		(case result
      		((0) band2)
     		((1) (error "Error in vigracl.segmentation:differenceofexponentialedgeimage: Difference of Exponential Edge Detection of image failed!")))))

(defun differenceofexponentialedgeimage (image scale gradient_threshold mark)
  (mapcar #'(lambda (arr) (differenceofexponentialedgeimage-band arr scale gradient_threshold mark)) image))

		  
;###############################################################################
;###################     RegionImage -> CrackEdgeImage      ####################
(defcfun ("vigra_regionimagetocrackedgeimage_c" vigra_regionimagetocrackedgeimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
    (mark :float))

(defun regionimagetocrackedgeimage-band (band mark)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band (- (* 2 width) 1) (- (* 2 height) 1) 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_regionimagetocrackedgeimage_c ptr_band ptr_band2 width height mark))))
    	(case result
      		((0) band2)
      		((1) (error "Error in vigracl.segmentation:regionimagetocrackedgeimage: Creation of CrackEdgeImage failed!")))))

(defun regionimagetocrackedgeimage (image mark)
	(mapcar #'(lambda (arr) (regionimagetocrackedgeimage-band arr mark)) image))