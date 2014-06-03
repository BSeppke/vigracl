(in-package #:vigracl)

;###############################################################################
;###################         Erosion                        ####################
(defcfun ("vigra_discerosion_c" vigra_discerosion_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(radius :int))

(defun erodeimage-band (band radius)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_discerosion_c ptr_band ptr_band2 width height radius))))
    	(case result
      		((0) band2)
      		((1) (error "Error in vigracl.morphology:erodeimage: Erosion of image failed!!")))))

(defun erodeimage (image radius)
  	(mapcar #'(lambda (arr) (erodeimage-band arr radius)) image))

;###############################################################################
;###################         Dilation                       ####################
(defcfun ("vigra_discdilation_c" vigra_discdilation_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(radius :int))

(defun dilateimage-band (band radius)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_discdilation_c ptr_band ptr_band2 width height radius))))
    	(case result
      		((0) band2)
      		((1) (error "Error in vigracl.morphology:dilateimage: Dilation of image failed!!")))))

(defun dilateimage (image radius)
  (mapcar #'(lambda (arr) (dilateimage-band arr radius)) image))

;###############################################################################
;###################         Opening                        ####################

(defun openingimage-band (arr radius)
  	(dilateimage-band (erodeimage-band arr radius) radius))

(defun openingimage (image radius)
  	(dilateimage (erodeimage image radius) radius))

;###############################################################################
;###################         Closing                        ####################

(defun closingimage-band (arr radius)
  	(erodeimage-band (dilateimage-band arr radius) radius))

(defun closingimage (image radius)
  	(erodeimage (dilateimage image radius) radius))


;###############################################################################
;###################         Upwind image                   ####################
(defcfun ("vigraext_upwind_c" vigraext_upwind_c) :int
	(band :pointer)
	(signum_band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(weight :float))

(defun upwindimage-band (band signum_band weight)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  	  ptr_band  	  :float :lisp-type single-float) 
						 (signum_band ptr_signum_band :float :lisp-type single-float) 
						 (band2 	  ptr_band2       :float :lisp-type single-float))
						(vigraext_upwind_c ptr_band ptr_signum_band ptr_band2 width height weight))))
    	(case result
      		((0) band2)
      		((1) (error "Error in vigracl.morphology:upwindimage: Upwinding of image failed!!")))))

(defun upwindimage (image signum_image radius)
  	(mapcar #'(lambda (arr signum_arr) (upwindimage-band arr signum_arr radius)) image signum_image))