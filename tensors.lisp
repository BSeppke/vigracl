(in-package #:vigracl)

;###############################################################################
;#########  Structure Tensor using inner and outer gaussians      ##############
(defcfun ("vigra_structuretensor_c" vigra_structuretensor_c) :int
	(band :pointer)
	(band_xx :pointer)
	(band_xy :pointer)
	(band_yy :pointer)
	(width :int)
	(height :int)
	(inner_scale   :float)
	(outer_scale   :float))

(defun structuretensor-band (band inner_scale outer_scale)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band_xx (make-band width height 0.0))
	 	   (band_xy (make-band width height 0.0))
	 	   (band_yy (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band    ptr_band    :float :lisp-type single-float) 
						 (band_xx ptr_band_xx :float :lisp-type single-float) 
						 (band_xy ptr_band_xy :float :lisp-type single-float) 
						 (band_yy ptr_band_yy :float :lisp-type single-float))
						(vigra_structuretensor_c ptr_band ptr_band_xx ptr_band_xy ptr_band_yy width height inner_scale outer_scale))))
    	(case result
      		((0) (list band_xx band_xy band_yy))
      		((1) (error "Error in vigracl.filters.structuretensor: Calculation of Structure Tensor failed!")))))

(defun structuretensor (image inner_scale outer_scale)
  	(pivot-list (mapcar #'(lambda (arr)  (structuretensor-band arr inner_scale outer_scale)) image)))


;###############################################################################
;#############               Koethes Boundary tensor              ##############
(defcfun ("vigra_boundarytensor_c" vigra_boundarytensor_c) :int
	(band :pointer)
	(band_xx :pointer)
	(band_xy :pointer)
	(band_yy :pointer)
	(width :int)
	(height :int)
	(scale   :float))

(defun boundarytensor-band (band scale)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band_xx (make-band width height 0.0))
	 	   (band_xy (make-band width height 0.0))
	 	   (band_yy (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band    ptr_band    :float :lisp-type single-float) 
						 (band_xx ptr_band_xx :float :lisp-type single-float) 
						 (band_xy ptr_band_xy :float :lisp-type single-float) 
						 (band_yy ptr_band_yy :float :lisp-type single-float))
						(vigra_boundarytensor_c ptr_band ptr_band_xx ptr_band_xy ptr_band_yy width height scale))))
    	(case result
      		((0) (list band_xx band_xy band_yy))
      		((1) (error "Error in vigracl.filters.boundarytensor: Calculation of Koethes Boundary Tensor failed!")))))

(defun boundarytensor (image scale)
  	(pivot-list (mapcar #'(lambda (arr)  (boundarytensor-band arr scale)) image)))

;###############################################################################
;#######     Koethes Boundary tensor (without 0th order response)      #########
(defcfun ("vigra_boundarytensor1_c" vigra_boundarytensor1_c) :int
	(band :pointer)
	(band_xx :pointer)
	(band_xy :pointer)
	(band_yy :pointer)
	(width :int)
	(height :int)
	(scale   :float))

(defun boundarytensor1-band (band scale)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band_xx (make-band width height 0.0))
	 	   (band_xy (make-band width height 0.0))
	 	   (band_yy (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band    ptr_band    :float :lisp-type single-float) 
						 (band_xx ptr_band_xx :float :lisp-type single-float) 
						 (band_xy ptr_band_xy :float :lisp-type single-float) 
						 (band_yy ptr_band_yy :float :lisp-type single-float))
						(vigra_boundarytensor1_c ptr_band ptr_band_xx ptr_band_xy ptr_band_yy width height scale))))
    	(case result
      		((0) (list band_xx band_xy band_yy))
      		((1) (error "Error in vigracl.filters.boundarytensor1: Calculation of Koethes Boundary Tensor (without 0th order term) failed!")))))

(defun boundarytensor1 (image scale)
  (pivot-list (mapcar #'(lambda (arr)  (boundarytensor1-band arr scale)) image)))

;###############################################################################
;#########     Eigenvalue Representation of a Tensor              ##############
(defcfun ("vigra_tensoreigenrepresentation_c" vigra_tensoreigenrepresentation_c) :int
	(band_xx :pointer)
	(band_xy :pointer)
	(band_yy :pointer)
	(band_lEV :pointer)
	(band_sEV :pointer)
	(band_angle :pointer)
	(width :int)
	(height :int))

(defun tensoreigenrepresentation-band (tensor)
  	(let* ((band_xx (first  tensor))
	 	   (band_xy (second tensor))
	 	   (band_yy (third  tensor))
	 	   (width  (band-width band_xx))
		   (height (band-height band_xx))
	 	   (band_lEV (make-band width height 0.0))
	 	   (band_sEV (make-band width height 0.0))
	 	   (band_angle (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band_xx    ptr_band_xx :float :lisp-type single-float) 
						 (band_xy    ptr_band_xy :float :lisp-type single-float) 
						 (band_yy    ptr_band_yy :float :lisp-type single-float) 
						 (band_lEV   ptr_band_lEV :float :lisp-type single-float) 
						 (band_sEV   ptr_band_sEV :float :lisp-type single-float) 
						 (band_angle ptr_band_angle :float :lisp-type single-float))
						(vigra_tensoreigenrepresentation_c ptr_band_xx ptr_band_xy ptr_band_yy ptr_band_lEV ptr_band_sEV ptr_band_angle width height))))
    	(case result
      		((0) (list band_lEV band_sEV band_angle))
      		((1) (error "Error in vigracl.filters.tensoreigenrepresentation: Calculation of the  Eigenvalue Representation of the Tensor failed!")))))

(defun tensoreigenrepresentation (tensor_image)
  	(pivot-list (mapcar #'(lambda (tensor)  (tensoreigenrepresentation-band tensor)) (pivot-list tensor_image))))

;###############################################################################
;#########                    Trace of a Tensor                   ##############
(defcfun ("vigra_tensortrace_c" vigra_tensortrace_c) :int
	(band_xx :pointer)
	(band_xy :pointer)
	(band_yy :pointer)
	(band_trace :pointer)
	(width :int)
	(height :int))

(defun tensortrace-band (tensor)
  	(let* ((band_xx (first  tensor))
	 	   (band_xy (second tensor))
	 	   (band_yy (third  tensor))
	 	   (width  (band-width band_xx))
		   (height (band-height band_xx))
	 	   (band_trace (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band_xx    ptr_band_xx :float :lisp-type single-float) 
						 (band_xy    ptr_band_xy :float :lisp-type single-float) 
						 (band_yy    ptr_band_yy :float :lisp-type single-float) 
						 (band_trace ptr_band_trace :float :lisp-type single-float))
						(vigra_tensortrace_c ptr_band_xx ptr_band_xy ptr_band_yy ptr_band_trace width height))))
    (case result
      ((0) band_trace)
      ((1) (error "Error in vigracl.filters.tensortrace: Calculation of the trace of the Tensor failed!")))))

(defun tensortrace (tensor_image)
  	(mapcar #'(lambda (tensor)  (tensortrace-band tensor)) (pivot-list tensor_image)))

;###############################################################################
;#########     Edge/Corner Representation of a Tensor             ##############
(defcfun ("vigra_tensortoedgecorner_c" vigra_tensortoedgecorner_c) :int
	(band_xx :pointer)
	(band_xy :pointer)
	(band_yy :pointer)
	(band_edgeness :pointer)
	(band_orientation :pointer)
	(band_cornerness :pointer)
	(width :int)
	(height :int))

(defun tensortoedgecorner-band (tensor)
  	(let* ((band_xx (first  tensor))
	 	   (band_xy (second tensor))
	 	   (band_yy (third  tensor))
	 	   (width  (band-width band_xx))
		   (height (band-height band_xx))
	 	   (band_edgeness (make-band width height 0.0))
	 	   (band_orientation (make-band width height 0.0))
	 	   (band_cornerness (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band_xx    	   ptr_band_xx :float :lisp-type single-float) 
						 (band_xy    	   ptr_band_xy :float :lisp-type single-float) 
						 (band_yy    	   ptr_band_yy :float :lisp-type single-float) 
						 (band_edgeness    ptr_band_edgeness :float :lisp-type single-float) 
						 (band_orientation ptr_band_orientation :float :lisp-type single-float) 
						 (band_cornerness  ptr_band_cornerness :float :lisp-type single-float))
						(vigra_tensortoedgecorner_c ptr_band_xx ptr_band_xy ptr_band_yy ptr_band_edgeness ptr_band_orientation ptr_band_cornerness width height))))
    	(case result
      		((0) (list band_edgeness band_orientation band_cornerness))
      		((1) (error "Error in vigracl.filters.tensortoedgecorner: Calculation of the Edge/Corner Representation of the Tensor failed!")))))

(defun tensortoedgecorner (tensor_image)
  	(pivot-list (mapcar #'(lambda (tensor)  (tensortoedgecorner-band tensor)) (pivot-list tensor_image))))

;###############################################################################
;#########           Hourglass-Filtering  of a Tensor             ##############
(defcfun ("vigra_hourglassfilter_c" vigra_hourglassfilter_c) :int
	(band_xx :pointer)
	(band_xy :pointer)
	(band_yy :pointer)
	(band_hg_xx :pointer)
	(band_hg_xy :pointer)
	(band_hg_yy :pointer)
	(width :int)
	(height :int)
    (sigma :float)
    (rho :float))

(defun hourglassfilter-band (tensor sigma rho)
  	(let* ((band_xx (first  tensor))
	 	   (band_xy (second tensor))
	 	   (band_yy (third  tensor))
	 	   (width  (band-width band_xx))
		   (height (band-height band_xx))
	 	   (band_hg_xx (make-band width height 0.0))
	 	   (band_hg_xy (make-band width height 0.0))
	 	   (band_hg_yy (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band_xx    ptr_band_xx :float :lisp-type single-float) 
						 (band_xy    ptr_band_xy :float :lisp-type single-float) 
						 (band_yy    ptr_band_yy :float :lisp-type single-float) 
						 (band_hg_xx ptr_band_hg_xx :float :lisp-type single-float) 
						 (band_hg_xy ptr_band_hg_xy :float :lisp-type single-float) 
						 (band_hg_yy ptr_band_hg_yy :float :lisp-type single-float))
						(vigra_hourglassfilter_c ptr_band_xx ptr_band_xy ptr_band_yy ptr_band_hg_xx ptr_band_hg_xy ptr_band_hg_yy width height sigma rho))))
    	(case result
    		((0) (list band_hg_xx band_hg_xy band_hg_yy))
    		((1) (error "Error in vigracl.filters.hourglassfilter: Hourglass-Filtering  of the Tensor failed!")))))

(defun hourglassfilter (tensor_image sigma rho)
  	(pivot-list (mapcar #'(lambda (tensor)  (hourglassfilter-band tensor sigma rho)) (pivot-list tensor_image))))
