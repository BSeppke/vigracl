(in-package #:vigracl)

;###############################################################################
;###################      Creation of SplineimageViews      ####################
(defcfun ("vigra_create_splineimageview1_c" vigra_create_splineimageview1_c) :pointer
	(band :pointer)
	(width :int)
	(height :int))
	
(defcfun ("vigra_create_splineimageview2_c" vigra_create_splineimageview2_c) :pointer
	(band :pointer)
	(width :int)
	(height :int))
	
(defcfun ("vigra_create_splineimageview3_c" vigra_create_splineimageview3_c) :pointer
	(band :pointer)
	(width :int)
	(height :int))
	
(defcfun ("vigra_create_splineimageview4_c" vigra_create_splineimageview4_c) :pointer
	(band :pointer)
	(width :int)
	(height :int))
	
(defcfun ("vigra_create_splineimageview5_c" vigra_create_splineimageview5_c) :pointer
	(band :pointer)
	(width :int)
	(height :int))

(defun create-splineimageview-band (band deg)
	(let* ((width  (band-width  band))
		   (height (band-height band))
		   (ptr    (with-array-as-foreign-pointer
						(band  ptr_band  :float :lisp-type single-float) 
						(case deg
							((1)	(vigra_create_splineimageview1_c ptr_band width height))
							((2)	(vigra_create_splineimageview2_c ptr_band width height))
							((3)	(vigra_create_splineimageview3_c ptr_band width height))
							((4)	(vigra_create_splineimageview4_c ptr_band width height))
							((5)	(vigra_create_splineimageview5_c ptr_band width height))
							(else   (error "Error in vigracl.splineimageview.delete-splineimageview: incompatible pointers used!"))))))
		(cons ptr deg)))

(defun create-splineimageview (image deg)
  	(mapcar #'(lambda (arr) (create-splineimageview-band arr deg)) image))	


;###############################################################################
;###################      Deletion of SplineimageViews      ####################
(defcfun ("vigra_delete_splineimageview1_c" vigra_delete_splineimageview1_c) :int
	(siv_ptr :pointer))
	
(defcfun ("vigra_delete_splineimageview2_c" vigra_delete_splineimageview2_c) :int
	(siv_ptr :pointer))
	
(defcfun ("vigra_delete_splineimageview3_c" vigra_delete_splineimageview3_c) :int
	(siv_ptr :pointer))
	
(defcfun ("vigra_delete_splineimageview4_c" vigra_delete_splineimageview4_c) :int
	(siv_ptr :pointer))
	
(defcfun ("vigra_delete_splineimageview5_c" vigra_delete_splineimageview5_c) :int
	(siv_ptr :pointer))


(defun delete-splineimageview-band (siv)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv))
		   (result (case deg
						((1)	(vigra_delete_splineimageview1_c ptr))
						((2)	(vigra_delete_splineimageview2_c ptr))
						((3)	(vigra_delete_splineimageview3_c ptr))
						((4)	(vigra_delete_splineimageview4_c ptr))
						((5)	(vigra_delete_splineimageview5_c ptr))
						(else   (error "Error in vigracl.splineimageview.delete-splineimageview: incompatible pointers used!")))))
		(case result
			((0) (rplaca siv  (null-pointer)))
			((1) (error "Error in vigracl.splineimageview.delete-splineimageview: Unable to delete pointer")))))

(defun delete-splineimageview (siv_image)
	(mapcar #'(lambda (siv) (delete-splineimageview-band siv)) siv_image))


;###############################################################################
;###################   Value Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_accessor_c" vigra_splineimageview1_accessor_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_accessor_c" vigra_splineimageview2_accessor_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_accessor_c" vigra_splineimageview3_accessor_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_accessor_c" vigra_splineimageview4_accessor_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_accessor_c" vigra_splineimageview5_accessor_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-value-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_accessor_c ptr x y))
			((2)	(vigra_splineimageview2_accessor_c ptr x y))
			((3)	(vigra_splineimageview3_accessor_c ptr x y))
			((4)	(vigra_splineimageview4_accessor_c ptr x y))
			((5)	(vigra_splineimageview5_accessor_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-value: incompatible pointers used!")))))

(defun splineimageview-value (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-value-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))
	
;###############################################################################
;###################      dx Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_dx_c" vigra_splineimageview1_dx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_dx_c" vigra_splineimageview2_dx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_dx_c" vigra_splineimageview3_dx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_dx_c" vigra_splineimageview4_dx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_dx_c" vigra_splineimageview5_dx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-dx-band (siv x y)
	(let* ((ptr  (car siv))
	       (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_dx_c ptr x y))
			((2)	(vigra_splineimageview2_dx_c ptr x y))
			((3)	(vigra_splineimageview3_dx_c ptr x y))
			((4)	(vigra_splineimageview4_dx_c ptr x y))
			((5)	(vigra_splineimageview5_dx_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-dx: incompatible pointers used!")))))

(defun splineimageview-dx (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-dx-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))
	
;###############################################################################
;###################      dy Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_dy_c" vigra_splineimageview1_dy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_dy_c" vigra_splineimageview2_dy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_dy_c" vigra_splineimageview3_dy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_dy_c" vigra_splineimageview4_dy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_dy_c" vigra_splineimageview5_dy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-dy-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_dy_c ptr x y))
			((2)	(vigra_splineimageview2_dy_c ptr x y))
			((3)	(vigra_splineimageview3_dy_c ptr x y))
			((4)	(vigra_splineimageview4_dy_c ptr x y))
			((5)	(vigra_splineimageview5_dy_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-dy: incompatible pointers used!")))))

(defun splineimageview-dy (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-dy-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))	
	
;###############################################################################
;###################      dxx Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_dxx_c" vigra_splineimageview1_dxx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_dxx_c" vigra_splineimageview2_dxx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_dxx_c" vigra_splineimageview3_dxx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_dxx_c" vigra_splineimageview4_dxx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_dxx_c" vigra_splineimageview5_dxx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-dxx-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_dxx_c ptr x y))
			((2)	(vigra_splineimageview2_dxx_c ptr x y))
			((3)	(vigra_splineimageview3_dxx_c ptr x y))
			((4)	(vigra_splineimageview4_dxx_c ptr x y))
			((5)	(vigra_splineimageview5_dxx_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-dxx: incompatible pointers used!")))))

(defun splineimageview-dxx (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-dxx-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))
	
;###############################################################################
;###################      dxy Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_dxy_c" vigra_splineimageview1_dxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_dxy_c" vigra_splineimageview2_dxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_dxy_c" vigra_splineimageview3_dxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_dxy_c" vigra_splineimageview4_dxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_dxy_c" vigra_splineimageview5_dxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-dxy-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_dxy_c ptr x y))
			((2)	(vigra_splineimageview2_dxy_c ptr x y))
			((3)	(vigra_splineimageview3_dxy_c ptr x y))
			((4)	(vigra_splineimageview4_dxy_c ptr x y))
			((5)	(vigra_splineimageview5_dxy_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-dxy: incompatible pointers used!")))))

(defun splineimageview-dxy (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-dxy-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))

	
;###############################################################################
;###################      dyy Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_dyy_c" vigra_splineimageview1_dyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_dyy_c" vigra_splineimageview2_dyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_dyy_c" vigra_splineimageview3_dyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_dyy_c" vigra_splineimageview4_dyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_dyy_c" vigra_splineimageview5_dyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-dyy-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_dyy_c ptr x y))
			((2)	(vigra_splineimageview2_dyy_c ptr x y))
			((3)	(vigra_splineimageview3_dyy_c ptr x y))
			((4)	(vigra_splineimageview4_dyy_c ptr x y))
			((5)	(vigra_splineimageview5_dyy_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-dyy: incompatible pointers used!")))))

(defun splineimageview-dyy (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-dyy-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))

	
;###############################################################################
;###################      dx3 Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_dx3_c" vigra_splineimageview1_dx3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_dx3_c" vigra_splineimageview2_dx3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_dx3_c" vigra_splineimageview3_dx3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_dx3_c" vigra_splineimageview4_dx3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_dx3_c" vigra_splineimageview5_dx3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-dx3-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_dx3_c ptr x y))
			((2)	(vigra_splineimageview2_dx3_c ptr x y))
			((3)	(vigra_splineimageview3_dx3_c ptr x y))
			((4)	(vigra_splineimageview4_dx3_c ptr x y))
			((5)	(vigra_splineimageview5_dx3_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-dx3: incompatible pointers used!")))))

(defun splineimageview-dx3 (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-dx3-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))

	
;###############################################################################
;###################      dxxy Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_dxxy_c" vigra_splineimageview1_dxxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_dxxy_c" vigra_splineimageview2_dxxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_dxxy_c" vigra_splineimageview3_dxxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_dxxy_c" vigra_splineimageview4_dxxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_dxxy_c" vigra_splineimageview5_dxxy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-dxxy-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_dxxy_c ptr x y))
			((2)	(vigra_splineimageview2_dxxy_c ptr x y))
			((3)	(vigra_splineimageview3_dxxy_c ptr x y))
			((4)	(vigra_splineimageview4_dxxy_c ptr x y))
			((5)	(vigra_splineimageview5_dxxy_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-dxxy: incompatible pointers used!")))))

(defun splineimageview-dxxy (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-dxxy-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))
	
;###############################################################################
;###################      dxyy Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_dxyy_c" vigra_splineimageview1_dxyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_dxyy_c" vigra_splineimageview2_dxyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_dxyy_c" vigra_splineimageview3_dxyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_dxyy_c" vigra_splineimageview4_dxyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_dxyy_c" vigra_splineimageview5_dxyy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-dxyy-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_dxyy_c ptr x y))
			((2)	(vigra_splineimageview2_dxyy_c ptr x y))
			((3)	(vigra_splineimageview3_dxyy_c ptr x y))
			((4)	(vigra_splineimageview4_dxyy_c ptr x y))
			((5)	(vigra_splineimageview5_dxyy_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-dxyy: incompatible pointers used!")))))

(defun splineimageview-dxyy (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-dxyy-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))
	
	
;###############################################################################
;###################      dy3 Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_dy3_c" vigra_splineimageview1_dy3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_dy3_c" vigra_splineimageview2_dy3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_dy3_c" vigra_splineimageview3_dy3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_dy3_c" vigra_splineimageview4_dy3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_dy3_c" vigra_splineimageview5_dy3_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-dy3-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_dy3_c ptr x y))
			((2)	(vigra_splineimageview2_dy3_c ptr x y))
			((3)	(vigra_splineimageview3_dy3_c ptr x y))
			((4)	(vigra_splineimageview4_dy3_c ptr x y))
			((5)	(vigra_splineimageview5_dy3_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-dy3: incompatible pointers used!")))))

(defun splineimageview-dy3 (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-dy3-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))
	
;###############################################################################
;###################      g2 Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_g2_c" vigra_splineimageview1_g2_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_g2_c" vigra_splineimageview2_g2_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_g2_c" vigra_splineimageview3_g2_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_g2_c" vigra_splineimageview4_g2_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_g2_c" vigra_splineimageview5_g2_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-g2-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_g2_c ptr x y))
			((2)	(vigra_splineimageview2_g2_c ptr x y))
			((3)	(vigra_splineimageview3_g2_c ptr x y))
			((4)	(vigra_splineimageview4_g2_c ptr x y))
			((5)	(vigra_splineimageview5_g2_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-g2: incompatible pointers used!")))))

(defun splineimageview-g2 (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-g2-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))
	
;###############################################################################
;###################      g2x Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_g2x_c" vigra_splineimageview1_g2x_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_g2x_c" vigra_splineimageview2_g2x_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_g2x_c" vigra_splineimageview3_g2x_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_g2x_c" vigra_splineimageview4_g2x_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_g2x_c" vigra_splineimageview5_g2x_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-g2x-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_g2x_c ptr x y))
			((2)	(vigra_splineimageview2_g2x_c ptr x y))
			((3)	(vigra_splineimageview3_g2x_c ptr x y))
			((4)	(vigra_splineimageview4_g2x_c ptr x y))
			((5)	(vigra_splineimageview5_g2x_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-g2x: incompatible pointers used!")))))

(defun splineimageview-g2x (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-g2x-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))
	
;###############################################################################
;###################      g2y Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_g2y_c" vigra_splineimageview1_g2y_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_g2y_c" vigra_splineimageview2_g2y_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_g2y_c" vigra_splineimageview3_g2y_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_g2y_c" vigra_splineimageview4_g2y_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_g2y_c" vigra_splineimageview5_g2y_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-g2y-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_g2y_c ptr x y))
			((2)	(vigra_splineimageview2_g2y_c ptr x y))
			((3)	(vigra_splineimageview3_g2y_c ptr x y))
			((4)	(vigra_splineimageview4_g2y_c ptr x y))
			((5)	(vigra_splineimageview5_g2y_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-g2y: incompatible pointers used!")))))

(defun splineimageview-g2y (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-g2y-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))

;###############################################################################
;###################      g2xx Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_g2xx_c" vigra_splineimageview1_g2xx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_g2xx_c" vigra_splineimageview2_g2xx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_g2xx_c" vigra_splineimageview3_g2xx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_g2xx_c" vigra_splineimageview4_g2xx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_g2xx_c" vigra_splineimageview5_g2xx_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-g2xx-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_g2xx_c ptr x y))
			((2)	(vigra_splineimageview2_g2xx_c ptr x y))
			((3)	(vigra_splineimageview3_g2xx_c ptr x y))
			((4)	(vigra_splineimageview4_g2xx_c ptr x y))
			((5)	(vigra_splineimageview5_g2xx_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-g2xx: incompatible pointers used!")))))

(defun splineimageview-g2xx (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-g2xx-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))

;###############################################################################
;###################      g2xy Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_g2xy_c" vigra_splineimageview1_g2xy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_g2xy_c" vigra_splineimageview2_g2xy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_g2xy_c" vigra_splineimageview3_g2xy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_g2xy_c" vigra_splineimageview4_g2xy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_g2xy_c" vigra_splineimageview5_g2xy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-g2xy-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_g2xy_c ptr x y))
			((2)	(vigra_splineimageview2_g2xy_c ptr x y))
			((3)	(vigra_splineimageview3_g2xy_c ptr x y))
			((4)	(vigra_splineimageview4_g2xy_c ptr x y))
			((5)	(vigra_splineimageview5_g2xy_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-g2xy: incompatible pointers used!")))))

(defun splineimageview-g2xy (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-g2xy-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))

;###############################################################################
;###################      g2yy Access of SplineimageViews     ####################
(defcfun ("vigra_splineimageview1_g2yy_c" vigra_splineimageview1_g2yy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview2_g2yy_c" vigra_splineimageview2_g2yy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview3_g2yy_c" vigra_splineimageview3_g2yy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview4_g2yy_c" vigra_splineimageview4_g2yy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defcfun ("vigra_splineimageview5_g2yy_c" vigra_splineimageview5_g2yy_c) :float
	(siv_ptr :pointer)
	(x :double)
	(y :double))
	
(defun splineimageview-g2yy-band (siv x y)
	(let* ((ptr  (car siv))
		   (deg  (cdr siv)))
		(case deg
			((1)	(vigra_splineimageview1_g2yy_c ptr x y))
			((2)	(vigra_splineimageview2_g2yy_c ptr x y))
			((3)	(vigra_splineimageview3_g2yy_c ptr x y))
			((4)	(vigra_splineimageview4_g2yy_c ptr x y))
			((5)	(vigra_splineimageview5_g2yy_c ptr x y))
			(else   (error "Error in vigracl.splineimageview.splineimageview-g2yy: incompatible pointers used!")))))

(defun splineimageview-g2yy (siv_image x y)
	(mapcar #'(lambda (siv) (splineimageview-g2yy-band siv (coerce x 'double-float) (coerce y 'double-float))) siv_image))