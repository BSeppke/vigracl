(in-package #:vigracl)

;###############################################################################
;###################          Generic Convolution           ####################
(defcfun ("vigra_convolveimage_c" vigra_convolveimage_c) :int
	(band :pointer)
	(kernel :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(kernel_width :int)
	(kernel_height :int))
	
(defun convolveimage-band (band kernelMat)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (kernel_width  (band-width kernelMat))
	 	   (kernel_height (band-height kernelMat))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  	ptr_band  	  :float  :lisp-type single-float)  
						 (kernelMat	ptr_kernelMat :double :lisp-type double-float)
						 (band2 	ptr_band2 	  :float  :lisp-type single-float))
						(vigra_convolveimage_c ptr_band ptr_kernelMat ptr_band2 width height  kernel_width kernel_height))))
    	(case result
     		((0) band2)
     		((1) (error "Error in vigracl.filters.colvolveimage: Convolution with kernel failed!"))
      		((2) (error "Error in vigracl.filters.colvolveimage: Kernel dimensions must be odd!")))))

(defun convolveimage (image kernelMat)
  	(mapcar #'(lambda (arr) (convolveimage-band arr kernelMat)) image))


;###############################################################################
;###################        Separable Convolution           ####################
(defcfun ("vigra_separableconvolveimage_c" vigra_separableconvolveimage_c) :int
	(band :pointer)
	(kernel_h :pointer)
	(kernel_v :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(kernel_width :int)
	(kernel_height :int))

(defun separableconvolveimage-band (band kernel_h kernel_v)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (kernel_width  (band-width kernel_h))
	 	   (kernel_height (band-height kernel_v))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  	ptr_band  	 :float  :lisp-type single-float) 
						 (kernel_h	ptr_kernel_h :double :lisp-type double-float) 
						 (kernel_v	ptr_kernel_v :double :lisp-type double-float)
						 (band2 	ptr_band2 	 :float  :lisp-type single-float))
						(vigra_separableconvolveimage_c ptr_band ptr_kernel_h  ptr_kernel_v ptr_band2 width height  kernel_width kernel_height))))
    	(case result
     		((0) band2)
     		((1) (error "Error in vigracl.filters.separablecolvolveimage: Convolution with kernel failed!"))
      		((2) (error "Error in vigracl.filters.separablecolvolveimage: Kernel dimensions must be odd!")))))

(defun separableconvolveimage (image kernel_h kernel_v)
  	(mapcar #'(lambda (arr) (separableconvolveimage-band arr kernel_h kernel_v)) image))


;###############################################################################
;###################          Gaussian Smoothing            ####################
(defcfun ("vigra_gaussiansmoothing_c" vigra_gaussiansmoothing_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(sigma   :float))

(defun gsmooth-band (band sigma)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2 (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float)) 
    					(vigra_gaussiansmoothing_c ptr_band ptr_band2 width height sigma))))
    	(case result
      		((0) band2)
      		((1) (error "Error in vigracl.filters.gsmooth: Gaussian smoothing failed!")))))

(defun gsmooth (image sigma)
  	(mapcar #'(lambda (arr) (gsmooth-band arr sigma)) image))

;###############################################################################
;###################      Gaussian Gradient (nX & nY)       ####################
(defcfun ("vigra_gaussiangradient_c" vigra_gaussiangradient_c) :int
	(band :pointer)
	(band_x :pointer)
	(band_y :pointer)
	(width :int)
	(height :int)
	(sigma   :float))

(defun gaussiangradient-band (band sigma)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band_x (make-band width height 0.0))
	 	   (band_y (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band   ptr_band   :float :lisp-type single-float) 
						 (band_x ptr_band_x :float :lisp-type single-float) 
						 (band_y ptr_band_y :float :lisp-type single-float))
						(vigra_gaussiangradient_c ptr_band ptr_band_x ptr_band_y width height sigma))))
    	(case result
      		((0) (list band_x band_y))
      		((1) (error "Error in vigracl.filters.gaussiangradient: Gaussian gradient magnitude failed!")))))

(defun gaussiangradient (image sigma)
  	(pivot-list (mapcar #'(lambda (arr) (gaussiangradient-band arr sigma)) image)))

;###############################################################################
;###################      Gaussian Gradient (Magnitude)     ####################
(defcfun ("vigra_gaussiangradientmagnitude_c" vigra_gaussiangradientmagnitude_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(sigma   :float))

(defun ggradient-band (band sigma)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2 (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_gaussiangradientmagnitude_c ptr_band ptr_band2 width height sigma))))
    	(case result
     		((0) band2)
      		((1) (error "Error in vigracl.filters.ggradient: Gaussian gradient magnitude failed!")))))

(defun ggradient (image sigma)
 	(mapcar #'(lambda (arr) (ggradient-band arr sigma)) image))

;###############################################################################
;###################        Laplacian Of Gaussian           ####################
(defcfun ("vigra_laplacianofgaussian_c" vigra_laplacianofgaussian_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(scale :float))

(defun laplacianofgaussian-band (band scale)
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2 (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_laplacianofgaussian_c ptr_band ptr_band2 width height scale))))
    	(case result
      		((0) band2)
		    ((1) (error "Error in vigracl.filters.laplacianofgaussian: Laplacian of Gaussian failed!")))))

(defun laplacianofgaussian (image scale)
  	(mapcar #'(lambda (arr) (laplacianofgaussian-band arr scale)) image))

  

;###############################################################################
;#############    Hessian Matrix of 2. order deriv gaussians      ##############
(defcfun ("vigra_hessianmatrixofgaussian_c" vigra_hessianmatrixofgaussian_c) :int
	(band :pointer)
	(band_xx :pointer)
	(band_xy :pointer)
	(band_yy :pointer)
	(width :int)
	(height :int)
	(scale   :float))

(defun hessianmatrixofgaussian-band (band scale)
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
						(vigra_hessianmatrixofgaussian_c ptr_band ptr_band_xx ptr_band_xy ptr_band_yy width height scale))))
    	(case result
      		((0) (list band_xx band_xy band_yy))
      		((1) (error "Error in vigracl.filters.hessianmatrixofgaussian: Calculation of Hessian Matrix (of gaussian 2. deriv.) failed!")))))

(defun hessianmatrixofgaussian (image scale)
  	(pivot-list (mapcar #'(lambda (arr)  (hessianmatrixofgaussian-band arr scale)) image)))

;###############################################################################
;###################          Gaussian Sharpening           ####################
(defcfun ("vigra_gaussiansharpening_c" vigra_gaussiansharpening_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
    (sharpening_factor :float)
    (scale :float))

(defun gsharpening-band (band sharpening_factor scale)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2 (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						 (vigra_gaussiansharpening_c ptr_band ptr_band2 width height sharpening_factor scale))))
    	(case result
      		((0) band2)
      		((1) (error "Error in vigracl.filters.gsharpening: Gaussian sharpening failed!")))))

(defun gsharpening (image sharpening_factor scale)
  	(mapcar #'(lambda (arr) (gsharpening-band arr sharpening_factor scale)) image))


;###############################################################################
;###################            Simple Sharpening           ####################

(defcfun ("vigra_simplesharpening_c" vigra_simplesharpening_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
    (sharpening_factor :float))

(defun sharpening-band (band sharpening_factor)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2 (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_simplesharpening_c ptr_band ptr_band2 width height sharpening_factor))))
    	(case result
      		((0) band2)
		    ((1) (error "Error in vigracl.filters.sharpening: Simple sharpening failed!")))))

(defun sharpening (image sharpening_factor )
  (mapcar #'(lambda (arr) (sharpening-band arr sharpening_factor )) image))
    

;###############################################################################
;###################          Median Filtering              ####################
(defcfun ("vigra_medianfilter_c" vigra_medianfilter_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int) 
	(height :int)
	(window_width :int) 
	(window_height :int))

(defun medianfilter-band (band window_width window_height)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2 (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_medianfilter_c ptr_band ptr_band2 width height window_width window_height))))
    	(case result
      		((0) band2)
      		((1) (error "Error in vigracl.filters.medianfilter: Median filtering failed!!")))))

(defun medianfilter (image window_width window_height)
  	(mapcar #'(lambda (arr) (medianfilter-band arr window_width window_height)) image))
       

;###############################################################################
;###################          Nonlinear Diffusion           ####################
(defcfun ("vigra_nonlineardiffusion_c" vigra_nonlineardiffusion_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int) 
	(height :int)
	(edge_threshold :float)
	(scale :float))

(defun nonlineardiffusion-band (band edge_threshold scale)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2 (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_nonlineardiffusion_c ptr_band ptr_band2 width height edge_threshold scale))))
    	(case result
      		((0) band2)
      		((1) (error "Error in vigracl.filters.nonlineardiffusion: Non linear Diffusion failed!!")))))

(defun nonlineardiffusion (image edge_threshold scale)
  	(mapcar #'(lambda (arr) (nonlineardiffusion-band arr edge_threshold scale)) image))
   
;###############################################################################
;###################             Shock Filtering            ####################

(defcfun ("vigra_shockfilter_c" vigra_shockfilter_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
    (sigma :float)
    (rho :float)
    (upwind_factor_h :float)
	(iterations :int))

(defun shockfilter-band (band sigma rho upwind_factor_h iterations)
  	(let* ((width  (band-width band))
		   (height (band-height band))
	 	   (band2 (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_shockfilter_c ptr_band ptr_band2 width height sigma rho upwind_factor_h iterations))))
    	(case result
      		((0) band2)
		    ((1) (error "Error in vigracl.filters.shockfilter: Shock filtering failed!"))
		    ((2) (error "Error in vigracl.filters.shockfilter: Iterations need to be > 0!")))))

(defun shockfilter (image sigma rho upwind_factor_h iterations)
  (mapcar #'(lambda (arr) (shockfilter-band arr sigma rho upwind_factor_h iterations)) image))