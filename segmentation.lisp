(in-package #:vigracl)

;###############################################################################
;###################         Label image                    ####################
(defcfun ("vigra_labelimage_c" vigra_labelimage_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(eight_connectivity :boolean))
	
(defcfun ("vigra_labelimagewithbackground_c" vigra_labelimagewithbackground_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(eight_connectivity :boolean)
	(background :float))

(defun labelimage-band (band  &optional (eight_connectivity T) (background nil))
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(if background
							(vigra_labelimagewithbackground_c ptr_band ptr_band2 width height eight_connectivity background)
							(vigra_labelimage_c ptr_band ptr_band2 width height eight_connectivity)))))
    	(if (= result -1)
			(error "Error in vigracl.segmentation.labelimage: Labeling of image failed!")
     		band2)))

(defun labelimage (image  &optional (eight_connectivity T)  (background nil))
  	(mapcar #'(lambda (arr) (labelimage-band arr eight_connectivity background)) image))


;###############################################################################
;###################      Watershed Transform (Union-Find)  ####################
(defcfun ("vigra_watershedsunionfind_c" vigra_watershedsunionfind_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(eight_connectivity :boolean))

(defun watersheds-uf-band (band &optional (eight_connectivity T))
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_watershedsunionfind_c ptr_band ptr_band2 width height eight_connectivity))))
    	(if (= result -1)
			(error "Error in vigracl.segmentation.watersheds-uf: Watershed transform of image failed!")
     		band2)))
	  
(defun watersheds-uf (image &optional (eight_connectivity T))
  (mapcar #'(lambda (arr) (watersheds-uf-band arr eight_connectivity)) image))

;###############################################################################
;###################  Watershed Transform (Region-growing)  ####################
(defcfun ("vigra_watershedsregiongrowing_c" vigra_watershedsregiongrowing_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(eight_connectivity :boolean)
    (keep_contours :boolean)
    (use_turbo :boolean)
    (stop_cost :double))

(defun watersheds-rg-band (band &optional (seeds-band '()) (eight_connectivity T) (keep_contours nil) (use_turbo nil) (stop-cost -1.0))
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2 (if (null seeds-band)
                      (array-map #'(lambda (p) (- p 1.0)) (labelimage-band (localminima-band band)))
                      seeds-band))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_watershedsregiongrowing_c ptr_band ptr_band2 width height eight_connectivity keep_contours use_turbo (coerce stop-cost 'double-float)))))
    	(if (= result -1)
			(error "Error in vigracl.segmentation.watersheds-rg: Watershed transform of image failed!")
     		band2)))
	  
(defun watersheds-rg (image &optional (seeds '()) (eight_connectivity T) (keep_contours nil) (use_turbo nil) (stop-cost -1.0))
  (mapcar #'(lambda (arr seed_arr) (watersheds-rg-band arr seed_arr eight_connectivity keep_contours use_turbo stop-cost)) 
  	image
    (if (null seeds)
        (image-map #'(lambda (p) (- p 1.0)) (labelimage (localminima image)))
        seeds)))


;###############################################################################
;###################      SLIC Segmentation Algorithm       ####################
(defcfun ("vigra_slic_gray_c" vigra_slic_gray_c) :int
	(band :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(seedDistance :int)
	(intensityScaling :double)
	(iterations :int))

(defun slic-band (band  &optional (seedDistance 15) (intensityScaling 20.0) (iterations 40))
  	(let* ((width  (band-width band))
	 	   (height (band-height band))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band  ptr_band  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_slic_gray_c ptr_band ptr_band2 width height seedDistance (coerce intensityScaling 'double-float) iterations))))
    	(if (= result -1)
			(error "Error in vigracl.segmentation.slic-band: SLIC segmentation of image failed!")
     		band2)))

(defcfun ("vigra_slic_rgb_c" vigra_slic_rgb_c) :int
	(band_r :pointer)
	(band_g :pointer)
	(band_b :pointer)
	(band2 :pointer)
	(width :int)
	(height :int)
	(seedDistance :int)
	(intensityScaling :double)
	(iterations :int))

(defun slic-rgb (band_r band_g band_b  &optional (seedDistance 15) (intensityScaling 20.0) (iterations 40))
  	(let* ((width  (band-width band_r))
	 	   (height (band-height band_r))
	 	   (band2  (make-band width height 0.0))
	 	   (result (with-arrays-as-foreign-pointers
						((band_r  ptr_band_r  :float :lisp-type single-float) 
						 (band_g  ptr_band_g  :float :lisp-type single-float) 
						 (band_b  ptr_band_b  :float :lisp-type single-float) 
						 (band2 ptr_band2 :float :lisp-type single-float))
						(vigra_slic_rgb_c ptr_band_r ptr_band_g ptr_band_b ptr_band2 width height seedDistance (coerce intensityScaling 'double-float) iterations))))
    	(if (= result -1)
			(error "Error in vigracl.segmentation.slic-rgb: SLIC segmentation of image failed!")
     		band2)))
	  
(defun slic (image &optional (seedDistance 15) (intensityScaling 20.0) (iterations 40))
  (if (= (length image) 3)
      (list (slic-rgb (image->red-band image) (image->green-band image) (image->blue-band image) seedDistance intensityScaling iterations))
      (mapcar #'(lambda (arr) (slic-band arr seedDistance intensityScaling iterations)) image)))


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
	
	
	  
;###############################################################################
;###################           Feature Extraction            ###################    
(defcfun ("vigra_extractfeatures_gray_c" vigra_extractfeatures_gray_c) :int
	(band :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
	(height :int)
    (max_label :int))

(defun extractfeatures-band (band label-band  &optional (max_label (array-reduce #'max label-band 0.0)))
  :lisp (sb-vm::set-floating-point-modes :traps '())
  (let* ((ml     (round max_label))
  		 (width  (band-width  band))
         (height (band-height band))
         (band3  (make-band (+ ml 1) 11))
         (result (with-arrays-as-foreign-pointers
						((band        ptr_band        :float :lisp-type single-float) 
						 (label-band  ptr_label-band  :float :lisp-type single-float) 
						 (band3       ptr_band3       :float :lisp-type single-float))
				    (vigra_extractfeatures_gray_c ptr_band ptr_label-band ptr_band3 width height ml))))
    (case result
    	((0) band3)
        ((1) (error	"Error in vigracl.segmentation.vigra_extractfeatures_gray_c: Region-wise feature extraction of gray image failed!")))))

(defcfun ("vigra_extractfeatures_rgb_c" vigra_extractfeatures_rgb_c) :int
	(band_r :pointer)
	(band_g :pointer)
	(band_b :pointer)
	(band2 :pointer)
	(band3 :pointer)
	(width :int)
	(height :int)
    (max_label :int))
    
(defun extractfeatures-rgb (band_r band_g band_b label-band &optional (max_label (array-reduce #'max label-band 0.0)))
  :lisp (sb-vm::set-floating-point-modes :traps '())
  (let* ((ml     (round max_label))
  		 (width  (band-width  band_r))
         (height (band-height band_r))
         (band3  (make-band (+ ml 1) 19))
         (result (with-arrays-as-foreign-pointers
						((band_r      ptr_r_band      :float :lisp-type single-float) 
						 (band_g      ptr_g_band      :float :lisp-type single-float) 
						 (band_b      ptr_b_band      :float :lisp-type single-float) 
						 (label-band  ptr_label-band  :float :lisp-type single-float) 
						 (band3       ptr_band3       :float :lisp-type single-float))
					(vigra_extractfeatures_rgb_c ptr_r_band ptr_g_band ptr_b_band ptr_label-band ptr_band3 width height ml))))
    (case result
    	((0) band3)
        ((1) (error	"Error in vigracl.segmentation.vigra_extractfeatures_rgb_c: Region-wise feature extraction of rgb image failed!")))))
	  
(defun extractfeatures (image labels &optional (max_label (image-reduce #'max labels 0.0)))
  (if (and (= (length image) 3) (= (length labels) 1))
      (list (extractfeatures-rgb (first image) (second image) (third image) (first labels)))
      (mapcar #'extractfeatures-band image labels max_label)))