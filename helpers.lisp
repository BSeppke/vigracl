(in-package #:vigracl)

;########################## General helpers ############################
;pivoting of two-dimensional lists:
; '((st_r_xx st_r_xy st_r_yy)           '((st_r_xx st_g_xx st_b_xx)
;   (st_g_xx st_g_xy st_g_yy)    --->     (st_r_xy st_g_xy st_b_xy)
;   (st_b_xx st_b_xy st_b_yy))            (st_r_yy st_g_yy st_b_yy)
(defun pivot-list (xs)
  	(if (null (car xs))
    	'()
     	(cons (mapcar #'car xs)
           	(pivot-list (mapcar #'cdr xs)))))

(defun curry (function &rest args)
    (lambda (&rest more-args)
      	(apply function (append args more-args))))

(defun copy-array (array)
  	"Copies an array into a ewly allocated one -> fast."
  	(let ((a (make-array (array-dimensions array) :adjustable nil)))
   		 (dotimes (i (array-total-size a) a)
      		(setf (row-major-aref a i) (row-major-aref array i)))))
(compile 'copy-array)

(defun array-map (func &rest arrays)  
	"Maps a given function to each element of the array and returns a new array"
  	(if (null (car arrays))
    	'()
    	(let* ((new_array   (make-array (array-dimensions (car arrays))
        					            :element-type (array-element-type (car arrays))
                    					:initial-element 0.0))
	   			(width  (array-dimension new_array 0))
           		(height (array-dimension new_array 1)))
      		(do* ((j 0 (+ j 1)))
	  			 ((= j height) new_array)
				(do* ((i 0 (+ i 1)))
	   				 ((= i width))
	  				(setf (aref new_array i j) 
	    				  (apply func (mapcar #'(lambda(array) (aref array i j)) arrays))))))))
(compile 'array-map)



;; Do broadcasting of bands iff all channel sizes are equally large and some
;; do just have one channel -> pump the single channels up!
(defun band-broadcasting (images)
  (if (null images)
      '()
      (let* ((all_band_counts (mapcar #'length images))
             (max_band_count  (apply #'max all_band_counts))
             (all_equal?      (every #'(lambda (x) (or (= 1 x) (= max_band_count x))) all_band_counts)))
        (if all_equal?
            (if (> max_band_count 1)
               (mapcar #'(lambda (img) (if (= (length img) max_band_count)
                                      img
                                      (make-list max_band_count :initial-element (car img))))
                    images)
               images)
            (error "vigracl.helpers.band-broadcasting: Cannot broadcast bands for processing!")))))


(defun image-map/unsafe (func &rest images)  
  	(if (null (car images))
      	'()
      	(cons (apply (curry #'array-map func)        (mapcar #'car images))
          	  (apply (curry #'image-map/unsafe func) (mapcar #'cdr images)))))
(compile 'image-map/unsafe)

(defun image-map (func &rest images)  
	(apply (curry #'image-map/unsafe func) (band-broadcasting images)))
(compile 'image-map)

(defun array-map! (func &rest arrays)  
  	"Maps a given function to each element of the array and returns a new array"
  	(if (null (car arrays))
      	'()
   		(let* ((my_array (car arrays))
	   		   (width  (array-dimension my_array 0))
           	   (height (array-dimension my_array 1)))
      		(do* ((j 0 (+ j 1)))
	 			 ((= j height) my_array)
				(do* ((i 0 (+ i 1)))
	   				 ((= i width))
	  				(setf (aref my_array i j) 
	    				  (apply func (mapcar #'(lambda(array) (aref array i j)) arrays))))))))
(compile 'array-map!)

(defun image-map!/unsafe (func &rest images)  
  	(if (null (car images))
		'()
      	(cons (apply (curry #'array-map! func)        (mapcar #'car images))
        	  (apply (curry #'image-map!/unsafe func) (mapcar #'cdr images)))))
(compile 'image-map!/unsafe)

(defun image-map! (func &rest images)  
	(apply (curry #'image-map!/unsafe func) (band-broadcasting images)))
(compile 'image-map!)

(defun array-reduce (func array seed)  
  	"Reduces an array to one single value as the std. reduce does for lists"
   	(let* ((width  (array-dimension array 0))
	  		(height (array-dimension array 1))
	  		(reduce_var seed))
      	(do* ((j 0 (+ j 1)))
	  		 ((= j height) reduce_var)
			(do* ((i 0 (+ i 1)))
	   			 ((= i width))
	    		(setf reduce_var 
	    			  (funcall func reduce_var  (aref array i j) ))))))
(compile 'array-reduce)

(defun image-reduce (func image seed)
  	(mapcar #'(lambda (band) (array-reduce func band seed)) image))
(compile 'image-reduce)

(defun array-for-each-index (func array)  
  	"Maps a given function to each pixel coordinate of the array and returns a new array"
   	(let* ((width  (array-dimension array 0))
           (height (array-dimension array 1)))
    	(do* ((j 0 (+ j 1)))
             ((= j height) array)
        	(do* ((i 0 (+ i 1)))
                 ((= i width))
				(funcall func i j)))))
(compile 'array-for-each-index)

(defun image-for-each-index (func image)  
  	"Maps a given function to each pixel coordinate and band_id  - (x y band_id) -> void -  of the array and returns a new array"
  	(let* ((numbands (length image))
	  	   (width  (array-dimension (car image) 0))
           (height (array-dimension (car image) 1)))
        (do* ((b 0 (+ b 1)))
             ((= b numbands) image)
        	(do* ((j 0 (+ j 1)))
                 ((= j height))
            	(do* ((i 0 (+ i 1)))
                     ((= i width))
			   		(funcall func i j b))))))


;######################### Image specific functions ############################

; create a band from width/height
; and optional: initial intensity
(defun make-band (width height &rest init-val)
	(make-array (list width height) 
			     :element-type 		'(single-float)
			     :initial-element 	(if (car init-val) 
			     						(car init-val) 
			     						0.0)))

; band accessor: width of a band
(defun band-width (band)
	(array-dimension band 0))

; band accessor: height of a band
(defun band-height (band)
	(array-dimension band 1))
	
; create an image from width/height
; and optional: initial color
(defun make-image (width height numBands &rest init-val)
  	(if (= numBands 0)
      	'() 
      	(if (= (length init-val) numBands)
         	 (cons (make-band width height (car init-val))
                   (apply (curry #'make-image width height (- numBands 1)) (cdr init-val)))
        	  "Error in vigracl.helpers.make-image error: if init-vals are used, there have to be as many values as bands")))
  
; image accessor: width of an image
(defun image-width (image)
	(array-dimension (car image) 0))

; image accessor: height of an image
(defun image-height (image)
	(array-dimension (car image) 1))

; image accessor: number of bands of an image
(defun image-numbands (image)
	(length image))

; better accessors - ref for bands of an image
(defun image-band (image band_id)
	(nth band_id image))

; better accessors - ref for images
(defun image-ref (image x y band_id)
	(aref (image-band image band_id) x y))
  
; better accessors - set for images
(defun image-set! (image x y band_id val)
	(set (image-ref image x y band_id) val))

; copying of images
(defun copy-image (image)
	(mapcar #'copy-array image))

; image -> channel bands
(defun image->red-band (image)
	(if (<= 3 (image-numbands image) 4)
    	(image-band image 0)
    	"Error in vigracl.helpers.image->red-band: Band extraction is only allowed for 3(4)-channel RGB(A) images"))

(defun image->green-band (image)
  	(if (<= 3 (image-numbands image) 4)
     	(image-band image 1)
     	"Error in vigracl.helpers.image->green-band: Band extraction is only allowed for 3(4)-channel RGB(A) images"))

(defun image->blue-band (image)
  	(if (<= 3 (image-numbands image) 4)
      	(image-band image 2)
      	"Error in vigracl.helpers.image->blue-band: Band extraction is only allowed for 3(4)-channel RGB(A) images"))

(defun image->alpha-band (image)
  	(if (= (image-numbands image) 4)
      	(image-band image 3)
      	"Error in vigracl.helpers.image->alpha-band: Band extraction is only allowed for 4-channel RGBA images"))

; image -> channel images
(defun image->red (image)
  	(list (image->red-band image)))

(defun image->green (image)
  	(list (image->green-band image)))

(defun image->blue (image)
  	(list (image->blue-band image)))

(defun image->alpha (image)
  	(list (image->alpha-band image)))