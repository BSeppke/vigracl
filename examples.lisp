;Always the first step: load the VIGRA CL Lib
(require 'asdf) 
(asdf:operate 'asdf:load-op 'vigracl)

(in-package :vigracl)

;Just as a simple example: My own gauss approx. filter for images
;Don't forget to compile -> enormeous speed-up!
(defun smooth_pixel (arr i j)
       (setf (aref arr i j) 
       	     (/ (+ (* 4 (aref arr i       j      ))
       	     	   (* 2 (aref arr (+ i 1) j      ))
	     	   (* 2 (aref arr (- i 1) j      ))
	     	   (* 2 (aref arr i       (+ j 1)))
	     	   (* 2 (aref arr i       (- j 1)))
       	     	   (* 1 (aref arr (+ i 1) (+ j 1)))
	     	   (* 1 (aref arr (+ i 1) (- j 1)))
	     	   (* 1 (aref arr (- i 1) (+ j 1)))
	     	   (* 1 (aref arr (- i 1) (- j 1))))
               16)))

(defun mysmooth-band (in_arr)
  (let ((arr (copy-array in_arr)))
      (do* ((j 1 (+ j 1)))
      	  ((= j (- (array-dimension arr 1) 1)) arr)
	      (do* ((i 1 (+ i 1)))
      	      	  ((= i (- (array-dimension arr 0) 1)))
		      (smooth_pixel arr i j)))))


;A second example: My own gauss approx. filter using convolution kernel
(defvar gauss_kernel (make-array '(3 3)
                    :element-type '(double-float)
                    :initial-element 0.0625d+00))
                    
(setf (aref gauss_kernel 1 0) 0.125d+00)
(setf (aref gauss_kernel 0 1) 0.125d+00)
(setf (aref gauss_kernel 1 1) 0.25d+00)
(setf (aref gauss_kernel 2 1) 0.125d+00)
(setf (aref gauss_kernel 1 2) 0.125d+00)

(defun load-example-image (filename)
	(let* ((img_basedir  (merge-pathnames "images/" vigracl-path))
		   (img_filename (merge-pathnames filename img_basedir)))
		(loadimage (namestring img_filename))))

(print "loading lenna-image")
(defvar img (load-example-image "lenna_face.png"))

(defvar img_padd (paddimage img 10 20 30 40))

(print "performance test gaussian smoothing")
(print "vigra-implicit-method:")
(time (defvar img_ig  (gsmooth img 0.5)))
(print "vigra-std.-convolution-method:")
(time (defvar img_eg (convolveimage img gauss_kernel)))
(print "my-own-method:")
(time (defvar img_mg (mapcar #'mysmooth-band img)))


(print "testing subimage and correlation facilities")
(defvar img_cut (subimage img 100 50 151 101)) ;;Mask needs to have odd size!

(defvar fcc_res (fastcrosscorrelation img img_cut))
(defvar fncc_res (fastnormalizedcrosscorrelation img img_cut))
(defvar pos_matches (localmaxima fncc_res))
(defvar neg_matches (localminima fncc_res))



; Tensor tests
(defvar img1_st  (structuretensor img 1.0 4.0))

; boundary tensor
(defvar img1_bt  (boundarytensor img 1.0))

; boundary tensor without 0 order parts
(defvar img1_bt1  (boundarytensor1 img 1.0))

;;tensor to eigen repr.
(defvar img1_st_te (tensoreigenrepresentation img1_st))

;tensor trace                
(defvar img1_st_tt (tensortrace img1_st))

;tensor to edge corner
(defvar img1_st_ec (tensortoedgecorner img1_st))

;tensor to hourglass-filtered tensor
(defvar img1_st_hg (hourglassfilter img1_st 1.0 1.0))

;FFT Tests following
(defvar imgrect (load-example-image  "rect.gif"))

;do fast fourier transform
(print "performing fft on image")
(time (defvar imgrectfft (fouriertransform imgrect)))

;Calculate the amplitude of the frequency spectrum
(defun complex-magnitude (real imag)
  (sqrt (+ (* real real) (* imag imag))))

(defvar imgfft (image-map #'complex-magnitude (first imgrectfft) (second imgrectfft)))

; Save:
;real part
(print "saving resulting fft images")
(saveimage (first imgrectfft) "rect-fft-real.png")
;imaginary part
(saveimage (second imgrectfft) "rect-fft-imag.png")
;amplitude
(saveimage imgfft "rect-fft.png")
;amplitude (sqrt scaled)
(saveimage (image-map #'sqrt imgfft) "rect-fft-sqrt.png")
;amplitude (log scaled)
(saveimage (image-map #'log imgfft) "rect-fft-log.png")

;Testing vigra methods
(print "performing watershed transform on resized gradient image")
(defvar img2  (regionimagetocrackedgeimage(labelimage
	    (watersheds
	     (ggradient 
	      (resizeimage img 800 800 4)
	      3.0))) 0.0))

(print "testing rotation and reflection functions on image")
(defvar img4 (reflectimage img 3))
(defvar img5 (rotateimage img 15.0 3))


(defvar matrix (make-array '(3 3)
                    :element-type '(double-float)
                    :initial-element 0.0d+00))
(defvar theta (/(* -15d+00 3.14157d+00) 180d+00))
(setf  (aref matrix 0 0) (cos theta))
(setf  (aref matrix 1 1) (cos theta))
(setf  (aref matrix 0 1)  (* -1.0 (sin theta)))
(setf  (aref matrix 1 0)  (sin theta))
(setf  (aref matrix 2 2)  1.0d+00)

(defvar img5aff (affinewarpimage img matrix 3))

(print "performing distance transform on canny edges of image")
(defvar img6 (distancetransform
		  (cannyedgeimage img 1.8 0.1 100.0)
		  0.0 2))

(print "testing difference of exponential edge detection on image")
(defvar img7 (differenceofexponentialedgeimage img 0.8 0.1 100.0))

(print "testing nearly all filters")
(defvar img8 (gsmooth img 3.0))
(defvar img9 (laplacianofgaussian img 3.0))
(defvar img10 (gsharpening img 0.5 3.0))
(defvar img11 (sharpening img 3.0))
(defvar img12 (nonlineardiffusion img 0.1 2.0))


(print "testing splineimageview facilities")
(defvar pos_x  34.23)
(defvar pos_y 123.23)
(defvar siv (create-splineimageview img 2))
	(splineimageview-value siv pos_x pos_y)
	(splineimageview-dx   siv pos_x pos_y)
	(splineimageview-dy   siv pos_x pos_y)
	(splineimageview-dxx  siv pos_x pos_y)
	(splineimageview-dxy  siv pos_x pos_y)
	(splineimageview-dyy  siv pos_x pos_y)
	(splineimageview-dx3  siv pos_x pos_y)
	(splineimageview-dxxy siv pos_x pos_y)
	(splineimageview-dxyy siv pos_x pos_y)
	(splineimageview-dy3  siv pos_x pos_y)
	(splineimageview-g2   siv pos_x pos_y)
	(splineimageview-g2x  siv pos_x pos_y)
	(splineimageview-g2y  siv pos_x pos_y)
	(splineimageview-g2xx siv pos_x pos_y)
	(splineimageview-g2xy siv pos_x pos_y)
	(splineimageview-g2yy siv pos_x pos_y)
(delete-splineimageview siv)

(print "saving resulting images")
(saveimage img2  "lenna-relabeled-watersheds-on-resized-gradient-image.png")

(saveimage img4  "lenna-reflected-both.png")
(saveimage img5  "lenna-rotated-15deg.png")
(saveimage img5aff  "lenna-rotated-15deg-aff.png")
(saveimage img6  "lenna-disttransform-on-canny.png")
(saveimage img7  "lenna-diff_of_exp.png")
(saveimage img8  "lenna-gsmooth-3.0.png")
(saveimage img9  "lenna-log-3.0.png")
(saveimage img10 "lenna-gsharpening-0.5-3.0.png")
(saveimage img11 "lenna-sharpening-3.0.png")
(saveimage img12 "lenna-nonlineardiffusion-0.1-2.0.png")
