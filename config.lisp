(in-package #:vigracl)

;; Module constants
(defvar vigracl-path  (asdf:system-source-directory 'vigracl))
(defvar vigracl-version "1.0.0")

;;According to the cl-cookbook
(defun split-string-at-char (string char)
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))

(defun split-string-at-dot (string)
    (split-string-at-char string #\.))
    
(defun split-string-at-newline (string)
    (split-string-at-char string #\newline))

;; For windows, we need to find out, which architecture CL is built
(defvar cl-bits (* 8 (foreign-type-size :pointer)))
(defvar cmake-flags (if (= cl-bits 32)
                        "-DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS=-m32 -DCMAKE_C_FLAGS=-m32"
                        "-DCMAKE_BUILD_TYPE=Release"))

;; Define the filename of the foreign library depending on the OS
(defvar vigracl-dylib-file
	#+darwin  "libvigra_c.dylib"
	#+unix    "libvigra_c.so"
	#+windows "vigra_c.dll")
	
(defvar vigracl-dylib-path (merge-pathnames vigracl-path vigracl-dylib-file))

(defvar vigra_c-path 	 (merge-pathnames "vigra_c/" vigracl-path))
(defvar vigra_c-bin-path (merge-pathnames "bin/" vigra_c-path))

(defun system-call (command)
 	(if (eq (uiop:run-program command) NIL)
 		(let* ((result (split-string-at-newline 
 							(with-output-to-string 
 								(asdf::*verbose-out*) 
 								(asdf:run-shell-command command)))))
 		(if (> (length result) 1)
 			(apply #'concatenate 'string (cdr result))  ;;Everything is okay - return the result (string list), else return NIL(FALSE):
 			NIL))
 		NIL))

(defun vigra-version ()
  (let* ((version_string (system-call "vigra-config --version")))
    (if (> (length version_string) 0)
        (progn (stringp version_string)
               (mapcar #'parse-integer (split-string-at-dot version_string)))
        '())))

(defun vigra-installed? () 
  (print "Searching for vigra >= 1.11.0 using 'vigra-config --version':")
  (let ((version (vigra-version)))
    (if (null version)
        NIL
        (or (and (= (first version) 1) (>= (second version) 11))
                (> (first version) 1)))))
  
; The compilation routine (at least for macosx and unix)
(defun build-vigra_c ()
  #+unix (if (vigra-installed?)
         	 ;;VIGRA is found! Try to compile vigra_c bindings
          	 (if (stringp (system-call (concatenate 'string  "cd " (namestring vigra_c-path) " && mkdir -p build && cd build && cmake " cmake-flags " .. && make && cd .. && rm -rf ./build")))
                 (stringp (system-call (concatenate 'string  "cp " (namestring (merge-pathnames vigra_c-bin-path vigracl-dylib-file)) " "
                								  		  (namestring vigracl-dylib-path))))
                 (error "making the vigra_c lib failed, although vigra seems to be installed"))
         	(error "Vigra is not found. Please check if the prefix path is set correctly in HOME/.profile environment file!"))
         	
  #+windows (let* ((bin_dir (concatenate 'string (namestring vigra_c-bin-path) "win" (write-to-string cl-bits) "/"))
				   (binaries (uiop:directory-files bin_dir "*.dll"))
				   (target_dir (concatenate 'string (namestring vigracl-path) "/")))
				(progn
					(mapcar #'(lambda (f) (uiop:copy-file f (concatenate 'string target_dir (pathname-name f) ".dll"))) binaries)
					T))
)

;;Enable Auto-Build of the vigra-c-lib if cannot be loaded!
(handler-case
	(load-foreign-library vigracl-dylib-path)
	(error () (progn
				(build-vigra_c)
				(load-foreign-library vigracl-dylib-path))))

