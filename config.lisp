(in-package #:vigracl)

;; Module constants
(defvar vigracl-path  (asdf:system-source-directory 'vigracl))
(defvar vigracl-version "1.0.0")

;;According to the cl-cookbook
(defun split-string-at-dot (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\. string :start i)
          collect (subseq string i j)
          while j))

;; For windows, we need to find out, which architecture CL is built
(defvar cl-bits (* 8 (foreign-type-size :pointer)))
(defvar cmake_flags (if (= cl-bits 32)
                        "-DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS=-m32 -DCMAKE_C_FLAGS=-m32"
                        "-DCMAKE_BUILD_TYPE=Release"))

;; Define the filename of the foreign library depending on the OS
(defvar vigracl-dylib-file
	#+darwin  "libvigra_c.dylib"
	#+unix	  "libvigra_c.so"
	#+windows "vigra_c.dll")
	
(defvar vigracl-dylib-path (merge-pathnames vigracl-path vigracl-dylib-file))


;; Stuff needed to compile the c-bindings if necessary...
(defvar base_login_script "~/.profile")

(defvar vigra_c-path 	 (merge-pathnames "vigra_c/" vigracl-path))
(defvar vigra_c-bin-path (merge-pathnames "bin/" vigra_c-path))

(defvar login_script (if (probe-file base_login_script)
                        	  (truename base_login_script)
                         	  (merge-pathnames vigra_c-path "fallback.profile")))

(defun easy-system-call (command)
 	(let* ((result (multiple-value-list (trivial-shell:shell-command command))))
 		(if (eq (caddr result) 0)
 			(car result)  ;;Everything is okay - return the result (string), else return NIL(FALSE):
 			NIL)))
 			
(defvar login_cmd (concatenate 'string "source " (namestring login_script)))
(defun system-env (arg) (easy-system-call (concatenate 'string login_cmd " && " arg)))

(defun vigra-version ()
  (let* ((version_string (system-env "vigra-config --version")))
    (if (> (length version_string) 0)
        (progn (stringp version_string)
               (mapcar #'parse-integer (split-string-at-dot version_string)))
        '())))

(defun vigra-installed? () 
  (print "Searching for vigra >= 1.11.0 using 'vigra-config --version':")
  (let ((version (vigra-version)))
    (if (null version)
        F
        (or (and (= (first version) 1) (>= (second version) 11))
                (> (first version) 1)))))
  
; The compilation routine (at least for macosx and unix)
(defun build-vigra_c ()
  #+unix (if (vigra-installed?)
         	 ;;VIGRA is found! Try to compile vigra_c bindings
          	 (if (stringp (system-env (concatenate 'string  "cd " (namestring vigra_c-path) " && mkdir -p build && cd build && cmake " cmake_flags " .. && make && cd .. && rm -rf ./build")))
                 (stringp (system-env (concatenate 'string  "cp " (namestring (merge-pathnames vigra_c-bin-path vigracl-dylib-file)) " "
                								  		  (namestring vigracl-dylib-path))))
                 (error "making the vigra_c lib failed, although vigra seems to be installed"))
         	(error "Vigra is not found. Please check if the prefix path is set correctly in HOME/.profile environment file!"))
         	
  #+windows (progn
               (easy-system-call (concatenate 'string "copy " (namestring vigra_c-bin-path) "\\win" (write-to-string cl-bits) "\\*.dll "
               												  (namestring vigracl-path)))
               T)
)

;;Enable Auto-Build of the vigra-c-lib if not already present!
(unless (probe-file vigracl-dylib-path)
   (build-vigra_c))
   
(load-foreign-library vigracl-dylib-path)


