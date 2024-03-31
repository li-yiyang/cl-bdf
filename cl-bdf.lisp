(defpackage #:cl-bdf
  (:use :cl :str)
  (:nicknames :bdf)
  (:export
   #:load-bdf-font                      ; load from file
   #:bdf                                ; bdf for specific font
   #:bdf-char                           ; bdf-char for each character
   
   #:font-size                          ; font size
   #:bdf-version                        ; bdf version
   #:content-version                    ; content version
   #:font-name                          ; name of font
   #:get-char                           ; get bdf-char from specific font
   )
  (:documentation
   "Use CL-BDF to parse BDF fonts.
Use `load-bdf-font' to load BDF font, return `bdf' object;
Use `get-char' to get BDF character from `bdf' font, return
a `bdf-font' object. "))

(in-package :cl-bdf)

;; ========== helper functions and macros ==========
;; + `setf-slots' is used for quick slot-value bindings

(defmacro setf-slots (obj &body bindings)
  "Quickly bind the slot value.
Bindings should be like:
  (setf-slots obj slot-name1 expr1
                  slot-name2 expr2)"
  `(progn
     ,@(loop for (slot-name expr) on bindings by #'cddr
             collect `(setf (slot-value ,obj ',slot-name) ,expr))))

;; ========== bdf ==========
;; The `bdf' is the BDF font interface.
;; Using `load-bdf-font' will read a BDF font from path,
;; and return a `bdf' object.

(defclass bdf ()
  (;; Headers
   (start-font      :reader bdf-version)
   (content-version :initform 0 :reader content-version)
   (font-name       :reader font-name)
   point-size
   x-resolution
   y-resolution
   bounding-x
   bounding-y
   x-offset
   y-offset
   (metrics-set :initform 0)

   ;; Properties
   (properties      :reader property-alist)

   ;; Chars
   (chars :initform (make-hash-table :test 'equal))))

;; ========== bdf font-size ==========

(defgeneric font-size (font)
  (:documentation
   "Get the bounding box size of `font'.
Return values of `bounding-x' and `bounding-y'. "))

(defmethod font-size ((font bdf))
  (with-slots (bounding-x bounding-y) font
    (values bounding-x bounding-y)))

;; ========== bdf get-char ==========

(defgeneric get-char (font des &optional fallback)
  (:documentation
   "Get the char by description `des' from `font'.
Return `bdf-char' object. "))

(defmethod get-char ((font bdf) des
                     &optional (fallback nil fallback-set?))
  (let ((char (gethash des (slot-value font 'chars))))
    (or char
        (when fallback-set?
          (gethash fallback (slot-value font 'chars))))))

;; ========== bdf-char ==========
;; The `bdf-char' is the interface for each
;; character in specific BDF font.
;; 
;; Using `get-char' on `bdf' font will return
;; a `bdf-char' object.

(defclass bdf-char ()
  (char-name
   (standard-endcoded? :initform t)
   encoding
   (swx :initform 0)
   (swy :initform 0)
   (dwx :initform 0)
   (dwy :initform 0)
   (bbw :initform 0)
   (bbh :initform 0)
   (bbx :initform 0)
   (bbx :initform 0)
   (bitmap :initarg :bitmap)))

;; ========== bdf parser ==========
;; The process of parsing `bdf' object from stream goes
;; like below:
;; 
;; 1. parse bdf header (see `parse-font-from-stream')
;; 2. parse chars (see `parse-chars!' and `parse-char')
;; 
;; Naming convention:
;; + method with `!' postfixed will bring in side effect

(defun read-str* (str)
  "Read the `str' repented value. "
  (with-input-from-string (stream str)
    (read stream)))

(defun read-line* (stream)
  "Read line from `stream'.
Return splited words. "
  (let ((line (read-line stream nil nil)))
    (when line (words line))))

;; ========== parse-properties ==========

(defun parse-properties! (font stream n)
  "Read `n' property from `stream' into `font'. "
  (loop for i below n
        for line = (read-line* stream)
        collect line into prop
        finally
           (let ((end (read-line* stream)))
             (if (string= (first end) "ENDPROPERTIES")
                 (setf-slots font
                   properties prop)
                 (error "ERROR WITH PROPERTIES SIZE.")))))

;; ========== parse-char ==========

(defun hex-str->bit-list (str)
  "Trun hex-str `str' into bit list. "
  (flet ((<- (char)
           (let* ((code (char-code char))
                  (val  (cond ((and (<= 48 code) (<= code 57))  (- code 48))
                              ((and (<= 65 code) (<= code 70))  (- code 55))
                              ((and (<= 97 code) (<= code 122)) (- code 87))
                              (t (error "Error with input. "))))
                  (bits ()))
             (dotimes (i 4 bits) (push (mod (ash val (- i 4)) 2) bits)))))
    (apply #'nconc (map 'list #'<- str))))

(defun parse-char (stream)
  "Parse char from `stream'.
Return a `bdf-char' object. "
  (loop with char = (make-instance 'bdf-char)
        for line = (read-line* stream)
        while line
        do (string-case (first line)
             ("STARTCHAR"               ; STARTCHAR string
              (setf-slots char char-name (second line)))
             ("ENCODING"                ; ENCODING integer (integer)
              (let ((enc (read-str* (second line))))
                (if (= enc -1)
                    (setf-slots char
                      standard-endcoded? nil
                      encoding           (read-str* (third line)))
                    (setf-slots char encoding enc))))
             ("SWIDTH"                  ; SWIDTH swx0 swy0
              (setf-slots char
                swx (read-str* (second line))
                swy (read-str* (third  line))))
             ("DWIDTH"                  ; DWIDTH dwx0 dwy0
              (setf-slots char
                dwx (read-str* (second line))
                dwy (read-str* (third  line))))
             ("BBX"                     ; BBX BBw BBh BBxoff0x BByoff0y
              (setf-slots char
                bbw (read-str* (second line))
                bbh (read-str* (third  line))
                bbx (read-str* (fourth line))
                bby (read-str* (fifth  line))))
             ("BITMAP"
              (setf-slots char
                bitmap (loop for row = (read-line stream)
                             while (not (string= row "ENDCHAR"))
                             collect (hex-str->bit-list row)))
              (return char)))))

(defun parse-chars! (font stream n)
  "Parse `n' char from `stream' into `font'. "
  (with-slots (chars) font
    (loop for i below n
          for char = (parse-char stream)
          do (setf (gethash (slot-value char 'char-name) chars) char))))

(defun parse-font-from-stream (stream)
  "Parse font from `stream'.
Return a bdf object. "
  (loop with font = (make-instance 'bdf)
        for line = (read-line* stream)
        while line
        do (string-case (first line)
             ("STARTFONT"               ; STARTFONT number
              (setf-slots font
                start-font (read-str* (second line))))
             ("CONTENTVERSION"          ; CONTENTVERSION integer
              (setf-slots font
                content-version (read-str* (second line))))
             ("FONT"                    ; FONT string
              (setf-slots font
                font-name (second line)))
             ("SIZE"                    ; SIZE PointSize Xres Yres
              (setf-slots font
                point-size   (read-str* (second line))
                x-resolution (read-str* (third  line))
                y-resolution (read-str* (fourth line))))
             ("FONTBOUNDINGBOX"  ; FONTBOUNDINGBOX FBBx FBBy Xoff Yoff
              (setf-slots font
                bounding-x (read-str* (second line))
                bounding-y (read-str* (third  line))
                x-offset   (read-str* (fourth line))
                y-offset   (read-str* (fifth  line))))
             ("METRICSSET"              ; METRICSSET integer
              (setf-slots font
                metrics-set (read-str* (second line))))
             ("STARTPROPERTIES"         ; STARTPROPERTIES n
              (parse-properties! font stream (read-str* (second line))))
             ("CHARS"                   ; CHARS n
              (parse-chars! font stream (read-str* (second line))))
             ("ENDFONT"                 ; ENDFONT
              (return font)))))

;; ========== load-bdf-font ==========

(defun load-bdf-font (path)
  "Load BDF font from `path'.
Return `bdf' object as BDF font object. "
  (with-open-file (stream path)
    (parse-font-from-stream stream)))

;; ========== test ==========
;; You could try the following code for test:
;; 
;; (ql:quickload :cl-bdf)
;; (bdf:load-font-from-stream
;;   (asdf:system-relative-pathname :cl-bdf "cptfont.bdf"))
;; 
;; The test font `cptfont.bdf' is from OpenGenera 2.
