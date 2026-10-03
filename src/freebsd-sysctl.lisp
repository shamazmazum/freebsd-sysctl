(in-package :freebsd-sysctl)

(defconstant +max-mib-len+ 20
  "Maximal number of elements in mib array")

(defconstant +max-foreign-len+ (* +max-mib-len+ 4)
  "Maximal foreign data chunk length")

(defcfun ("strerror" strerror) :string
  (errnum :int))

(deftype pointer () t)
(deftype foreign-data () '(or integer single-float string))

(serapeum:-> get-errno () (values (signed-byte 32) &optional))
(defun get-errno ()
  #+sbcl (sb-alien:get-errno)
  #-sbcl 0)

(define-condition sysctl-error (error)
  ((errno :initform 0
          :initarg :errno
          :reader sysctl-error-errno)
   (message :initarg :message
            :reader sysctl-error-message))
  (:report (lambda (c s)
             (if (zerop (sysctl-error-errno c))
                 (write-line (sysctl-error-message c) s)
                 (format s "sysctl system call error: ~a"
                         (strerror (sysctl-error-errno c)))))))


(defcfun ("sysctl" %sysctl) :int
  (name    (:pointer :int))
  (namelen :uint)
  (oldp    :pointer)
  (oldlenp (:pointer :size))
  (newp    :pointer)
  (newlen  :size))

(defcfun ("sysctlnametomib" %sysctl-name=>mib) :int
  (name  :string)
  (mibp  (:pointer :int))
  (sizep (:pointer :size)))

(serapeum:-> fill-foreign-array (pointer (simple-array (signed-byte 32) (*))
                                 &key (:offset (unsigned-byte 32)))
             (values &optional))
(defun fill-foreign-array (foreign array &key (offset 0))
  (when (> (+ offset (length array)) +max-mib-len+)
    (error 'sysctl-error :message "mib array is too long"))
  (loop for i below (length array) do
    (setf (mem-aref foreign :int (+ offset i))
          (aref array i)))
  (values))

(serapeum:-> sysctl-name=>mib (string)
             (values (simple-array (signed-byte 32) (*)) &optional))
(defun sysctl-name=>mib (name)
  "Get sysctl mib array corresponding to sysctl name."
  (when (> (1+ (length name)) +max-foreign-len+)
    (error 'sysctl-error :message "sysctl name is too long"))
  (with-foreign-objects ((size :size)
                         (mib  :int +max-mib-len+))
    (setf (mem-aref size :size) +max-mib-len+)
    (let ((result (%sysctl-name=>mib name mib size)))
      (unless (zerop result)
        (error 'sysctl-error :errno (get-errno))))
    (let ((new-size (mem-aref size :size)))
      (when (= +max-mib-len+ new-size)
        (error "mib array is too long"))
      (let ((result (make-array new-size :element-type '(signed-byte 32))))
        (loop for i below new-size do
          (setf (aref result i)
                (mem-aref mib :int i)))
        result))))

(serapeum:-> sysctl-type ((simple-array (signed-byte 32) (*)))
             (values string &optional))
(defun sysctl-type (mib)
  (with-foreign-objects ((foreign-mib :int  +max-mib-len+)
                         (type        :char +max-foreign-len+)
                         (len         :size))
    (fill-foreign-array foreign-mib mib :offset 2)
    (setf (mem-aref foreign-mib :int 0) +ctl-sysctl+
          (mem-aref foreign-mib :int 1) +ctl-sysctl-oidfmt+
          (mem-aref len :size) +max-foreign-len+)
    (let ((result (%sysctl foreign-mib
                           (+ 2 (length mib))
                           type len
                           (null-pointer) 0)))
      (unless (zerop result)
        (error 'sysctl-error :errno (get-errno))))
    (foreign-string-to-lisp type :offset 4)))

(serapeum:-> sysctl-mib=>name ((simple-array (signed-byte 32) (*)))
             (values string &optional))
(defun sysctl-mib=>name (mib)
  "Get sysctl name corresponding to mib array"
  (with-foreign-objects ((foreign-mib :int  +max-mib-len+)
                         (name        :char +max-foreign-len+)
                         (len         :size))
    (fill-foreign-array foreign-mib mib :offset 2)
    (setf (mem-aref foreign-mib :int 0) +ctl-sysctl+
          (mem-aref foreign-mib :int 1) +ctl-sysctl-name+
          (mem-aref len :size) +max-foreign-len+)
    (let ((result (%sysctl foreign-mib
                           (+ 2 (length mib))
                           name len
                           (null-pointer) 0)))
      (unless (zerop result)
        (error 'sysctl-error :errno (get-errno))))
    (foreign-string-to-lisp name :max-chars (mem-aref len :size))))

(serapeum:-> parse-temperature ((signed-byte 32) integer)
             (values single-float &optional))
(defun parse-temperature (temp precision)
  (- (/ temp (expt 10.0 precision)) 273.15))

(serapeum:-> interpret-result (pointer (unsigned-byte 32) string)
             (values foreign-data &optional))
(defun interpret-result (data length type)
  (cond
    ((string= type "A")
     (foreign-string-to-lisp data :max-chars length))
    ((string= type "I")
     (if (/= length 4) (error 'sysctl-error :message "Wrong data length"))
     (mem-ref data :int))
    ((string= type "IU")
     (if (/= length 4) (error 'sysctl-error :message "Wrong data length"))
     (mem-ref data :uint))
    ((string= type "L")
     (if (/= length 8) (error 'sysctl-error :message "Wrong data length"))
     (mem-ref data :long))
    ((string= type "LU")
     (if (/= length 8) (error 'sysctl-error :message "Wrong data length"))
     (mem-ref data :ulong))
    ((string= (subseq type 0 2) "IK")
     (if (/= length 4) (error 'sysctl-error :message "Wrong data length"))
     (parse-temperature (mem-ref data :int)
                        (if (> (length type) 2)
                            (parse-integer type :start 2)
                            1)))
    (t (error 'sysctl-error :message "Unknown data format"))))

(serapeum:-> output-data (pointer string foreign-data)
             (values (unsigned-byte 32) &optional))
(defun output-data (foreign-data type data)
  (cond
    ((string= type "A")
     (lisp-string-to-foreign (the string data)
                             foreign-data +max-foreign-len+)
     (1+ (length data)))
    ((string= type "I")
     (setf (mem-aref foreign-data :int) (the integer data))
     4)
    ((string= type "IU")
     (setf (mem-aref foreign-data :uint) (the (integer 0) data))
     4)
    ((string= type "L")
     (setf (mem-aref foreign-data :long) (the integer data))
     8)
    ((string= type "LU")
     (setf (mem-aref foreign-data :ulong) (the (integer 0) data))
     8)
    (t (error 'sysctl-error :message "Unknown data format"))))

(serapeum:-> sysctl ((simple-array (signed-byte 32) (*)) &optional (or foreign-data null))
             (values foreign-data (or foreign-data null) &optional))
(defun sysctl (mib &optional new-value)
  "Perform sysctl call for a value specified by mib array. If new-value is
 specified, it will be set as a new value for that sysctl. Two values are
 returned: the old and the new value."
  (let ((type (sysctl-type mib)))
    (with-foreign-objects ((foreign-mib :int +max-mib-len+)
                           (old-data    :uint8 +max-foreign-len+)
                           (new-data    :uint8 +max-foreign-len+)
                           (old-len     :size))
      (fill-foreign-array foreign-mib mib)
      (setf (mem-aref old-len :size) +max-foreign-len+)
      (let ((new-len  (if new-value (output-data new-data type new-value) 0))
            (new-data (if new-value new-data (null-pointer))))
        (let ((result (%sysctl foreign-mib
                               (length mib)
                               old-data old-len
                               new-data new-len)))
          (unless (zerop result)
            (error 'sysctl-error :errno (get-errno)))))
      (values
       (interpret-result old-data (mem-aref old-len :size) type)
       new-value))))

(serapeum:-> sysctl-by-name (string &optional (or foreign-data null))
             (values foreign-data (or foreign-data null) &optional))
(defun sysctl-by-name (name &optional new-value)
  "Same as SYSCTL, only it accepts string name for sysctl rather than mib array."
  (sysctl (sysctl-name=>mib name) new-value))

(serapeum:-> list-sysctls (string)
             (values list &optional))
(defun list-sysctls (name)
  "Returns a list of sysctls for the node with name NAME."
  (let* ((mib (sysctl-name=>mib name))
         (original-length (length mib)))
    (if (string/= (sysctl-type mib) "N")
        (error 'sysctl-error :message "Please specify a node"))
    (labels ((%go (mib list)
               (let ((new-mib
                       (with-foreign-objects ((foreign-mib :int +max-mib-len+)
                                              (new-mib-ptr :int +max-mib-len+)
                                              (len         :size))
                         (fill-foreign-array foreign-mib mib :offset 2)
                         (setf (mem-aref foreign-mib :int 0) +ctl-sysctl+
                               (mem-aref foreign-mib :int 1) +ctl-sysctl-next+
                               (mem-aref len :size) +max-foreign-len+)
                         (let ((result (%sysctl foreign-mib
                                                (+ 2 (length mib))
                                                new-mib-ptr len
                                                (null-pointer) 0)))
                           (unless (or (zerop result)
                                       (= (get-errno) +enoent+))
                             (error 'sysctl-error :errno (get-errno))))
                         (let ((new-len (/ (mem-aref len :size)
                                           (foreign-type-size :int))))
                           (when (= +max-mib-len+ new-len)
                             (error 'sysctl-error :message "mib is too long"))
                           (let ((mib (make-array new-len :element-type '(signed-byte 32))))
                             (loop for i below new-len do
                               (setf (aref mib i) (mem-aref new-mib-ptr :int i)))
                             mib)))))
                 (if (equalp (subseq new-mib 0 original-length)
                             (subseq     mib 0 original-length))
                     (%go new-mib (cons (sysctl-mib=>name new-mib) list))
                     list))))
      (%go mib nil))))
