(in-package :freebsd-sysctl)

(defctype size-t :ulong)

(defconstant +max-mib-len+ 20
  "Maximal number of elements in mib array")

(defconstant +max-foreign-len+ (* +max-mib-len+ 4)
  "Maximal foreign data chunk length")

(defcfun ("strerror" strerror) :string
  (errnum :int))

(defun get-errno ()
  (mem-aref (foreign-symbol-pointer "errno") :int))

(define-condition sysctl-error (error)
  ((errno :initform 0
          :initarg :errno
          :reader sysctl-error-errno)
   (message :initarg :message
            :reader sysctl-error-message))
  (:report (lambda (c s)
             (if (/= (sysctl-error-errno c) 0)
                 (format s "sysctl system call error: ~a" (strerror (sysctl-error-errno c)))
                 (write-line (sysctl-error-message c) s)))))

(defcfun ("sysctl" sysctl%) :int
  (name :pointer :int)
  (namelen :uint)
  (oldp :pointer)
  (oldlenp :pointer size-t)
  (newp :pointer)
  (newlen size-t))

#+nil
(defcfun ("sysctlbyname" sysctl-by-name%) :int
  (name :string)
  (oldp :pointer)
  (oldlenp :pointer size-t)
  (newp :pointer)
  (newlen size-t))

(defcfun ("sysctlnametomib" sysctl-name=>mib%) :int
  (name :string)
  (mibp :pointer :int)
  (sizep :pointer size-t))

(defun sysctl-name=>mib (name)
  (declare (type string name))
  "Get sysctl mib array corresponding to sysctl name."
  (if (>= (1+ (length name)) +max-foreign-len+)
      (error 'sysctl-error :message "sysctl name is too long"))
  (with-foreign-objects ((str :char +max-foreign-len+)
                         (size 'size-t)
                         (mib :int +max-mib-len+))
    (lisp-string-to-foreign name str (1+ (length name)))
    (setf (mem-aref size 'size-t) +max-mib-len+)
    (let ((result (sysctl-name=>mib% str mib size)))
      (if (/= result 0) (error 'sysctl-error :errno (get-errno))))
    (let ((new-size (mem-aref size 'size-t)))
      (if (= +max-mib-len+ new-size)
          (error "mib array is too long"))
      (make-array new-size :initial-contents
                  (loop for i below new-size collect (mem-aref mib :int i))))))

(defun sysctl-type (mib)
  (declare (type simple-vector mib))
  (if (>= (+ 2 (length mib)) +max-mib-len+)
      (error 'sysctl-error :message "mib array is too long"))
  (with-foreign-objects ((foreign-mib :int +max-mib-len+)
                         (type :char +max-foreign-len+)
                         (len 'size-t))
    (loop for i below (length mib) do
         (setf (mem-aref foreign-mib :int (+ 2 i))
               (aref mib i)))
    (setf (mem-aref foreign-mib :int 0) 0
          (mem-aref foreign-mib :int 1) 4
          (mem-aref len 'size-t) +max-foreign-len+)
    (let ((result (sysctl% foreign-mib
                           (+ 2 (length mib))
                           type len
                           (null-pointer) 0)))
      (if (/= result 0) (error 'sysctl-error :errno (get-errno))))
    (foreign-string-to-lisp type :offset 4)))

(defun sysctl-mib=>name (mib)
  "Get sysctl name corresponding to mib array"
  (declare (type simple-vector mib))
  (if (>= (+ 2 (length mib)) +max-mib-len+)
      (error 'sysctl-error :message "mib array is too long"))
  (with-foreign-object (foreign-mib :int +max-mib-len+)
    (loop
       for i below (length mib) do
         (setf (mem-aref foreign-mib :int (+ 2 i))
               (aref mib i)))
    (setf (mem-aref foreign-mib :int 0) 0
          (mem-aref foreign-mib :int 1) 1)
    (with-foreign-object (name :char +max-foreign-len+)
      (with-foreign-object (len 'size-t)
        (setf (mem-aref len 'size-t) +max-foreign-len+)
        (let ((result (sysctl% foreign-mib
                               (+ 2 (length mib))
                               name len
                               (null-pointer) 0)))
          (if (/= result 0) (error 'sysctl-error :errno (get-errno))))
        (foreign-string-to-lisp name :max-chars (mem-aref len 'size-t))))))

(defun parse-temperature (temp precision)
  (- (/ temp (expt 10.0 precision)) 273.15))

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

(defun sysctl (mib &optional new-value)
  "Perform sysctl call for a value specified by mib array. If new-value is
 specified, it will be set as a new value for that sysctl. Two values are
 returned: the old and the new value."
  (declare (type simple-vector mib))
  (if (>= (length mib) +max-mib-len+)
      (error 'sysctl-error :message "mib array is too long"))
  (let ((type (sysctl-type mib)))
    (with-foreign-objects ((foreign-mib :int +max-mib-len+)
                           (old-data :uint8 +max-foreign-len+)
                           (new-data :uint8 +max-foreign-len+)
                           (old-len 'size-t))
      (loop for i below (length mib) do
           (setf (mem-aref foreign-mib :int i)
                 (aref mib i)))
      (setf (mem-aref old-len 'size-t) +max-foreign-len+)
      (let ((new-len (if new-value (output-data new-data type new-value) 0))
            (new-data (if new-value new-data (null-pointer))))
        (let ((result (sysctl% foreign-mib
                               (length mib)
                               old-data old-len
                               new-data new-len)))
          (if (/= result 0) (error 'sysctl-error :errno (get-errno)))))
      (values
       (interpret-result old-data (mem-aref old-len 'size-t) type)
       new-value))))

(defun sysctl-by-name (name &optional new-value)
  "Same as SYSCTL, only it accepts string name for sysctl rather than mib array."
  (declare (type string name))
  (sysctl (sysctl-name=>mib name) new-value))

(defun list-sysctls (name)
  "Returns a list of sysctls for the node with name NAME."
  (declare (type string name))
  (let* ((mib (sysctl-name=>mib name))
         (original-length (length mib)))
    (if (string/= (sysctl-type mib) "N")
        (error 'sysctl-error :message "Please specify a node"))
    (labels ((do-list-sysctls (mib list)
               (if (>= (+ 2 (length mib)) +max-mib-len+)
                   (error 'sysctl-error :message "mib array is too long"))
               (with-foreign-objects ((foreign-mib :int +max-mib-len+)
                                      (new-mib :int +max-mib-len+)
                                      (len 'size-t))
                 (loop for i below (length mib) do
                      (setf (mem-aref foreign-mib :int (+ 2 i))
                            (aref mib i)))
                 (setf (mem-aref foreign-mib :int 0) 0
                       (mem-aref foreign-mib :int 1) 2
                       (mem-aref len 'size-t) +max-foreign-len+)
                 (let ((result (sysctl% foreign-mib
                                        (+ 2 (length mib))
                                        new-mib len
                                        (null-pointer) 0)))
                   (if (= result 0) list))
                   (let ((new-mib
                          (let ((new-len (/ (mem-aref len 'size-t) 4)))
                            (if (= +max-mib-len+ new-len)
                                (error 'sysctl-error :message "mib is too long"))
                            (make-array new-len :initial-contents
                                        (loop for i below new-len collect
                                             (mem-aref new-mib :int i))))))
                     (if (equalp (subseq new-mib 0 original-length)
                                 (subseq mib 0 original-length))
                       (do-list-sysctls new-mib (cons (sysctl-mib=>name new-mib) list))
                       list)))))
      (do-list-sysctls mib nil))))
