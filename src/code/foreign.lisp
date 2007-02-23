;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/foreign.lisp,v 1.54 2006/04/26 20:49:23 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "SYSTEM")

(in-package "ALIEN")
(export '(load-foreign))
(in-package "SYSTEM")
(import 'alien:load-foreign)

#+sparc (defconstant foreign-segment-start #xe0000000)
#+sparc (defconstant foreign-segment-size  #x00100000)

#+hppa (defconstant foreign-segment-start #x10C00000)
#+hppa (defconstant foreign-segment-size  #x00400000)

#+(and bsd x86)
(defconstant foreign-segment-start #x0E000000)
#+(and bsd x86) 
(defconstant foreign-segment-size  #x02000000)

(defvar *previous-linked-object-file* nil)
#-(or linux bsd svr4 irix)
(defvar *foreign-segment-free-pointer* foreign-segment-start)

(defun pick-temporary-file-name (&optional (base "/tmp/tmp~D~C"))
  (let ((code (char-code #\A)))
    (loop
      (let ((name (format nil base (unix:unix-getpid) (code-char code))))
	(multiple-value-bind
	    (fd errno)
	    (unix:unix-open name
			    (logior unix:o_wronly unix:o_creat unix:o_excl)
			    #o666)
	  (cond ((not (null fd))
		 (unix:unix-close fd)
		 (return name))
		((not (= errno unix:eexist))
		 (error "Could not create temporary file ~S: ~A"
			name (unix:get-unix-error-msg errno)))
		
		((= code (char-code #\Z))
		 (setf code (char-code #\a)))
		((= code (char-code #\z))
		 (return nil))
		(t
		 (incf code))))))))

#+(or (and FreeBSD (not elf)) (and sparc (not svr4)))
(alien:def-alien-type exec
  (alien:struct nil
    (magic c-call:unsigned-long)
    (text c-call:unsigned-long)
    (data c-call:unsigned-long)
    (bss c-call:unsigned-long)
    (syms c-call:unsigned-long)
    (entry c-call:unsigned-long)
    (trsize c-call:unsigned-long)
    (drsize c-call:unsigned-long)))

#-(or linux bsd svr4)
(defun allocate-space-in-foreign-segment (bytes)
  (let* ((pagesize-1 (1- (get-page-size)))
	 (memory-needed (logandc2 (+ bytes pagesize-1) pagesize-1))
	 (addr (int-sap *foreign-segment-free-pointer*))
	 (new-ptr (+ *foreign-segment-free-pointer* memory-needed)))
    (when (> new-ptr (+ foreign-segment-start foreign-segment-size))
      (error "Not enough memory left."))
    (setf *foreign-segment-free-pointer* new-ptr)
    (allocate-system-memory-at addr memory-needed)
    addr))


;;; ELF object file loading. Note that the conditionalization is
;;; assuming that all of Linux/x86, Linux/Alpha, FreeBSD/x86,
;;; OpenBSD/x86, and Solaris ports support ELF. 
;;;
;;; The following definitions are taken from
;;; /usr/include/sys/elf_common.h and /usr/include/sys/elf32.h.
;;;
#+(or linux (and bsd (not darwin)) svr4)
(progn
(alien:def-alien-type elf-address      (alien:unsigned 32))
(alien:def-alien-type elf-half-word    (alien:unsigned 16))
(alien:def-alien-type elf-offset       (alien:unsigned 32))
(alien:def-alien-type elf-signed-word  (alien:integer  32))
(alien:def-alien-type elf-word         (alien:unsigned 32))
(alien:def-alien-type elf-size         (alien:unsigned 32))

(alien:def-alien-type eheader
    ;;"Elf file header."
  (alien:struct nil
    (elf-ident                      (alien:array (alien:unsigned 8) 16))
    (elf-type                        elf-half-word)
    (elf-machine                     elf-half-word)
    (elf-version                     elf-word)
    (elf-entry                       elf-address)
    (elf-program-header-offset       elf-offset)
    (elf-section-header-offset       elf-offset)
    (elf-flags                       elf-word)
    (elf-header-size                 elf-half-word)
    (elf-program-header-entry-size   elf-half-word)
    (elf-program-header-count        elf-half-word)
    (elf-section-header-entry-size   elf-half-word)
    (elf-section-header-count        elf-half-word)
    (elf-section-name-strings        elf-half-word)))

;; Indices into the elf-ident array, as per SVR4 ABI
(defconstant ei-mag0          0) ; Magic number, byte 0
(defconstant ei-mag1          1) ; Magic number, byte 1
(defconstant ei-mag2          2) ; Magic number, byte 2
(defconstant ei-mag3          3) ; Magic number, byte 3
(defconstant ei-class         4) ; class of machine
(defconstant ei-data          5) ; data format
(defconstant ei-version       6) ; ELF format version
(defconstant ei-osabi         7) ; Operating system / ABI identification
(defconstant ei-abiversion    8) ; ABI version
(defconstant ei-pad           9) ; Start of padding
(defconstant ei-nident       16) ; Size of elf-ident array

;; values for elf-type
(defconstant et-relocatable   1)
(defconstant et-executable    2)
(defconstant et-shared-object 3)
(defconstant et-core-file     4)

;; values for elf-ident[ei-osabi]
(defconstant elfosabi-sysv         0)
(defconstant elfosabi-hpux         1)
(defconstant elfosabi-netbsd       2)
(defconstant elfosabi-linux        3)
(defconstant elfosabi-hurd         4)
(defconstant elfosabi-86open       5)
(defconstant elfosabi-solaris      6)
(defconstant elfosabi-monterey     7)
(defconstant elfosabi-irix         8)
(defconstant elfosabi-freebsd      9)
(defconstant elfosabi-tru64       10)
(defconstant elfosabi-modesto     11)
(defconstant elfosabi-openbsd     12)
(defconstant elfosabi-arm         97)
(defconstant elfosabi-standalone 255)

(alien:def-alien-type pheader
;;"Program header."
  (alien:struct nil
    (p-type             elf-word)      ; Entry type.
    (p-offset           elf-offset)    ; File offset of contents.
    (p-virtual-address  elf-address)   ; Virtual address in mem. image.
    (p-physical-address elf-address)   ; Physical address (not used).
    (p-file-size        elf-size)      ; Size of contents in file.
    (p-memory-size      elf-size)      ; Size of contents in memory.
    (p-flags            elf-word)      ; Access permission flags.
    (p-alignment        elf-size)))    ; Alignment in memory and file.

(defconstant +elf-magic+
  (make-array 4 :element-type '(unsigned-byte 8)
	        :initial-contents '(127 69 76 70))) ; 0x7f-E-L-F
(defun elf-p (h)
  "Make sure the header starts with the ELF magic value."
  (dotimes (i 4 t)
    (unless (= (alien:deref h i) (aref +elf-magic+ i))
      (return nil))))

(defun elf-osabi (h)
  "Return the `osabi' field in the padding of the ELF file."
  (alien:deref h ei-osabi))

(defun elf-osabi-name (id)
  (cond
    ((eql id elfosabi-sysv) "Unix System V ABI")
    ((eql id elfosabi-hpux) "HP-UX")
    ((eql id elfosabi-netbsd) "NetBSD")
    ((eql id elfosabi-linux) "Linux")
    ((eql id elfosabi-hurd) "GNU/Hurd")
    ((eql id elfosabi-86open) "86Open common IA32 ABI")
    ((eql id elfosabi-solaris) "Solaris")
    ((eql id elfosabi-monterey) "Monterey")
    ((eql id elfosabi-irix) "IRIX")
    ((eql id elfosabi-freebsd) "FreeBSD")
    ((eql id elfosabi-tru64) "Tru64 Unix")
    ((eql id elfosabi-modesto) "Novell Modesto")
    ((eql id elfosabi-openbsd) "OpenBSD")
    ((eql id elfosabi-arm) "ARM")
    ((eql id elfosabi-standalone) "Standalone/Embedded")
    (t (format nil "Unknown ABI (~D)" id))))

(defun elf-executable-p (n)
  "Given a file type number, determine whether the file is executable."
  (= n et-executable))

(defun file-shared-library-p (pathname)
  (with-open-file (obj pathname
                       :direction :input
                       :element-type '(unsigned-byte 8))
    (let ((fd (lisp::fd-stream-fd obj)))
      (alien:with-alien ((header eheader))
        (unix:unix-read fd (alien:alien-sap header) (alien:alien-size eheader :bytes))
        (when (elf-p (alien:slot header 'elf-ident))
          (eql et-shared-object (alien:slot header 'elf-type)))))))
) ;; #+(or linux (and bsd (not darwin)) svr4)



;; Darwin loading of foreign code.  This uses the dlopen shims and thus
;; appears like ELF to the rest of the code in this file.  However testing
;; for shared libs obviously needs to test for Mach-O dylibs, and not
;; ELF shared libraries...
#+darwin
(progn

(alien:def-alien-type machheader
  (alien:struct nil
    (magic       (alien:unsigned 32))
    (cputype	 (alien:signed 32))
    (cpusubtype  (alien:signed 32))
    (filetype    (alien:unsigned 32))
    (ncmds       (alien:unsigned 32))
    (sizeofcmds  (alien:unsigned 32))
    (flags       (alien:unsigned 32))))

;; values for magic
(defconstant mh-magic	#xfeedface)

;; values for filetype
(defconstant mh-object        #x1)
(defconstant mh-execute       #x2)
(defconstant mh-fvmlib        #x3)
(defconstant mh-core          #x4)
(defconstant mh-preload       #x5)
(defconstant mh-dylib         #x6)
(defconstant mh-dylinker      #x7)
(defconstant mh-bundle        #x8)
(defconstant mh-dylib-stub    #x9)

;;; Support for loading multi-arch ("fat") shared libraries.
(alien:def-alien-type fat-header
  (alien:struct nil
    (magic       (alien:unsigned 32))
    (nfat-arch   (alien:unsigned 32))))

(alien:def-alien-type fat-arch
  (alien:struct nil
    (cputype     (alien:signed 32))
    (cpusubtype  (alien:signed 32))
    (offset      (alien:unsigned 32))
    (size        (alien:unsigned 32))
    (align       (alien:unsigned 32))))

(defconstant fat-header-magic #xcafebabe)

(defun mach-o-p (h)
  "Make sure the header starts with the mach-o magic value."
  (eql (alien:slot h 'magic) mh-magic))

;;; Read an unsigned 32-bit big-endian number from STREAM.
(defun read-u32-be (stream)
  (let ((n 0))
    (setf (ldb (byte 8 24) n) (read-byte stream))
    (setf (ldb (byte 8 16) n) (read-byte stream))
    (setf (ldb (byte 8 8)  n) (read-byte stream))
    (setf (ldb (byte 8 0)  n) (read-byte stream))
    n))

;;; Read the 32-bit magic number from STREAM then rewind it.
(defun read-object-file-magic (stream)
  (let ((pos (file-position stream)))
    (prog1
        (read-u32-be stream)
      (file-position stream pos))))

;;; XXX For a Darwin/x86 port, these functions will need to swap the
;;; byte order of the structure members. Apple's documentation states
;;; that all the fields of FAT-HEADER and FAT-ARCH are big-endian.
(defun read-mach-header (stream sap)
  (unix:unix-read (lisp::fd-stream-fd stream) sap
                  (alien:alien-size machheader :bytes)))

(defun read-fat-header (stream sap)
  (unix:unix-read (lisp::fd-stream-fd stream) sap
                  (alien:alien-size fat-header :bytes)))

(defun read-fat-arch (stream sap)
  (unix:unix-read (lisp::fd-stream-fd stream) sap
                  (alien:alien-size fat-arch :bytes)))

;;; Return a list of offsets in STREAM which contain Mach-O headers.
;;; For single-architecture binaries, this will return (0), emulating
;;; the previous behavior of loading the header from the start of the
;;; file.  For fat binaries, there will be one offset in the result
;;; list for each architecture present in the file.
(defun read-mach-header-offsets (stream)
  (let ((magic (read-object-file-magic stream)))
    (cond ((eql magic mh-magic)
           (list 0))
          ((eql magic fat-header-magic)
           (alien:with-alien ((fat-header fat-header)
                              (fat-arch fat-arch))
             (read-fat-header stream (alien:alien-sap fat-header))
             (loop
                for i from 0 below (alien:slot fat-header 'nfat-arch)
                do (read-fat-arch stream (alien:alien-sap fat-arch))
                collect (alien:slot fat-arch 'offset))))
          (t nil))))

;;; Return true if the Mach-O HEADER represents a shared library.
(defun shared-mach-header-p (header)
  (and (eql (alien:slot header 'magic) mh-magic)
       (or (eql (alien:slot header 'filetype) mh-dylib)
           (eql (alien:slot header 'filetype) mh-bundle))))

(defun file-shared-library-p (pathname)
  (with-open-file (obj pathname
                       :direction :input
                       :element-type '(unsigned-byte 8))
    (let ((offsets (read-mach-header-offsets obj)))
      (when offsets
        (alien:with-alien ((header machheader))
          (loop
             for offset in offsets
             do (file-position obj offset)
                (read-mach-header obj (alien:alien-sap header))
             thereis (shared-mach-header-p header)))))))
) ; #+darwin



;; "old-style" loading of foreign code. This involves calling a
;; platform-specific script that is installed as
;; library:load-foreign.csh to convert the object files into a form
;; that is suitable for being stuffed into memory at runtime. 
#-(or linux bsd svr4)
(progn
(defun load-object-file (name)
  ;; NAME designates a tempory file created by ld via "load-foreign.csh".
  ;; Its contents are in a form suitable for stuffing into memory for
  ;; execution. This function extracts the location and size of the
  ;; relevant bits and reads them into memory.

  #|| library:load-foreign.csh
  #!/bin/csh -fx
  ld -N -R $argv[1] -Ttext $argv[2] -o $argv[3] $argv[5-]
  if ($status != 0) exit 1

  nm -gp $argv[3] > $argv[4]
  if ($status != 0) exit 2
  exit 0
  ||#

  (format t ";;; Loading object file...~%")
  (multiple-value-bind (fd errno) (unix:unix-open name unix:o_rdonly 0)
    (unless fd
      (error "Could not open ~S: ~A" name (unix:get-unix-error-msg errno)))
    (unwind-protect
	(alien:with-alien ((header eheader))
	  (unix:unix-read fd
			  (alien:alien-sap header)
			  (alien:alien-size eheader :bytes))
	  (unless (elf-p (alien:slot header 'elf-ident))
	    (error (format nil "~A is not an ELF file." name)))

	  (let ((osabi (elf-osabi (alien:slot header 'elf-ident)))
		(expected-osabi #+NetBSD elfosabi-netbsd
				#+FreeBSD elfosabi-freebsd))
	    (unless (= osabi expected-osabi)
	      (error "~A is not a ~A executable, it's a ~A executable."
		     name
		     (elf-osabi-name expected-osabi)
		     (elf-osabi-name osabi))))

	  (unless (elf-executable-p (alien:slot header 'elf-type))
	    (error (format nil "~A is not executable." name)))
	  
	  (alien:with-alien ((program-header pheader))
	    (unix:unix-read fd
			    (alien:alien-sap program-header)
			    (alien:alien-size pheader :bytes))
	    (let* ((addr (system::allocate-space-in-foreign-segment
			  (alien:slot program-header 'p-memory-size))))
	      (unix:unix-lseek
	       fd (alien:slot program-header 'p-offset) unix:l_set)
	      (unix:unix-read
	       fd addr (alien:slot program-header 'p-file-size)))))      
      (unix:unix-close fd))))

(defun parse-symbol-table (name)
  "Parse symbol table file created by load-foreign script.  Modified
to skip undefined symbols which don't have an address."
  (format t ";;; Parsing symbol table...~%")
  (let ((symbol-table (make-hash-table :test #'equal)))
    (with-open-file (file name)
      (loop
	(let ((line (read-line file nil nil)))
	  (unless line
	    (return))
	  (unless (eql (aref line 0) #\space)   ; Skip undefined symbols....
	    (let* ((symbol (subseq line 11))
		   (address (parse-integer line :end 8 :radix 16))
		   (kind (aref line 9))	; filter out .o file names
		   (old-address (gethash symbol lisp::*foreign-symbols*)))
	      (unless (or (null old-address) (= address old-address)
			  (char= kind #\F))
		(warn "~S moved from #x~8,'0X to #x~8,'0X.~%"
		      symbol old-address address))
	      (setf (gethash symbol symbol-table) address))))))
    (setf lisp::*foreign-symbols* symbol-table)))
) ;; #-(or linux bsd svr4)



;;; pw-- This seems to work for FreeBSD. The MAGIC field is not tested
;;; for correct file format so it may croak if ld fails to produce the
;;; expected results. It is probably good enough for now.
#+(or (and FreeBSD (not ELF)) (and sparc (not svr4)))
(defun load-object-file (name)
  (format t ";;; Loading object file...~%")
  (multiple-value-bind (fd errno) (unix:unix-open name unix:o_rdonly 0)
    (unless fd
      (error "Could not open ~S: ~A" name (unix:get-unix-error-msg errno)))
    (unwind-protect
	(alien:with-alien ((header exec))
	  (unix:unix-read fd
			  (alien:alien-sap header)
			  (alien:alien-size exec :bytes))
	  (let* ((len-of-text-and-data
		  (+ (alien:slot header 'text) (alien:slot header 'data)))
		 (memory-needed
		  (+ len-of-text-and-data (alien:slot header 'bss)))
		 (addr (allocate-space-in-foreign-segment memory-needed)))
	    (unix:unix-read fd addr len-of-text-and-data)))
      (unix:unix-close fd))))


#+hppa
(alien:def-alien-type nil
    (alien:struct sys_clock
                  (secs c-call:unsigned-int)
                  (nanosecs c-call:unsigned-int)))
#+hppa
(alien:def-alien-type nil
    (alien:struct header
                  (system_id c-call:short)
                  (a_magic c-call:short)
                  (version_id c-call:unsigned-int)
                  (file_time (alien:struct sys_clock))
                  (entry_space c-call:unsigned-int)
                  (entry_subspace c-call:unsigned-int)
                  (entry_offset c-call:unsigned-int)
                  (aux_header_location c-call:unsigned-int)
                  (aux_header_size c-call:unsigned-int)
                  (som_length c-call:unsigned-int)
                  (presumed_dp c-call:unsigned-int)
                  (space_location c-call:unsigned-int)
                  (space_total c-call:unsigned-int)
                  (subspace_location c-call:unsigned-int)
                  (subspace_total c-call:unsigned-int)
                  (loader_fixup_location c-call:unsigned-int)
                  (loader_fixup_total c-call:unsigned-int)
                  (space_strings_location c-call:unsigned-int)
                  (space_strings_size c-call:unsigned-int)
                  (init_array_location c-call:unsigned-int)
                  (init_array_total c-call:unsigned-int)
                  (compiler_location c-call:unsigned-int)
                  (compiler_total c-call:unsigned-int)
                  (symbol_location c-call:unsigned-int)
                  (symbol_total c-call:unsigned-int)
                  (fixup_request_location c-call:unsigned-int)
                  (fixup_request_total c-call:unsigned-int)
                  (symbol_strings_location c-call:unsigned-int)
                  (symbol_strings_size c-call:unsigned-int)
                  (unloadable_sp_location c-call:unsigned-int)
                  (unloadable_sp_size c-call:unsigned-int)
                  (checksum c-call:unsigned-int)))

#+hppa
(alien:def-alien-type nil
    (alien:struct aux_id
                  #|
                  (mandatory c-call:unsigned-int 1)
                  (copy c-call:unsigned-int 1)
                  (append c-call:unsigned-int 1)
                  (ignore c-call:unsigned-int 1)
                  (reserved c-call:unsigned-int 12)
                  (type c-call:unsigned-int 16)
                  |#
                  (dummy c-call:unsigned-int)
                  (length c-call:unsigned-int)))
#+hppa
(alien:def-alien-type nil
    (alien:struct som_exec_auxhdr
                  (som_auxhdr (alien:struct aux_id))
                  (exec_tsize c-call:long)
                  (exec_tmem c-call:long)
                  (exec_tfile c-call:long)
                  (exec_dsize c-call:long)
                  (exec_dmem c-call:long)
                  (exec_dfile c-call:long)
                  (exec_bsize c-call:long)
                  (exec_entry c-call:long)
                  (exec_flags c-call:long)
                  (exec_bfill c-call:long)))

#+hppa
(alien:def-alien-routine ("bzero" unix-bzero) c-call:void
  (s alien:system-area-pointer)
  (n c-call:unsigned-long))

#+hppa
(defconstant reloc-magic #x106)
#+hppa
(defconstant cpu-pa-risc1-0 #x20b)
#+hppa
(defconstant cpu-pa-risc1-1 #x210)
#+hppa
(defconstant cpu-pa-risc-max #x2ff)

#+hppa
(defun load-object-file (name)
  (format t ";;; Loading object file...~%")
  (multiple-value-bind (fd errno) (unix:unix-open name unix:o_rdonly 0)
    (unless fd
      (error "Could not open ~S: ~A" name (unix:get-unix-error-msg errno)))
    (unwind-protect
        (alien:with-alien ((header (alien:struct som_exec_auxhdr)))
          (unix:unix-lseek fd (alien:alien-size (alien:struct header) :bytes)
                           unix:l_set)
          (unix:unix-read fd
                          (alien:alien-sap header)
                          (alien:alien-size (alien:struct som_exec_auxhdr)
                                            :bytes))
          (let* ((tmem (alien:slot header 'exec_tmem))
                 (tsize (alien:slot header 'exec_tsize))
                 (dmem (alien:slot header 'exec_dmem))
                 (dsize (alien:slot header 'exec_dsize))
                 (bsize (alien:slot header 'exec_bsize))
                 (memory-needed (+ tsize dsize bsize (* 2 4096)))
                 (addr (allocate-space-in-foreign-segment memory-needed)))
            (unix-bzero addr memory-needed) ;force valid
            (unix:unix-lseek fd (alien:slot header 'exec_tfile) unix:l_set)
            (unix:unix-read fd (system:int-sap tmem) tsize)
            (unix:unix-lseek fd (alien:slot header 'exec_dfile) unix:l_set)
            (unix:unix-read fd (system:int-sap dmem) dsize)
            (unix-bzero (system:int-sap (+ dmem dsize)) bsize)
            ;;(format t "tmem ~X tsize ~X dmem ~X dsize ~X bsize ~X~%"
            ;;        tmem tsize dmem dsize bsize)
            ;;(format t "tfile ~X dfile ~X~%"
            ;;        (alien:slot header 'exec_tfile)
            ;;        (alien:slot header 'exec_dfile))
            (alien:alien-funcall (alien:extern-alien
                                  "sanctify_for_execution"
                                  (alien:function c-call:void
                                                  alien:system-area-pointer
                                                  c-call:unsigned-long))
                                 addr (+ (- dmem tmem) dsize bsize))
            ))
      (unix:unix-close fd))))

#-(or linux bsd solaris irix)
(progn
(defun parse-symbol-table (name)
  (format t ";;; Parsing symbol table...~%")
  (let ((symbol-table (make-hash-table :test #'equal)))
    (with-open-file (file name)
      (loop
	(let ((line (read-line file nil nil)))
	  (unless line
	    (return))
	  (let* ((symbol (subseq line 11))
		 (address (parse-integer line :end 8 :radix 16))
		 #+BSD (kind (aref line 9))	; filter out .o file names
		 (old-address (gethash symbol lisp::*foreign-symbols*)))
	    (unless (or (null old-address) (= address old-address)
			#+BSD (char= kind #\F))
	      (warn "~S moved from #x~8,'0X to #x~8,'0X.~%"
		    symbol old-address address))
	    (setf (gethash symbol symbol-table) address)))))
    (setf lisp::*foreign-symbols* symbol-table)))

(defun load-foreign (files &key
			   (libraries '("-lc"))
			   (base-file
			    #-hpux
			    (merge-pathnames *command-line-utility-name*
					     "path:")
			    #+hpux "library:cmucl.orig")
			   (env ext:*environment-list*)
		     	   (verbose *load-verbose*))
  "Load-foreign loads a list of C object files into a running Lisp.  The files
  argument should be a single file or a list of files.  The files may be
  specified as namestrings or as pathnames.  The libraries argument should be a
  list of library files as would be specified to ld.  They will be searched in
  the order given.  The default is just \"-lc\", i.e., the C library.  The
  base-file argument is used to specify a file to use as the starting place for
  defined symbols.  The default is the C start up code for Lisp.  The env
  argument is the Unix environment variable definitions for the invocation of
  the linker.  The default is the environment passed to Lisp."
  (let ((output-file (pick-temporary-file-name))
	(symbol-table-file (pick-temporary-file-name))
	(error-output (make-string-output-stream))
	(files (if (atom files) (list files) files)))

    (when verbose
      (format t ";;; Running library:load-foreign.csh...~%")
      (force-output))
    #+hpux
    (dolist (f files)
      (with-open-file (stream f :element-type '(unsigned-byte 16))
	(unless (let ((sysid (read-byte stream)))
                  (or (eql sysid cpu-pa-risc1-0)
		      (and (>= sysid cpu-pa-risc1-1)
			   (<= sysid cpu-pa-risc-max))))
	  (error "Object file is wrong format, so can't load-foreign:~
		  ~%  ~S"
		 f))
	(unless (eql (read-byte stream) reloc-magic)
	  (error "Object file is not relocatable, so can't load-foreign:~
		  ~%  ~S"
		 f))))

    (let ((proc (ext:run-program
		 "library:load-foreign.csh"
		 (list* (or *previous-linked-object-file*
			    (namestring (truename base-file)))
			(format nil "~X"
				*foreign-segment-free-pointer*)
			output-file
			symbol-table-file
			(append (mapcar
				 #'(lambda (name)
				     (or (unix-namestring name)
					 (error 'simple-file-error
						:pathname name
						:format-control
						"File does not exist: ~A."
						:format-arguments
						(list name))))
				 
				 files)
				libraries))
		 :env env
		 :input nil
		 :output error-output
		 :error :output)))
      (unless proc
	(error "Could not run library:load-foreign.csh"))
      (unless (zerop (ext:process-exit-code proc))
	(system:serve-all-events 0)
	(error "library:load-foreign.csh failed:~%~A"
	       (get-output-stream-string error-output)))
      (load-object-file output-file)
      (parse-symbol-table symbol-table-file)
      (unix:unix-unlink symbol-table-file)
      (let ((old-file *previous-linked-object-file*))
	(setf *previous-linked-object-file* output-file)
	(when old-file
	  (unix:unix-unlink old-file)))))
  (when verbose
    (format t ";;; Done.~%")
    (force-output)))


(export '(alternate-get-global-address))

(defun alternate-get-global-address (symbol)
  (declare (type simple-string symbol)
	   (ignore symbol))
  0)
) ;; #-(or linux bsd solaris irix)


;; Modern dlopen()-based loading of shared libraries
#+(or linux bsd solaris irix)
(progn

(defconstant rtld-lazy 1
  "Lazy function call binding")
(defconstant rtld-now 2
  "Immediate function call binding")
#+(and linux glibc2)
(defconstant rtld-binding-mask #x3
  "Mask of binding time value")

(defconstant rtld-global #-irix #x100 #+irix 4
  "If set the symbols of the loaded object and its dependencies are
   made visible as if the object were linked directly into the program")

(defvar *global-table* nil)
;;; Dynamically loaded stuff isn't there upon restoring from a
;;; save--this is primarily for irix, which resolves tzname at
;;; runtime, resulting in *global-table* being set in the saved core
;;; image, resulting in havoc upon restart.
#-linkage-table
(pushnew #'(lambda () (setq *global-table* nil))
	 ext:*after-save-initializations*)

(defvar *dso-linker*
  #+solaris "/usr/ccs/bin/ld"
  #-solaris "/usr/bin/ld")

(alien:def-alien-routine dlopen system-area-pointer
  (file c-call:c-string) (mode c-call:int))
(alien:def-alien-routine dlsym system-area-pointer
  (lib system-area-pointer)
  (name c-call:c-string))
(alien:def-alien-routine dlclose void (lib system-area-pointer))
(alien:def-alien-routine dlerror c-call:c-string)

;;; Ensure we've opened our own binary so can resolve global variables
;;; in the lisp image that come from libraries. This used to happen
;;; only in alternate-get-global-address, and only if no libraries
;;; were dlopened already, but that didn't work if something was
;;; dlopened before any problem global vars were used. So now we do
;;; this in any function that can add to the global-table, as well as
;;; in alternate-get-global-address.
(defun ensure-lisp-table-opened ()
  (unless *global-table*
    ;; Prevent recursive call if dlopen isn't defined
    (setf *global-table* (acons (int-sap 0) nil nil))
    (setf *global-table* (acons (dlopen nil rtld-lazy) nil nil))
    (when (zerop (system:sap-int (caar *global-table*)))
      (error "Can't open global symbol table: ~S" (dlerror)))))

(defun convert-object-file-path (path)
  ;; Convert path to something that dlopen might like, which means
  ;; translating logical pathnames and converting search-lists to the
  ;; first path that exists.
  (cond ((lisp::logical-pathname-p (pathname path))
	 (translate-logical-pathname path))
	((ignore-errors (ext:search-list-defined-p (pathname path)))
	 (ext:enumerate-search-list (s (pathname path)
				       path)
	   (when (probe-file s)
	     (return s))))
	(t
	 path)))

(defun load-object-file (file &optional (recordp t))
  (ensure-lisp-table-opened)
  ; rtld global: so it can find all the symbols previously loaded
  ; rtld now: that way dlopen will fail if not all symbols are defined.
  (let* ((filename (namestring (convert-object-file-path file)))
	 (sap (dlopen filename (logior rtld-now rtld-global))))
    (cond ((zerop (sap-int sap))
	   (let ((err-string (dlerror))
		 (sap (dlopen filename (logior rtld-lazy rtld-global))))
	     ;; For some reason dlerror always seems to return NIL,
	     ;; which isn't very informative.
	     (when (zerop (sap-int sap))
	       (return-from load-object-file
		 (values nil (format nil  "Can't open object ~S: ~S" file err-string))))
	     (dlclose sap)
	     (return-from load-object-file
	       (values nil
		       (format nil "LOAD-OBJECT-FILE: Unresolved symbols in file ~S: ~S"
			       file err-string)))))
	  ((and recordp (null (assoc sap *global-table* :test #'sap=)))
	   (setf *global-table* (acons sap file *global-table*)))
	  (t nil))))

;;; Clear close all dlopened libraries and clear out the entries in
;;; *global-table*, prior to doing a save-lisp.

(defun close-global-table ()
  (loop for lib-entry in *global-table*
	for (sap) = lib-entry
	do (progn
	     (dlclose sap)
	     ;; Probably not necessary, but neater than leaving around
	     ;; stale handles in the saved image.
	     (setf (car lib-entry) (int-sap 0)))))

;;; Open all the libraries in *GLOBAL-TABLE*. We open them in the same
;;; order as the first time they were loaded, so that any dependencies
;;; on load order are respected.
(defun reinitialize-global-table ()
  (loop for lib-entry in (reverse *global-table*)
	for (sap . lib-path) = lib-entry
	when lib-path
     do
       (loop
	  (restart-case 
	      (let ((new-sap (dlopen (namestring (convert-object-file-path lib-path))
				     (logior rtld-now rtld-global))))
		(cond ((zerop (sap-int new-sap))
		       ;; We're going down
		       (error "Couldn't open library ~S: ~S" lib-path (dlerror)))
		      (t
		       (format t "Reloaded library ~S~%" lib-path)
		       (force-output)))

		(setf (car lib-entry) new-sap)
		(return))
	    (continue ()
	      :report "Ignore library and continue"
	      (return))
	    (try-again ()
	      :report "Try reloading again"
	      )
	    (new-library ()
	      :report "Choose new library path"
	      (format *query-io* "Enter new library path: ")
	      (setf lib-path (read))))))
  (alien:alien-funcall (alien:extern-alien "os_resolve_data_linkage"
                                           (alien:function c-call:void))))

(defun alternate-get-global-address (symbol)
  (ensure-lisp-table-opened)
  ;; find the symbol in any of the loaded objects,
  ;; search in reverse order of loading, later loadings
  ;; take precedence
  (let ((result 0))
       (do ((table *global-table* (cdr table)))
	   ((or (null (car table)) (not (zerop result))))
	   (setq result (sap-int (dlsym (caar table) symbol))))
       (values result)))

(defun load-foreign (files &key
			   (libraries '("-lc"))
			   (base-file nil)
			   (env ext:*environment-list*)
		           (verbose *load-verbose*))
  "Load C object files into the running Lisp. The FILES argument
should be a single file or a list of files. The files may be specified
as namestrings or as pathnames. The LIBRARIES argument should be a
list of library files as would be specified to ld. They will be
searched in the order given. The default is just \"-lc\", i.e., the C
library. The BASE-FILE argument is used to specify a file to use as
the starting place for defined symbols. The default is the C start up
code for Lisp. The ENV argument is the Unix environment variable
definitions for the invocation of the linker. The default is the
environment passed to Lisp."
  ;; Note: dlopen remembers the name of an object, when dlopenin
  ;; the same name twice, the old objects is reused.
  (declare (ignore base-file))
  ;; if passed a single shared object that can be loaded directly via
  ;; dlopen(), do that instead of using the linker
  (when (atom files)
    (when verbose
      (format t ";;; Opening as shared library ~A ...~%" files))
    (multiple-value-bind (ok &optional error-string)
	(load-object-file files)
      (cond (ok
	     (when verbose
	       (format t ";;; Done.~%")
	       (force-output))
	     (return-from load-foreign))
	    (error-string
	     (format t "~A~%" error-string)
	     (force-output))))

    ;; If we get here, we couldn't open the file as a shared library.
    ;; Try again assuming it's an object file.
    (when verbose
      (format t ";;; Trying as object file ~A...~%" files)))
  
  
  (let ((output-file (pick-temporary-file-name
		      (concatenate 'string "/tmp/~D~C" (string (gensym)))))
	(error-output (make-string-output-stream)))
 
    (when verbose
      (format t ";;; Running ~A...~%" *dso-linker*)
      (force-output))
    
    (let ((proc (ext:run-program
		 *dso-linker*
		 (list*
		  #+(or solaris linux FreeBSD4) "-G"
		  #+(or OpenBSD NetBSD irix) "-shared"
		  #+darwin "-dylib"
		  "-o"
		  output-file
		  ;; Cause all specified libs to be loaded in full
		  #+(or OpenBSD linux FreeBSD4 NetBSD) "--whole-archive"
		  #+solaris "-z" #+solaris "allextract"
		  #+darwin "-all_load"
		  (append (mapcar
			   #'(lambda (name)
			       (or (unix-namestring name)
				   (error 'simple-file-error
					  :pathname name
					  :format-control
					  "File does not exist: ~A."
					  :format-arguments
					  (list name))))
			   (if (atom files)
			       (list files)
			       files))
			  ;; Return to default ld behaviour for libs
			  (list
			   #+(or OpenBSD linux FreeBSD4 NetBSD)
			   "--no-whole-archive"
			   #+solaris "-z" #+solaris "defaultextract")
			  libraries))
		 ;; on Linux/AMD64, we need to tell the platform linker to use the 32-bit
		 ;; linking mode instead of the default 64-bit mode. This can be done either
		 ;; via the LDEMULATION environment variable, or via the "-m" command-line
		 ;; option. Here we assume that LDEMULATION will be ignored by the platform
		 ;; linker on Linux/i386 platforms. 
		 :env `(#+(and x86 linux) (:ldemulation . "elf_i386") ,@env)
		 :input nil
		 :output error-output
		 :error :output)))
      (unless proc
	(error "Could not run ~A" *dso-linker*))
      (unless (zerop (ext:process-exit-code proc))
	(system:serve-all-events 0)
	(error "~A failed:~%~A" *dso-linker*
	       (get-output-stream-string error-output)))
      (load-object-file output-file nil)
      (unix:unix-unlink output-file))
    (when verbose
      (format t ";;; Done.~%")
      (force-output))))

#+linkage-table
(pushnew #'reinitialize-global-table ext:*after-save-initializations*)
) ;; #+(or linux bsd solaris irix)
