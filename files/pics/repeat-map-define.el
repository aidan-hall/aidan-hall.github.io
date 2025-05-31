;;; repeat-map-define --- Define a new repeat map easily.
;;; Commentary:
;;; This is silly.
;;; Code:

(require 'repeat)

(defun repeat-map--create (binds)
  "Create a keymap using BINDS.
See `repeat-map-define' for the required format."
   (let ((map (make-sparse-keymap)))
     (dolist (m binds)
       (let ((key (car m)) (command (cadr m)))
	 (define-key map key command)
	 )
       )
     map))

(defun repeat-map--register (name binds)
  "Register the functions from BINDS with the repeat map NAME.
If any of these functions are called, the repeat map will start."
  (dolist (m binds)
    (put (cadr m) 'repeat-map name)
    ))

(defmacro repeat-map-define (name binds)
  "Create a new keymap called NAME, using BINDS.
The BINDS argument is a list of lists like this:
\(key function\)
After that, if `repeat-mode' is enabled and one of the functions in BINDS is
pressed, the repeat map will be started."
  (list 'repeat-map--register
	(list 'defvar name
	      (list 'repeat-map--create binds))
	binds)
  )

(provide 'repeat-map-define)
;;; repeat-map-define.el ends here
