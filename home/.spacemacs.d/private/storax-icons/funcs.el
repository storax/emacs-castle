;;; funcs.el --- Functions for loading icons

;;; Commentary:

;;; Code:

(defun storax/create-img (path &optional text type asc)
  "Return string with img at PATH.

TEXT is the alternative text and is determines the minimum width.  Defaults to '  '.
TYPE can be nil or an image type.  E.g. 'png, 'svg.
ASC the ascent in percent.  Defaults to 80."
  (let ((text (if text text "  "))
        (asc (if asc asc 80)))
    (propertize
     text 'display (create-image path type nil :ascent asc :mask 'heuristic))))

(defun storax/load-vendor-img (name &optional text type asc)
  "Load an image with NAME from the image directory."
  (storax/create-img (concat storax-icons/imgdir name) text type asc))

;;; funcs.el ends here
