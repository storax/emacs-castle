;;; config.el --- storax-helm-icons layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(configuration-layer/declare-layer 'storax-icons)

(defvar storax/helm-file-icons
  (list
   `("py" . ,storax/icon-python)
   `("/" . ,storax/icon-folder)
   `("zip" . ,storax/icon-archive)
   `("gz" . ,storax/icon-archive)
   `("rar" . ,storax/icon-archive)
   `("xz" . ,storax/icon-archive)
   `("whl" . ,storax/icon-archive)
   `("json" . ,storax/icon-json)
   `("rst" . ,storax/icon-rst)
   `("txt" . ,storax/icon-txt)
   `("pdf" . ,storax/icon-pdf)
   `("ini" . ,storax/icon-config)
   `("cfg" . ,storax/icon-config)
   `("conf" . ,storax/icon-config)
   `("cpp" . ,storax/icon-cpp)
   `("hpp" . ,storax/icon-cpp)
   `("h" . ,storax/icon-file)
   `("mp4" . ,storax/icon-movie)
   `("mkv" . ,storax/icon-movie)
   `("avi" . ,storax/icon-movie)
   `("mov" . ,storax/icon-movie)
   `("flv" . ,storax/icon-movie)
   `("mpeg" . ,storax/icon-movie)
   `("mpg" . ,storax/icon-movie)
   `("coffee" . ,storax/icon-coffee)
   `("css" . ,storax/icon-css)
   `("qss" . ,storax/icon-qss)
   `("js" . ,storax/icon-js)
   `("jpg" . ,storax/icon-image)
   `("jpeg" . ,storax/icon-image)
   `("png" . ,storax/icon-image)
   `("gif" . ,storax/icon-image)
   `("svg" . ,storax/icon-image)
   `("tiff" . ,storax/icon-image)
   `("xml" . ,storax/icon-xml)
   `("html" . ,storax/icon-xml)
   `("htm" . ,storax/icon-xml)
   `("el" . ,storax/icon-el)
   `("rb" . ,storax/icon-ruby)
   `("md" . ,storax/icon-md)
   `("sh" . ,storax/icon-shell)
   `("db" . ,storax/icon-db)
   `("sql" . ,storax/icon-sql)
   `("yaml" . ,storax/icon-yaml)
   `("yml" . ,storax/icon-yaml))
  "Icons for helm find file.")

;;; config.el ends here
