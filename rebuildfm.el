;;; rebuildfm --- Client for rebuildfm

;; Copyright (C) 2014 by Wataru MIYAGUNI

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-rebuildfm
;; Keywords: appletv airplay rebuildfm
;; Version: 0.0.1

;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; A client for rebuild.fm <http://rebuild.fm> .

;;; Code:

(require 'ctable)
(require 'request)
(require 'xml)
(require 'view)
(require 'popwin)
(require 'airplay)

(defconst rebuildfm->feed_url "http://feeds.rebuild.fm/rebuildfm")
(defconst rebuildfm->buffer "*rebuil.fm*")

(defvar rebuildfm->episodes nil)

(defun rebuildfm:feeds ()
  (let (response data)
    (setq response
          (request rebuildfm->feed_url
                   :parser (lambda () (xml-parse-region (point) (point-max)))
                   :sync t))
    (setq data (request-response-data response))
    (xml-get-children
     (first (xml-get-children (first data) 'channel)) 'item)))

(defun rebuildfm:episodes ()
  (unless rebuildfm->episodes
    (let ((get (lambda (node prop)
                 (nth 2 (assoc prop node))))
          (ary (lambda (node prop)
                 (nth 1 (assoc prop node)))))
      (setq rebuildfm->episodes
            (mapcar
             (lambda (f)
               (let ((feed (xml-node-children f)))
                 (list
                  'title       (funcall get feed 'title)
                  'subtitle    (funcall get feed 'itunes:subtitle)
                  'description (funcall get feed 'description)
                  'duration    (funcall get feed 'itunes:duration)
                  'url (cdr (assoc 'url (funcall ary feed 'enclosure))))))
             (rebuildfm:feeds)))))
  rebuildfm->episodes)

(defun rebuildfm:-table-column-model ()
  (list (make-ctbl:cmodel :title "Title" :align 'left)
        (make-ctbl:cmodel :title "Time"  :align 'left)))

(defun rebuildfm:-table-data-model (episodes)
  (mapcar (lambda (episode)
            (list (plist-get episode 'title)
                  (plist-get episode 'duration)))
          episodes))

(defun rebuildfm:-table-model (episodes)
  (make-ctbl:model
   :column-model (rebuildfm:-table-column-model)
   :data (rebuildfm:-table-data-model episodes)))

(defun rebuildfm:-table-component ()
  (let (cp)
    (setq cp (ctbl:create-table-component-buffer
              :model (rebuildfm:-table-model (rebuildfm:episodes))))
    (ctbl:cp-add-click-hook
     cp (lambda ()
          (rebuildfm:-onclick (car (ctbl:cp-get-selected cp)))))
    cp))

(defun rebuildfm:-onclick (num)
  (let ((episode (nth num (rebuildfm:episodes))))
    (rebuildfm:show (plist-get episode 'subtitle))
    (rebuildfm:play (plist-get episode 'url))))

(defun rebuildfm:show (msg)
  (with-current-buffer (get-buffer-create rebuildfm->buffer)
    (view-mode-disable)
    (erase-buffer)
    (insert msg)
    (view-mode-enable))
  (popwin:popup-buffer rebuildfm->buffer :noselect nil))

(defun rebuildfm:play (url)
  (airplay/video:play url))

;;;###autoload
(defun rebuildfm:stop ()
  (interactive)
  (airplay:stop))

;;;###autoload
(defun rebuildfm ()
  (interactive)
  (let ((component (rebuildfm:-table-component)))
    (pop-to-buffer (ctbl:cp-get-buffer component))))
