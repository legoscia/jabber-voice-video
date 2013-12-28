;;; jabber-httpd.el --- simple ad-hoc HTTP server    -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple HTTP server, inspired by simple-httpd, with the difference
;; that multiple listeners can be created, with no global variables
;; being used.

;;; Code:

;;;###autoload
(defun jabber-httpd-listen (name dispatch &optional plist)
  (make-network-process
   :name name
   :server t
   :host "localhost"
   :service t
   :coding 'binary
   :plist (append (list :dispatch dispatch) plist)
   :log 'jabber-httpd--accept
   :filter 'jabber-httpd--filter))

(defun jabber-httpd--accept (_server client _message)
  (set-process-buffer client (generate-new-buffer "jabber-httpd")))

(defun jabber-httpd--parse (string)
  "Parse client http header into alist.

This was taken from httpd-parse in simple-httpd.el."
  (let* ((lines (split-string string "[\n\r]+"))
         (req (list (split-string (car lines))))
         (post (cadr (split-string string "\r\n\r\n"))))
    (dolist (line (butlast (cdr lines)))
      (push (list (car (split-string line ": "))
                  (mapconcat 'identity
                             (cdr (split-string line ": ")) ": ")) req))
    (push (list "Content" post) req)
    (reverse req)))

(defun jabber-httpd--filter (proc string)
  (let ((buffer (process-buffer proc)))
    (if (not (buffer-live-p buffer))
	(message "Got %S with dead process" string)
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert string)

	(goto-char (point-min))
	(when (search-forward "\r\n\r\n" nil t)
	  ;; We have the request.
	  (let* ((request (jabber-httpd--parse (delete-and-extract-region (point-min) (point))))
		 (method (caar request))
		 (path (cadar request)))
	    (if (not (string= method "GET"))
		(jabber-httpd--send-error proc "405" "Method not allowed" '(("Allow" . "GET")))
	      (message "Request: %s %s %S" method path request)
	      (if (or (zerop (length path)) (/= (aref path 0) ?/))
		  ;; Empty path, or path doesn't start with slash
		  (jabber-httpd--send-error proc "400" "Bad request" ())
		(let* ((file (substring path 1))
		       (function (cdr (assoc file (process-get proc :dispatch)))))
		  (if function
		      (funcall function proc request)
		    (jabber-httpd--send-error proc "404" "File not found" ())))))))))))

(defun jabber-httpd--send-error (proc code text headers)
  (let ((error-html
	 (encode-coding-string
	  (concat
	   "<!DOCTYPE html>\n"
	   "<html><head>\n"
	   "<title>" code " " text "</title>\n"
	   "</head><body>\n"
	   "<h1>" code " " text "</h1>\n"
	   "</body></html>\n")
	  'utf-8)))
    (process-send-string
     proc
     (concat
      (format "HTTP/1.1 %s %s\r\n" code text)
      "Connection: close\r\n"
      "Content-type: text/html; charset=utf-8\r\n"
      "Content-length: " (number-to-string (length error-html)) "\r\n"
      (apply 'concat (mapcar (lambda (h) (concat (car h) ": " (cdr h) "\r\n")) headers))
      "\r\n" error-html))
    (process-send-eof proc)))

(defun jabber-httpd--send-200 (proc type contents)
  ;; NB: contents should be ASCII, or encoded as binary
  (process-send-string
   proc
   (concat "HTTP/1.1 200 OK\r\n"
	   "Content-type: " type "\r\n"
	   "Content-length: " (number-to-string (length contents)) "\r\n"
	   "Connection: close\r\n"
	   "\r\n"
	   contents))
  (process-send-eof proc))

(provide 'jabber-httpd)
;;; jabber-httpd.el ends here
