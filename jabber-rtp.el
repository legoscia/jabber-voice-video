;;; jabber-rtp.el --- voice and video calls using Jingle/RTP  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

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

;; 

;;; Code:

(require 'websocket)
(require 'json)

;;;###autoload
(eval-after-load "jabber-jingle"
  '(add-to-list 'jabber-jingle-applications
		(cons "urn:xmpp:jingle:apps:rtp:1" 'start-jabber-jingle-rtp)))

;;;###autoload
(eval-after-load "jabber-disco"
  '(mapc 'jabber-disco-advertise-feature
	 '("urn:xmpp:jingle:apps:rtp:1"
	   "urn:xmpp:jingle:apps:rtp:audio"
	   "urn:xmpp:jingle:apps:rtp:video"
	   "urn:xmpp:jingle:transports:ice-udp:1")))

(defconst jabber-rtp-htdocs-dir
  (expand-file-name "htdocs" (file-name-directory load-file-name)))

(defun jabber-rtp--htdocs-index.html (proc _request)
  (let ((ws-contact (process-get proc :ws-contact)))
    (insert-file-contents-literally (expand-file-name "index.html.template" jabber-rtp-htdocs-dir))
    (goto-char (point-min))
    (while (search-forward "@WS_URL@" nil t)
      (replace-match (format "ws://127.0.0.1:%s" (second ws-contact)) t t))
    (jabber-httpd--send-200
     proc "text/html; charset=utf-8" (buffer-string))))

(defun jabber-rtp--htdocs-main.js (proc _request)
  (with-temp-buffer
    (insert-file-contents-literally (expand-file-name "main.js" jabber-rtp-htdocs-dir))
    (jabber-httpd--send-200
     proc "application/javascript" (buffer-string))))

(defun jabber-rtp--htdocs-adapter.js (proc _request)
  (with-temp-buffer
    (insert-file-contents-literally (expand-file-name "adapter.js" jabber-rtp-htdocs-dir))
    (jabber-httpd--send-200
     proc "application/javascript" (buffer-string))))

;;;###autoload (autoload 'start-jabber-jingle-rtp "jabber-rtp")
(define-state-machine jabber-jingle-rtp
  :start ((jc sid jid our-role)
	  "Start a Jingle RTP state-machine."
	  (let ((state-data
		 (list :jc jc :sid sid :jid jid :our-role our-role)))
	    (ecase our-role
	      (:initiator
	       (list :initiating state-data))
	      (:responder
	       (list :responding state-data))))))

(define-state jabber-jingle-rtp :responding (fsm state-data event callback)
  (case (car-safe event)
    (:session-initiate
     (let* ((content (second event))
	    (description (jabber-xml-path content '(description)))
	    (transport (jabber-xml-path content '(("urn:xmpp:jingle:transports:ice-udp:1" . "transport")))))
       (if (null transport)
	   ;; The other party is not using ICE-UDP - give up.
	   (progn
	     (jabber-send-iq (plist-get state-data :jc)
			     (plist-get state-data :jid)
			     "set"
			     `(jingle ((xmlns . "urn:xmpp:jingle:1")
				       (action . "session-terminate")
				       (sid . ,(plist-get state-data :sid)))
				      (reason () (unsupported-transports)))
			     nil nil nil nil)
	     (list nil state-data))
	 ;; Let's look at the description.
	 (let ((media (jabber-xml-get-attribute description 'media))
	       (payload-types (jabber-xml-get-children description 'payload-type))
	       ;; TODO: <encryption>, <bandwidth>
	       )
	   (plist-put state-data :sdp-from-initiator (jabber--jingle-rtp-to-sdp media payload-types)))
	 ;; Let's start our HTTP server.
	 (let* ((ws-server
		 (websocket-server
		  t
		  ;; TODO: on-close
		  :on-open
		  (lexical-let ((fsm fsm))
		    (lambda (ws)
		      (fsm-send fsm (list :ws-open ws))))
		  :on-message
		  (lexical-let ((fsm fsm))
		    (lambda (ws frame)
		      (let ((payload (websocket-frame-payload frame)))
			(message "Got %s" payload)
			(fsm-send fsm (list :ws payload)))))))
		(ws-contact (process-contact ws-server))
		(http-server
		 (jabber-httpd-listen
		  "jabber-jingle-rtp-httpd"
		  '(("" . jabber-rtp--htdocs-index.html)
		    ("adapter.js" . jabber-rtp--htdocs-adapter.js)
		    ("main.js" . jabber-rtp--htdocs-main.js))
		  (list :ws-contact ws-contact)))
		(http-contact (process-contact http-server))
		(accept-action
		 (lexical-let ((url (format "http://%s:%s" (first http-contact) (second http-contact))))
		   (lambda (_button) (browse-url url))))
		(reject-action
		 (lexical-let ((fsm fsm))
		   (lambda (_button) (fsm-send fsm :reject)))))
	   (plist-put state-data :ws-server ws-server)
	   (plist-put state-data :http-server http-server)
	   (with-current-buffer (jabber-chat-create-buffer
				 (plist-get state-data :jc)
				 (plist-get state-data :jid))
	     (ewoc-enter-last
	      jabber-chat-ewoc
	      (list :notice
		    (concat (jabber-jid-displayname (plist-get state-data :jid))
			    " wants to start a voice/video conversation.\n"
			    " " (make-text-button "Accept" nil 'action accept-action)
			    " " (make-text-button "Reject" nil 'action reject-action))))
	     ;; XXX: alert!
	     (list :responding state-data)
)))))
    (:ws-open
     (let ((ws-client (second event)))
       (plist-put state-data :ws-client ws-client)
       (websocket-send-text
	ws-client
	(json-encode `((type . "offer")
		       (body . ,(plist-get state-data :sdp-from-initiator)))))
       (list :responding state-data)))
    (:ws
     (let ((payload (json-read-from-string (second event))))
       (cond
	((equal "candidate" (cdr (assq 'type payload)))
	 (let* ((sdp (cdr (assq 'sdp payload)))
		(jingle-description (jabber--sdp-to-jingle sdp)))
	   ;; TODO
	   )
	 )
	((equal "error" (cdr (assq 'type payload)))
	 (destructuring-bind (_error reason message . maybe-sdp)
	     ;; XXX: don't split maybe-sdp into lines
	     (split-string (cdr (assq 'body payload)) "\n")
	   (message "Cannot negotiate audio/video call with %s: %s"
		    (jabber-jid-displayname (plist-get state-data :jid))
		    message)
	   (jabber-send-iq (plist-get state-data :jc)
			   (plist-get state-data :jid)
			   "set"
			   `(jingle ((xmlns . "urn:xmpp:jingle:1")
				     (action . "session-terminate")
				     (sid . ,(plist-get state-data :sid)))
				    (content
				     ;; TODO: more attributes
				     ()
				     (description
				      ;; TODO: more attributes
				      ((xmlns . "urn:xmpp:jingle:apps:rtp:1"))
				      ,@(when maybe-sdp (jabber--sdp-to-jingle maybe-sdp)))
				     (transport
				      ((xmlns . "urn:xmpp:jingle:transports:ice-udp:1"))))
				    (reason ()
					    (,(intern reason))
					    (text () ,message)))
			     nil nil nil nil)
	   (list nil state-data))))))))

(defconst jabber--sdp-payload-types
  '(
    (0 . "PCMU")        ; A   8,000       1
    (3 . "GSM")         ; A   8,000       1
    (4 . "G723")        ; A   8,000       1
    (5 . "DVI4")        ; A   8,000       1
    (6 . "DVI4")        ; A  16,000       1
    (7 . "LPC")         ; A   8,000       1
    (8 . "PCMA")        ; A   8,000       1
    (9 . "G722")        ; A   8,000       1
    (10 . "L16")        ; A  44,100       2
    (11 . "L16")        ; A  44,100       1
    (12 . "QCELP")      ; A   8,000       1
    (13 . "CN")         ; A   8,000       1
    (14 . "MPA")        ; A  90,000       (see text)
    (15 . "G728")       ; A   8,000       1
    (16 . "DVI4")       ; A  11,025       1
    (17 . "DVI4")       ; A  22,050       1
    (18 . "G729")       ; A   8,000       1
    (25 . "CelB")       ; V  90,000
    (26 . "JPEG")       ; V  90,000
    (28 . "nv")         ; V  90,000
    (31 . "H261")       ; V  90,000
    (32 . "MPV")        ; V  90,000
    (33 . "MP2T")       ; AV 90,000
    (34 . "H263")       ; V  90,000
    )
  "Payload type definitions from RFC 3551.")

;; <description xmlns="urn:xmpp:jingle:apps:rtp:1" media="audio">
;;   <payload-type id="110" name="SPEEX" clockrate="16000"/>
;; </description>

(defun jabber--jingle-rtp-to-sdp (media payload-types)
  (concat
   "v=0\r\n"
   "o=username " (apply 'format "%06d%06d%06d" (current-time)) " 1 IN IP4 0.0.0.0\r\n"
   "s=call\r\n"
   "t=0 0\r\n"
   ;; TODO: not always audio.
   "m=audio "
   ;; TODO: can we get away with port 9999?
   "9999 "
   ;; TODO: more profiles?
   "RTP/AVP "
   (mapconcat
    (lambda (payload-type)
      (jabber-xml-get-attribute payload-type 'id))
    payload-types " ")
   "\r\n"
   "c=IN IP4 0.0.0.0\r\n"
   (apply
    'concat
    (mapcar
     (lambda (payload-type)
       (jabber-xml-let-attributes
	   (id name clockrate ptime maxptime) payload-type
	 (concat
	  (unless (assq (string-to-number id) jabber--sdp-payload-types)
	    (format "a=rtpmap:%s %s/%s\r\n" id name clockrate))
	  (when ptime
	    (format "a=ptime:%s\r\n" ptime))
	  (when maxptime
	    (format "a=maxptime:%s\r\n" maxptime))
	  (let ((parameters (jabber-xml-get-children payload-type 'parameter)))
	    (when parameters
	      (format "a=fmtp:%s %s\r\n"
		      id
		      (mapconcat
		       (lambda (parameter)
			 (jabber-xml-let-attributes (name value) parameter
			   (format "%s=%s" name value)))
		       parameters
		       ";")))))))
     payload-types))))

;; v=0
;; o=Mozilla-SIPUA-28.0a2 14618 0 IN IP4 0.0.0.0
;; s=SIP Call
;; t=0 0
;; a=ice-ufrag:0e2a1e6c
;; a=ice-pwd:aef9d60532180137e63e54ef980ea5e0
;; a=fingerprint:sha-256 A6:FB:6D:89:A6:0D:16:D3:21:BB:FD:70:02:CE:7D:11:B5:7C:5B:1B:53:87:C3:5D:32:A2:D4:79:D9:D8:DA:7D
;; m=audio 9 RTP/SAVPF 109 0 8 101
;; c=IN IP4 0.0.0.0
;; a=rtpmap:109 opus/48000/2
;; a=ptime:20
;; a=rtpmap:0 PCMU/8000
;; a=rtpmap:8 PCMA/8000
;; a=rtpmap:101 telephone-event/8000
;; a=fmtp:101 0-15
;; a=sendrecv
;; a=setup:actpass
;; a=rtcp-mux
;; m=video 9 RTP/SAVPF 120
;; c=IN IP4 0.0.0.0
;; a=rtpmap:120 VP8/90000
;; a=sendrecv
;; a=rtcp-fb:120 nack
;; a=rtcp-fb:120 nack pli
;; a=rtcp-fb:120 ccm fir
;; a=setup:actpass
;; a=rtcp-mux

(defun jabber--sdp-to-jingle (sdp)
  (let ((lines (split-string sdp "[\r\n]+" t))
	descriptions)
    ;; Skip to media description
    (while (and lines
		(not (string-prefix-p "m=" (car lines))))
      (pop lines))
    ;; Add media descriptions
    (while lines
      (unless (string-match "^m=\\([a-z]+\\) [0-9]+ \\([A-Z/]+\\) \\([0-9 ]+\\)" (car lines))
	(error "Bad media line: %S" (car lines)))
      (let* ((media (match-string 1 (car lines)))
	     (rtp-profile (match-string 2 (car lines)))
	     (formats (split-string (match-string 3 (car lines)) " " t))
	     rtpmaps fmtps
	     payload-types)
	(pop lines)
	(while (and lines (not (string-match-p "^m=" (car lines))))
	  (let ((line (pop lines)))
	    (cond
	     ((string-match "^a=rtpmap:\\([0-9]+\\) \\([^/]+\\)\\(?:/\\([0-9]+\\)\\(?:/\\([0-9]+\\)\\)?\\)?" line)
	      (let ((id (match-string 1 line))
		    (name (match-string 2 line))
		    (clockrate (match-string 3 line))
		    (channels (match-string 4 line)))
		(push (list id name clockrate channels) rtpmaps)
		))
	     ((string-match "^a=ptime:\\([0-9]+\\)" line)
	      ;; TODO. also maxptime
	      )
	     ((string-match "^a=fmtp:\\([0-9]+\\) \\(.*\\)" line)
	      (let ((id (match-string 1 line))
		    (data (match-string 2 line)))
		(push (list id data) fmtps))))))

	(dolist (id formats)
	  (let ((format-entry (assq (string-to-number id) jabber--sdp-payload-types))
		(rtpmap (assoc id rtpmaps))
		(fmtp (assoc id fmtps))
		name clockrate channels)
	    (cond
	     (format-entry
	      ;; XXX: should we include clockrate and channels from the list in the RFC?
	      (setq name (cdr format-entry)))
	     (rtpmap
	      (setq name (second rtpmap))
	      (setq clockrate (third rtpmap))
	      (setq channels (fourth rtpmap))))
	    (push
	     `(payload-type
	       ((id . ,id)
		(name . ,name)
		,@(when clockrate
		    `((clockrate . ,clockrate)))
		,@(when channels
		    `((channels . ,channels))))
	       ;; TODO: fmtp
	       nil)
	     payload-types)))

	(push
	 `(description
	   ((xmlns . "urn:xmpp:jingle:apps:rtp:1")
	    (media . ,media))
	   ,@(nreverse payload-types))
	 descriptions)))

    descriptions))

(provide 'jabber-rtp)
;;; jabber-rtp.el ends here
