(defcustom anything-c-rurema-suggest-url
  "http://rurema.clear-code.com/api:internal/auto-complete/?term="
  "URL used for looking up Rurema suggestions."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-rurema-suggest-search-url
  "http://rurema.clear-code.com/query:"
  "URL used for Rurema searching."
  :type 'string
  :group 'anything-config)

(defcustom anything-rurema-suggest-use-curl-p nil
  "*When non--nil use CURL to get info from `anything-c-rurema-suggest-url'.
Otherwise `url-retrieve-synchronously' is used."
  :type 'boolean
  :group 'anything-config)

(defun anything-rurema-suggest ()
  "Preconfigured `anything' for rurema search with rurema suggest."
  (interactive)
  (anything-other-buffer 'anything-c-source-rurema-suggest "*anything rurema*"))

;;; Rurema Suggestions
(defvar anything-rm-sug-lgh-flag 0)
(defun anything-c-rurema-suggest-fetch (input)
  "Fetch suggestions for INPUT from JSON buffer.
Return an alist with elements like (data)."
  (let ((request (concat anything-c-rurema-suggest-url
                         (url-hexify-string input))))
    (flet ((fetch ()
                  (json-read-from-string
                   (car (cdr
                         (split-string
                          (funcall 'buffer-string)
                          "\n\n"
                          ))))))
      (if anything-rurema-suggest-use-curl-p
          (with-temp-buffer
            (call-process "curl" nil t nil request)
            (fetch))
          (with-current-buffer
              (url-retrieve-synchronously request)
            (fetch))))))


(defun anything-c-rurema-suggest-set-candidates ()
  "Set candidates with result and number of rurema results found."
  (let ((suggestions (anything-c-rurema-suggest-fetch anything-input)))
    (append suggestions nil)))

(defvar anything-c-rurema-suggest-default-browser-function nil
  "*The browse url function you prefer to use with rurema suggest.
When nil, use the first browser function available
See `anything-browse-url-default-browser-alist'.")

(defun anything-c-rurema-suggest-action (candidate)
  "Default action to jump to a rurema suggested candidate."
  (let ((arg (concat anything-c-rurema-suggest-search-url
                                 (url-hexify-string candidate))))
    (anything-aif anything-c-rurema-suggest-default-browser-function
        (funcall it arg)
      (anything-c-browse-url arg))))

(defvar anything-c-source-rurema-suggest
  '((name . "Rurema Suggest")
    (candidates . anything-c-rurema-suggest-set-candidates)
    (action . (("Rurema Search" . anything-c-rurema-suggest-action)))
    (volatile)
    (requires-pattern . 1)
    (delayed)))
