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
  "Fetch suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (let ((request (concat anything-c-rurema-suggest-url
                         (url-hexify-string input))))
    (flet ((fetch ()
             (loop
                with result-alist = (xml-get-children
                                     (car (xml-parse-region (point-min) (point-max)))
                                     'CompleteSuggestion)
                for i in result-alist
                for data = (cdr (caadr (assoc 'suggestion i)))
                for nqueries = (cdr (caadr (assoc 'num_queries i)))
                for ldata = (length data)
                do
                  (when (> ldata anything-rm-sug-lgh-flag)
                    (setq anything-rm-sug-lgh-flag ldata))
                collect (cons data nqueries) into cont
                finally return cont)))
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
    (setq suggestions (loop for i in suggestions
                         for interval = (- anything-rm-sug-lgh-flag (length (car i)))
                         for elm = (concat (car i)
                                           (make-string (+ 2 interval) ? )
                                           "(" (cdr i) " results)")
                         collect (cons elm (car i))))
    (if (some (lambda (data) (equal (cdr data) anything-input)) suggestions)
        suggestions
        ;; if there is no suggestion exactly matching the input then
        ;; prepend a Search on Rurema item to the list
        (append
         suggestions
         (list (cons (concat "Search for " "'" anything-input "'" " on Rurema")
                     anything-input))))))

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
    (requires-pattern . 3)
    (delayed)))
