(in-package :montezuma)

(registry::define-system-event-hook initialize-langutils (start-app)
  langutils:init-langutils)

(defclass stemming-analyzer (analyzer) ())

(defmethod token-stream
    ((self stemming-analyzer) field string)
  (declare (ignore field))
  (make-instance
   'stemming-converter
   :input (make-instance
	   'lowercase-filter
	   :input (make-instance 'standard-tokenizer
				 :input string))))

(defclass stemming-converter (token-filter) ())

(defmethod next-token ((self stemming-converter))
  (with-slots (input) self
    (let ((token (next-token input)))
      (when token
	(if (not (langutils:stopword?
		  (token-image token)))
	    (make-token (langutils:get-lemma
			 (token-image token))
			(token-start token)
			(token-end token))
	    (next-token self))))))