(in-package :registry-test)

(def-suite models
    :description "Test the Registry data models"
    :in registry-test)

(in-suite models)

(defstruct model-info
  class-name
  class
  slot-names
  tests
  values
  other-values
  mid)

(defun model-info-instance (model-info &optional
                            (class-name (model-info-class-name model-info)))
  (awhen (model-info-mid model-info)
    (registry::get-model class-name it)))

(defun drop-model-info-instances (model-infos)
  (elephant:drop-instances (delete nil (mapcar 'model-info-instance model-infos))))

(defun sympack (x)
  (or (find-symbol (string x) :registry)
      (error "No symbol named ~x in package REGISTRY"
             (string x))))

(defun new-model-info (class-name slot-names tests values other-values)
  (setf class-name (sympack class-name)
        slot-names (mapcar 'sympack slot-names))
  (let* ((class (find-class class-name))
         (slotds (c2mop:class-slots class))
         (values-tail values)
         initargs)
    (dolist (slot-name slot-names)
      (let ((slotd (find slot-name slotds
                         :test #'eq :key #'c2mop:slot-definition-name)))
        (push (car (c2mop:slot-definition-initargs slotd)) initargs)
        (push (pop values-tail) initargs)))
    (let ((instance (apply #'make-instance class-name (nreverse initargs))))
      (make-model-info
       :class-name class-name
       :class class
       :slot-names slot-names
       :tests tests
       :values values
       :other-values other-values
       :mid (registry::mid instance)))))

(defun make-model-infos (&rest descriptions)
  (let ((infos nil)
        (done nil))
    (unwind-protect
         (dolist (desc descriptions (progn (setf done t) (nreverse infos)))
           (destructuring-bind (class-name slot-names tests values other-values)
               desc
             (push (new-model-info class-name slot-names tests values other-values)
                   infos)))
      (unless done
        (drop-model-info-instances infos)))))

(defvar *model-dont-compare-value* (list :dont-compare))

(defvar *break-testing-article-class* nil) ;for debugging ticket #282

(defun test-model-infos (model-infos)
  (flet ((check-values (class-name instance info values)
           (loop
              for slot-name in (model-info-slot-names info)
              for was = (slot-value instance slot-name)
              for value in values
              for test in (model-info-tests info)
              unless (eq value *model-dont-compare-value*)
              do
                (is-true (funcall test was value)
                         "~a value for slot ~a was: ~a, sb: ~a"
                         class-name slot-name was value))))
    (metatilities:collect-garbage)
    (dolist (info model-infos)
      (let* ((class-name (model-info-class-name info))
             (instance (model-info-instance info class-name)))
        (check-values class-name instance info (model-info-values info))
        (loop
           for slot-name in (model-info-slot-names info)
           for value in (model-info-other-values info)
           do
             (setf (slot-value instance slot-name) value))
	(if (and (eq class-name 'registry::article) *break-testing-article-class*)
	    (break "after set values"))))
    (metatilities:collect-garbage)
    (dolist (info model-infos)
      (let* ((class-name (model-info-class-name info))
             (instance (model-info-instance info class-name)))
	(if (and *break-testing-article-class* (eq class-name 'registry::article))
	    (break "before check values"))
        (check-values class-name
                      instance
                      info
                      (model-info-other-values info))))))

(defun run-model-test ()
  (let* ((user (or (registry::get-user "test user")
                   (make-instance 'registry::user
                                  :username "test user")))
         (user2 (or (registry::get-user "test user 2")
                    (make-instance 'registry::user
                                  :username "test user 2")))
         (question1 (make-instance
                     'registry::question
                     :name "Question name"
                     :prompt "What is the wing velocity of a fully-laden sparrow?"))
         (question2 (make-instance
                     'registry::question
                     :name "Question name 2"
                     :prompt "What is your favorite color?"))
         (group1 (make-instance
                  'registry::survey-group
                  :name "Test group 1"))
         (group2 (make-instance
                  'registry::survey-group
                  :name "Test group 2"))
         (cat1 (make-instance
                  'registry::forum-category
                  :name "Test category"
                  :short "Test"))
         (cat2 (make-instance
                  'registry::forum-category
                  :name "Test category 2"
                  :short "Test 2"))
         (topic1 (make-instance
                  'registry::forum-topic
                  :number 1
                  :category cat1
                  :owner user
                  :content "Test topic 1"
                  :content-type :html))
         (topic2 (make-instance
                  'registry::forum-topic
                  :number 2
                  :category cat2
                  :owner user
                  :content "Test topic 2"
                  :content-type :markdown))
         (constraint1 (make-instance 'registry::constraint))
         (constraint2 (make-instance 'registry::constraint)))
    (unwind-protect
         (let ((infos (make-model-infos
                       `(announcement
                         (date content)
                         (eql equal)
                         (,(get-universal-time) "announcement content")
                         (,(1+ (get-universal-time)) "announcement content too"))
                       `(article
                         (title owner page order content content-type)
                         (equal eq equal eql equal eq)
                         ("article title"
                          nil "article page" 1 "article content" :html)
                         ("article title too"
                          ,user "article page too" 2
                          "article content too" :markup))
                       `(blog-entry
                         (title author content date)
                         (equal eq equal eql)
                         ("blog title" nil "blog content" ,(get-universal-time))
                         ("blog title too"
                          ,user "blog content too" ,(1+ (get-universal-time))))
                       `(comment
                         (target author content date status)
                         (eq eq equal eql eq)
                         (:target nil "comment content" ,(get-universal-time) :open)
                         (:tegrat
                          ,user "comment content too" ,(1+ (get-universal-time))
                          :cleared))
                       `(forum-topic
                         (current-post-number number category owner
                          subject content content-type date-created date-updated)
                         (eql eql eq eq equal equal eq eql eql)
                         (0 1 ,cat1 ,user "Forum topic" "Forum content"
                             :html
                             ,(get-universal-time) ,(1+ (get-universal-time)))
                         (10 2 ,cat2 ,user2 "Forum topic 2" "Forum content 2"
                             :markdown
                             ,(+ 2 (get-universal-time)) ,(+ 3 (get-universal-time))))
                       `(forum-post
                         (owner number post-datetime topic title content content-type)
                         (eq eql eql eq equal equal eq)
                         (user 1 ,(get-universal-time) ,topic1
                          "Post title" "Post content" :html)
                         (user2 2 ,(1+ (get-universal-time)) ,topic2
                          "Post title 2" "Post content 2" :markdown))
                       `(message-template
                         (subject body event)
                         (equal equal eq)
                         ("test subject" "test body" :test-event)
                         ("test subject 2" "test body 2" :test-event2))
                       `(population
                         (name keywords constraints)
                         (equal equal eq)
                         ("test population" "test population keywords" constraints1)
                         ("test population 2" "test population keywords 2"
                                              constraints2))
                       `(registration-request
                         (username password email magic-key date)
                         (equal equal equal equal eql)
                         ("reg user" "reg password" "reg email" "reg magic key"
                                     ,*model-dont-compare-value*)
                         ("reg user 2" "reg password 2" "reg email 2"
                                       "reg magic key 2" ,(1+ (get-universal-time))))
                       `(survey-group
                         (name advice questions rules owner parent type)
                         (equal equal equal equal eq eq eq)
                         ("Test survey group 1"
                          "Advice 1" (,question1) ,group1 nil nil :single)
                         ("Test survey group 2"
                          "Advice 2"
                          (,question1 question2) ,group2 user ,group1 :multi))
                       `(question
                         (name prompt question-help data-help data-type
                               data-subtype data-constraint
                               view-type choices parent required)
                         (equal equal equal equal eq eq eq eq equal eq eq)
                         ("test question name"
                          "test question prompt" "test question help"
                          "test data help" :string nil nil
                          :auto nil nil nil)
                         ("test question name 2"
                          "test question prompt 2" "test question help 2"
                          "test data help 2" :integer :subtype
                          ,constraint1 :view-type (1 2 3) ,group1 t))
                       `(answer
                         (question user entry-time value id history)
                         (eq eq eql equal eql equal)
                         (,question1
                          nil ,(get-universal-time) "answer value" 1 ("history"))
                         (,question2
                          ,user ,(1+ (get-universal-time)) "answer value too"
                          2 ("history" "more-history")))
                       )))
           (unwind-protect
                (test-model-infos infos)
             (drop-model-info-instances infos)))
      (elephant:drop-instances
       (list user user2 question1 question2 group1 group2
             cat1 cat2 topic1 topic2
             constraint1 constraint2)))))

(test model-infos
  (run-model-test))
