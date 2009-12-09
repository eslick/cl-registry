(in-package :registry)

(registry-proclamations)

;;
;; Geographic coordinate class
;;

(defmodel geotag ()
  ((user :accessor geotag-user :initarg :user :initform nil :index t)
   (lat :accessor geotag-latitude :initarg :latitude :initform 0)
   (long :accessor geotag-longitude :initarg :longitude :initform 0)))

(defmethod print-object ((geo geotag) stream)
  (format stream "#<GEOTAG ~A, ~A>" (geotag-latitude geo)
	  (geotag-longitude geo)))

(defun get-geotag (user)
  (get-instance-by-value 'geotag 'user user))

(defun get-geotag-entries (users)
  (mapcar #'get-dataset-entry
	  (remove-nulls
	   (mapcar #'get-geotag users))))

(defmethod get-dataset-entry ((object geotag))
  "Get a visualization api compatible dataset from an object"
  (with-slots (user lat long) object
    (list lat long (object-id user))))

(defmethod make-geotag (user &optional override)
  (let ((tag (get-geotag user)))
    (if (and tag (not override))
	tag
	(multiple-value-bind (lat lon)
	    (geocode-user user)
	  (when tag (drop-instance tag))
	  (if (and lat lon)
	      (progn (setf tag (make-instance 'geotag :user user))
		     (setf (geotag-latitude tag) lat
			   (geotag-longitude tag) lon)
		     tag)
	      (warn "Geotag looking failed for ~A (~A ~A)" user lat lon))))))


;;
;; Geo code
;;

;; Compute location from preference information, seed this with 
;; information from questionnaires

(defparameter *city-question* nil)
(defparameter *state-question*  nil)
(defparameter *country-question* nil)

(defun google-api-key ()
  "")

(defun get-geo-city (user)
  (get-preference :residence-city user))

(defun get-geo-state (user)
  (get-preference :residence-prov user))

(defun get-geo-country (user)
  (get-preference :residence-country user))
    

(defun geocode-user (user)
  (google-lookup-geocode (get-geo-city user)
			 (get-geo-state user)
			 (lookup-iso-code-country (get-geo-country user))))

    
(defun google-lookup-geocode (city state country)
  (let* ((query (google-geocode-query-string city state country))
	 (uri  (google-geocode-query-url query (google-api-key)))
	 (result (split "," (drakma:http-request uri))))
    (when (equal (first result) "200")
      (values (read-from-string (third result)) (read-from-string (fourth result))))))
    


(defun google-geocode-query-string (city state country)
  (declare (ignore state city))
  (regex-replace-all "\\s" 
 		     (format nil "~A" 
 			     (or country ""))
 		     "+"))
;; 		     (format nil "~A, ~A" 
;; 			     (or state "")
;; 			     (or country ""))
;; 		     "+"))
;; 		     (format nil "~A, ~A, ~A" 
;; 			     (or city "")
;; 			     (or state "")
;; 			     (or country ""))
;; 		     "+"))


(defun google-geocode-query-url (query key)
  (format nil "http://maps.google.com/maps/geo?q=~A&output=csv&key=~A"
	  query key))


;;
;; Conversions
;;


(defparameter *state-codes*
  '(("Alaska" . "AK")
    ("Alabama" . "AL")
    ("Arkansas" . "AR")
    ("American Samoa" . "AS")
    ("Arizona" . "AZ")
    ("California" . "CA")
    ("Colorado" . "CO")
    ("Connecticut" . "CT")
    ("District of Columbia" . "DC")
    ("Delaware" . "DE")
    ("Florida" . "FL")
    ("Federated States of Micronesia" . "FM")
    ("Georgia" . "GA")
    ("Guam" . "GU")
    ("Hawaii" . "HI")
    ("Iowa" . "IA")
    ("Idaho" . "ID")
    ("Illinois" . "IL")
    ("Indiana" . "IN")
    ("Kansas" . "KS")
    ("Kentucky" . "KY")
    ("Louisiana" . "LA")
    ("Massachusetts" . "MA")
    ("Maryland" . "MD")
    ("Maine" . "ME")
    ("Marshall Islands" . "MH")
    ("Michigan" . "MI")
    ("Minnesota" . "MN")
    ("Missouri" . "MO")
    ("Northern Mariana Islands" . "MP")
    ("Mississippi" . "MS")
    ("Montana" . "MT")
    ("North Carolina" . "NC")
    ("North Dakota" . "ND")
    ("Nebraska" . "NE")
    ("New Hampshire" . "NH")
    ("New Jersey" . "NJ")
    ("New Mexico" . "NM")
    ("Nevada" . "NV")
    ("New York" . "NY")
    ("Ohio" . "OH")
    ("Oklahoma" . "OK")
    ("Oregon" . "OR")
    ("Pennsylvania" . "PA")
    ("Puerto Rico" . "PR")
    ("Palau" . "PW")
    ("Rhode Island" . "RI")
    ("South Carolina" . "SC")
    ("South Dakota" . "SD")
    ("Tennessee" . "TN")
    ("Texas" . "TX")
    ("Utah" . "UT")
    ("Virginia" . "VA")
    ("Virgin Islands" . "VI")
    ("Vermont" . "VT")
    ("Washington" . "WA")
    ("West Virginia" . "WV")
    ("Wisconsin" . "WI")
    ("Wyoming"     . "WY")))

(defparameter *iso-country-codes*
'(("Afghanistan" . "af") ("Albania" . "al")
  ("Algeria" . "dz") ("American Samoa" . "as") ("Andorra" . "ad")
  ("Angola" . "ao") ("Anguilla" . "ai") ("Antarctica" . "aq")
  ("Antigua and Barbuda" . "ag") ("Argentina" . "ar") ("Armenia" . "am")
  ("Aruba" . "aw") ("Ascension Island" . "ac") ("Australia" . "au")
  ("Austria" . "at") ("Azerbaijan" . "az") ("Bahamas" . "bs")
  ("Bahrain" . "bh") ("Bangladesh" . "bd") ("Barbados" . "bb")
  ("Belarus" . "by") ("Belgium" . "be") ("Belize" . "bz")
  ("Benin" . "bj") ("Bermuda" . "bm") ("Bhutan" . "bt")
  ("Bolivia" . "bo") ("Bosnia and Herzegovina" . "ba")
  ("Botswana" . "bw") ("Bouvet Island" . "bv") ("Brazil" . "br")
  ("British Virgin Islands" . "vg")
  ("British Indian Ocean Territory" . "io") ("Brunei Darussalam" . "bn")
  ("Bulgaria" . "bg") ("Burkina Faso" . "bf") ("Burundi" . "bi")
  ("Cambodia" . "kh") ("Cameroon" . "cm") ("Canada" . "ca")
  ("Cape Verde" . "cv") ("Cayman Islands" . "ky")
  ("Central African Republic" . "cf") ("Chad" . "td") ("Chile" . "cl")
  ("China" . "cn") ("Christmas Island" . "cx")
  ("Cocos (Keeling) Island" . "cc") ("Colombia" . "co")
  ("Comoros" . "km") ("Congo, Republic of" . "cg")
  ("Congo, Democratic Republic of" . "cd") ("Cook Islands" . "ck")
  ("Costa Rica" . "cr") ("Cote d'Ivoire" . "ci") ("Croatia" . "hr")
  ("Cuba" . "cu") ("Cyprus" . "cy") ("Czech Republic" . "cz")
  ("Denmark" . "dk") ("Djibouti" . "dj") ("Dominica" . "dm")
  ("Dominican Republic" . "do") ("Ecuador" . "ec") ("Egypt" . "eg")
  ("El Salvador" . "sv") ("Equatorial Guinea" . "gq") ("Eritrea" . "er")
  ("Estonia" . "ee") ("Ethiopia" . "et")
  ("Falkland Islands (Malvinas)" . "fk") ("Faroe Islands" . "fo")
  ("Fiji" . "fj") ("Finland" . "fi") ("France" . "fr")
  ("French Guiana" . "gf") ("French Polynesia" . "pf")
  ("French Southern Territories" . "tf") ("Gabon" . "ga")
  ("Gambia" . "gm") ("Georgia" . "ge") ("Germany" . "de")
  ("Ghana" . "gh") ("Gibraltar" . "gi") ("Greece" . "gr")
  ("Greenland" . "gl") ("Grenada" . "gd") ("Guadeloupe" . "gp")
  ("Guam" . "gu") ("Guatemala" . "gt") ("Guernsey" . "gg")
  ("Guinea" . "gn") ("Guinea-Bissau" . "gw") ("Guyana" . "gy")
  ("Haiti" . "ht") ("Heard and McDonald Islands" . "hm")
  ("Honduras" . "hn") ("Hong Kong" . "hk") ("Hungary" . "hu")
  ("Iceland" . "is") ("India" . "in") ("Indonesia" . "id")
  ("Iran" . "ir") ("Iraq" . "iq") ("Ireland" . "ie")
  ("Isle of Man" . "im") ("Israel" . "il") ("Italy" . "it")
  ("Jamaica" . "jm") ("Japan" . "jp") ("Jersey" . "je")
  ("Jordan" . "jo") ("Kazakhstan" . "kz") ("Kenya" . "ke")
  ("Kiribati" . "ki") ("Korea, North" . "kp") ("Korea, South" . "kr")
  ("Kuwait" . "kw") ("Kyrgyzstan" . "kg") ("Laos" . "la")
  ("Latvia" . "lv") ("Lebanon" . "lb") ("Lesotho" . "ls")
  ("Liberia" . "lr") ("Libya" . "ly") ("Liechtenstein" . "li")
  ("Lithuania" . "lt") ("Luxembourg" . "lu") ("Macau" . "mo")
  ("Macedonia" . "mk") ("Madagascar" . "mg") ("Malawi" . "mw")
  ("Malaysia" . "my") ("Maldives" . "mv") ("Mali" . "ml")
  ("Malta" . "mt") ("Marshall Islands" . "mh") ("Martinique" . "mq")
  ("Mauritania" . "mr") ("Mauritius" . "mu") ("Mayotte" . "yt")
  ("Mexico" . "mx") ("Micronesia" . "fm") ("Moldova" . "md")
  ("Monaco" . "mc") ("Mongolia" . "mn") ("Montserrat" . "ms")
  ("Morocco" . "ma") ("Mozambique" . "mz") ("Myanmar" . "mm")
  ("Namibia" . "na") ("Nauru" . "nr") ("Nepal" . "np")
  ("Netherlands" . "nl") ("Netherlands Antilles" . "an")
  ("New Caledonia" . "nc") ("New Zealand" . "nz") ("Nicaragua" . "ni")
  ("Niue" . "nu") ("Niger" . "ne") ("Nigeria" . "ng")
  ("Norfolk Island" . "nf") ("Northern Mariana Islands" . "mp")
  ("Norway" . "no") ("Oman" . "om") ("Pakistan" . "pk") ("Palau" . "pw")
  ("Palestinian Territory, Occupied" . "ps") ("Panama" . "pa")
  ("Papua New Guinea" . "pg") ("Paraguay" . "py") ("Peru" . "pe")
  ("Philippines" . "ph") ("Pitcairn Island" . "pn") ("Poland" . "pl")
  ("Portugal" . "pt") ("Puerto Rico" . "pr") ("Qatar" . "qa")
  ("Reunion" . "re") ("Romania" . "ro") ("Russia" . "ru")
  ("Rwanda" . "rw") ("Saint Barthelemy" . "bl") ("Saint Helena" . "sh")
  ("Saint Kitts and Nevis" . "kn") ("Saint Lucia" . "lc")
  ("Saint Martin" . "mf") ("Saint Pierre and Miquelon" . "pm")
  ("Saint Vincent and the Grenadines" . "vc") ("Samoa" . "ws")
  ("San Marino" . "sm") ("Sao Tome and Principe" . "st")
  ("Saudia Arabia" . "sa") ("Senegal" . "sn") ("Serbia" . "cs")
  ("Seychelles" . "sc") ("Sierra Leone" . "sl") ("Singapore" . "sg")
  ("Slovakia" . "sk") ("Slovenia" . "si") ("Solomon Islands" . "sb")
  ("Somalia" . "so") ("South Africa" . "za")
  ("South Georgia and the South Sandwich Islands" . "gs")
  ("Spain" . "es") ("Sri Lanka" . "lk") ("Sudan" . "sd")
  ("Suriname" . "sr") ("Svalbard and Jan Mayen Islands" . "sj")
  ("Swaziland" . "sz") ("Sweden" . "se") ("Switzerland" . "ch")
  ("Syria" . "sy") ("Taiwan" . "tw") ("Tajikistan" . "tj")
  ("Tanzania" . "tz") ("Thailand" . "th") ("Timor-Leste" . "tp")
  ("Togo" . "tg") ("Tokelau" . "tk") ("Tonga" . "to")
  ("Trinidad and Tobago" . "tt") ("Tunisia" . "tn") ("Turkey" . "tr")
  ("Turkmenistan" . "tm") ("Turks and Caicos Islands" . "tc")
  ("Tuvalu" . "tv") ("Uganda" . "ug") ("Ukraine" . "ua")
  ("United Arab Emirates" . "ae") ("United Kingdom" . "gb")
  ("United States of America" . "us") 
  ("United States Virgin Islands" . "vi") ("Uruguay" . "uy")
  ("US Minor Outlying Islands" . "um") ("USSR" . "su")
  ("Uzbekistan" . "uz") ("Vanuatu" . "vu")
  ("Vatican City State (Holy See)" . "va") ("Venezuela" . "ve")
  ("Vietnam" . "vn") ("Wallis and Futuna Islands" . "wf")
  ("Western Sahara" . "eh") ("Yemen" . "ye") ("Yugoslavia" . "yu")
  ("Zambia" . "zm") ("Zimbabwe" . "zw")))

(defparameter *country-code-aliases*
  '(("UK" . "uk") ("United States" . "us") ("USA" . "us") ("US" . "us")))

(defun lookup-us-state-code (state-name)
  (aif (find state-name *state-codes*
	     :key #'first
	     :test #'equalp)
       (cdr it)
       state-name))

(defun lookup-iso-country-code (country-name)
  (awhen (or (find country-name *iso-country-codes*
		   :key #'first
		   :test #'equalp)
	     (find country-name *country-code-aliases*
		   :key #'first
		   :test #'equalp))
    (string-upcase (cdr it))))

(defun lookup-iso-code-country (country-code)
  (awhen (find country-code *iso-country-codes*
	       :key #'cdr
	       :test #'equalp)
    (car it)))
