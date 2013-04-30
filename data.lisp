(defvar *class-list* (quote (
(:name "math 008" :title	"college algebra"	:section "07" :code	 "44962"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "42/40"	                :days "tr"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "mh 423"	    	:professor "d adams"                              )
(:name "math 071" :title	"calc for bus-aviat"	:section "10" :code	"48010"  :credit	"3"	:type "sem"	:capacity "24/40"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "dh 318"	        :professor "s ahmed"                    )
(:name "math 030p" :title	"cal i with precal"	:section "08" :code     "51013" :credit	         "5"	:type "sem" 	:capacity "27/35"						:days "mwf"	:time "1030-1150"   :dates "08/22/12-12/10/12"	:location "dmh 167"		:professor "s ahmed"                              )
(:name "math 019" :title	"precalculus"	        :section "02" :code	 "40586"  :credit	 "5"	:type "sem"  	 :notes "b4,39,52"	                :capacity "37/37"	                :days "mtwr"    :time "0900-1005"    :dates "08/22/12-12/10/12"	:location "dh 243"	    	:professor "s arabhi"                             )
(:name "math 019w" :title	"precalculus workshop"	:section "19" :code	 "50604"  :credit	 "1"	:type "lab" 				:capacity "27/32"	                :days "mw"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "dh 547"	    	:professor "j becker")
(:name "math 042w" :title	"discrete math wksp"	:section "02" :code	"45084"  :credit	"1"	:type "lab" 	:capacity "25/30"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "wsq 001"	        :professor "j becker"                   )
(:name "math 030" :title	"calculus i"	        :section "02" :code	 "42814"  :credit	 "3"	:type "sem" 	:notes "39,42,52"	        :capacity "49/55"	                :days "mw"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "dh 351"	    	:professor "m beeson"                             )
(:name "math 030" :title	"calculus i"	        :section "01" :code	 "42376"  :credit	 "3"	:type "sem" 	:notes "39,42,52"	        :capacity "54/55"	                :days "mw"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "dh 351"	    	:professor "m beeson"                             )
(:name "math 133a" :title	"ord diff eq"	        :section "01" :code	"40621"  :credit	"3"	:type "sem"	:capacity "38/40"						:days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "mh 323"	        :professor "m beeson"                   )
(:name "math 030w" :title	"calculus i workshop"	:section "07" :code      "45028" :credit	         "1"	:type "lab" 	:capacity "17/32"						:days "mw"	:time "1800-1915"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "m beeson"                             )
(:name "math 071w" :title	"calc wkshp bus-avi"	:section "06" :code	"48029"  :credit	"1"	:type "lab"	:capacity "18/32"						:days "mw"	:time "1500-1615"   :dates "08/22/12-12/10/12"	:location "dh 547"	        :professor "m beeson"                   )

(:name "math 071w" :title	"calc wkshp bus-avi"	:section "11" :code	"48034"  :credit	"1"	:type "lab"	:capacity "13/32"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "dh 547"	        :professor "m blockus"                  )
(:name "math 128a" :title	"abstract algebra i"	:section "01" :code	"40619"  :credit	"3"	:type "sem"	:capacity "27/28"						:days "tr"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 235"	        :professor "m blockus"                  )
;;(:name "math 030w" :title	"calculus i workshop"	:section "16"  :credit	         "1"	:type "lab" 	:capacity "31/32"						:days "tr"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "dh 547"		:professor "m blockus"                            )
(:name "math 042" :title	"discrete math"	        :section "01" :code "40601" :credit	         "3"	:type "sem" 	:capacity "37/37"						:days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "mh 423"		:professor "m blockus"                            )
(:name "math 042" :title	"discrete math"	        :section "02"  :code "41634" :credit	         "3"	:type "sem" 	:capacity "37/37"						:days "mw"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 423"		:professor "m blockus"                            )

(:name "math 031" :title	"calculus ii"	        :section "12" :code "48460" :credit	         "4"	:type "sem" 	:capacity "40/37"						:days "mwf"	:time "0730-0840"   :dates "08/22/12-12/10/12"	:location "mh 323"		:professor "m bodas"                              )
(:name "math 071" :title	"calc for bus-aviat"	:section "01" :code	"43783"  :credit	"3"	:type "sem" :notes "	39,52" :capacity "47/40"	        		                :days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "dh 318"	        :professor "m bodas"                    )
(:name "math 008" :title	"college algebra"	:section "12" :code	 "50795"  :credit	 "3"	:type "sem"        :notes "b4"				:capacity "40/40"	                :days "mw"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "mh 320"	    	:professor "m bodas"                              )

(:name "math 031" :title	"calculus ii"	        :section "13"   :code "48461" :credit	         "4"	:type "sem" 	:capacity "37/37"						:days "mwf"	:time "1030-1140"   :dates "08/22/12-12/10/12"	:location "dh 318"		:professor "m cayco-gajic"                        )
(:name "math 203" :title	"appl math comp&stat proj" :section "01" :code	"45133"  :credit	"3"	:type "sem"	:capacity "6/10"						:days "tr"	:time "0830-0920"	    :dates "08/22/12-12/10/12"	:location "dh 280"	        :professor "m cayco-gajic"              )
(:name "math 008w" :title	"coll algebra wkshp"	:section "02" :code	 "44964"  :credit	 "1"	:type "lab" 				:capacity "26/32"	                :days "mw"      :time "1330-1445"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "m cayco-gajic"                        )
(:name "math 019w" :title	"precalculus workshop"	:section "01" :code	 "44971"  :credit	 "1"	:type "lab" 				:capacity "29/32"	                :days "mw"      :time "0730-0845"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "m cayco-gajic"                        )
;;(:name "math 180h" :title	"ind studies/honors"	:section "05" :code	"44297"  :credit	"3"	:type "sem"	:capacity  "1/1"						:days "tba"	:time "tba"	    :dates "08/22/12-12/10/12"	                                                 :professor "m cayco-gajic"              )

(:name "math 006a" :title "entry level math i"	:section "23" :code	 "42347"  :credit	"0"	:type "act" 				:capacity "22/25"	    				:days "mw" :time "1030-1120" 	:location "dh 181"	    	:professor "a chen"                                     )
(:name "math 006a" :title "entry level math i"	:section "24" :code	 "42348"  :credit	"0"	:type "act" 				:capacity "17/25"	    				:days "mw" :time "1200-1250"	:location "dh 181"	    	:professor "a chen"                                     )
(:name "math 006a" :title "entry level math i"	:section "25" :code	 "42349"  :credit	"0"	:type "act" 				:capacity "20/25"	    				:days "mw" :time "1330-1420"    :location "dh 181"	    	:professor "a chen"                                     )
(:name "math 006a" :title "entry level math i"	:section "26" :code	 "40569"  :credit	"0"	:type "act" 				:capacity "18/25"	    				:days "mw" :time "1420-1520"	:location "dh 181"	    	:professor "a chen"                                     )

(:name "math 008" :title  "college algebra"	:section "10" :code	 "50553"  :credit	"3"	:type "sem"       :notes "b4"				:capacity "42/50"	                :days "mw"      :time "1330-1445"    :dates "08/22/12-12/10/12"	:location "cl 234"	    	:professor "s chen"                               )
(:name "math 006a" :title "entry level math i"	:section "14" :code	 "40568"  :credit	"0"	:type "act" 				:capacity "13/25"	    					:days "mw" :time "1200-1250"	:location "sci 321"	    	:professor "s chen"                                     )

;;(:name "math 203" :title	"appl math comp&stat proj" :section "02" :code	"43792"  :credit	"3"	:type "sem"	:capacity "0/10"						:days "tba"	:time "tba"	    :dates "08/22/12-12/10/12"	:location "dh 280"	        :professor "s crunk"                                       )
(:name "math 008w" :title	"coll algebra wkshp"	:section "03" :code	 "44965"  :credit	 "1"	:type "lab" 				:capacity "7/32"	                        :days "tr"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "mh 234"	    	:professor "s crunk"                              )
(:name "math 265" :title	"time series"	        :section "01" :code	"45142"  :credit	"3"	:type "sem"	:capacity "22/20"						:days "tr"	:time "1730-1845"   :dates "08/22/12-12/10/12"	:location "mh 320"                 :professor "s crunk"	                                               )
(:name "math 269" :title	"stat consulting"	:section "01" :code	"48490"  :credit	"3"	:type "lec"	:capacity "12/30"						:days "tr"	:time "1900-2015"   :dates "08/22/12-12/10/12"	:location "mh 320"	         :professor "s crunk"                                      )
(:name "math 019w" :title	"precalculus workshop"	:section "12" :code	 "44983"  :credit	 "1"	:type "lab" 				:capacity "31/32"	                :days "tr"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "s crunk"                              )

(:name "math 006l" :title	"entry level math"	:section "10" :code	 "41483"  :credit	"5"	:type "sem" :type "97"	        		:capacity "197/225"	    							:location "sci 142"	    	:professor "s desousa"                                  )
;;(:name "math 110l" :title	"math computing lab"	:section "01" :code	"40611"  :credit	"1"	:type "lab"	:capacity "8/10"						        :days "tba"	:time "tba"	    :dates "08/22/12-12/10/12"	:location "mh 221"	        :professor "s desousa"                  )
(:name "unvs 015a" :title	"statway 1"	        :section "03" :code	 "50530"  :credit	 "5"	:type "lec" 		                :capacity "0/30"	                        :days "mtwr"    :time "1330-1445"    :dates "08/22/12-12/10/12"	:location "cl 225"	    	:professor "s desousa")

(:name "math 133a" :title	"ord diff eq"	        :section "06" :code	"41528"  :credit	"3"	:type "sem"	:capacity "41/42"						:days "tr"	:time "1500-1615"   :dates "08/22/12-12/10/12"	:location "mh 323"	        :professor "r dodd"                     )
(:name "math 133a" :title	"ord diff eq"	        :section "05" :code	"40625"  :credit	"3"	:type "sem"	:capacity "44/42"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 323"	        :professor "r dodd"                     )
(:name "math 133a" :title	"ord diff eq"	        :section "07" :code	"48260"  :credit	"3"	:type "sem"	:capacity "37/40"						:days "tr"	:time "1800-1915"   :dates "08/22/12-12/10/12"	:location "mh 323"	        :professor "r dodd"                     )
(:name "math 019w" :title	"precalculus workshop"	:section "10" :code	 "44981"  :credit	 "1"	:type "lab" 	:notes "61"			:capacity "31/32"	                :days "tr"      :time "0900-1015"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "r dodd"                               )
(:name "math 019w" :title	"precalculus workshop"	:section "11" :code	 "44982"  :credit	 "1"	:type "lab" 				:capacity "33/32"	                :days "tr"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "r dodd"                               )

(:name "math 071w" :title	"calc wkshp bus-avi"	:section "09" :code	"48032"  :credit	"1"	:type "lab"	:capacity "28/32"						:days "tr"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "dh 547"	        :professor "a dreiling"                 )
(:name "math 008" :title	"college algebra"	:section "08" :code	 "46450"  :credit	 "3"	:type "sem"       :notes "b4"				:capacity "39/40"	                :days "tr"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "mh 423"	    	:professor "a dreiling"                           )


(:name "math 030w" :title	"calculus i workshop"	:section "16" :code	 "50607"  :credit	 "1.5"	:type "lab" 				:capacity "33/32"	                :days "tr"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "dh 547"	    	:professor "FA"                             )
(:name "math 019w" :title	"precalculus workshop"	:section "13" :code	 "44984"  :credit	 "1"	:type "lab" 				:capacity "33/32"	                :days "tr"      :time "1330-1445"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "FA"                             )
(:name "math 019w" :title	"precalculus workshop"	:section "14" :code	 "44985"  :credit	 "1"	:type "lab" 				:capacity "30/32"	                :days "tr"      :time "1500-1615"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "FA"                             )


(:name "math 032" :title	"calculus iii"	        :section "13" :code "51008" :credit	         "3"	:type "sem" 	:capacity "44/50"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 523"		:professor "c fan"                                )

(:name "math 003a" :title	"intensive learn i"	:section "01" :code	 "41565"  :credit	"4"	:type "sem" :notes "97"	    		:capacity "21/25"	  		:days "mtwr"     :time "0800-0850"					:location "dh 303"	  	:professor "t fish"   )
(:name "math 070" :title	"finite math"	        :section "04" :code	"49522"  :credit	"3"	:type "sem" 	:capacity "38/40"						:days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "sh 239"	        :professor "t fish"                     )


(:name "math 032w" :title	"calculus iii wkshp"	:section "02" :code      "45075" :credit         "1"	:type "lab" 	:capacity "34/32"						:days "mw"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 234"		:professor "l foster"                             )
(:name "math 071" :title	"calc for bus-aviat"	:section "02" :code	"48008"  :credit	"3"	:type "sem"	:capacity "58/60"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "dh 318"	        :professor "l foster"                   )
(:name "math 071" :title	"calc for bus-aviat"	:section "03" :code	"43784"  :credit	"3"	:type "sem"	:notes "39,52" :capacity "55/60"             		                :days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "dh 318"	        :professor "l foster"                   )
(:name "math 019w" :title	"precalculus workshop"	:section "02" :code	 "44972"  :credit	 "1"	:type "lab" 				:capacity "34/32"	                :days "mw"      :time "0900-1015"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "l foster"                             )
(:name "math 143m" :title	"num analy sci comp"	:section "01" :code	"48272"  :credit	"3"	:type "sem"	:capacity "17/32"						:days "mw"	:time "1900-2015"   :dates "08/22/12-12/10/12"	:location "mh 235"	        :professor "l foster"                   )

(:name "math 019w" :title	"precalculus workshop"	:section "20" :code	 "50605"  :credit	 "1"	:type "lab" 				:capacity "29/32"	                :days "tr"      :time "1500-1615"    :dates "08/22/12-12/10/12"	:location "dh 547"	    	:professor "d goldston"                           )
(:name "math 030" :title	"calculus i"	        :section "04" :code	 "43577"  :credit	 "3"	:type "sem" 	:notes "39,42,52,97"	        :capacity "60/55"	                :days "tr"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "dh 318"	    	:professor "d goldston"                           )
(:name "math 031w" :title	"calculus ii workshop"	:section "15" :code      "48408"  :credit	         "1"	:type "lab" 	:capacity "23/32"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "sh 345"		:professor "d goldston"                           )
(:name "math 008" :title	"college algebra"	:section "06" :code	 "43275"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "41/40"	                :days "tr"      :time "0900-1015"    :dates "08/22/12-12/10/12"	:location "mh 423"	    	:professor "d goldston"                           )


(:name "math 163" :title	"probability theory"	:section "01" :code	"40629"  :credit	"3"	:type "sem" :notes "97"	:capacity "33/28"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 234"	        :professor "a gottlieb"                 )
(:name "math 163" :title	"probability theory"	:section "02" :code	"46499"  :credit	"3"	:type "sem"	:capacity "26/28"						:days "mw"	:time "1600-1715"   :dates "08/22/12-12/10/12"	:location "mh 234"	        :professor "a gottlieb"                 )
(:name "math 019w" :title	"precalculus workshop"	:section "18" :code	 "46698"  :credit	 "1"	:type "lab" 				:capacity "30/32"	                :days "mw"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "mh 233"	    	:professor "a gottlieb"                           )
(:name "math 071w" :title	"calc wkshp bus-avi"	:section "03" :code	"45225"  :credit	"1"	:type "lab"	:capacity "29/32"						:days "mw"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 233"	        :professor "a gottlieb"                 )



(:name "math 006a" :title "entry level math i"	:section "21" :code	 "40570"  :credit	"0"	:type "act" 				:capacity "16/25"	    			:days "mw"	:time "0800-0850"			:location "dh 181"	    	:professor "d goulette"                                 )
(:name "math 008" :title	"college algebra"	:section "02" :code	 "41092"  :credit	 "3"	:type "lab" 				:capacity "21/25"	                :days "mw"      :time "0900-1015"    :dates "08/22/12-12/10/12"	:location "sh 347"	    	:professor "d goulette"                           )


(:name "math 100w" :title	"writing workshop"	:section "01" :code	"49551"  :credit	"3"	:type "sem" :notes "z"	:capacity "31/28"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "eng 401"	        :professor "j hilliard"                 )


(:name "math 019" :title	"precalculus"	        :section "12" :code	 "43566"  :credit	 "5"	:type "sem"  	 :notes "b4,39,52"			:capacity "42/42"	                :days "mwf"     :time "1200-1320"    :dates "08/22/12-12/10/12"	:location "sh 347"	    	:professor "t hsu"                                )
(:name "math 019w" :title	"precalculus workshop"	:section "05" :code	 "44976"  :credit	 "1"	:type "lab" 				:capacity "31/32"	                :days "mw"      :time "1330-1445"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "t hsu"                                )
(:name "math 108" :title	"intro to proofs"	:section "01" :code	"41356"  :credit	"3"	:type "sem"	:capacity "23/32"						:days "mw"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 235"	        :professor "t hsu"                      )
(:name "math 019w" :title	"precalculus workshop"	:section "07" :code	 "44978"  :credit	 "1"	:type "lab" 				:capacity "29/32"	                :days "mw"      :time "1630-1745"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "t hsu"                                )

(:name "math 071" :title	"calc for bus-aviat"	:section "05" :code	"45200"  :credit	"3"	:type "sem"	:capacity "32/40"						:days "mw"	:time "1800-1915"   :dates "08/22/12-12/10/12"	:location "dh 318"	        :professor "t huynh"                    )

(:name "math 019" :title	"precalculus"	        :section "06" :code	 "40589"  :credit	 "5"	:type "sem"  	 :notes "b4,39,52"	                :capacity "38/37"	                :days "mtwr"    :time "1500-1605"    :dates "08/22/12-12/10/12"	:location "dh 243"	    	:professor "k jensen"                             )
(:name "math 010" :title	"math for gen ed"	:section "04" :code	 "40982"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "37/40"	                :days "mw"      :time "1630-1745"    :dates "08/22/12-12/10/12"	:location "mh 224"	    	:professor "k jensen"                             )
(:name "math 008" :title	"college algebra"	:section "11" :code	 "50683"  :credit	 "3"	:type "sem"       :notes "b4"				:capacity "39/40"	                :days "tr"      :time "1330-1445"    :dates "08/22/12-12/10/12"	:location "sh 239"	    	:professor "k jensen"                             )


(:name "math 008" :title	"college algebra"	:section "04" :code	 "41553"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "45/40"	                :days "mw"      :time "1630-1745"    :dates "08/22/12-12/10/12"	:location "mh 423"	    	:professor "a jiru"                               )
(:name "math 019" :title	"precalculus"	        :section "14" :code	 "44191"  :credit	 "5"	:type "sem"  :notes "b4,97"			:capacity "35/37"	                :days "mwr"     :time "1500-1620"    :dates "08/22/12-12/10/12"	:location "cl 234"	    	:professor "a jiru"                               )

(:name "math 071" :title	"calc for bus-aviat"	:section "04" :code	"48019"  :credit	"3"	:type "sem"	:capacity "21/40"						:days "mw"	:time "1630-1745"   :dates "08/22/12-12/10/12"	:location "dh 318"	        :professor "j jordan"                   )

(:name "math 032" :title	"calculus iii"	        :section "12" :code "50554" :credit	         "3"	:type "sem" 	:capacity "40/40"						:days "tr"	:time "1500-1615"   :dates "08/22/12-12/10/12"	:location "sh 240"		:professor "h katsuura"                           )
(:name "math 032" :title	"calculus iii"	        :section "11" :code "49539" :credit	         "3"	:type "sem" 	:capacity "39/42"						:days "tr"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 224"		:professor "h katsuura"                           )
(:name "math 071" :title	"calc for bus-aviat"	:section "07" :code "45202"  :credit	"3"	:type "sem"	:capacity "35/40"						        :days "tr"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "dh 318"	        :professor "h katsuura"                 )
(:name "math 032" :title	"calculus iii"	        :section "09" :code "48409" :credit	         "3"	:type "sem" 	:capacity "34/40"						:days "tr"	:time "1630-1745"   :dates "08/22/12-12/10/12"	:location "mh 323"		:professor "h katsuura"                           )

(:name "math 123" :title	"diff eq and linear alg":section "01" :code "45284"  :credit	"3"	:type "sem"	:capacity "40/40"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "cl 234"	        :professor "k kellum"                   )
(:name "math 030w" :title	"calculus i workshop"	:section "04" :code "45025" :credit	         "1"	:type "lab" 	:capacity "30/32"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "k kellum"                             )
(:name "math 031" :title	"calculus ii"	        :section "03" :code "40595" :credit	         "4"	:type "sem" 	:capacity "42/42"						:days "mtwr"	:time "0930-1020"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "k kellum"                             )
(:name "math 031" :title	"calculus ii"	        :section "04" :code "40596" :credit	         "4"	:type "sem" 	:capacity "42/42"						:days "mtwr"	:time "1030-1120"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "k kellum"                             )

(:name "math 032w" :title	"calculus iii wkshp"	:section "03" :code "45076"  :credit	         "1"	:type "lab" 	:capacity "29/32"						:days "tr"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "n khorlin"                            )
(:name "math 032w" :title	"calculus iii wkshp"	:section "04"  :code "46731" :credit	         "1"	:type "lab" 	:capacity "31/32"						:days "tr"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "mh 234"		:professor "n khorlin"                            )

(:name "math 032" :title	"calculus iii"	        :section "07" :code "43608" :credit	         "3"	:type "sem" 	:capacity "42/40"						:days "tr"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "mh 320"		:professor "p koev"                               )
(:name "math 243a" :title	"adv numerical analysis":section "01" :code	"48369"  :credit	"3"	:type "sem"	:capacity "10/28"						:days "tr"	:time "1730-1845"   :dates "08/22/12-12/10/12"	:location "mh 234"	        :professor "p koev" )
(:name "math 032" :title	"calculus iii"	        :section "08"  :code "44249"  :credit	         "3"	:type "sem" 	:capacity "42/40"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 320"		:professor "p koev"                               )

(:name "math 161a" :title	"applied stats i"	:section "03" :code	"40627"  :credit	"3"	:type "sem" :notes "97"	:capacity "40/37"						:days "tr"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "sh 347"	        :professor "o kovaleva"                 )
(:name "math 261b" :title	"design analy expts"	:section "01" :code	"48489"  :credit	"3"	:type "lec"	:capacity "21/30"						:days "mw"	:time "1600-1715"   :dates "08/22/12-12/10/12"	:location "mh 320"	                                               )
(:name "math 161a" :title	"applied stats i"	:section "01" :code	"43006"  :credit	"3"	:type "sem" :notes "97"	:capacity "36/37"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "mh 320"	        :professor "o kovaleva"                 )
(:name "math 161b" :title	"applied stats ii"	:section "01" :code	"40628"  :credit	"3"	:type "sem" 	:capacity "15/28"						:days "tr"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "sh 347"	        :professor "o kovaleva"                 )


(:name "math 030w" :title	"calculus i workshop"	:section "02"  :code    "45023"     :credit	         "1"	:type "lab" 	:capacity "34/32"						:days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "r kubelka"                            )
(:name "math 275" :title	"topology"	        :section "01" :code	"48492"  :credit	"3"	:type "sem"	:capacity "7/15"						:days "mw"	:time "1600-1715"   :dates "08/22/12-12/10/12"	:location "mh 233"	                                               )
(:name "math 133a" :title	"ord diff eq"	        :section "03" :code	"40623"  :credit	"3"	:type "sem"	:capacity "33/42"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "mh 323"	        :professor "r kubelka"                  )
(:name "math 133a" :title	"ord diff eq"	        :section "02" :code	"40622"  :credit	"3"	:type "sem"	:capacity "40/42"						:days "mw"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 323"	        :professor "r kubelka"                  )
(:name "math 071w" :title	"calc wkshp bus-avi"	:section "04" :code	"48027"  :credit	"1"	:type "lab"	:capacity "25/32"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 233"	        :professor "r kubelka"                  )


(:name "math 031w" :title	"calculus ii workshop"	:section "12" :code "46834" :credit	         "1"	:type "lab" 	:capacity "29/32"						:days "tr"	:time "1500-1615"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "b lee"                                )
(:name "math 258" :title	"categorical data"	:section "01" :code	"50555"  :credit	"3"	:type "lec"	:capacity "18/30"						:days "mw"	:time "1730-1845"   :dates "08/22/12-12/10/12"	:location "sh 344"	        :professor "b lee"                                       )
(:name "math 161a" :title	"applied stats i"	:section "02" :code "40626"  :credit	"3"	:type "sem" :notes "97"	:capacity "33/37"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 320"	        :professor "b lee"                      )
(:name "math 030w" :title	"calculus i workshop"	:section "11" :code "45032" :credit	         "1"	:type "lab" 	:capacity "32/32"						:days "tr"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "b lee"                                )

(:name "math 006a" :title "entry level math i"	:section "13" :code	 "40567"  :credit	"0"	:type "act" 				:capacity "20/25"	    			:days "mw"	:time "1030-1120"			:location "sci 321"	    	:professor "y li"                                       )
(:name "math 006a" :title "entry level math i"	:section "11" :code	 "42164"  :credit	"0"	:type "act" 				:capacity "21/25"	    			:days "mw"	:time "0800-0850"		:location "sci 321"	    	:professor "y li"                                       )
(:name "math 006a" :title "entry level math i"	:section "12" :code	 "40566"  :credit	"0"	:type "act" 				:capacity "19/25"	    			:days "mw"	:time "0900-0950"		:location "sci 321"	    	:professor "y li"                                       )

(:name "math 031" :title	"calculus ii"	        :section "08"  :code "43640"  :credit	         "4"	:type "sem" 	:capacity "32/37"						:days "mtwr"	:time "1430-1520"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "r low"                                )
(:name "math 031" :title	"calculus ii"	        :section "09"  :code "45035" :credit	         "4"	:type "sem" 	:capacity "36/37"						:days "mtwr"	:time "1530-1620"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "r low"                                )
(:name "math 031" :title	"calculus ii"	        :section "11"  :code "46870" :credit	         "4"	:type "sem" 	:capacity "36/37"						:days "mwr"	:time "1800-1910"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "r low"                                )

(:name "math 031" :title	"calculus ii"	        :section "01" :code "40593"  :credit	         "4"	:type "sem" 	:capacity "42/37"						:days "mtwr"	:time "0730-0820"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "j lum"                                )

(:name "math 019w" :title	"precalculus workshop"	:section "16" :code	 "46696"  :credit	 "1"	:type "lab" 				:capacity "17/32"	                :days "tr"      :time "1800-1915"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "j maruskin"                           )
(:name "math 129a" :title	"linear algebra i"	:section "02" :code	"41526"  :credit	"3"	:type "sem" :notes "97,75" :capacity "44/50"						:days "mw"	:time "1730-1845"   :dates "08/22/12-12/10/12"	:location "dmh 234"	        :professor "j maruskin"                 )
(:name "math 177" :title	"lin nonlin optim"	:section "01" :code	"48362"  :credit	"3"	:type "sem"	:capacity "24/50"						:days "mw"	:time "1600-1715"   :dates "08/22/12-12/10/12"	:location "dmh 149b"	         :professor "j maruskin"                 )
(:name "math 019w" :title	"precalculus workshop"	:section "15" :code	 "44986"  :credit	 "1"	:type "lab" 				:capacity "21/32"	                :days "tr"      :time "1630-1745"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "j maruskin"                           )

(:name "math 006a" :title "entry level math i"	:section "22" :code	 "40571"  :credit	"0"	:type "act" 				:capacity "19/25"	    			:days "mw"	:time "0900-0950"			:location "dh 181"	    	:professor "r miller"                                   )


(:name "math 006a" :title "entry level math i"	:section "15" :code	 "42346"  :credit	"0"	:type "act" 				:capacity "16/25"	    					:days "mw"	:time "1330-1420"	:location "sci 321"	    	:professor "l mitchell"                                 )
(:name "math 008" :title	"college algebra"	:section "03" :code	 "41514"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "41/40"	                :days "mw"      :time "1500-1615"    :dates "08/22/12-12/10/12"	:location "dh 318"	    	:professor "l mitchell"                           )

(:name "math 012" :title	"number systems"	:section "01" :code	 "40581"  :credit	 "3"	:type "sem"  :notes "b4"				:capacity "38/40"	                :days "mw"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "mh 425"	    	:professor "w newball"                            )
(:name "math 030p" :title	"cal i with precal"	:section "05" :code "42422"    :credit	         "5"	:type "sem" 	:capacity "44/40"						:days "mtwr"	:time "1500-1605"   :dates "08/22/12-12/10/12"	:location "mh 224"		:professor "w newball"                            )
(:name "math 019" :title	"precalculus"	        :section "09" :code	 "41536"  :credit	 "5"	:type "sem"  	 :notes "b4,39,52"			:capacity "37/37"	                :days "mwf"     :time "0730-0850"    :dates "08/22/12-12/10/12"	:location "sh 347"	    	:professor "w newball"                            )

(:name "math 031w" :title	"calculus ii workshop"	:section "13"  :code "46871" :credit	         "1"	:type "lab" 	:capacity "24/32"						:days "tr"	:time "1630-1745"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "h ng"                                 )
(:name "math 167" :title	"programming in sas"	:section "01" :code	"43791"  :credit	"3"	:type "sem"	:capacity "24/28"						:days "tr"	:time "0730-0845"   :dates "08/22/12-12/10/12"	:location "cl 234"	        :professor "h ng"                       )
(:name "math 167" :title	"programming in sas"	:section "02" :code	"46500"  :credit	"3"	:type "sem"	:capacity "28/28"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "cl 234"	        :professor "h ng"                       )
(:name "math 030w" :title	"calculus i workshop"	:section "09"  :code "45030" :credit	         "1"	:type "lab" 	:capacity "33/32"						:days "tr"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "h ng"                                 )
(:name "math 071" :title	"calc for bus-aviat"	:section "09" :code	"48009"  :credit	"3"	:type "sem"	:capacity "41/40"						:days "tr"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "cl 234"	        :professor "h ng"                       )

(:name "math 019" :title	"precalculus"	        :section "07" :code	 "40590"  :credit	 "5"	:type "sem"  	 :notes "b4,39,52"	                :capacity "38/37"	                :days "mtwr"    :time "1630-1735"    :dates "08/22/12-12/10/12"	:location "dh 243"	    	:professor "a nguyen"                             )

(:name "math 003b" :title	"intensive learn ii"	:section "03" :code	 "50811"  :credit	"4"	:type "sem" 				:capacity "16/25"	    		:days "mtwr" :time "1130-1220"					:location "dh 303"	    	:professor "t nguyen"                                   )
(:name "math 003b" :title	"intensive learn ii"	:section "01" :code	 "50809"  :credit	"4"	:type "sem" 				:capacity "19/25"	    		:days "mtwr" :time "0900-0950"					:location "dh 303"	    	:professor "t nguyen"                                   )
(:name "math 003a" :title	"intensive learn i"	:section "16" :code	 "50311"  :credit	"4"	:type "sem" 	        		:capacity "16/25"	    		:days "mtwr" :time "1030-1120"					:location "dh 303"	    	:professor "t nguyen"       )

(:name "math 010" :title	"math for gen ed"	:section "07" :code	 "41535"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "36/40"	                :days "tr"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "cl 234"	    	:professor "v nguyen"                             )
(:name "math 010" :title	"math for gen ed"	:section "06" :code	 "41297"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "36/40"	                :days "tr"      :time "0900-1015"    :dates "08/22/12-12/10/12"	:location "mh 320"	    	:professor "v nguyen"                             )
(:name "math 010" :title	"math for gen ed"	:section "05" :code	 "41136"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "36/40"	                :days "tr"      :time "0730-0845"    :dates "08/22/12-12/10/12"	:location "mh 323"	    	:professor "v nguyen"                             )


(:name "math 030p" :title	"cal i with precal"	:section "02" :code "41519"  :credit	         "5"	:type "sem" 	:capacity "42/42"						:days "mtwr"	:time "0900-1005"   :dates "08/22/12-12/10/12"	:location "mh 224"		:professor "s obaid"                              )
(:name "math 031" :title	"calculus ii"	        :section "07"  :code "41633" :credit	         "4"	:type "sem" 	:capacity "42/42"						:days "mtwr"	:time "1330-1420"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "s obaid"                              )
(:name "math 132" :title	"adv calc i"	        :section "01" :code   "44006"  :credit	"3"	:type "lec"	:capacity "19/28"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "mh 235"	        :professor "s obaid"                    )

(:name "math 006l" :title	"entry level math"	:section "15" :code	 "42350"  :credit	"0"	:type "lab" 				:capacity "23/25"	    		:days "tr" :time "1130-1220"					:location "dh 181"	    	:professor "r ortiz"                                    )
(:name "math 006l" :title	"entry level math"	:section "14" :code	 "40574"  :credit	"0"	:type "lab" 				:capacity "24/25"	    		:days "tr" :time "1030-1120"					:location "dh 181"	    	:professor "r ortiz"                                    )
(:name "math 006l" :title	"entry level math"	:section "17" :code	 "42662"  :credit	"0"	:type "lab" 				:capacity "16/25"	    		:days "tr" :time "1230-1320"					:location "dh 181"	    	:professor "r ortiz"                                    )


(:name "math 070" :title	"finite math"	        :section "06" :code	"50684"  :credit	"3"	:type "sem" 	:capacity "38/40"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 323"	        :professor "l papay"                    )
(:name "math 070" :title	"finite math"	        :section "08" :code	"51009"  :credit	"3"	:type "sem"	:capacity "31/40"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "is 113"	        :professor "l papay"                    )


(:name "math 106" :title	"intuitive geometry"	:section "01" :code	"40608"  :credit	"3"	:type "sem" :notes "97"	:capacity "16/32"						:days "tr"	:time "0900-1030"   :dates "08/22/12-12/10/12"	:location "mh 425"	        :professor "b pence"                    )
(:name "math 107a" :title	"explor in algebra"	:section "01" :code	"40610"  :credit	"3"	:type "sem" :notes "102"	:capacity "11/28"						:days "tr"	:time "1600-1715"   :dates "08/22/12-12/10/12"	:location "mh 425"	        :professor "b pence"                    )
(:name "math 106" :title	"intuitive geometry"	:section "02" :code	"41524"  :credit	"3"	:type "sem" :notes "97"	:capacity "23/32"						:days "tr"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 425"	        :professor "b pence"                    )

(:name "math 031" :title	"calculus ii"	        :section "05"  :code "40597" :credit	         "4"	:type "sem" 	:capacity "41/42"						:days "mtwr"	:time "1130-1220"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "b peterson"                           )
(:name "math 030p" :title	"cal i with precal"	:section "04" :code "40592"  :credit	         "5"	:type "sem" 	:capacity "40/42"						:days "mtwr"	:time "1330-1435"   :dates "08/22/12-12/10/12"	:location "mh 224"		:professor "b peterson"                           )
(:name "math 126" :title	"theory of num i"	:section "01" :code  "41357"  :credit	"3"	:type "sem"	:capacity "26/32"						:days "tr"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "mh 235"	        :professor "b peterson"                 )

(:name "math 019" :title	"precalculus"	        :section "11" :code	 "44008"  :credit	 "5"	:type "sem"  :notes "b4"				:capacity "38/40"	                :days "mwf"     :time "1030-1150"    :dates "08/22/12-12/10/12"	:location "cl 234"	    	:professor "r pfiefer"                            )
(:name "math 115" :title	"mod geom & trans"	:section "01" :code	"48250"  :credit	"3"	:type "sem"	:capacity "17/32"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 235"	        :professor "r pfiefer"                  )
(:name "math 019" :title	"precalculus"	        :section "10" :code	 "44007"  :credit	 "5"	:type "sem"  :notes "b4"				:capacity "40/40"	                :days "mwf"     :time "0900-1020"    :dates "08/22/12-12/10/12"	:location "cl 234"	    	:professor "r pfiefer"                            )

(:name "math 019w" :title	"precalculus workshop"	:section "03" :code	 "44974"  :credit	 "1"	:type "lab" 				:capacity "35/32"	                :days "mw"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "f rivera"                             )
(:name "math 008w" :title	"coll algebra wkshp"	:section "01" :code	 "44963"  :credit	 "1"	:type "sem"       :notes "b4,97"			:capacity "37/40"	                :days "mw"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "mh 234"	    	:professor "f rivera"                             )
(:name "mted 394" :title	"sec school math"	:section "01" :code	 "40668"  :credit	 "3"	:type "sem"       :notes "b4,97"			:capacity "37/40"	                :days "t"      :time "1600-1845"    :dates "08/22/12-12/10/12"	:location "mh 426"	    	:professor "f rivera"                             )
;;(:name "math 106" :title	"intuitive geometry"	:section "80" :code	"40609"  :credit	"3"	:type "sem" :notes "101"	:capacity "25/32"						:days "tba"	:time "tba"	    :dates "08/22/12-12/10/12"	                :professor "f rivera"                   )

(:name "math 030w" :title	"calculus i workshop"	:section "06"  :code "45027" :credit	         "1"	:type "lab" 	:capacity "31/32"						:days "mw"	:time "1630-1745"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "c roddick"                            )
(:name "math 030w" :title	"calculus i workshop"	:section "05"  :code "45026" :credit	         "1"	:type "lab" 	:capacity "30/32"						:days "mw"	:time "1500-1615"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "c roddick"                            )
(:name "math 105" :title	"concepts in math"	:section "02" :code	"41523"  :credit	"3"	:type "sem" :notes "97"	:capacity "29/32"						:days "mw"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 425"	        :professor "c roddick"                  )
(:name "math 105" :title	"concepts in math"	:section "01" :code	"40607"  :credit	"3"	:type "sem" :notes "97"	:capacity "29/32"						:days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "mh 425"	        :professor "c roddick"                  )


(:name "math 105" :title	"concepts in math"	:section "03" :code	"41354"  :credit	"3"	:type "sem" :notes "102"	:capacity "14/32"						:days "tr"	:time "1730-1845"   :dates "08/22/12-12/10/12"	:location "mh 425"	        :professor "p rogers"                   )


(:name "math 031w" :title	"calculus ii workshop"	:section "01" :code  "45038"  :credit	         "1"	:type "lab" 	:capacity "11/32"						:days "mw"	:time "0730-0845"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "l roper"                              )
(:name "math 031w" :title	"calculus ii workshop"	:section "04"  :code "45044" :credit	         "1"	:type "lab" 	:capacity "29/32"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "l roper"                              )
(:name "math 012" :title	"number systems"	:section "03" :code	 "40583"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52,97"	        :capacity "25/32"	                :days "tr"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "mh 425"	    	:professor "l roper"                              )
(:name "math 031w" :title	"calculus ii workshop"	:section "02" :code  "45042" :credit	         "1"	:type "lab" 	:capacity "31/32"						:days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "l roper"                              )
;;(:name "math 110l" :title	"math computing lab"	:section "03" :code	"40613"  :credit	"1"	:type "lab"	:capacity "7/10"						 :days "tba"	:time "tba"	    :dates "08/22/12-12/10/12"	:location "mh 221"	        :professor "l roper"                    )
(:name "math 012" :title	"number systems"	:section "04" :code	 "40584"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"	                :capacity "34/32"	                :days "tr"      :time "1330-1445"    :dates "08/22/12-12/10/12"	:location "mh 425"	    	:professor "l roper"                              )
(:name "math 031w" :title	"calculus ii workshop"	:section "03"  :code "45043" :credit	         "1"	:type "lab" 	:capacity "34/32"						:days "mw"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "l roper"                              )


(:name "math 019w" :title	"precalculus workshop"	:section "04" :code	 "44975"  :credit	 "1"	:type "lab" 				:capacity "33/32"	                :days "mw"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "k ruiz"                               )
(:name "math 031w" :title	"calculus ii workshop"	:section "14" :code "48407" :credit	         "1"	:type "lab" 	:capacity "13/32"						:days "tr"	:time "1800-1915"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "k ruiz"                               )
(:name "math 031w" :title	"calculus ii workshop"	:section "10" :code  "45055" :credit	         "1"	:type "lab" 	:capacity "29/32"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "k ruiz"                               )
(:name "math 008" :title	"college algebra"	:section "09" :code	 "46872"  :credit	 "3"	:type "sem"       :notes "b4"				:capacity "38/40"	                :days "tr"      :time "1630-1745"    :dates "08/22/12-12/10/12"	:location "dmh 164"	    	:professor "k ruiz"                               )


(:name "math 031w" :title	"calculus ii workshop"	:section "05" :code "45045" :credit	         "1"	:type "lab" 	:capacity "28/32"						:days "mw"	:time "1500-1615"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "m saleem"                             )
(:name "math 032" :title	"calculus iii"	        :section "02"  :code "40599" :credit	         "3"	:type "sem" 	:capacity "40/42"						:days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "mh 320"		:professor "m saleem"                             )
(:name "math 031w" :title	"calculus ii workshop"	:section "06"  :code "45049" :credit	         "1"	:type "lab" 	:capacity "33/32"						:days "mw"	:time "1630-1745"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "m saleem"                             )
(:name "math 112" :title	"vector calculus"	:section "01" :code	"43787"  :credit	"3"	:type "lec"	:capacity "24/32"						:days "mw"	:time "0730-0845"   :dates "08/22/12-12/10/12"	:location "mh 235"	        :professor "m saleem"                   )

(:name "math 031w" :title	"calculus ii workshop"	:section "17"  :code "50803" :credit	         "1"	:type "lab" 	:capacity "15/32"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "dh 547"		:professor "r sanders"                            )
(:name "math 031w" :title	"calculus ii workshop"	:section "16" :code "50802"  :credit	         "1"	:type "lab" 	:capacity "21/32"						:days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "dh 547"		:professor "r sanders"                            )
(:name "math 042w" :title	"discrete math wksp"	:section "01"  :code "45083"  :credit	         "1"	:type "lab" 	:capacity "20/30"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "r sanders"                            )


(:name "math 030w" :title	"calculus i workshop"	:section "08" :code "45029" :credit	         "1"	:type "lab" 	:capacity "22/32"						:days "tr"	:time "0730-0845"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "e schmeichel"                         )
(:name "math 019w" :title	"precalculus workshop"	:section "17" :code	 "46697"  :credit	 "1"	:type "lab" 				:capacity "34/32"	                :days "tr"      :time "0900-1015"    :dates "08/22/12-12/10/12"	:location "mh 234"	    	:professor "e schmeichel"                         )
(:name "math 042" :title	"discrete math"	        :section "03"  :code "43646" :credit	         "3"	:type "sem" 	:capacity "39/40"						:days "tr"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 320"		:professor "e schmeichel"                         )
(:name "math 142" :title	"intro to comb"	        :section "01" :code	"48271"  :credit	"3"	:type "sem"	:capacity "20/28"						:days "tr"	:time "1600-1715"   :dates "08/22/12-12/10/12"	:location "mh 320"	        :professor "e schmeichel"               )
(:name "math 042" :title	"discrete math"	        :section "04"  :code "45499" :credit	         "3"	:type "sem" 	:capacity "38/40"						:days "tr"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "mh 323"		:professor "e schmeichel"                         )


(:name "math 019" :title	"precalculus"	        :section "15" :code	 "46695"  :credit	 "5"	:type "sem" :notes "b4"				:capacity "42/37"	                :days "mwr"     :time "1630-1750"    :dates "08/22/12-12/10/12"	:location "cl 234"	    	:professor "l sega"                               )
(:name "math 010" :title	"math for gen ed"	:section "09" :code	 "43273"  :credit	 "3"	:type "sem"  :notes "b4"				:capacity "41/40"	                :days "tr"      :time "1330-1445"    :dates "08/22/12-12/10/12"	:location "sh 347"	    	:professor "l sega"                               )
(:name "math 010" :title	"math for gen ed"	:section "08" :code	 "44005"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "39/40"	                :days "tr"      :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "dh 318"	    	:professor "l sega"                               )


(:name "math 030w" :title	"calculus i workshop"	:section "15"  :code "50606" :credit	         "1"	:type "lab" 	:capacity "36/32"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "dh 547"		:professor "s simic"                              )
(:name "math 030w" :title	"calculus i workshop"	:section "03" :code "45024"  :credit	         "1"	:type "lab" 	:capacity "33/32"						:days "mw"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "s simic"                              )
;;(:name "math 110l" :title	"math computing lab"	:section "04" :code	"40614"  :credit	"1"	:type "lab"	:capacity "8/10"						        :days "tba"	:time "tba"	    :dates "08/22/12-12/10/12"	:location "mh 221"	        :professor "s simic"                    )
;;(:name "math 110l" :title	"math computing lab"	:section "05" :code	"40615"  :credit	"1"	:type "lab"	:capacity "10/10"						:days "tba"	:time "tba"	    :dates "08/22/12-12/10/12"	:location "mh 221"	        :professor "s simic"                    )
(:name "math 113" :title	"differential geom"	:section "01" :code	"48248"  :credit	"3"	:type "sem"	:capacity "13/32"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 235"	        :professor "s simic"                    )


(:name "math 019" :title	"precalculus"	        :section "03" :code	 "43565"  :credit	 "5"	:type "sem"  	 :notes "b4,39,52,97"               :capacity "33/37"                        :days "mtwr"    :time "1030-1135"    :dates "08/22/12-12/10/12"	:location "dh 243"	    	:professor "t smith"                              )
(:name "math 031" :title	"calculus ii"	        :section "02" :code "40594" :credit	         "4"	:type "sem" 	:capacity "41/37"						:days "mtwr"	:time "0830-0920"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "t smith"                              )
(:name "math 031" :title	"calculus ii"	        :section "06"  :code "41323" :credit	         "4"	:type "sem" 	:capacity "42/37"						:days "mtwr"	:time "1230-1320"   :dates "08/22/12-12/10/12"	:location "mh 424"		:professor "t smith"                              )


(:name "math 032w" :title	"calculus iii wkshp"	:section "01" :code      "45073" :credit	         "1"	:type "lab" 	:capacity "31/32"					:days "mw"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "mh 235"	:professor "w so"                                 )
(:name "math 129a" :title	"linear algebra i"	:section "08" :code	"49525"  :credit	"3"	:type "sem"	:capacity "40/42"						:days "mw"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "sh 345"	        :professor "w so"                       )
(:name "math 129a" :title	"linear algebra i"	:section "07" :code	"49524"  :credit	"3"	:type "sem"	:capacity "30/44"						:days "mw"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "cl 238"	        :professor "w so"                       )
(:name "math 229" :title	"adv matrix theory"	:section "01" :code	"48368"  :credit	"3"	:type "sem"	:capacity "13/28"						:days "mw"	:time "1730-1845"   :dates "08/22/12-12/10/12"	:location "mh 233"	        :professor "w so"                                       )
(:name "math 019w" :title	"precalculus workshop"	:section "06" :code	 "44977"  :credit	 "1"	:type "lab" 				:capacity "34/32"	                :days "mw"      :time "1500-1615"    :dates "08/22/12-12/10/12"	:location "wsq 001"	    	:professor "w so"                                 )

(:name "mted 184y" :title	"stdt tchg III"	        :section "01" :code	 "41939"  :credit	 "3"	:type "sem"  :notes "b4,97"			:capacity "35/37"	                :days "r"     :time "1600-1900"    :dates "08/22/12-12/10/12"	:location "mh 426"	    	:professor "sliva-spitzer"                               )


(:name "math 129a" :title	"linear algebra i"	:section "05" :code	"44250"  :credit	"3"	:type "sem"	:capacity "40/42"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "mh 423"	        :professor "m stanley"                  )
(:name "math 129a" :title	"linear algebra i"	:section "06" :code	"48257"  :credit	"3"	:type "sem"	:capacity "39/42"						:days "tr"	:time "1500-1615"   :dates "08/22/12-12/10/12"	:location "mh 423"	        :professor "m stanley"                  )
(:name "math 031w" :title	"calculus ii workshop"	:section "09"  :code "45054"  :credit	         "1"	:type "lab" 	:capacity "31/32"						:days "tr"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "m stanley"                            )
(:name "math 031w" :title	"calculus ii workshop"	:section "11"  :code "45059" :credit	         "1"	:type "lab" 	:capacity "33/32"						:days "tr"	:time "1200-1315"   :dates "08/22/12-12/10/12"	:location "wsq 001"		:professor "m stanley"                            )
(:name "math 129b" :title	"linear algebra ii"	:section "01" :code	"45293"  :credit	"3"	:type "sem"	:capacity "12/32"						:days "tr"	:time "1730-1845"   :dates "08/22/12-12/10/12"	:location "mh 235"	        :professor "m stanley"                  )


(:name "math 030p" :title	"cal i with precal"	:section "03" :code "40591" :credit	         "5"	:type "sem" 	:capacity "39/40"						:days "mtwr"	:time "1200-1305"   :dates "08/22/12-12/10/12"	:location "mh 224"		:professor "a strong"                             )
(:name "math 032" :title	"calculus iii"	        :section "05"  :code "41324" :credit	         "3"	:type "sem" 	:capacity "37/40"						:days "mw"	:time "1500-1615"   :dates "08/22/12-12/10/12"	:location "mh 323"		:professor "a strong"                             )
(:name "math 032" :title	"calculus iii"	        :section "10"  :code "49537" :credit	         "3"	:type "sem" 	:capacity "37/40"						:days "mw"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "dmh 358"		:professor "a strong"                             )


(:name "math 019" :title	"precalculus"	        :section "08" :code	 "41518"  :credit	 "5"	:type "sem"  	 :notes "b4,39,52"			:capacity "39/50"	                :days "mtr"     :time "1800-1920"    :dates "08/22/12-12/10/12"	:location "dh 515"	    	:professor "a talebi"                             )
(:name "math 042" :title	"discrete math"	        :section "05"  :code "46694"    :credit	         "3"	:type "sem" 	:capacity "51/50"						:days "tr"	:time "1630-1745"   :dates "08/22/12-12/10/12"	:location "dh 515"		:professor "a talebi"                             )


(:name "math 006a" :title "entry level math i"	:section "10" :code	 "42163"  :credit	"3"	:type "sem" :notes "97"	        		:capacity "89/150"	    		:days "tr"	:time "0800-0850"				:location "sci 142"	    	:professor "p tanniru"                                  )
(:name "math 010" :title	"math for gen ed"	:section "01" :code	 "40578"  :credit	 "3"	:type "lab" 				:capacity "26/32"	                :days "mw"      :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "sh 347"	    	:professor "p tanniru"                            )
(:name "math 006a" :title "entry level math i"	:section "20" :code	 "43005"  :credit	"3"	:type "sem" :notes "97"	        		:capacity "112/150"	    		:days "tr"      :time "1230-1320"				:location "sci 142"	    	:professor "p tanniru"                                  )

(:name "math 003a" :title	"intensive learn i"	:section "11" :code	 "44945"  :credit	"4"	:type "sem" :notes "97"	        		:capacity "21/25"	    	:days "mtwr"	:time "1230-1320"					:location "dh 303"	    	:professor "a tran"         )
(:name "math 003a" :title	"intensive learn i"	:section "12" :code	 "45541"  :credit	"4"	:type "sem" :notes "97"	        		:capacity "17/25"	    	:days "mtwr"	:time "1330-1420"					:location "dh 303"	    	:professor "a tran"         )


(:name "math 003a" :title	"intensive learn i"	:section "18" :code	 "50313"  :credit	"4"	:type "sem" 				:capacity "11/25"	    		:days "mtwr"	:time "1130-1220"				:location "dh 219"	    	:professor "q tran"         )
(:name "math 003a" :title	"intensive learn i"	:section "08" :code	 "40563"  :credit	"4"	:type "sem" :notes "97"	    		:capacity "21/25"	    		:days "mtwr"	:time "1030-1120"				:location "dh 219"	    	:professor "q tran"         )
(:name "math 003a" :title	"intensive learn i"	:section "19" :code	 "50777"  :credit	"4"	:type "sem" 				:capacity "0/25"	    		:days "mtwr"	:time "0800-0850"				        :location "mh 233"      :professor "q tran"                                            	)
(:name "math 006l" :title	"entry level math"	:section "21" :code	 "50899"  :credit	 "0"	:type "sem" 				:capacity "21/25"	                :days "tr"      :time "0900-0950"    :dates "08/22/12-12/10/12"	:location "mh 323"	    	:professor "q tran"                               )
(:name "math 006l" :title	"entry level math"                           :code	 "50898"  :credit	 "5"	                		                                        :days "mw"      :time "0900-1012"    :dates "08/22/12-12/10/12"	:location "mh 233"	    	:professor "q tran"                               )


(:name "math 012" :title	"number systems"	:section "02" :code	 "40582"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52,97"	        :capacity "32/32"	                :days "mw"      :time "1600-1715"    :dates "08/22/12-12/10/12"	:location "mh 426"	    	:professor "j trubey"                             )

(:name "math 010" :title	"math for gen ed"	:section "03" :code	 "40580"  :credit	 "3"	:type "sem"  	 :notes "b4,39,52"			:capacity "40/40"	                :days "mw"      :time "1500-1615"    :dates "08/22/12-12/10/12"	:location "mh 423"	    	:professor "m van-der-poel"                       )
(:name "math 019" :title	"precalculus"	        :section "13" :code	 "44176"  :credit	 "5"	:type "sem"  :notes "b4"				:capacity "37/37"	                :days "mwf"     :time "1330-1450"    :dates "08/22/12-12/10/12"	:location "sh 347"	    	:professor "m van-der-poel"                       )


(:name "math 030p" :title	"cal i with precal"	:section "07" :code      "49535" :credit	         "5"	:type "sem" 	:capacity "43/40"						:days "mtwr"	:time "0730-0835"   :dates "08/22/12-12/10/12"	:location "mh 423"		:professor "m vartanian"                          )
(:name "math 030" :title	"calculus i"	        :section "03" :code	 "42434"  :credit	 "3"	:type "sem" 	:notes "39,42,52,97"	        :capacity "48/40"	                :days "tr"      :time "0900-1015"    :dates "08/22/12-12/10/12"	:location "spxe 077"    	:professor "m vartanian"                          )

(:name "math 030" :title	"calculus i"	        :section "05" :code      "44255" :credit                      	         "3"	:type "sem" 	:capacity "38/40"						:days "tr"	:time "1330-1445"   :dates "08/22/12-12/10/12"	:location "is 113"		:professor "s vergara"                            )
(:name "unvs 015a" :title	"statway i"	        :section "01" :code	"48730"  :credit	"6"	:type "sem"	:capacity "10/10"						:days "mtwr"	:time "1030-1145"	    :dates "08/22/12-12/10/12"	:location "cl 225"	        :professor "s vergara"                  )
(:name "unvs 015a" :title	"statway i"	        :section "02" :code	"50477"  :credit	"6"	:type "sem"	:capacity "10/10"						:days "mtwr"	:time "1200-1315"	    :dates "08/22/12-12/10/12"	:location "cl 225"	        :professor "s vergara"                  )

;;(:name "math 015a" :title	"statway a"	        :section "01" :code	 "50528"  :credit	 "5"	:type "sem"  	 :notes "b4,39,97"	                :capacity "33/32"	                :days "mtwr"    :time "1030-1145"    :dates "08/22/12-12/10/12"	:location "cl 225"	    	:professor "s vergara")
;;(:name "math 015a" :title	"statway a"	        :section "02" :code	 "50529"  :credit	 "5"	:type "lec" 		                :capacity "0/30"	                        :days "mtwr"    :time "1200-1315"    :dates "08/22/12-12/10/12"	:location "cl 225"	    	:professor "s vergara"                            )
;;(:name "math 110l" :title	"math computing lab"	:section "02" :code	"40612"  :credit	"1"	:type "lab"	:capacity "10/10"						:days "tba"	:time "tba"	    :dates "08/22/12-12/10/12"	:location "mh 221"	        :professor "s vergara"                  )

(:name "math 070" :title	"finite math"	        :section "05" :code	"49523"  :credit	"3"	:type "sem" 	:capacity "44/46"						:days "tr"	:time "0900-1015"   :dates "08/22/12-12/10/12"	:location "sh 347"	        :professor "j wang"                     )
(:name "math 019" :title	"precalculus"	        :section "01" :code	 "40585"  :credit	 "5"	:type "lec" 		                :capacity "0/30"	                :days "mtwr"    :time "0730-0835"    :dates "08/22/12-12/10/12"	:location "dh 243"	    	:professor "j wang")


(:name "math 006l" :title	"entry level math"	:section "12" :code	 "40572"  :credit	"0"	:type "lab" 				:capacity "24/25"	    		:days "tr" :time "1200-1250"					:location "sci 321"	    	:professor "jiayin wang"                                     )
(:name "math 006l" :title	"entry level math"	:section "18" :code	 "46320"  :credit	"0"	:type "lab" 				:capacity "23/25"	    		:days "tr" :time "0830-0920"					:location "dh 181"	    	:professor "jiayin wang"                                     )
(:name "math 006l" :title	"entry level math"	:section "19" :code	 "46321"  :credit	"0"	:type "lab" 				:capacity "23/25"	    		:days "tr" :time "0930-1020"					:location "dh 181"	    	:professor "jiayin wang"                                     )


(:name "math 006l" :title	"entry level math"	:section "16" :code	 "42351"  :credit	"0"	:type "lab" 				:capacity "18/25"	    		:days "tr"  :time "1200-1250"					:location "sci 321"	    	:professor "x yin"                                      )
(:name "math 006l" :title	"entry level math"	:section "11" :code	 "41484"  :credit	"0"	:type "lab" 				:capacity "23/25"	    		:days "tr"  :time "0830-0920"					:location "dh 181"	    	:professor "x yin"                                      )
(:name "math 006l" :title	"entry level math"	:section "13" :code	 "40573"  :credit	"0"	:type "lab" 				:capacity "23/25"	    		:days "tr"  :time "0930-1020"					:location "dh 181"	    	:professor "x yin"                                      )


(:name "math 030w" :title	"calculus i workshop"	:section "13" :code	 "45034"  :credit	"1.5"	:type "lab" 				:capacity "23/25"	    		:days "tr"  :time "1630-1745"					:location "wsq 1"	    	:professor "andrew yu")
(:name "math 030w" :title	"calculus i workshop"	:section "12" :code	 "45033"  :credit	"1.5"	:type "lab" 				:capacity "23/25"	    		:days "tr"  :time "1500-1615"					:location "wsq 1"	    	:professor "andrew yu")
(:name "math 030w" :title	"calculus i workshop"	:section "10" :code	 "45031"  :credit	"1.5"	:type "lab" 				:capacity "23/25"	    		:days "tr"  :time "1030-1145"					:location "wsq 1"	    	:professor "andrew yu")


(:name "math 019" :title	"precalculus"	        :section "04" :code	 "40587"  :credit	 "5"	:type "sem"  	 :notes "b4,39,52"	                :capacity "38/37"	                :days "mtwr"    :time "1200-1305"    :dates "08/22/12-12/10/12"	:location "dh 243"	    	:professor "e zabric"                             )
(:name "math 019" :title	"precalculus"	        :section "05" :code	 "40588"  :credit	 "5"	:type "sem"  	 :notes "b4,39,52"	                :capacity "37/37"	                :days "mtwr"    :time "1330-1435"    :dates "08/22/12-12/10/12"	:location "dh 243"	    	:professor "e zabric"                             )
(:name "math 071" :title	"calc for bus-aviat"	:section "08" :code	"46451"  :credit	"3"	:type "sem"	:capacity "39/40"						                :days "tr"	:time "1030-1145"   :dates "08/22/12-12/10/12"	:location "mh 323"	        :professor "e zabric"                   )

(:name "math 003a" :title	"intensive learn i"	:section "05" :code	 "40560"  :credit	"4"	:type "sem" :notes "97"	    		:capacity "21/25"	  				:days "mtwr"	:time "0930-1020"		:location "dh 219"	  	:professor "m zoubeidi"   )
(:name "math 003a" :title	"intensive learn i"	:section "02" :code	 "40558"  :credit	"4"	:type "sem" :notes "97"	    		:capacity "12/25"	  				:days "mtwr"	:time "0830-0920"			:location "dh 219"	  	:professor "m zoubeidi"   )
(:name "math 003b" :title	"intensive learn ii"	:section "02" :code	 "50810"  :credit	"4"	:type "sem" 				:capacity "14/25"	    				:days "mtwr"	:time "1230-1320"			:location "dh 219"	    	:professor "m zoubeidi"                                 )

)))

