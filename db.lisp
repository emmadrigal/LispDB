(defun isKeyAvaliable (privateKey Table)
;; Checks if a private key has already been used inside a table
     (cond ((> (list-length Table) 0)   
            (cond ((string-equal privateKey (caar Table))
                  nil)
            (t (isKeyAvaliable privateKey (cdr Table) ) )
            ))
     (t t);;Table is empty, key is available
     )
)

(defun returnRecord (privateKey Table)
;;Returns a specific record, existance of the record has already been proved
      (cond ((string-equal privateKey (caar Table))  (car Table));;Since the private key is unique, a match is the record being looked for
            (t (returnRecord privateKey (cdr Table))));;Checks the next record in the table
)

(defun returnTable (tableName dataBase)
;;Returns a specific table, existance of the table has already been proved
      (cond ((string-equal tableName (caaar Database))   (car Database));;Table name already used, dataBase is composed of tables, first entry in table represents its composition, first entry of the composition is the name
            (t (returnTable tableName (cdr Database))))  ;;Checks for the next table in the DB;;dataBase is empty, table doesn't exist
)

(defun isPKSafe (modifications privateKey)
;;When a record is being modified, it verifies that the privateKey isnt being modified, privateKey referes to the generic name asigned
      (cond ((> (list-length modifications) 0)
            (cond ((string-equal (car modifications) privateKey) nil)
                  (t (isPKSafe (cddr modifications) privateKey)) ))
      (t t))
)

(defun changeOneEntry (tableTitle oldRecord modifications newRecord)
;;Goes through the table and changes the specific one required, can make one modification
      (cond ( (> (list-length tableTitle) 0)
            (cond ((string-equal (car tableTitle) (car modifications))
                  (append newRecord (list (cadr modifications)) (cdr oldRecord) ))
            (t (changeOneEntry (cdr tableTitle) (cdr oldRecord) modifications (append newRecord (list (car oldRecord)))))))
      (t newRecord))
)

(defun editRecord (modifications record tableTitle)
;;Applies the changeOneEntry to all the requested modifications
      (cond ( (> (list-length modifications) 0)
                  (editRecord  (cddr modifications) (changeOneEntry tableTitle record modifications '())  tableTitle))
            (t record))
)


(defun modifyRecord (command Table)
;;Modifies an entry making the required checks, returns the modified table
      (cond ((not (isKeyAvaliable (car command) Table))
            (cond ((evenp (list-length (cdr command)))
                      (cond ((isPKSafe (cdr command) (caar Table)) 
                           (append  (recordDelete (car command) Table '()) (list (editRecord (cdr command) (returnRecord (car command) Table)  (cdar Table))) ))
                      (t (print "Invalid command id being modified"))))
                  (t (print "Invalid number of arguments"))))
      (t (print "Record doesn't exist"))  )
)


(defun recordModifier (command dataBase)
;;modifies records with information based on the information recieved from the user
      (cond ((tableExists (car command) dataBase);;Checks existance of the table
            ;;Appends the dataBase minus the modified table and the modified table with the new record
                  (append (tableDelete (car command) dataBase '())
                        (list (modifyRecord (cdr command) (returnTable (car command) dataBase)))))
            (t dataBase));;If the table doesn't exist no action is executed
)

(defun appendNewRecord (command Table)
;;Recieves a command to add a new record and a command to add it to the table
     (cond ((isKeyAvaliable (car command) Table);;Checks availability of the pk
                  (append Table (list command)))
            (t Table);;If the key isn't available don't modify the record
     )
)

(defun addRecordError ( dataBase)
    (print "Requested table doesn't exist")
    (cond (t dataBase))
)

(defun newRecord (command dataBase)
;;Adds a new record into the dataBase, this assumes that the command being recieved already recieves the data in the correct format
      (cond ((tableExists (car command) dataBase);;Checks existance of the table
            ;;Appends the dataBase minus the modified table and the modified table with the new record
                  (append (tableDelete (car command) dataBase '())
                        (list (appendNewRecord (cdr command) (returnTable (car command) dataBase) ))))
            (t (addRecordError  dataBase)));;If the table doesn't exist no action is executed
)


(defun recordDelete (privateKey Table newTable)
;;Recieves the table without the information entry, and deletes a record
      (cond ((> (list-length Table) 0)
            (cond ((string-equal (caar Table) privateKey)
                        (append newTable (cdr Table)))
                  (t 
                        (recordDelete privateKey (cdr Table) (append newTable (list (car Table)))))
            ))
            (t newTable))
)

(defun recordDeleterError (error dataBase)
    (print error)
    (cond (t database))
)

(defun recordDeleter (command dataBase)
;;Deletes a record when requested by the user, doesn't verify the reference by a foreign record since this still hasn't been implemented
      (cond  ( (tableExists (car command) dataBase) 
                  (cond ((not (isKeyAvaliable (cadr command) (returnTable (car command) dataBase))) 
                              (append (tableDelete (car command) dataBase '())
                                    (list (recordDelete (cadr command) (returnTable (car command) dataBase) '() ))))
                        (t (recordDeleterError "sorry, the requested record doesn't exist" dataBase)) ))
            (t (recordDeleterError "sorry, the requested table doesn't exist" dataBase))
      )
)

(defun tableDelete (tableName dataBase newDB)
;;Deletes a table if it exists inside of the dataBase. The newDB is used to storing the values that have already been evaluated, on the first call it must be an empty list
;;This function doesn verify if the specific table is empty
      (cond ((> (list-length dataBase) 0)
            (cond ((string-equal (caaar dataBase) tableName)
                        (append newDB (cdr dataBase)))
                  (t 
                        (tableDelete tableName (cdr dataBase) (append newDB (list (car dataBase)))))
            ))
            (t newDB))
)

(defun tableDeleteError (error dataBase)
    (print error)
    (cond (t dataBase))
)

(defun tableDeleter (command dataBase)
;;Deletes a table based upon data given by the user
      (cond ((tableExists (car command) dataBase)
                  (cond ((< (list-length (returnTable (car command) dataBase)) 2)
                              (tableDelete (car command) dataBase '())) 
                        (t (tableDeleteError "you must empty the table before deleting it" dataBase))))
            (t (tableDeleteError "Sorry the table doesn't exist" dataBase)))
)

(defun tableExists (tableName dataBase)
;; Checks if the name of the table has already been used in an existing table
           (cond ((> (list-length database) 0)
               (cond ((string-equal tableName (caaar Database))  t);;Table name already used, dataBase is composed of tables, first entry in table represents its composition, first entry of the composition is the name
                     (t (tableExists tableName (cdr Database)))
               )
           )    ;;Checks for the next table in the DB
           (t nil)
     );;dataBase is empty, table doesn't exist
)

(defun addTableError (dataBase) 
;;Called when an insertion into the database fails
    (print "Invalid name for table, already in use")
    (cond (t dataBase))
)

(defun addTable (command dataBase)
;;Adds a new table to the dataBase as long as it contains the required fields, recieves the information without the addt/addtable command
     (cond ((tableExists (car command) dataBase) (addTableError dataBase))
           (t (append dataBase (list (list command))))
     )
)

(defun functSelect (command dataBase)
;;Reading the first parameter from de CLI it decides which function is being recieved and acts accordingly
     (cond ((or (string-equal (car command) "addt") (string-equal (car command) "addtable"))        (readFromUser (addTable (cdr command) dataBase)))
           ((or (string-equal (car command) "addr") (string-equal (car command) "addReference"))    (print "Se leyó creación de nueva record"))
           ((or (string-equal (car command) "remr") (string-equal (car command) "removeReference")) (print "Se leyó eliminacion de referencia"))
           ((or (string-equal (car command) "ins")  (string-equal (car command) "insert"))          (readFromUser (newRecord (cdr command) dataBase)))
           ((or (string-equal (car command) "ud")   (string-equal (car command) "update"))          (readFromUser (recordModifier (cdr command) dataBase)))
           ((or (string-equal (car command) "rr")   (string-equal (car command) "remover"))         (readFromUser (recordDeleter (cdr command) dataBase)))
           ((or (string-equal (car command) "dt")   (string-equal (car command) "deltable"))        (readFromUser (tableDeleter (cdr command) dataBase)))
           ((string-equal (car command) "cproc")                                                    (print "Se leyó cproc"))
           ((string-equal (car command) "eval")                                                     (print "Se leyó eval"))
           ((string-equal (car command) "query")                                                    (print "Se leyó un query"))
           ((string-equal (car command) "showall")                                                  (print "Mostrando toda la DB"))
           (t (print "Unknown command"))
     )
)

(defun my-split (string &key (delimiterp #'delimiterp))
;; Splits a string into a list containing substrings, this division is stablished by delimiterp
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiterp (c) (or (char= c #\Space)))
;; Marks which characters work as a delimiter when revieving input from the CL


(defun my-read () (my-split (READ-LINE)))

(defun readFromUser (dataBase)
;; Cicle that mantains the dataBase in the stack
     (functSelect (my-read) dataBase)
)


(readFromUser '() )