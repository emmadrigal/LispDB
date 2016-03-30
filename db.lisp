(defun isKeyAvaliable (privateKey Table)
;; Checks if a private key has already been used inside a table
     (cond ((> (list-lenght Table) 0)   
            (cond ((eq privateKey (cadar Table))
                  (nil))
            (t (isKeyAvaliable (cdr Table) ) )
            ))
     (t t);;Table is empty, key is available
     )
)

(defun appendNewRecord (command Table)
;;Recieves a command to add a new record and a command to add it to the table
     (cond ((isKeyAvaliable (car command) Table);;Checks availability of the pk
            (append Table command))
            (t Table);;If the key isn't available don't modify the record
     )
)

(defun newRecord (command dataBase) (
;;Adds a new record into the dataBase, this assumes that the command being recieved already recieves the data in the correct format
      (cond ((tableExists (car command) dataBase);;Checks existance of the table
            ;;Appends the dataBase minus the modified table and the modified table with the new record
            (append (tableDelete (car command) dataBase '())
                    (appendNewRecord (cdr command) (returnTable (car command) dataBase))))
            (t dataBase));;If the table doesn't exist no action is executed
)

(defun tableDelete (tableName dataBase newDB)
;;Deletes a table if it exists inside of the dataBase. The newDB is used to storing the values that have already been evaluated, on the first call it must be an empty list
;;This function doesn verify if the specific table is empty
      (cond ((> list-lenght dataBase 0)
            (cond ((string-equal (caaar dataBase) tableName)
                  (append newDB (cdr dataBase)))
                  (t (tableDelete tableName (cdr dataBase) (append newDB (car dataBase))))))
            (t newDB))
)

(defun returnTable (tableName dataBase)
;;Returns a specific table, existance of the table has already been tested
      (cond ((eq tableName (caaar Database))  (car Database));;Table name already used, dataBase is composed of tables, first entry in table represents its composition, first entry of the composition is the name
            (t (tableExists (cdr Database))))  ;;Checks for the next table in the DB;;dataBase is empty, table doesn't exist
)

(defun tableExists (tableName dataBase)
;; Checks if the name of the table has already been used in an existing table
           (cond ((> (list-lenght database) 0)
               (cond ((eq tableName (caaar Database))  t);;Table name already used, dataBase is composed of tables, first entry in table represents its composition, first entry of the composition is the name
                     (t (tableExists (cdr Database)))
               )
           )    ;;Checks for the next table in the DB
           (t nil)
     );;dataBase is empty, table doesn't exist
)


(defun addTable (command dataBase)
     ;;Adds a new table to the dataBase as long as it contains the required fields, recieves the information without the addt/addtable command
     (cond ((tableExists (car command) dataBase) (print "Invalid name for table, already in use"))
           (t (readFromUser (append dataBase command)))
     )
)


(defun functSelect (command dataBase)
     ;;Reading the first parameter from de CLI it decides which function is being recieved and acts accordingly
     (cond ((or (string-equal (car command) "addt") (string-equal (car command) "addtable"))        (addTable (cdr command) dataBase))
           ((or (string-equal (car command) "addr") (string-equal (car command) "addReference"))    (print "Se leyó creación de nueva record"))
           ((or (string-equal (car command) "remr") (string-equal (car command) "removeReference")) (print "Se leyó eliminacion de referencia"))
           ((or (string-equal (car command) "ins")  (string-equal (car command) "insert"))          (newRecord (cdr command) dataBase))
           ((or (string-equal (car command) "ud")   (string-equal (car command) "update"))          (print "Se leyó el update de un record"))
           ((or (string-equal (car command) "rr")   (string-equal (car command) "remover"))         (print "Se leyó eliminacion de un record"))
           ((or (string-equal (car command) "dt")   (string-equal (car command) "deltable"))        (print "Se leyó eliminacion de una tabla"))
           ((string-equal (car command) "query")                                                    (print "Se leyó un query"))
           ((string-equal (car command) "cproc")                                                    (print "Se leyó cproc"))
           ((string-equal (car command) "eval")                                                     (print "Se leyó eval"))
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
;; Marks which characters work as a delimiter when revieving input from CL


(defun my-read () (my-split (READ-LINE)))

(defun readFromUser (dataBase)
     ;; Cicle that mantains the dataBase in the stack
     (functSelect (my-read) dataBase)
     (readFromUser dataBase)
)

(readFromUser '())