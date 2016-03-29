
(defun tableExists (tableName dataBase)
     ;; Checks if the name of the table has already been used in an existing table, recursively goes through the tables existing in the dataBase
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
           ((or (string-equal (car command) "ins")  (string-equal (car command) "insert"))          (print "Se leyó creación de nuevo record"))
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