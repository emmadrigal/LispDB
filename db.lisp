

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
     ;;Adds a new table to the dataBase as long as it contains the required fields
     (cond (((tableExists (car command)) dataBase) (print "Invalid name for table"))
           (append dataBase '(command) )
     )
)


(defun functSelect (command)
     ;;Reading the first parameter from de CLI it decides which function is being recieved and acts accordingly
     (cond ((or (string-equal (car command) "addt") (string-equal (car funct) "addtable"))        (print "Se leyó creación de nueva tabla"))
           ((or (string-equal (car command) "addr") (string-equal (car funct) "addReference"))    (print "Se leyó creación de nueva referencia"))
           ((or (string-equal (car command) "remr") (string-equal (car funct) "removeReference")) (print "Se leyó eliminacion de referencia"))
           ((or (string-equal (car command) "ins")  (string-equal (car funct) "insert"))          (print "Se leyó creación de nuevo record"))
           ((or (string-equal (car command) "ud")   (string-equal (car funct) "update"))          (print "Se leyó el update de un record"))
           ((or (string-equal (car command) "rr")   (string-equal (car funct) "remover"))         (print "Se leyó eliminacion de un record"))
           ((or (string-equal (car command) "dt")   (string-equal (car funct) "deltable"))        (print "Se leyó eliminacion de una tabla"))
           ((string-equal (car command) "query")                                                  (print "Se leyó un query"))
           ((string-equal (car command) "cproc")                                                  (print "Se leyó cproc"))
           ((string-equal (car command) "eval")                                                   (print "Se leyó eval"))
           ((string-equal (car command) "showall")                                                (print "Mostrando toda la DB"))
           (t                          (print "Unknown command"))
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


(defun my-read (data) (my-split data))

(defun readFromUser (dataBase)
     ;; Cicle that mantains the dataBase in the stack
     (functSelect (my-read (READ-LINE)))
     (readFromUser dataBase)
)

(readFromUser '())