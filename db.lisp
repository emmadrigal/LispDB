

(defun tableExists (tableName, dataBase)
     ;; Checks if the name of the table has already been used in an existing table, recursively goes through the tables existing in the dataBase
     (cond ((list-lenght database) > 0) 
          (cond ((eq tableName (caaar Database))  t);;Table name already used, dataBase is composed of tables, first entry in table represents its composition, first entry of the composition is the name
                (t (tableExists (cdr Database)))    ;;Checks for the next table in the DB
          )
          (t nil);;dataBase is empty, table doesn't exist
     )
)


(defun addTable (name, dataBase)
     ;;Adds a new table to the dataBase as long as it contains the required fields
     (cond ((tableExists name dataBase) (print "Invalid name for table"))
           (append dataBase '((name)) )
     )
)


(defun functSelect (funct)
     ;;Reading the first parameter from de CLI it decides which function is being recieved and acts accordingly
     (cond ((eq funct 'addt)            (print "Se leyó creación de nueva tabla"))
           ((eq funct 'addtable)        (print "Se leyó creación de nueva tabla"))
           ((eq funct 'addr)            (print "Se leyó creación de nueva referencia"))
           ((eq funct 'addReference)    (print "Se leyó creación de nueva referencia"))
           ((eq funct 'remr)            (print "Se leyó eliminacion de referencia"))
           ((eq funct 'removeReference) (print "Se leyó eliminacion de referencia"))
           ((eq funct 'ins)             (print "Se leyó creación de nuevo record"))
           ((eq funct 'insert)          (print "Se leyó el update de un record"))
           ((eq funct 'up)              (print "Se leyó el update de un record"))
           ((eq funct 'update)          (print "Se leyó creación de nueva record"))
           ((eq funct 'rr)              (print "Se leyó creación de nuevo record"))
           ((eq funct 'remover)         (print "Se leyó creación de nueva record"))
           ((eq funct 'dt)              (print "Se leyó creación de nuevo record"))
           ((eq funct 'deltable)        (print "Se leyó creación de nueva record"))
           ((eq funct 'query)           (print "Se leyó creación de nuevo record"))
           ((eq funct 'cproc)           (print "Se leyó creación de nuevo record"))
           ((eq funct 'eval)            (print "Se leyó creación de nueva record"))
           (t                          (print "Comando desconocido"))
           )
)

(defun readFromUser (dataBase)
     ;; Cicle that mantains the dataBase in the stack
     (functSelect (read))
     (readFromUser dataBase)
)

(readFromUser '())
